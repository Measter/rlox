use codespan_reporting::diagnostic::Diagnostic;
use fnv::FnvHashMap as HashMap;
use lasso::{Rodeo, Spur};
use rlox::{
    ast::{ExpressionKind, Statement},
    program::{ExpressionId, Program, StatementId},
    source_file::{FileId, SourceLocation},
    token::{Token, TokenKind},
};

use crate::{
    chunk::{Chunk, ConstId, OpCode},
    value::{StringObject, Value},
};

type CompileResult<T> = Result<T, Diagnostic<FileId>>;

pub struct AstCompiler<'a> {
    interner: &'a Rodeo,
    program: &'a Program,
    chunk: Chunk,
    constant_ids: HashMap<Spur, ConstId>,
}

impl<'a> AstCompiler<'a> {
    pub fn compile(
        statements: &[StatementId],
        interner: &'a Rodeo,
        program: &'a Program,
        file_id: FileId,
    ) -> CompileResult<Chunk> {
        let mut compiler = Self {
            interner,
            program,
            chunk: Chunk::new(),
            constant_ids: HashMap::default(),
        };

        compiler.statement_list(statements)?;
        compiler
            .chunk
            .write(OpCode::Return, SourceLocation::new(file_id, 0..0));

        Ok(compiler.chunk)
    }

    fn statement_list(&mut self, statements: &[StatementId]) -> CompileResult<()> {
        for &statement in statements {
            self.statement(statement)?;
        }

        Ok(())
    }

    fn statement(&mut self, statement: StatementId) -> CompileResult<()> {
        match &self.program[statement] {
            Statement::Block { .. } => todo!(),
            Statement::Class { .. } => todo!(),
            Statement::Expression(expr) => self.expression(*expr),
            Statement::Function(_) => todo!(),
            Statement::If { .. } => todo!(),
            Statement::Print { keyword, expr } => {
                self.expression(*expr)?;
                self.chunk.write(OpCode::Print, keyword.location);
                Ok(())
            }
            Statement::Return { .. } => todo!(),
            Statement::Variable { name, initializer } => self.var_statement(*name, *initializer),
            Statement::While { .. } => todo!(),
        }
    }

    fn var_statement(
        &mut self,
        name: Token,
        initializer: Option<ExpressionId>,
    ) -> CompileResult<()> {
        let global = self.parse_variable(name)?;

        if let Some(init) = initializer {
            self.expression(init)?;
        } else {
            self.emit_constant(Value::Nil, name.lexeme, name.location)?;
        }

        self.define_variable(name, global)
    }

    fn define_variable(&mut self, name: Token, global: ConstId) -> CompileResult<()> {
        self.chunk.write(OpCode::DefineGlobal, name.location);
        self.chunk.write(global, name.location);
        Ok(())
    }

    fn parse_variable(&mut self, name: Token) -> CompileResult<ConstId> {
        self.identifier_constant(name)
    }

    fn identifier_constant(&mut self, name: Token) -> CompileResult<ConstId> {
        match self.constant_ids.get(&name.lexeme) {
            Some(&ident) => Ok(ident),
            None => {
                let ident = self.chunk.add_constant(
                    Value::String(StringObject::Literal(name.lexeme)),
                    name.location,
                )?;

                self.constant_ids.insert(name.lexeme, ident);
                Ok(ident)
            }
        }
    }

    fn expression(&mut self, expression: ExpressionId) -> CompileResult<()> {
        match &self.program[expression].kind {
            ExpressionKind::Assign { name, value } => {
                self.expression(*value)?;
                self.chunk.write(OpCode::SetGlobal, name.location);
                let id = self.identifier_constant(*name)?;
                self.chunk.write(id, name.location);
            }
            ExpressionKind::Binary {
                left,
                operator,
                right,
            } => self.binary_expression(*left, *operator, *right)?,
            ExpressionKind::Call { .. } => todo!(),
            ExpressionKind::Get { .. } => todo!(),
            ExpressionKind::Grouping { expression, .. } => self.expression(*expression)?,
            ExpressionKind::Literal { value } => self.literal_expression(*value)?,
            ExpressionKind::Logical { .. } => todo!(),
            ExpressionKind::Set { .. } => todo!(),
            ExpressionKind::This { .. } => todo!(),
            ExpressionKind::Super { .. } => todo!(),
            ExpressionKind::Unary { operator, right } => {
                self.expression(*right)?;
                match operator.kind {
                    TokenKind::Minus => self.chunk.write(OpCode::Negate, operator.location),
                    TokenKind::Bang => self.chunk.write(OpCode::Not, operator.location),
                    t => panic!("ICE:: Unexpected token kind in unary: {:?}", t),
                }
            }
            ExpressionKind::Variable { name } => {
                self.chunk.write(OpCode::GetGlobal, name.location);
                let id = self.identifier_constant(*name)?;
                self.chunk.write(id, name.location);
            }
        }

        Ok(())
    }

    fn binary_expression(
        &mut self,
        left: ExpressionId,
        operator: Token,
        right: ExpressionId,
    ) -> CompileResult<()> {
        self.expression(left)?;
        self.expression(right)?;

        match operator.kind {
            TokenKind::Minus => self.chunk.write(OpCode::Subtract, operator.location),
            TokenKind::Plus => self.chunk.write(OpCode::Add, operator.location),
            TokenKind::Percent => self.chunk.write(OpCode::Modulo, operator.location),
            TokenKind::Slash => self.chunk.write(OpCode::Divide, operator.location),
            TokenKind::Star => self.chunk.write(OpCode::Multiply, operator.location),

            TokenKind::EqualEqual => self.chunk.write(OpCode::Equal, operator.location),
            TokenKind::Greater => self.chunk.write(OpCode::Greater, operator.location),
            TokenKind::Less => self.chunk.write(OpCode::Less, operator.location),

            TokenKind::BangEqual => {
                self.chunk.write(OpCode::Equal, operator.location);
                self.chunk.write(OpCode::Not, operator.location);
            }
            TokenKind::GreaterEqual => {
                self.chunk.write(OpCode::Less, operator.location);
                self.chunk.write(OpCode::Not, operator.location);
            }
            TokenKind::LessEqual => {
                self.chunk.write(OpCode::Greater, operator.location);
                self.chunk.write(OpCode::Not, operator.location);
            }

            t => panic!("ICE: Invalid binary operator token: {:?}", t),
        }

        Ok(())
    }

    fn literal_expression(&mut self, token: Token) -> CompileResult<()> {
        match token.kind {
            TokenKind::StringLiteral(val) => {
                self.emit_constant(
                    Value::String(StringObject::Literal(val)),
                    token.lexeme,
                    token.location,
                )?;
            }
            TokenKind::NumberLiteral(val) => {
                self.emit_constant(Value::Number(val), token.lexeme, token.location)?;
            }
            TokenKind::BooleanLiteral(true) => self.chunk.write(OpCode::True, token.location),
            TokenKind::BooleanLiteral(false) => self.chunk.write(OpCode::False, token.location),
            TokenKind::NilLiteral => self.chunk.write(OpCode::Nil, token.location),
            _ => panic!("ICE: Expected literal TokenKind, found {:?}", token.kind),
        }

        Ok(())
    }

    fn emit_constant(
        &mut self,
        value: Value,
        lexeme: Spur,
        location: SourceLocation,
    ) -> CompileResult<ConstId> {
        let id = match self.constant_ids.get(&lexeme) {
            Some(id) => *id,
            None => {
                let id = self.chunk.add_constant(value, location)?;
                self.constant_ids.insert(lexeme, id);
                id
            }
        };
        self.chunk.write(OpCode::Constant, location);
        self.chunk.write(id, location);
        Ok(id)
    }
}
