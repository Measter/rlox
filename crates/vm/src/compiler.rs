use std::{iter::Peekable, slice::Iter};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use fnv::FnvHashMap as HashMap;
use lasso::{Rodeo, Spur};
use rlox::{
    source_file::{FileId, SourceLocation},
    token::{Token, TokenKind},
    DiagnosticEmitter,
};

use crate::{
    chunk::{Chunk, ConstId, OpCode},
    value::{StringObject, Value},
};

type ParseResult<T> = Result<T, Diagnostic<FileId>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Default for Precedence {
    fn default() -> Self {
        Self::None
    }
}

impl Precedence {
    fn next(self) -> Self {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => self,
        }
    }
}

#[derive(Clone, Copy, Default)]
struct ParseRule {
    prefix: Option<fn(&mut Compiler<'_, '_>, bool) -> ParseResult<()>>,
    infix: Option<fn(&mut Compiler<'_, '_>) -> ParseResult<()>>,
    precedence: Precedence,
}

impl ParseRule {
    fn get_rule(kind: TokenKind) -> Self {
        match kind {
            TokenKind::Bang => Self {
                prefix: Some(|c, _| c.unary()),
                ..Default::default()
            },
            TokenKind::BangEqual => Self {
                infix: Some(|c| c.binary()),
                ..Default::default()
            },
            TokenKind::EqualEqual => Self {
                prefix: None,
                infix: Some(|c| c.binary()),
                precedence: Precedence::Equality,
            },
            TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::Less
            | TokenKind::LessEqual => Self {
                prefix: None,
                infix: Some(|c| c.binary()),
                precedence: Precedence::Comparison,
            },

            TokenKind::LeftParen => Self {
                prefix: Some(|c, _| c.grouping()),
                ..Default::default()
            },

            TokenKind::NilLiteral | TokenKind::BooleanLiteral(_) => Self {
                prefix: Some(|c, _| c.literal()),
                ..Default::default()
            },
            TokenKind::NumberLiteral(_) => Self {
                prefix: Some(|c, _| c.number()),
                ..Default::default()
            },
            TokenKind::StringLiteral(_) => Self {
                prefix: Some(|c, _| c.string()),
                ..Default::default()
            },

            TokenKind::Percent | TokenKind::Slash | TokenKind::Star => Self {
                prefix: None,
                infix: Some(|c| c.binary()),
                precedence: Precedence::Factor,
            },
            TokenKind::Minus => Self {
                prefix: Some(|c, _| c.unary()),
                infix: Some(|c| c.binary()),
                precedence: Precedence::Term,
            },
            TokenKind::Plus => Self {
                prefix: None,
                infix: Some(|c| c.binary()),
                precedence: Precedence::Term,
            },

            TokenKind::Identifier => Self {
                prefix: Some(|c, b| c.variable(b)),
                ..Default::default()
            },
            _ => Default::default(),
        }
    }
}

pub struct Compiler<'collection, 'interner> {
    source: &'collection [Token],
    tokens: Peekable<Iter<'collection, Token>>,
    next_idx: usize,
    current_token: Token,
    interner: &'interner Rodeo,
    chunk: Chunk,
    constant_ids: HashMap<Spur, ConstId>,
}

impl<'collection, 'interner> Compiler<'collection, 'interner> {
    fn new(source: &'collection [Token], interner: &'interner Rodeo) -> Self {
        Self {
            source,
            tokens: source.iter().peekable(),
            interner,
            next_idx: 0,
            current_token: Token {
                kind: TokenKind::Eof,
                lexeme: interner.get("this").unwrap(),
                location: SourceLocation::default(),
            },
            chunk: Chunk::new(),
            constant_ids: HashMap::default(),
        }
    }

    pub fn compile(
        source: &'collection [Token],
        emitter: &mut DiagnosticEmitter<'_>,
        interner: &'interner Rodeo,
    ) -> Result<Chunk, Vec<Diagnostic<FileId>>> {
        let mut compiler = Compiler::new(source, interner);
        let mut diags = Vec::new();

        compiler.advance();
        while !compiler.matches(|k| k == TokenKind::Eof) {
            if let Err(e) = compiler.declaration() {
                diags.push(e);
                compiler.synchronize();
            }
        }
        if let Err(e) = compiler.expect(TokenKind::Eof, "EOF", Vec::new) {
            diags.push(e);
        }
        compiler.end_compiler();

        diags.is_empty().then(|| compiler.chunk).ok_or(diags)
    }

    fn end_compiler(&mut self) {
        self.emit_return();
    }

    fn binary(&mut self) -> ParseResult<()> {
        let op_type = self.previous();
        let rule = ParseRule::get_rule(op_type.kind);
        self.parse_precedence(rule.precedence.next())?;

        match op_type.kind {
            TokenKind::Minus => self.chunk.write(OpCode::Subtract, op_type.location),
            TokenKind::Plus => self.chunk.write(OpCode::Add, op_type.location),
            TokenKind::Percent => self.chunk.write(OpCode::Modulo, op_type.location),
            TokenKind::Slash => self.chunk.write(OpCode::Divide, op_type.location),
            TokenKind::Star => self.chunk.write(OpCode::Multiply, op_type.location),

            TokenKind::EqualEqual => self.chunk.write(OpCode::Equal, op_type.location),
            TokenKind::Greater => self.chunk.write(OpCode::Greater, op_type.location),
            TokenKind::Less => self.chunk.write(OpCode::Less, op_type.location),

            TokenKind::BangEqual => {
                self.chunk.write(OpCode::Equal, op_type.location);
                self.chunk.write(OpCode::Not, op_type.location);
            }
            TokenKind::GreaterEqual => {
                self.chunk.write(OpCode::Less, op_type.location);
                self.chunk.write(OpCode::Not, op_type.location);
            }
            TokenKind::LessEqual => {
                self.chunk.write(OpCode::Greater, op_type.location);
                self.chunk.write(OpCode::Not, op_type.location);
            }
            t => panic!("ICE: Invalid binary token: {:?}", t),
        }

        Ok(())
    }

    fn literal(&mut self) -> ParseResult<()> {
        let literal_token = self.previous();
        match literal_token.kind {
            TokenKind::BooleanLiteral(true) => {
                self.chunk.write(OpCode::True, literal_token.location)
            }
            TokenKind::BooleanLiteral(false) => {
                self.chunk.write(OpCode::False, literal_token.location)
            }
            TokenKind::NilLiteral => self.chunk.write(OpCode::Nil, literal_token.location),
            _ => panic!("ICE: None-literal token kind {:?}", literal_token.kind),
        }

        Ok(())
    }

    fn grouping(&mut self) -> ParseResult<()> {
        let left_paren = self.previous();
        self.expression()?;
        self.expect(TokenKind::RightParen, "`)`", || {
            vec![
                Label::secondary(left_paren.location.file_id, left_paren.source_range())
                    .with_message("opened here"),
            ]
        })?;

        Ok(())
    }

    fn number(&mut self) -> ParseResult<()> {
        let previous = self.previous();

        match previous.kind {
            TokenKind::NumberLiteral(n) => {
                self.emit_constant(Value::Number(n), previous.lexeme, previous.location)?;
                Ok(())
            }
            _ => {
                let next_lexeme = self.interner.resolve(&previous.lexeme);
                let diag = Diagnostic::error()
                    .with_message(format!("expected number literal, found `{}`", next_lexeme))
                    .with_labels(vec![Label::primary(
                        previous.location.file_id,
                        previous.source_range(),
                    )]);
                Err(diag)
            }
        }
    }

    fn string(&mut self) -> ParseResult<()> {
        let token = self.previous();
        let key = if let TokenKind::StringLiteral(key) = token.kind {
            key
        } else {
            panic!("ICE: Expected StringLiteral, found {:?}", token.kind);
        };

        let obj = Value::String(StringObject::Literal(key));

        self.emit_constant(obj, token.lexeme, token.location)?;
        Ok(())
    }

    fn variable(&mut self, can_assign: bool) -> ParseResult<()> {
        let prev = self.previous();
        self.named_variable(prev, can_assign)
    }

    fn named_variable(&mut self, prev: Token, can_assign: bool) -> ParseResult<()> {
        let arg = self.identifier_constant(prev)?;

        if can_assign && self.matches(|k| k == TokenKind::Equal) {
            self.expression()?;
            self.chunk.write(OpCode::SetGlobal, prev.location);
        } else {
            self.chunk.write(OpCode::GetGlobal, prev.location);
        }

        self.chunk.write(arg, prev.location);
        Ok(())
    }

    fn unary(&mut self) -> ParseResult<()> {
        let operator_type = self.previous();
        self.parse_precedence(Precedence::Unary)?;

        match operator_type.kind {
            TokenKind::Minus => self.chunk.write(OpCode::Negate, operator_type.location),
            TokenKind::Bang => self.chunk.write(OpCode::Not, operator_type.location),
            t => panic!("ICE: Unexpected token kind in unary: {:?}", t),
        }

        Ok(())
    }

    fn parse_precedence(&mut self, precendence: Precedence) -> ParseResult<()> {
        self.advance();
        let prev_token = self.previous();

        let prefix_rule = match ParseRule::get_rule(prev_token.kind).prefix {
            Some(prefix_rule) => prefix_rule,
            None => {
                let prev_lexeme = self.interner.resolve(&prev_token.lexeme);
                let diag = Diagnostic::error()
                    .with_message(format!("expected expression found `{}`", prev_lexeme))
                    .with_labels(vec![Label::primary(
                        prev_token.location.file_id,
                        prev_token.source_range(),
                    )]);

                return Err(diag);
            }
        };

        let can_assign = precendence <= Precedence::Assignment;
        prefix_rule(self, can_assign)?;

        while precendence <= ParseRule::get_rule(self.current_token.kind).precedence {
            self.advance();
            match ParseRule::get_rule(self.previous().kind).infix {
                Some(f) => f(self)?,
                None => panic!("ICE: Expected infix function"),
            }
        }

        if can_assign && self.matches(|k| k == TokenKind::Equal) {
            let equal_token = self.previous();
            let diag = Diagnostic::error()
                .with_message("Invalid assignment target.")
                .with_labels(vec![Label::primary(
                    equal_token.location.file_id,
                    equal_token.source_range(),
                )]);
            return Err(diag);
        }

        Ok(())
    }

    fn expression(&mut self) -> ParseResult<()> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn statement(&mut self) -> ParseResult<()> {
        if self.matches(|k| k == TokenKind::Print) {
            self.print_statement()
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> ParseResult<()> {
        let print_token = self.previous();
        self.expression()?;
        self.expect(TokenKind::SemiColon, "`;`", Vec::new)?;
        self.chunk.write(OpCode::Print, print_token.location);

        Ok(())
    }

    fn expression_statement(&mut self) -> ParseResult<()> {
        self.expression()?;
        self.expect(TokenKind::SemiColon, "`;`", Vec::new)?;
        self.chunk.write(OpCode::Pop, self.current_token.location);

        Ok(())
    }

    fn declaration(&mut self) -> ParseResult<()> {
        if self.matches(|k| k == TokenKind::Var) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> ParseResult<()> {
        let global = self.parse_variable()?;

        if self.matches(|k| k == TokenKind::Equal) {
            self.expression()?;
        } else {
            self.emit_constant(
                Value::Nil,
                self.interner.get("nil").unwrap(),
                self.current_token.location,
            )?;
        }

        self.expect(TokenKind::SemiColon, "`;`", Vec::new)?;

        self.define_variable(global)
    }

    fn define_variable(&mut self, global: ConstId) -> ParseResult<()> {
        self.chunk
            .write(OpCode::DefineGlobal, self.current_token.location);
        self.chunk.write(global, self.current_token.location);

        Ok(())
    }

    fn parse_variable(&mut self) -> ParseResult<ConstId> {
        self.expect(TokenKind::Identifier, "IDENT", Vec::new)?;
        let prev = self.previous();
        self.identifier_constant(prev)
    }

    fn identifier_constant(&mut self, token: Token) -> ParseResult<ConstId> {
        match self.constant_ids.get(&token.lexeme) {
            Some(&ident) => Ok(ident),
            None => {
                let ident = self.chunk.add_constant(
                    Value::String(StringObject::Literal(token.lexeme)),
                    token.location,
                )?;

                self.constant_ids.insert(token.lexeme, ident);
                Ok(ident)
            }
        }
    }

    fn emit_return(&mut self) {
        let prev = self.previous().location;
        self.chunk.write(OpCode::Return, prev);
    }

    fn emit_constant(
        &mut self,
        value: Value,
        lexeme: Spur,
        location: SourceLocation,
    ) -> ParseResult<ConstId> {
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

    fn expect(
        &mut self,
        expected_kind: TokenKind,
        kind_str: &str,
        labels: impl Fn() -> Vec<Label<FileId>>,
    ) -> ParseResult<Token> {
        if self.current_token.kind == expected_kind {
            return Ok(self.advance());
        }

        let mut labels = labels();
        labels.push(
            Label::primary(
                self.current_token.location.file_id,
                self.current_token.source_range(),
            )
            .with_message("expected here"),
        );

        let next_lexeme = self.interner.resolve(&self.current_token.lexeme);
        let diag = Diagnostic::error()
            .with_message(format!("expected {} found `{}`", kind_str, next_lexeme))
            .with_labels(labels);

        Err(diag)
    }

    fn matches(&mut self, kinds: impl Fn(&TokenKind) -> bool) -> bool {
        kinds(&self.current_token.kind)
            .then(|| self.advance())
            .is_some()
    }

    fn advance(&mut self) -> Token {
        let current = self
            .tokens
            .next()
            .map(|t| {
                self.next_idx += 1;
                *t
            })
            .unwrap_or_else(|| self.current_token);
        self.current_token = current;

        current
    }

    fn previous(&mut self) -> Token {
        self.source
            .get(self.next_idx.saturating_sub(2))
            .or_else(|| self.source.last())
            .copied()
            .unwrap()
    }

    fn synchronize(&mut self) {
        while self.current_token.kind != TokenKind::Eof {
            if self.previous().kind == TokenKind::SemiColon
                || self.current_token.kind.is_statement_start()
            {
                return;
            }
            self.advance();
        }

        self.advance();
    }
}
