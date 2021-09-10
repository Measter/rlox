use std::{iter::Peekable, rc::Rc, slice::Iter};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use lasso::Rodeo;

use crate::{
    ast::{Expression, ExpressionId, Function, Statement},
    token::{Token, TokenKind},
    FileId,
};

type ParseResult<T> = Result<T, Diagnostic<FileId>>;

// program        → declaration* EOF ;

// declaration    → classDecl
//                | funDecl
//                | varDecl
//                | statement ;
// classDecl      → "class" IDENTIER "{" function* "}" ;
// funDecl        → "fun" function ;
// function       → IDENTIFIER "(" paramaters? ")" block ;
// parameters     → IDENTIFIER ( "," IDENTIFIER )* ;
// varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
// statement      → exprStmt
//                | forStmt
//                | ifStmt
//                | printStmt
//                | returnStmt
//                | whileStmt
//                | block ;
// forStmt        → "for" "(" ( varDecl | exprStment | ";" )
//                  expression? ";"
//                  expression? ")" statement ;
// whileStmnt     → "while" "(" expression ")" statement;
// block          → "{" declaration "}" ;
// exprStmt       → expression ";" ;
// printStmt      → "print" expression ";" ;
// returnStmt     → "return" expression? ";" ;
// ifStmt         → "if" "(" expression ")" statement
//                ( "else" statement )? ;

// expression     → assignment ;
// assignment     → ( call "." )? IDENTIFIER "=" assignment | logic_or ;
// logic_or       → logic_and ( "or" logic_and )* ;
// logic_and      → equality ( "and" equality )* ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" | "%" ) unary )* ;
// unary          → ( "!" | "-" ) unary | call ;
// call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
// arguments      → expression ( "," expression )* ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")" | IDENTIFIER;

pub struct ParseSuccess {
    pub program: Vec<Statement>,
    pub diags: Vec<Diagnostic<FileId>>,
}

#[derive(Debug)]
pub struct Parser<'collection, 'interner> {
    source: &'collection [Token],
    tokens: Peekable<Iter<'collection, Token>>, // Yay nested iterators!
    current: usize,
    recoverable_diags: Vec<Diagnostic<FileId>>,
    interner: &'interner mut Rodeo,
    file_id: FileId,
    expr_id: usize,
}

impl<'collection, 'interner> Parser<'collection, 'interner> {
    fn new(tokens: &'collection [Token], interner: &'interner mut Rodeo, file_id: FileId) -> Self {
        Self {
            source: tokens,
            tokens: tokens.iter().peekable(),
            current: 0,
            recoverable_diags: Vec::new(),
            interner,
            file_id,
            expr_id: 0,
        }
    }

    fn expression_id(&mut self) -> ExpressionId {
        let id = ExpressionId::new(self.expr_id, self.file_id);
        self.expr_id += 1;
        id
    }

    pub fn parse(
        tokens: &'collection [Token],
        interner: &'interner mut Rodeo,
        file_id: FileId,
    ) -> Result<ParseSuccess, Vec<Diagnostic<FileId>>> {
        let mut parser = Self::new(tokens, interner, file_id);
        let mut diags = Vec::new();
        let mut statements = Vec::new();

        while !parser.is_at_end() {
            match parser.declaration() {
                Ok(statement) => statements.push(statement),
                Err(err) => {
                    diags.push(err);
                    parser.synchronize();
                }
            }
        }

        diags
            .is_empty()
            .then(|| ParseSuccess {
                program: statements,
                diags: parser.recoverable_diags,
            })
            .ok_or(diags)
    }

    fn declaration(&mut self) -> ParseResult<Statement> {
        self.try_parse_statement(TokenKind::Class, Self::class_declaration)
            .or_else(|| self.try_parse_statement(TokenKind::Fun, Self::function_declaration))
            .or_else(|| self.try_parse_statement(TokenKind::Var, Self::var_declaration))
            .unwrap_or_else(|| self.statement())
    }

    fn class_declaration(&mut self) -> ParseResult<Statement> {
        let class_token = self.previous();
        let name = self.expect(TokenKind::Identifier, "ident", Vec::new)?;
        let left_brace = self.expect(TokenKind::LeftBrace, "`{`}", Vec::new)?;

        let mut methods = Vec::new();

        loop {
            if matches!(self.tokens.peek(), Some(t) if t.kind == TokenKind::RightBrace)
                && !self.is_at_end()
            {
                break;
            }

            let func = match self.function_declaration()? {
                Statement::Function(f) => Rc::try_unwrap(f).unwrap(), // A bit inefficient, but whatever...
                _ => unreachable!(),
            };

            methods.push(Rc::new(func));
        }

        let right_brace = self.expect(TokenKind::RightBrace, "`}`", || {
            vec![Label::secondary(
                left_brace.source_id,
                left_brace.source_range(),
            )]
        })?;

        let source_range = class_token.source_start..right_brace.source_range().end;

        Ok(Statement::Class {
            name,
            methods,
            source_range,
        })
    }

    fn function_declaration(&mut self) -> ParseResult<Statement> {
        let name = self.expect(TokenKind::Identifier, "ident", Vec::new)?;
        let left_paren = self.expect(TokenKind::LeftParen, "`(`", Vec::new)?;

        let mut parameters = Vec::new();

        if self.matches(|k| k == TokenKind::RightParen).is_none() {
            loop {
                if parameters.len() >= 255 {
                    let diag = Diagnostic::error()
                        .with_message("can't have more than 255 parameters")
                        .with_labels(vec![Label::primary(name.source_id, name.source_range())]);
                    return Err(diag);
                }
                parameters.push(self.expect(TokenKind::Identifier, "ident", Vec::new)?);

                if self.matches(|k| k == TokenKind::Comma).is_none() {
                    break;
                }
            }

            let left_paren_range = left_paren.source_range();
            self.expect(TokenKind::RightParen, "')'", || {
                vec![
                    Label::secondary(left_paren.source_id, left_paren_range.clone())
                        .with_message("opened here"),
                ]
            })?;
        }

        self.expect(TokenKind::LeftBrace, "`{`", Vec::new)?;
        let body = match self.block_statement()? {
            Statement::Block { statements } => statements,
            _ => unreachable!(),
        };

        Ok(Statement::Function(Rc::new(Function {
            name,
            parameters,
            body,
        })))
    }

    fn var_declaration(&mut self) -> ParseResult<Statement> {
        let name = self.expect(TokenKind::Identifier, "ident", Vec::new)?;

        let initializer = self
            .matches(|k| k == TokenKind::Equal)
            .is_some()
            .then(|| self.expression())
            .transpose()?;

        self.expect(TokenKind::SemiColon, "`;`", Vec::new)?;

        Ok(Statement::Variable { name, initializer })
    }

    fn statement(&mut self) -> ParseResult<Statement> {
        self.try_parse_statement(TokenKind::For, Self::for_statement)
            .or_else(|| self.try_parse_statement(TokenKind::If, Self::if_statement))
            .or_else(|| self.try_parse_statement(TokenKind::Print, Self::print_statement))
            .or_else(|| self.try_parse_statement(TokenKind::Return, Self::return_statement))
            .or_else(|| self.try_parse_statement(TokenKind::While, Self::while_statement))
            .or_else(|| self.try_parse_statement(TokenKind::LeftBrace, Self::block_statement))
            .unwrap_or_else(|| self.expression_statement())
    }

    fn try_parse_statement(
        &mut self,
        kind: TokenKind,
        then_parse: fn(&mut Self) -> ParseResult<Statement>,
    ) -> Option<ParseResult<Statement>> {
        self.matches(|k| k == kind)
            .is_some()
            .then(|| then_parse(self))
    }

    fn for_statement(&mut self) -> ParseResult<Statement> {
        // We'll just desugar into a while loop.

        let for_token = self.previous();
        let for_token_range = for_token.source_range();
        let left_paren = self.expect(TokenKind::LeftParen, "`(`", || {
            vec![
                Label::secondary(for_token.source_id, for_token_range.clone())
                    .with_message("for-statements require parenthesis"),
            ]
        })?;

        let init = if self.matches(|k| k == TokenKind::SemiColon).is_some() {
            None
        } else if self.matches(|k| k == TokenKind::Var).is_some() {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = self
            .matches(|k| k == TokenKind::SemiColon)
            .is_none()
            .then(|| self.expression())
            .transpose()?;
        self.expect(TokenKind::SemiColon, "`;`", Vec::new)?;

        let increment = self
            .matches(|k| k == TokenKind::SemiColon)
            .is_none()
            .then(|| self.expression())
            .transpose()?;

        let left_paren_range = left_paren.source_range();
        self.expect(TokenKind::RightParen, "`)`", || {
            vec![
                Label::secondary(left_paren.source_id, left_paren_range.clone())
                    .with_message("opened here"),
            ]
        })?;

        let mut body = vec![self.statement()?];
        if let Some(increment) = increment {
            body.push(Statement::Expression(increment));
        }

        let condition = condition.unwrap_or_else(|| {
            Expression::literal(
                Token {
                    kind: TokenKind::Boolean(true),
                    lexeme: self.interner.get_or_intern_static("true"),
                    source_start: 0,
                    source_len: 0,
                    source_id: self.file_id,
                },
                self.expression_id(),
            )
        });

        let while_loop = Statement::While {
            condition,
            body: Box::new(Statement::Block { statements: body }),
        };

        if let Some(init) = init {
            Ok(Statement::Block {
                statements: vec![init, while_loop],
            })
        } else {
            Ok(while_loop)
        }
    }

    fn if_statement(&mut self) -> ParseResult<Statement> {
        let if_token = self.previous();
        let if_token_range = if_token.source_range();
        let left_paren = self.expect(TokenKind::LeftParen, "`(`", || {
            vec![Label::secondary(if_token.source_id, if_token_range.clone())
                .with_message("if-statements require parenthesis")]
        })?;
        let condition = self.expression()?;
        let left_paren_range = left_paren.source_range();
        self.expect(TokenKind::RightParen, "`)`", || {
            vec![
                Label::secondary(left_paren.source_id, left_paren_range.clone())
                    .with_message("opened here"),
            ]
        })?;

        let then_branch = self.statement()?;
        let else_branch = self
            .matches(|k| k == TokenKind::Else)
            .is_some()
            .then(|| self.statement())
            .transpose()?;

        Ok(Statement::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
        })
    }

    fn block_statement(&mut self) -> ParseResult<Statement> {
        let left_brace = self.previous();
        let mut statements = Vec::new();

        while !matches!(self.tokens.peek(), Some(t) if t.kind == TokenKind::RightBrace) {
            statements.push(self.declaration()?);
        }

        let left_brace_range = left_brace.source_range();
        self.expect(TokenKind::RightBrace, "`}`", || {
            vec![
                Label::secondary(left_brace.source_id, left_brace_range.clone())
                    .with_message("opened here"),
            ]
        })?;

        Ok(Statement::Block { statements })
    }

    fn print_statement(&mut self) -> ParseResult<Statement> {
        let value = self.expression()?;
        self.expect(TokenKind::SemiColon, "`;`", Vec::new)?;
        Ok(Statement::Print(value))
    }

    fn return_statement(&mut self) -> ParseResult<Statement> {
        let keyword = self.previous();
        let value = if self.matches(|k| k == TokenKind::SemiColon).is_none() {
            let value = self.expression()?;
            self.expect(TokenKind::SemiColon, "`;`", Vec::new)?;
            Some(value)
        } else {
            None
        };

        Ok(Statement::Return { keyword, value })
    }

    fn while_statement(&mut self) -> ParseResult<Statement> {
        let while_token = self.previous();
        let while_token_range = while_token.source_range();
        let left_paren = self.expect(TokenKind::LeftParen, "`(`", || {
            vec![
                Label::secondary(while_token.source_id, while_token_range.clone())
                    .with_message("while-loops require parenthesis"),
            ]
        })?;

        let condition = self.expression()?;
        let left_paren_range = left_paren.source_range();
        self.expect(TokenKind::RightParen, "`)`", || {
            vec![
                Label::secondary(left_paren.source_id, left_paren_range.clone())
                    .with_message("opened here"),
            ]
        })?;

        let body = self.statement()?;
        Ok(Statement::While {
            condition,
            body: Box::new(body),
        })
    }

    fn expression_statement(&mut self) -> ParseResult<Statement> {
        let value = self.expression()?;
        self.expect(TokenKind::SemiColon, "`;`", Vec::new)?;
        Ok(Statement::Expression(value))
    }

    fn expression(&mut self) -> ParseResult<Expression> {
        self.assignment()
    }

    fn assignment(&mut self) -> ParseResult<Expression> {
        let expr = self.logical_or()?;

        if let Some(equals) = self.matches(|k| k == TokenKind::Equal) {
            let value = self.assignment()?;

            match expr {
                Expression::Variable { name, .. } => {
                    return Ok(Expression::assign(name, value, self.expression_id()))
                }
                Expression::Get { object, name, .. } => {
                    return Ok(Expression::Set {
                        object,
                        name,
                        value: Box::new(value),
                        id: self.expression_id(),
                    })
                }
                _ => (),
            }

            let diag = Diagnostic::error()
                .with_message("invalid assignment target")
                .with_labels(vec![Label::primary(
                    equals.source_id,
                    equals.source_range(),
                )]);

            self.recoverable_diags.push(diag);
        }

        Ok(expr)
    }

    fn logical_or(&mut self) -> ParseResult<Expression> {
        self.binary_expression(
            Self::logical_and,
            |k| k == TokenKind::Or,
            Expression::logical,
        )
    }

    fn logical_and(&mut self) -> ParseResult<Expression> {
        self.binary_expression(Self::equality, |k| k == TokenKind::And, Expression::logical)
    }

    fn equality(&mut self) -> ParseResult<Expression> {
        self.binary_expression(
            Self::comparison,
            |k| matches!(k, TokenKind::BangEqual | TokenKind::EqualEqual),
            Expression::binary,
        )
    }

    fn comparison(&mut self) -> ParseResult<Expression> {
        self.binary_expression(
            Self::term,
            |k| {
                matches!(
                    k,
                    TokenKind::Greater
                        | TokenKind::GreaterEqual
                        | TokenKind::Less
                        | TokenKind::LessEqual
                )
            },
            Expression::binary,
        )
    }

    fn term(&mut self) -> ParseResult<Expression> {
        self.binary_expression(
            Self::factor,
            |k| matches!(k, TokenKind::Minus | TokenKind::Plus),
            Expression::binary,
        )
    }

    fn factor(&mut self) -> ParseResult<Expression> {
        self.binary_expression(
            Self::unary,
            |k| matches!(k, TokenKind::Slash | TokenKind::Star | TokenKind::Percent),
            Expression::binary,
        )
    }

    fn binary_expression(
        &mut self,
        next_func: fn(&mut Self) -> Result<Expression, Diagnostic<FileId>>,
        kinds: fn(&TokenKind) -> bool,
        constructor: fn(Expression, Token, Expression, ExpressionId) -> Expression,
    ) -> ParseResult<Expression> {
        let mut expr = next_func(self)?;

        while let Some(operator) = self.matches(kinds) {
            let right = next_func(self)?;
            expr = constructor(expr, operator, right, self.expression_id());
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ParseResult<Expression> {
        if let Some(operator) = self.matches(|k| matches!(k, TokenKind::Bang | TokenKind::Minus)) {
            let right = self.unary()?;
            Ok(Expression::unary(operator, right, self.expression_id()))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> ParseResult<Expression> {
        let mut expr = self.primary()?;

        loop {
            if self.matches(|k| k == TokenKind::LeftParen).is_some() {
                expr = self.finish_call(expr)?;
            }
            if self.matches(|k| k == TokenKind::Dot).is_some() {
                let name = self.expect(TokenKind::Identifier, "ident", Vec::new)?;
                expr = Expression::get(expr, name, self.expression_id());
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expression) -> ParseResult<Expression> {
        let mut arguments = Vec::new();
        let left_paren = self.previous();

        let right_paren = match self.matches(|k| k == TokenKind::RightParen) {
            Some(p) => p,
            None => {
                loop {
                    if arguments.len() >= 255 {
                        let diag = Diagnostic::error()
                            .with_message("can't have more than 255 parameters")
                            .with_labels(vec![Label::primary(
                                callee.source_id(),
                                callee.source_range(),
                            )]);
                        return Err(diag);
                    }
                    arguments.push(self.expression()?);

                    if self.matches(|k| k == TokenKind::Comma).is_none() {
                        break;
                    }
                }
                let left_paren_range = left_paren.source_range();
                self.expect(TokenKind::RightParen, "')'", || {
                    vec![
                        Label::secondary(left_paren.source_id, left_paren_range.clone())
                            .with_message("opened here"),
                    ]
                })?
            }
        };

        let range_start = callee.source_range().start;
        let range_end = right_paren.source_range().end;

        Ok(Expression::call(
            callee,
            arguments,
            range_start..range_end,
            self.expression_id(),
        ))
    }

    fn primary(&mut self) -> ParseResult<Expression> {
        let next_token = self.advance();
        match next_token.kind {
            kind if kind.is_literal() => Ok(Expression::literal(next_token, self.expression_id())),
            TokenKind::Identifier => Ok(Expression::variable(next_token, self.expression_id())),
            TokenKind::LeftParen => {
                let left_paren = next_token;
                let expr = self.expression()?;
                let left_paren_range = left_paren.source_range();
                let right_paren =
                    self.expect(TokenKind::RightParen, "right parenthesis", || {
                        vec![
                            Label::secondary(left_paren.source_id, left_paren_range.clone())
                                .with_message("opened here"),
                        ]
                    })?;

                let left_range = left_paren.source_range();
                let right_range = right_paren.source_range();
                let range = left_range.start..right_range.end;

                Ok(Expression::grouping(expr, range, self.expression_id()))
            }
            TokenKind::This => Ok(Expression::this(next_token, self.expression_id())),
            kind => {
                let msg = match kind {
                    k if k.is_keyword() => "keyword ",
                    k if k.is_binary() || k.is_unary() => "operator ",
                    TokenKind::Eof => "",
                    TokenKind::SemiColon => "",
                    _ => "unknown token ",
                };

                let next_token_lexeme = self.interner.resolve(&next_token.lexeme);

                Err(Diagnostic::error()
                    .with_message(format!(
                        "expected expression found {}`{}`",
                        msg, next_token_lexeme
                    ))
                    .with_labels(vec![Label::primary(
                        next_token.source_id,
                        next_token.source_range(),
                    )]))
            }
        }
    }

    fn expect(
        &mut self,
        expected_kind: TokenKind,
        kind_str: &str,
        labels: impl Fn() -> Vec<Label<FileId>>,
    ) -> ParseResult<Token> {
        let next = self.advance();
        if next.kind == expected_kind {
            return Ok(next);
        }

        let mut labels = labels();
        labels.push(
            Label::primary(next.source_id, next.source_range()).with_message("expected here"),
        );

        let next_lexeme = self.interner.resolve(&next.lexeme);
        let diag = Diagnostic::error()
            .with_message(format!("expected {} found `{}`", kind_str, next_lexeme))
            .with_labels(labels);

        Err(diag)
    }

    fn matches(&mut self, kinds: impl Fn(&TokenKind) -> bool) -> Option<Token> {
        self.tokens
            .peek()
            .map(|t| t.kind)
            .filter(kinds)
            .map(|_| self.advance())
    }

    fn is_at_end(&mut self) -> bool {
        self.tokens
            .peek()
            .map(|t| t.kind == TokenKind::Eof)
            .unwrap_or(true)
    }

    fn advance(&mut self) -> Token {
        self.tokens
            .next()
            .map(|token| {
                self.current += 1;
                *token
            })
            .unwrap_or_else(|| self.previous())
    }

    fn previous(&self) -> Token {
        self.source[self.current]
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().kind == TokenKind::SemiColon {
                return;
            }

            if matches!(self.tokens.peek(), Some(t) if t.kind.is_statement_start()) {
                return;
            }

            self.advance();
        }
    }
}
