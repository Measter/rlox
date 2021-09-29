use std::{iter::Peekable, slice::Iter};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use lasso::Rodeo;

use crate::{
    ast::{ExpressionKind, Function, Statement},
    program::{ExpressionId, Program, StatementId},
    source_file::SourceLocation,
    token::{Token, TokenKind},
    FileId,
};

type ParseResult<T> = Result<T, Diagnostic<FileId>>;

// program        → declaration* EOF ;

// declaration    → classDecl
//                | funDecl
//                | varDecl
//                | statement ;
// classDecl      → "class" IDENTIER ( "<" IDENTIFIER )? "{" function* "}" ;
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
//                | "(" expression ")" | IDENTIFIER
//                | "super" "." IDENTIFIER ;

pub struct ParseSuccess {
    pub program: Vec<StatementId>,
    pub diags: Vec<Diagnostic<FileId>>,
}

#[derive(Debug)]
pub struct Parser<'collection, 'interner, 'program> {
    source: &'collection [Token],
    tokens: Peekable<Iter<'collection, Token>>, // Yay nested iterators!
    current: usize,
    recoverable_diags: Vec<Diagnostic<FileId>>,
    interner: &'interner mut Rodeo,
    program: &'program mut Program,
    file_id: FileId,
}

impl<'collection, 'interner, 'program> Parser<'collection, 'interner, 'program> {
    fn new(
        tokens: &'collection [Token],
        interner: &'interner mut Rodeo,
        file_id: FileId,
        program: &'program mut Program,
    ) -> Self {
        Self {
            source: tokens,
            tokens: tokens.iter().peekable(),
            current: 0,
            recoverable_diags: Vec::new(),
            interner,
            file_id,
            program,
        }
    }

    pub fn parse(
        tokens: &'collection [Token],
        interner: &'interner mut Rodeo,
        file_id: FileId,
        program: &'program mut Program,
    ) -> Result<ParseSuccess, Vec<Diagnostic<FileId>>> {
        let mut parser = Self::new(tokens, interner, file_id, program);
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

    fn declaration(&mut self) -> ParseResult<StatementId> {
        self.try_parse_statement(TokenKind::Class, Self::class_declaration)
            .or_else(|| {
                self.matches(|k| k == TokenKind::Fun).map(|t| {
                    self.function_declaration(t)
                        .map(|s| self.program.add_statement(self.file_id, s))
                })
            })
            .or_else(|| self.try_parse_statement(TokenKind::Var, Self::var_declaration))
            .unwrap_or_else(|| self.statement())
    }

    fn class_declaration(&mut self, class_token: Token) -> ParseResult<StatementId> {
        let name = self.expect(TokenKind::Identifier, "ident", Vec::new)?;

        let superclass = self
            .matches(|k| k == TokenKind::Less)
            .map(|_| {
                let super_name = self.expect(TokenKind::Identifier, "ident", Vec::new)?;
                Ok(self
                    .program
                    .add_expression(self.file_id, ExpressionKind::variable(super_name)))
            })
            .transpose()?;

        let left_brace = self.expect(TokenKind::LeftBrace, "`{`}", Vec::new)?;

        let mut methods = Vec::new();

        loop {
            if matches!(self.tokens.peek(), Some(t) if t.kind == TokenKind::RightBrace)
                && !self.is_at_end()
            {
                break;
            }

            // There's no actual function token here, so just stick in the last one (should be a brace).
            let func = match self.function_declaration(self.previous())? {
                Statement::Function(f) => f,
                _ => unreachable!(),
            };

            methods.push(func);
        }

        let right_brace = self.expect(TokenKind::RightBrace, "`}`", || {
            vec![Label::secondary(
                left_brace.location.file_id,
                left_brace.source_range(),
            )]
        })?;

        let source_range = class_token.location.source_start..right_brace.source_range().end;

        Ok(self.program.add_statement(
            self.file_id,
            Statement::Class {
                name,
                methods,
                source_range,
                superclass,
            },
        ))
    }

    fn function_declaration(&mut self, _: Token) -> ParseResult<Statement> {
        let name = self.expect(TokenKind::Identifier, "ident", Vec::new)?;
        let left_paren = self.expect(TokenKind::LeftParen, "`(`", Vec::new)?;

        let mut parameters = Vec::new();

        if self.matches(|k| k == TokenKind::RightParen).is_none() {
            loop {
                if parameters.len() >= 255 {
                    let diag = Diagnostic::error()
                        .with_message("can't have more than 255 parameters")
                        .with_labels(vec![Label::primary(
                            name.location.file_id,
                            name.source_range(),
                        )]);
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
                    Label::secondary(left_paren.location.file_id, left_paren_range.clone())
                        .with_message("opened here"),
                ]
            })?;
        }

        let left_brace = self.expect(TokenKind::LeftBrace, "`{`", Vec::new)?;
        let body = match self.block_statement(left_brace)? {
            Statement::Block { statements } => statements,
            _ => unreachable!(),
        };

        Ok(Statement::Function(self.program.add_function(
            self.file_id,
            Function {
                name,
                parameters,
                body,
            },
        )))
    }

    fn var_declaration(&mut self, _: Token) -> ParseResult<StatementId> {
        let name = self.expect(TokenKind::Identifier, "ident", Vec::new)?;

        let initializer = self
            .matches(|k| k == TokenKind::Equal)
            .is_some()
            .then(|| self.expression())
            .transpose()?;

        self.expect(TokenKind::SemiColon, "`;`", Vec::new)?;

        Ok(self
            .program
            .add_statement(self.file_id, Statement::Variable { name, initializer }))
    }

    fn statement(&mut self) -> ParseResult<StatementId> {
        self.try_parse_statement(TokenKind::For, Self::for_statement)
            .or_else(|| self.try_parse_statement(TokenKind::If, Self::if_statement))
            .or_else(|| self.try_parse_statement(TokenKind::Print, Self::print_statement))
            .or_else(|| self.try_parse_statement(TokenKind::Return, Self::return_statement))
            .or_else(|| self.try_parse_statement(TokenKind::While, Self::while_statement))
            .or_else(|| {
                self.matches(|k| k == TokenKind::LeftBrace).map(|t| {
                    self.block_statement(t)
                        .map(|s| self.program.add_statement(self.file_id, s))
                })
            })
            .unwrap_or_else(|| self.expression_statement())
    }

    fn try_parse_statement(
        &mut self,
        kind: TokenKind,
        then_parse: fn(&mut Self, Token) -> ParseResult<StatementId>,
    ) -> Option<ParseResult<StatementId>> {
        self.matches(|k| k == kind).map(|t| then_parse(self, t))
    }

    fn for_statement(&mut self, for_token: Token) -> ParseResult<StatementId> {
        // We'll just desugar into a while loop.

        let for_token_range = for_token.source_range();
        let left_paren = self.expect(TokenKind::LeftParen, "`(`", || {
            vec![
                Label::secondary(for_token.location.file_id, for_token_range.clone())
                    .with_message("for-statements require parenthesis"),
            ]
        })?;

        let init = if self.matches(|k| k == TokenKind::SemiColon).is_some() {
            None
        } else if let Some(var_token) = self.matches(|k| k == TokenKind::Var) {
            Some(self.var_declaration(var_token)?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = self
            .matches(|k| k == TokenKind::SemiColon)
            .is_none()
            .then(|| {
                let expr = self.expression()?;

                self.expect(TokenKind::SemiColon, "`;`", Vec::new)?;
                Ok(expr)
            })
            .transpose()?;

        let increment = self
            .matches(|k| k == TokenKind::RightParen)
            .is_none()
            .then(|| {
                let expr = self.expression()?;

                let left_paren_range = left_paren.source_range();
                self.expect(TokenKind::RightParen, "`)`", || {
                    vec![
                        Label::secondary(left_paren.location.file_id, left_paren_range.clone())
                            .with_message("opened here"),
                    ]
                })?;
                Ok(expr)
            })
            .transpose()?;

        let mut body = vec![self.statement()?];
        if let Some(increment) = increment {
            body.push(
                self.program
                    .add_statement(self.file_id, Statement::Expression(increment)),
            );
        }

        let condition = condition.unwrap_or_else(|| {
            self.program.add_expression(
                self.file_id,
                ExpressionKind::literal(Token {
                    kind: TokenKind::BooleanLiteral(true),
                    lexeme: self.interner.get_or_intern_static("true"),
                    location: SourceLocation {
                        file_id: self.file_id,
                        source_start: 0,
                        len: 0,
                    },
                }),
            )
        });

        let while_body = self
            .program
            .add_statement(self.file_id, Statement::Block { statements: body });

        let while_loop = self.program.add_statement(
            self.file_id,
            Statement::While {
                condition,
                body: while_body,
            },
        );

        if let Some(init) = init {
            Ok(self.program.add_statement(
                self.file_id,
                Statement::Block {
                    statements: vec![init, while_loop],
                },
            ))
        } else {
            Ok(while_loop)
        }
    }

    fn if_statement(&mut self, if_token: Token) -> ParseResult<StatementId> {
        let if_token_range = if_token.source_range();
        let left_paren = self.expect(TokenKind::LeftParen, "`(`", || {
            vec![
                Label::secondary(if_token.location.file_id, if_token_range.clone())
                    .with_message("if-statements require parenthesis"),
            ]
        })?;
        let condition = self.expression()?;
        let left_paren_range = left_paren.source_range();
        self.expect(TokenKind::RightParen, "`)`", || {
            vec![
                Label::secondary(left_paren.location.file_id, left_paren_range.clone())
                    .with_message("opened here"),
            ]
        })?;

        let then_branch = self.statement()?;
        let else_branch = self
            .matches(|k| k == TokenKind::Else)
            .is_some()
            .then(|| self.statement())
            .transpose()?;

        Ok(self.program.add_statement(
            self.file_id,
            Statement::If {
                condition,
                then_branch,
                else_branch,
            },
        ))
    }

    fn block_statement(&mut self, left_brace: Token) -> ParseResult<Statement> {
        let mut statements = Vec::new();

        while !matches!(self.tokens.peek(), Some(t) if t.kind == TokenKind::RightBrace) {
            statements.push(self.declaration()?);
        }

        let left_brace_range = left_brace.source_range();
        self.expect(TokenKind::RightBrace, "`}`", || {
            vec![
                Label::secondary(left_brace.location.file_id, left_brace_range.clone())
                    .with_message("opened here"),
            ]
        })?;

        Ok(Statement::Block { statements })
    }

    fn print_statement(&mut self, _: Token) -> ParseResult<StatementId> {
        let value = self.expression()?;
        self.expect(TokenKind::SemiColon, "`;`", Vec::new)?;
        Ok(self
            .program
            .add_statement(self.file_id, Statement::Print(value)))
    }

    fn return_statement(&mut self, keyword: Token) -> ParseResult<StatementId> {
        let value = if self.matches(|k| k == TokenKind::SemiColon).is_none() {
            let value = self.expression()?;
            self.expect(TokenKind::SemiColon, "`;`", Vec::new)?;
            Some(value)
        } else {
            None
        };

        Ok(self
            .program
            .add_statement(self.file_id, Statement::Return { keyword, value }))
    }

    fn while_statement(&mut self, while_token: Token) -> ParseResult<StatementId> {
        let while_token_range = while_token.source_range();
        let left_paren = self.expect(TokenKind::LeftParen, "`(`", || {
            vec![
                Label::secondary(while_token.location.file_id, while_token_range.clone())
                    .with_message("while-loops require parenthesis"),
            ]
        })?;

        let condition = self.expression()?;
        let left_paren_range = left_paren.source_range();
        self.expect(TokenKind::RightParen, "`)`", || {
            vec![
                Label::secondary(left_paren.location.file_id, left_paren_range.clone())
                    .with_message("opened here"),
            ]
        })?;

        let body = self.statement()?;
        Ok(self
            .program
            .add_statement(self.file_id, Statement::While { condition, body }))
    }

    fn expression_statement(&mut self) -> ParseResult<StatementId> {
        let value = self.expression()?;
        self.expect(TokenKind::SemiColon, "`;`", Vec::new)?;
        Ok(self
            .program
            .add_statement(self.file_id, Statement::Expression(value)))
    }

    fn expression(&mut self) -> ParseResult<ExpressionId> {
        self.assignment()
    }

    fn assignment(&mut self) -> ParseResult<ExpressionId> {
        let expr = self.logical_or()?;

        if let Some(equals) = self.matches(|k| k == TokenKind::Equal) {
            let value = self.assignment()?;

            match self.program[expr].kind {
                ExpressionKind::Variable { name, .. } => {
                    return Ok(self
                        .program
                        .add_expression(self.file_id, ExpressionKind::assign(name, value)));
                }
                ExpressionKind::Get { object, name, .. } => {
                    return Ok(self
                        .program
                        .add_expression(self.file_id, ExpressionKind::set(object, name, value)));
                }
                _ => (),
            }

            let diag = Diagnostic::error()
                .with_message("invalid assignment target")
                .with_labels(vec![Label::primary(
                    equals.location.file_id,
                    equals.source_range(),
                )]);

            self.recoverable_diags.push(diag);
        }

        Ok(expr)
    }

    fn logical_or(&mut self) -> ParseResult<ExpressionId> {
        self.binary_expression(
            Self::logical_and,
            |k| k == TokenKind::Or,
            ExpressionKind::logical,
        )
    }

    fn logical_and(&mut self) -> ParseResult<ExpressionId> {
        self.binary_expression(
            Self::equality,
            |k| k == TokenKind::And,
            ExpressionKind::logical,
        )
    }

    fn equality(&mut self) -> ParseResult<ExpressionId> {
        self.binary_expression(
            Self::comparison,
            |k| matches!(k, TokenKind::BangEqual | TokenKind::EqualEqual),
            ExpressionKind::binary,
        )
    }

    fn comparison(&mut self) -> ParseResult<ExpressionId> {
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
            ExpressionKind::binary,
        )
    }

    fn term(&mut self) -> ParseResult<ExpressionId> {
        self.binary_expression(
            Self::factor,
            |k| matches!(k, TokenKind::Minus | TokenKind::Plus),
            ExpressionKind::binary,
        )
    }

    fn factor(&mut self) -> ParseResult<ExpressionId> {
        self.binary_expression(
            Self::unary,
            |k| matches!(k, TokenKind::Slash | TokenKind::Star | TokenKind::Percent),
            ExpressionKind::binary,
        )
    }

    fn binary_expression(
        &mut self,
        next_func: fn(&mut Self) -> Result<ExpressionId, Diagnostic<FileId>>,
        kinds: fn(&TokenKind) -> bool,
        constructor: fn(ExpressionId, Token, ExpressionId) -> ExpressionKind,
    ) -> ParseResult<ExpressionId> {
        let mut expr = next_func(self)?;

        while let Some(operator) = self.matches(kinds) {
            let right = next_func(self)?;
            expr = self
                .program
                .add_expression(self.file_id, constructor(expr, operator, right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ParseResult<ExpressionId> {
        if let Some(operator) = self.matches(|k| matches!(k, TokenKind::Bang | TokenKind::Minus)) {
            let right = self.unary()?;
            Ok(self
                .program
                .add_expression(self.file_id, ExpressionKind::unary(operator, right)))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> ParseResult<ExpressionId> {
        let mut expr = self.primary()?;

        loop {
            if self.matches(|k| k == TokenKind::LeftParen).is_some() {
                expr = self.finish_call(expr)?;
            } else if self.matches(|k| k == TokenKind::Dot).is_some() {
                let name = self.expect(TokenKind::Identifier, "ident", Vec::new)?;
                expr = self
                    .program
                    .add_expression(self.file_id, ExpressionKind::get(expr, name));
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: ExpressionId) -> ParseResult<ExpressionId> {
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
                                self.program[callee].source_id(),
                                self.program[callee].source_range(self.program),
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
                        Label::secondary(left_paren.location.file_id, left_paren_range.clone())
                            .with_message("opened here"),
                    ]
                })?
            }
        };

        let range_start = self.program[callee].source_range(self.program).start;
        let range_end = right_paren.source_range().end;

        Ok(self.program.add_expression(
            self.file_id,
            ExpressionKind::call(callee, arguments, range_start..range_end),
        ))
    }

    fn primary(&mut self) -> ParseResult<ExpressionId> {
        let next_token = self.advance();
        match next_token.kind {
            kind if kind.is_literal() => Ok(self
                .program
                .add_expression(self.file_id, ExpressionKind::literal(next_token))),
            TokenKind::Identifier => Ok(self
                .program
                .add_expression(self.file_id, ExpressionKind::variable(next_token))),
            TokenKind::LeftParen => {
                let left_paren = next_token;
                let expr = self.expression()?;
                let left_paren_range = left_paren.source_range();
                let right_paren =
                    self.expect(TokenKind::RightParen, "right parenthesis", || {
                        vec![Label::secondary(
                            left_paren.location.file_id,
                            left_paren_range.clone(),
                        )
                        .with_message("opened here")]
                    })?;

                let left_range = left_paren.source_range();
                let right_range = right_paren.source_range();
                let range = left_range.start..right_range.end;

                Ok(self
                    .program
                    .add_expression(self.file_id, ExpressionKind::grouping(expr, range)))
            }
            TokenKind::This => Ok(self
                .program
                .add_expression(self.file_id, ExpressionKind::this(next_token))),
            TokenKind::Super => {
                self.expect(TokenKind::Dot, "`.`", Vec::new)?;
                let method = self.expect(TokenKind::Identifier, "ident", || {
                    vec![
                        Label::secondary(next_token.location.file_id, next_token.source_range())
                            .with_message("superclass method expected"),
                    ]
                })?;

                Ok(self.program.add_expression(
                    self.file_id,
                    ExpressionKind::super_keyword(next_token, method),
                ))
            }
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
                        next_token.location.file_id,
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
            Label::primary(next.location.file_id, next.source_range())
                .with_message("expected here"),
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
        self.source
            .get(self.current.saturating_sub(1))
            .or_else(|| self.source.last())
            .copied()
            .unwrap()
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
