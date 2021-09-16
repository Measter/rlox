use std::{iter::Peekable, str::CharIndices};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use lasso::Rodeo;
use rlox::source_file::SourceLocation;

use crate::{
    token::{Token, TokenKind},
    FileId,
};

fn is_ident_start(c: char) -> bool {
    matches!(c, 'a'..= 'z' | 'A'..='Z' | '_')
}

fn is_ident_continue(c: char) -> bool {
    is_ident_start(c) || matches!(c, '0'..='9')
}

fn get_ident_kind(ident: &str) -> TokenKind {
    match ident {
        "and" => TokenKind::And,
        "class" => TokenKind::Class,
        "else" => TokenKind::Else,
        "false" => TokenKind::Boolean(false),
        "for" => TokenKind::For,
        "fun" => TokenKind::Fun,
        "if" => TokenKind::If,
        "nil" => TokenKind::Nil,
        "or" => TokenKind::Or,
        "print" => TokenKind::Print,
        "return" => TokenKind::Return,
        "super" => TokenKind::Super,
        "this" => TokenKind::This,
        "true" => TokenKind::Boolean(true),
        "var" => TokenKind::Var,
        "while" => TokenKind::While,
        _ => TokenKind::Identifier,
    }
}

pub struct Lexer<'a> {
    source: &'a str,
    chars: Peekable<CharIndices<'a>>,
    tokens: Vec<Token>,
    interner: &'a mut Rodeo,
    file_id: FileId,

    cur_token_start: usize,
    next_token_start: usize,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str, interner: &'a mut Rodeo, file_id: FileId) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
            tokens: Vec::new(),
            interner,
            file_id,

            cur_token_start: 0,
            next_token_start: 0,
        }
    }

    pub fn scan_tokens(
        source: &'a str,
        interner: &'a mut Rodeo,
        file_id: FileId,
    ) -> Result<Vec<Token>, Vec<Diagnostic<FileId>>> {
        let mut scanner = Self::new(source, interner, file_id);
        let mut diags = Vec::new();

        while !scanner.is_at_end() {
            scanner.cur_token_start = scanner.next_token_start;
            if let Err(d) = scanner.scan_token() {
                diags.push(d);
            }
        }

        scanner.tokens.push(Token {
            kind: TokenKind::Eof,
            lexeme: scanner.interner.get_or_intern_static("<EOF>"),
            location: SourceLocation {
                file_id: scanner.file_id,
                source_start: scanner.source.len(),
                len: 0,
            },
        });

        diags.is_empty().then(|| scanner.tokens).ok_or(diags)
    }

    fn is_at_end(&mut self) -> bool {
        self.chars.peek().is_none()
    }

    fn scan_token(&mut self) -> Result<(), Diagnostic<FileId>> {
        let ch = self.advance();
        // Default character is NULL, which we aren't giving special handling.
        let (_, next_ch) = self.chars.peek().copied().unwrap_or_default();

        match (ch, next_ch) {
            (' ' | '\r' | '\t' | '\n', _) => {} // Ignore these!

            ('"', _) => self.lex_string()?,
            ('0'..='9', _) => self.lex_number()?,
            _ if is_ident_start(ch) => self.lex_identifier(),

            // Potentially-combining characters
            ('!', '=') => {
                self.advance();
                self.add_simple_token(TokenKind::BangEqual)
            }
            ('=', '=') => {
                self.advance();
                self.add_simple_token(TokenKind::EqualEqual)
            }
            ('<', '=') => {
                self.advance();
                self.add_simple_token(TokenKind::LessEqual)
            }
            ('>', '=') => {
                self.advance();
                self.add_simple_token(TokenKind::GreaterEqual)
            }
            // Comment handling.
            ('/', '/') => {
                while !matches!(self.chars.peek(), Some((_, '\n'))) && !self.is_at_end() {
                    self.advance();
                }
            }

            // Simple single-characters
            ('(', _) => self.add_simple_token(TokenKind::LeftParen),
            (')', _) => self.add_simple_token(TokenKind::RightParen),
            ('{', _) => self.add_simple_token(TokenKind::LeftBrace),
            ('}', _) => self.add_simple_token(TokenKind::RightBrace),
            (',', _) => self.add_simple_token(TokenKind::Comma),
            ('.', _) => self.add_simple_token(TokenKind::Dot),
            ('-', _) => self.add_simple_token(TokenKind::Minus),
            ('+', _) => self.add_simple_token(TokenKind::Plus),
            (';', _) => self.add_simple_token(TokenKind::SemiColon),
            ('*', _) => self.add_simple_token(TokenKind::Star),
            ('!', _) => self.add_simple_token(TokenKind::Bang),
            ('=', _) => self.add_simple_token(TokenKind::Equal),
            ('<', _) => self.add_simple_token(TokenKind::Less),
            ('>', _) => self.add_simple_token(TokenKind::Greater),
            ('/', _) => self.add_simple_token(TokenKind::Slash),
            ('%', _) => self.add_simple_token(TokenKind::Percent),

            _ => {
                return Err(Diagnostic::error()
                    .with_message(format!("unexpected character '{}'", ch))
                    .with_labels(vec![Label::primary(
                        self.file_id,
                        self.cur_token_start..self.next_token_start,
                    )]))
            }
        }

        Ok(())
    }

    fn lex_identifier(&mut self) {
        while matches!(self.chars.peek(), Some(&(_, c)) if is_ident_continue(c)) {
            self.advance();
        }

        let lexeme = &self.source[self.cur_token_start..self.next_token_start];
        let kind = get_ident_kind(lexeme);

        self.tokens.push(Token {
            kind,
            lexeme: self.interner.get_or_intern(lexeme),
            location: SourceLocation {
                file_id: self.file_id,
                source_start: self.cur_token_start,
                len: lexeme.len(),
            },
        });
    }

    fn lex_number(&mut self) -> Result<(), Diagnostic<FileId>> {
        while matches!(self.chars.peek(), Some((_, '0'..='9'))) {
            self.advance();
        }

        if matches!(self.chars.peek(), Some((_, '.')))
            && matches!(
                &self.source[self.next_token_start + 1..].chars().next(),
                Some(('0'..='9'))
            )
        {
            self.advance();

            while matches!(self.chars.peek(), Some((_, '0'..='9'))) {
                self.advance();
            }
        }

        let lexeme = &self.source[self.cur_token_start..self.next_token_start];
        let literal = match lexeme.parse() {
            Ok(lit) => lit,
            Err(_) => {
                return Err(Diagnostic::error()
                    .with_message("invalid number literal")
                    .with_labels(vec![Label::primary(
                        self.file_id,
                        self.cur_token_start..self.next_token_start,
                    )]))
            }
        };

        self.tokens.push(Token {
            kind: TokenKind::Number(literal),
            lexeme: self.interner.get_or_intern(lexeme),
            location: SourceLocation {
                file_id: self.file_id,
                source_start: self.cur_token_start,
                len: lexeme.len(),
            },
        });

        Ok(())
    }

    fn lex_string(&mut self) -> Result<(), Diagnostic<FileId>> {
        while !matches!(self.chars.peek(), Some((_, '"'))) && !self.is_at_end() {
            self.advance();
        }

        if self.is_at_end() {
            return Err(Diagnostic::error()
                .with_message("unexpected end of string")
                .with_labels(vec![Label::primary(
                    self.file_id,
                    self.cur_token_start..self.source.len(),
                )]));
        }

        self.advance();

        let lexeme = &self.source[self.cur_token_start..self.next_token_start];
        let literal = lexeme.trim_matches('"');

        self.tokens.push(Token {
            kind: TokenKind::String(self.interner.get_or_intern(literal)),
            lexeme: self.interner.get_or_intern(lexeme),
            location: SourceLocation {
                file_id: self.file_id,
                source_start: self.cur_token_start,
                len: lexeme.len(),
            },
        });

        Ok(())
    }

    fn advance(&mut self) -> char {
        let (idx, ch) = self.chars.next().expect("Unexpected end of input");
        self.next_token_start = idx + ch.len_utf8();
        ch
    }

    // Used for simple ASCII tokens.
    fn add_simple_token(&mut self, kind: TokenKind) {
        let lexeme = &self.source[self.cur_token_start..self.next_token_start];
        self.tokens.push(Token {
            kind,
            lexeme: self.interner.get_or_intern(lexeme),
            location: SourceLocation {
                file_id: self.file_id,
                source_start: self.cur_token_start,
                len: lexeme.len(),
            },
        })
    }
}
