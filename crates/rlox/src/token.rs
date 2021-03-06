use std::ops::Range;

use lasso::Spur;

use crate::{source_file::SourceLocation, FileId};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenKind {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Percent,
    SemiColon,
    Slash,
    Star,

    // One- or two-character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier,

    // Literals
    StringLiteral(Spur),
    NumberLiteral(f64),
    BooleanLiteral(bool),
    NilLiteral,

    // Keywords
    And,
    Class,
    Else,
    Fun,
    For,
    If,
    Or,
    Print,
    Return,
    Super,
    This,
    Var,
    While,

    Eof,
}

// Saves having a bunch of borrows in the parser. Looks dumb.
impl PartialEq<TokenKind> for &'_ TokenKind {
    #[inline(always)]
    fn eq(&self, other: &TokenKind) -> bool {
        **self == *other
    }
}

impl TokenKind {
    #[inline(always)]
    pub fn is_literal(self) -> bool {
        use TokenKind::*;
        matches!(
            self,
            StringLiteral(_) | NumberLiteral(_) | BooleanLiteral(_) | NilLiteral
        )
    }

    #[inline(always)]
    pub fn is_binary(self) -> bool {
        use TokenKind::*;
        matches!(
            self,
            Minus
                | Plus
                | Star
                | Slash
                | Percent
                | BangEqual
                | Equal
                | EqualEqual
                | Greater
                | GreaterEqual
                | Less
                | LessEqual
        )
    }

    #[inline(always)]
    pub fn is_logical(self) -> bool {
        use TokenKind::*;
        matches!(self, And | Or)
    }

    #[inline(always)]
    pub fn is_unary(self) -> bool {
        use TokenKind::*;
        matches!(self, Bang | Minus)
    }

    #[inline(always)]
    pub fn is_statement_start(self) -> bool {
        use TokenKind::*;
        matches!(self, Class | Fun | Var | For | If | While | Print | Return)
    }

    #[inline(always)]
    pub fn is_keyword(self) -> bool {
        use TokenKind::*;

        matches!(
            self,
            And | Class | Else | Fun | For | If | Or | Print | Return | Super | This | Var | While
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: Spur,
    pub location: SourceLocation,
}

impl Token {
    #[inline(always)]
    pub fn make_ident(lexeme: Spur, source_id: FileId, range: Range<usize>) -> Self {
        Self {
            kind: TokenKind::Identifier,
            lexeme,
            location: SourceLocation {
                file_id: source_id,
                source_start: range.start,
                len: range.end - range.start,
            },
        }
    }
}

impl Token {
    #[inline(always)]
    pub fn source_range(self) -> Range<usize> {
        self.location.range()
    }
}
