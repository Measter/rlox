use std::ops::Range;

use lasso::Spur;

use crate::FileId;

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
    String(Spur),
    Number(f64),
    Boolean(bool),
    Nil,

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
    fn eq(&self, other: &TokenKind) -> bool {
        **self == *other
    }
}

impl TokenKind {
    pub fn is_literal(self) -> bool {
        use TokenKind::*;
        matches!(self, String(_) | Number(_) | Boolean(_) | Nil)
    }

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

    pub fn is_logical(self) -> bool {
        use TokenKind::*;
        matches!(self, And | Or)
    }

    pub fn is_unary(self) -> bool {
        use TokenKind::*;
        matches!(self, Bang | Minus)
    }

    pub fn is_statement_start(self) -> bool {
        use TokenKind::*;
        matches!(self, Class | Fun | Var | For | If | While | Print | Return)
    }

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
    pub source_id: FileId,
    pub source_start: usize,
    pub source_len: usize,
}

impl Token {
    pub fn make_ident(lexeme: Spur, source_id: FileId, range: Range<usize>) -> Self {
        Self {
            kind: TokenKind::Identifier,
            lexeme,
            source_id,
            source_start: range.start,
            source_len: range.end - range.start,
        }
    }
}

impl Token {
    pub fn source_range(self) -> Range<usize> {
        self.source_start..(self.source_start + self.source_len)
    }
}
