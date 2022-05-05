#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // one or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // literals.
    Identifier,
    String,
    Number,

    // keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof,

    // to skip
    Skip,
}

#[derive(Clone, Debug)]
pub enum Literal {
    String(String),
    Number(f64),
    Boolean(bool),
    Identifier(String),
    Nil,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Option<Literal>,
    pub line: usize,
}

impl Token {
    pub fn new(
        token_type: TokenType, lexeme: String, literal: Option<Literal>, line: usize
    ) -> Token {
        Token {
            token_type,
            lexeme,
            literal,
            line,
        }
    }
}
