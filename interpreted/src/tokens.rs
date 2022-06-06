use std::{
    fmt,
    hash::{Hash, Hasher},
    mem,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    String(String),
    Number(f64),
    Boolean(bool),
    Identifier(String),
    Nil,
}

fn integer_decode(val: f64) -> (u64, i16, i8) {
    let bits: u64 = unsafe { mem::transmute(val) };
    let sign: i8 = if bits >> 63 == 0 { 1 } else { -1 };
    let mut exponent: i16 = ((bits >> 52) & 0x7ff) as i16;
    let mantissa = if exponent == 0 {
        (bits & 0xfffffffffffff) << 1
    } else {
        (bits & 0xfffffffffffff) | 0x10000000000000
    };

    exponent -= 1023 + 52;

    (mantissa, exponent, sign)
}

impl Hash for Literal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Literal::String(string) => string.hash(state),
            Literal::Identifier(string) => string.hash(state),
            Literal::Boolean(boolean) => boolean.hash(state),
            Literal::Nil => 0.hash(state),
            Literal::Number(number) => {
                let (mantissa, exponent, sign) = integer_decode(*number);

                mantissa.hash(state);
                exponent.hash(state);
                sign.hash(state);
            }
        };
    }
}

impl Eq for Literal {}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = match self {
            Literal::String(string) => format!("'{}'", string),
            Literal::Identifier(string) => string.clone(),
            Literal::Boolean(boolean) => boolean.to_string(),
            Literal::Number(number) => number.to_string(),
            Literal::Nil => "nil".to_string(),
        };

        write!(f, "{}", output)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Option<Literal>,
    pub line: usize,
}

impl Token {
    pub fn new(
        token_type: TokenType,
        lexeme: String,
        literal: Option<Literal>,
        line: usize,
    ) -> Token {
        Token {
            token_type,
            lexeme,
            literal,
            line,
        }
    }
}
