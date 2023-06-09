use std::fmt;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Colon,
    Comma,
    Dot,
    Minus,
    Percent,
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
    MinusEqual,
    PlusEqual,
    StarEqual,
    SlashEqual,

    // literals.
    Identifier,
    String,
    Number,

    // keywords.
    And,
    As,
    Assert,
    Break,
    Class,
    Continue,
    Delete,
    Else,
    Eof,
    False,
    For,
    Foreach,
    Function,
    If,
    Import,
    In,
    Nil,
    NotIn,
    Or,
    Print,
    Println,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // to skip
    Skip,
    Error,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = match self {
            TokenType::LeftParen => "leftparen",
            TokenType::RightParen => "rightparen",
            TokenType::LeftBrace => "leftbrace",
            TokenType::RightBrace => "rightbrace",
            TokenType::LeftBracket => "leftbracket",
            TokenType::RightBracket => "rightbracket",
            TokenType::Colon => "colon",
            TokenType::Comma => "comma",
            TokenType::Dot => "dot",
            TokenType::Minus => "minus",
            TokenType::Percent => "percent",
            TokenType::Plus => "plus",
            TokenType::Semicolon => "semicolon",
            TokenType::Slash => "slash",
            TokenType::Star => "star",
            TokenType::Bang => "bang",
            TokenType::BangEqual => "bangequal",
            TokenType::Equal => "equal",
            TokenType::EqualEqual => "equalequal",
            TokenType::Greater => "greater",
            TokenType::GreaterEqual => "greaterequal",
            TokenType::Less => "less",
            TokenType::LessEqual => "lessequal",
            TokenType::MinusEqual => "minusequal",
            TokenType::PlusEqual => "plusequal",
            TokenType::SlashEqual => "slashequal",
            TokenType::StarEqual => "starequal",
            TokenType::Identifier => "identifier",
            TokenType::String => "string",
            TokenType::Number => "number",
            TokenType::And => "and",
            TokenType::As => "as",
            TokenType::Assert => "assert",
            TokenType::Break => "break",
            TokenType::Class => "class",
            TokenType::Continue => "continue",
            TokenType::Else => "else",
            TokenType::Eof => "eof",
            TokenType::Delete => "delete",
            TokenType::False => "false",
            TokenType::For => "for",
            TokenType::Foreach => "foreach",
            TokenType::Function => "function",
            TokenType::If => "if",
            TokenType::Import => "import",
            TokenType::In => "in",
            TokenType::Nil => "nil",
            TokenType::NotIn => "notin",
            TokenType::Or => "or",
            TokenType::Print => "print",
            TokenType::Println => "println",
            TokenType::Return => "return",
            TokenType::Super => "super",
            TokenType::This => "this",
            TokenType::True => "true",
            TokenType::Var => "var",
            TokenType::While => "while",
            TokenType::Skip => "skip",
            TokenType::Error => "error",
        };

        write!(f, "{output}")
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub start: usize,
    pub length: usize,
    pub line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, start: usize, length: usize, line: usize) -> Token {
        Token {
            token_type,
            start,
            length,
            line,
        }
    }
}
