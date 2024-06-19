#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Error(String),
    EndOfCode,
    EndOfLine,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Function,
    IntLiteral(i64),
    Identifier(&'a str),
}
