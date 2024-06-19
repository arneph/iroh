use crate::tokens::Token;
use crate::tokens::Token::*;

pub fn next(code: &str) -> (Token, &str) {
    next_after_skipped_whitespace(skip_whitespace(code))
}

fn skip_whitespace(code: &str) -> &str {
    let mut remaining_code = code;
    while remaining_code.chars().next() == Some(' ') || remaining_code.chars().next() == Some('\t')
    {
        remaining_code = &remaining_code[1..];
    }
    remaining_code
}

fn next_after_skipped_whitespace(code: &str) -> (Token, &str) {
    match code.chars().next() {
        None => (EndOfCode, ""),
        Some('\n') => (EndOfLine, &code[1..]),
        Some('(') => (OpenParen, &code[1..]),
        Some(')') => (CloseParen, &code[1..]),
        Some('{') => (OpenCurly, &code[1..]),
        Some('}') => (CloseCurly, &code[1..]),
        Some('+') => (Plus, &code[1..]),
        Some('-') => (Minus, &code[1..]),
        Some('*') => (Star, &code[1..]),
        Some('/') => (Slash, &code[1..]),
        Some('%') => (Percent, &code[1..]),
        Some('0'..='9') => {
            let (i, remaining_code) = next_int_literal(code);
            (IntLiteral(i), remaining_code)
        }
        Some('A'..='Z' | 'a'..='z') => next_keyword_or_identifier(code),
        Some(c) => (Error(format!("Unexpected character: '{c}'")), &code[1..]),
    }
}

fn next_int_literal(code: &str) -> (i64, &str) {
    let end = code.chars().position(|c| match c {
        '0'..='9' => false,
        _ => true,
    });
    let (lit, remaining_code) = match end {
        None => (code, ""),
        Some(end) => (&code[0..end], &code[end..]),
    };
    (lit.parse().unwrap(), remaining_code)
}

fn next_keyword_or_identifier(code: &str) -> (Token, &str) {
    let end = code.chars().position(|c| match c {
        'A'..='Z' | 'a'..='z' | '0'..='9' => false,
        _ => true,
    });
    let (identifier, remaining_code) = match end {
        None => (code, ""),
        Some(end) => (&code[0..end], &code[end..]),
    };
    (
        match identifier {
            "fn" => Function,
            identifier => Identifier(identifier),
        },
        remaining_code,
    )
}
