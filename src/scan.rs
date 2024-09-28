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
    let c1 = code.chars().next();
    let c2 = code.chars().nth(1);
    match (c1, c2) {
        (None, _) => (EndOfCode, ""),
        (Some('\n'), _) => (EndOfLine, &code[1..]),
        (Some('('), _) => (OpenParen, &code[1..]),
        (Some(')'), _) => (CloseParen, &code[1..]),
        (Some('{'), _) => (OpenCurly, &code[1..]),
        (Some('}'), _) => (CloseCurly, &code[1..]),
        (Some(','), _) => (Comma, &code[1..]),
        (Some(':'), _) => (Colon, &code[1..]),
        (Some('-'), Some('>')) => (RightArrow, &code[2..]),
        (Some('|'), Some('|')) => (Or, &code[2..]),
        (Some('&'), Some('&')) => (And, &code[2..]),
        (Some('='), Some('=')) => (Equal, &code[2..]),
        (Some('!'), Some('=')) => (NotEqual, &code[2..]),
        (Some('<'), Some('=')) => (LessThanOrEqual, &code[2..]),
        (Some('>'), Some('=')) => (GreaterThanOrEqual, &code[2..]),
        (Some('<'), Some('<')) => (ShiftLeft, &code[2..]),
        (Some('>'), Some('>')) => (ShiftRight, &code[2..]),
        (Some('<'), _) => (LessThan, &code[1..]),
        (Some('>'), _) => (GreaterThan, &code[1..]),
        (Some('+'), _) => (Plus, &code[1..]),
        (Some('-'), _) => (Minus, &code[1..]),
        (Some('*'), _) => (Star, &code[1..]),
        (Some('/'), _) => (Slash, &code[1..]),
        (Some('%'), _) => (Percent, &code[1..]),
        (Some('|'), _) => (BitwiseOr, &code[1..]),
        (Some('^'), _) => (BitwiseXor, &code[1..]),
        (Some('&'), _) => (BitwiseAnd, &code[1..]),
        (Some('0'..='9'), _) => {
            let (i, remaining_code) = next_int_literal(code);
            (IntLiteral(i), remaining_code)
        }
        (Some('A'..='Z' | 'a'..='z'), _) => next_keyword_or_identifier(code),
        (Some(c), _) => (Error(format!("Unexpected character: '{c}'")), &code[1..]),
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
            "if" => If,
            "else" => Else,
            "false" => False,
            "true" => True,
            identifier => Identifier(identifier),
        },
        remaining_code,
    )
}
