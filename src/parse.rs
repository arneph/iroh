use crate::ast::BinaryOp::*;
use crate::ast::*;
use crate::tokens::Token;

pub fn parse_func<'a>(tokens: &'a [Token]) -> Option<Func<'a>> {
    let tokens = consume(tokens, &Token::Function)?;
    let (name, tokens) = parse_identifier(tokens)?;
    let tokens = consume(tokens, &Token::OpenParen)?;
    let tokens = consume(tokens, &Token::CloseParen)?;
    let tokens = consume(tokens, &Token::OpenCurly)?;
    let tokens = maybe_consume(tokens, &Token::EndOfLine)?;
    let (result, tokens) = parse_expr(tokens, 0)?;
    let tokens = maybe_consume(tokens, &Token::EndOfLine)?;
    consume(tokens, &Token::CloseCurly)?;
    Some(Func { name, result })
}

fn parse_expr<'a>(tokens: &'a [Token], min_precedence: i8) -> Option<(Expr, &'a [Token<'a>])> {
    let (operand_a, mut tokens) = parse_unary_expr(tokens)?;
    let mut expr = Expr::IntLit(operand_a);
    loop {
        let op = match tokens.first() {
            Some(Token::Plus) => Add,
            Some(Token::Minus) => Subtract,
            Some(Token::Star) => Multipy,
            Some(Token::Slash) => Divide,
            Some(Token::Percent) => Remainder,
            _ => return Some((expr, tokens)),
        };
        let actual_precedence = precedence(op.clone());
        if actual_precedence < min_precedence {
            return Some((expr, tokens));
        }
        let operand_b;
        (operand_b, tokens) = parse_expr(&tokens[1..], actual_precedence + 1)?;
        expr = Expr::BinaryExpr(Box::new(BinaryExpr {
            operand_a: expr,
            op,
            operand_b,
        }));
    }
}

fn precedence(op: BinaryOp) -> i8 {
    match op {
        Add | Subtract => 0,
        Multipy | Divide | Remainder => 1,
    }
}

fn parse_unary_expr<'a>(tokens: &'a [Token]) -> Option<(IntLit, &'a [Token<'a>])> {
    let (result, tokens) = parse_int_literal(tokens)?;
    Some((IntLit { value: result }, tokens))
}

fn parse_int_literal<'a>(tokens: &'a [Token]) -> Option<(i64, &'a [Token<'a>])> {
    match tokens.first() {
        Some(&Token::IntLiteral(value)) => Some((value, &tokens[1..])),
        _ => None,
    }
}

fn parse_identifier<'a>(tokens: &'a [Token]) -> Option<(&'a str, &'a [Token<'a>])> {
    match tokens.first() {
        Some(&Token::Identifier(name)) => Some((name, &tokens[1..])),
        _ => None,
    }
}

fn consume<'a>(tokens: &'a [Token], expected_token: &Token) -> Option<&'a [Token<'a>]> {
    match tokens.first() {
        None => None,
        Some(actual_token) => {
            if expected_token == actual_token {
                Some(&tokens[1..])
            } else {
                None
            }
        }
    }
}

fn maybe_consume<'a>(tokens: &'a [Token], expected_token: &Token) -> Option<&'a [Token<'a>]> {
    match tokens.first() {
        None => None,
        Some(actual_token) => {
            if expected_token == actual_token {
                Some(&tokens[1..])
            } else {
                Some(tokens)
            }
        }
    }
}
