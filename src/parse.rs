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
    let (mut expr, mut tokens) = parse_unary_expr(tokens)?;
    loop {
        let op = match tokens.first() {
            Some(Token::Or) => Or,
            Some(Token::And) => And,
            Some(Token::Equal) => Equal,
            Some(Token::NotEqual) => NotEqual,
            Some(Token::LessThan) => LessThan,
            Some(Token::LessThanOrEqual) => LessThanOrEqual,
            Some(Token::GreaterThanOrEqual) => GreaterThanOrEqual,
            Some(Token::GreaterThan) => GreaterThan,
            Some(Token::Plus) => Add,
            Some(Token::Minus) => Subtract,
            Some(Token::Star) => Multipy,
            Some(Token::Slash) => Divide,
            Some(Token::Percent) => Remainder,
            Some(Token::ShiftLeft) => ShiftLeft,
            Some(Token::ShiftRight) => ShiftRight,
            Some(Token::BitwiseOr) => BitwiseOr,
            Some(Token::BitwiseXor) => BitwiseXor,
            Some(Token::BitwiseAnd) => BitwiseAnd,
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
        Or => 0,
        And => 1,
        Equal | NotEqual | LessThan | LessThanOrEqual | GreaterThanOrEqual | GreaterThan => 2,
        Add | Subtract | BitwiseOr | BitwiseXor => 3,
        Multipy | Divide | Remainder | ShiftLeft | ShiftRight | BitwiseAnd => 4,
    }
}

fn parse_unary_expr<'a>(tokens: &'a [Token]) -> Option<(Expr, &'a [Token<'a>])> {
    match tokens.first() {
        Some(Token::False) => Some((Expr::BoolLit(BoolLit::False), &tokens[1..])),
        Some(Token::True) => Some((Expr::BoolLit(BoolLit::True), &tokens[1..])),
        Some(&Token::IntLiteral(value)) => Some((Expr::IntLit(IntLit { value }), &tokens[1..])),
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
