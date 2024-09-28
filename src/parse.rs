use crate::ast::BinaryOp::*;
use crate::ast::*;
use crate::tokens::Token;
use crate::types::*;

pub fn parse_program<'a>(tokens: &'a [Token]) -> Option<Program<'a>> {
    let mut tokens = tokens;
    let mut funcs: Vec<Func<'a>> = vec![];
    loop {
        match tokens.first() {
            None | Some(&Token::EndOfCode) => {
                break;
            }
            Some(&Token::EndOfLine) => {
                tokens = &tokens[1..];
            }
            Some(&Token::Function) => {
                let (func, remaining_tokens) = parse_func(tokens)?;
                funcs.push(func);
                tokens = remaining_tokens;
            }
            Some(_) => return None,
        }
    }
    Some(Program { funcs: funcs })
}

pub fn parse_func<'a>(tokens: &'a [Token]) -> Option<(Func<'a>, &'a [Token<'a>])> {
    let tokens = consume(tokens, &Token::Function)?;
    let (name, tokens) = parse_identifier(tokens)?;
    let tokens = consume(tokens, &Token::OpenParen)?;
    let (args, tokens) = parse_args(tokens)?;
    let tokens = consume(tokens, &Token::CloseParen)?;
    let (result, tokens) = match tokens.first() {
        Some(&Token::RightArrow) => {
            let tokens = consume(tokens, &Token::RightArrow)?;
            let (result, tokens) = parse_result(tokens)?;
            (Some(result), tokens)
        }
        _ => (None, tokens),
    };
    let tokens = consume(tokens, &Token::OpenCurly)?;
    let tokens = maybe_consume(tokens, &Token::EndOfLine)?;
    let (body, tokens) = parse_expr(tokens, 0)?;
    let tokens = maybe_consume(tokens, &Token::EndOfLine)?;
    let tokens = consume(tokens, &Token::CloseCurly)?;
    Some((
        Func {
            name,
            args,
            result,
            body,
        },
        tokens,
    ))
}

fn parse_args<'a>(tokens: &'a [Token]) -> Option<(Vec<Arg<'a>>, &'a [Token<'a>])> {
    let (first_arg, mut tokens) = match tokens.first() {
        Some(&Token::Identifier(_)) => parse_arg(tokens)?,
        _ => return Some((Vec::new(), tokens)),
    };
    let mut args = vec![first_arg];
    loop {
        if tokens.first() != Some(&Token::Comma) {
            return Some((args, tokens));
        }
        let remaining_tokens = consume(tokens, &Token::Comma)?;
        let (arg, remaining_tokens) = parse_arg(remaining_tokens)?;
        args.push(arg);
        tokens = remaining_tokens;
    }
}

fn parse_arg<'a>(tokens: &'a [Token]) -> Option<(Arg<'a>, &'a [Token<'a>])> {
    let (name, tokens) = parse_identifier(tokens)?;
    let tokens = consume(tokens, &Token::Colon)?;
    let (typ, tokens) = parse_type(tokens)?;
    Some((Arg { name, typ }, tokens))
}

fn parse_result<'a>(tokens: &'a [Token]) -> Option<(Type, &'a [Token<'a>])> {
    parse_type(tokens)
}

fn parse_type<'a>(tokens: &'a [Token]) -> Option<(Type, &'a [Token<'a>])> {
    let (name, tokens) = parse_identifier(tokens)?;
    let typ = match name {
        "bool" => Some(Type::Bool),
        "int" => Some(Type::Int),
        _ => None,
    }?;
    Some((typ, tokens))
}

fn parse_expr<'a>(tokens: &'a [Token], min_precedence: i8) -> Option<(Expr<'a>, &'a [Token<'a>])> {
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
        expr = Expr::BinaryExpr(BinaryExpr {
            operand_a: Box::new(expr),
            operand_b: Box::new(operand_b),
            op,
        });
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

fn parse_unary_expr<'a>(tokens: &'a [Token]) -> Option<(Expr<'a>, &'a [Token<'a>])> {
    match tokens.first() {
        Some(Token::False) => Some((Expr::BoolLit(BoolLit::False), &tokens[1..])),
        Some(Token::True) => Some((Expr::BoolLit(BoolLit::True), &tokens[1..])),
        Some(&Token::IntLiteral(value)) => Some((Expr::IntLit(IntLit { value }), &tokens[1..])),
        Some(Token::If) => {
            let (if_expr, tokens) = parse_if_expr(tokens)?;
            Some((Expr::IfExpr(if_expr), tokens))
        }
        Some(&Token::Identifier(_)) => {
            let (name, tokens) = parse_identifier(tokens)?;
            if tokens.first() == Some(&Token::OpenParen) {
                let (call, tokens) = parse_call(name, tokens)?;
                Some((Expr::Call(call), tokens))
            } else {
                Some((Expr::Identifier(Identifier { name }), tokens))
            }
        }
        _ => None,
    }
}

fn parse_call<'a>(func_name: &'a str, tokens: &'a [Token]) -> Option<(Call<'a>, &'a [Token<'a>])> {
    let tokens = consume(tokens, &Token::OpenParen)?;
    if tokens.first() == Some(&Token::CloseParen) {
        let tokens = consume(tokens, &Token::CloseParen)?;
        return Some((
            Call {
                func_name,
                args: Vec::new(),
            },
            tokens,
        ));
    }
    let (first_arg, mut tokens) = parse_expr(tokens, 0)?;
    let mut args = vec![first_arg];
    loop {
        if tokens.first() != Some(&Token::Comma) {
            let tokens = consume(tokens, &Token::CloseParen)?;
            return Some((Call { func_name, args }, tokens));
        }
        let remaining_tokens = consume(tokens, &Token::Comma)?;
        let (arg, remaining_tokens) = parse_expr(remaining_tokens, 0)?;
        args.push(arg);
        tokens = remaining_tokens;
    }
}

fn parse_if_expr<'a>(tokens: &'a [Token]) -> Option<(IfExpr<'a>, &'a [Token<'a>])> {
    let tokens = consume(tokens, &Token::If)?;
    let (cond, tokens) = parse_expr(tokens, 0)?;
    let tokens = consume(tokens, &Token::OpenCurly)?;
    let (expr_if_true, tokens) = parse_expr(tokens, 0)?;
    let tokens = consume(tokens, &Token::CloseCurly)?;
    let tokens = consume(tokens, &Token::Else)?;
    let tokens = consume(tokens, &Token::OpenCurly)?;
    let (expr_if_false, tokens) = parse_expr(tokens, 0)?;
    let tokens = consume(tokens, &Token::CloseCurly)?;
    Some((
        IfExpr {
            cond: Box::new(cond),
            expr_if_true: Box::new(expr_if_true),
            expr_if_false: Box::new(expr_if_false),
        },
        tokens,
    ))
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
