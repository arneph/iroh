use std::fmt::Display;

use crate::ast;
use crate::ir;
use crate::ir::InstrAttributes;
use crate::ir::Value;
use crate::types::Type::*;
use crate::types::*;

pub enum Error {
    IncompatibleBinaryOperandsAndOperation,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Error::*;
        match self {
            IncompatibleBinaryOperandsAndOperation => {
                write!(f, "incompatible binary operands and operation")
            }
        }
    }
}

pub fn build_func(func: ast::Func) -> Result<ir::Func, Error> {
    let mut ctx = BlockContext::new();
    let (instrs, result) = build_instrs_for_expr(&func.result, &mut ctx)?;
    let block = ir::Block {
        name: "entry".to_string(),
        instrs,
        result,
    };
    Ok(ir::Func {
        name: func.name.to_string(),
        blocks: vec![block],
    })
}

struct BlockContext {
    computed_value_count: u64,
}

impl BlockContext {
    fn new() -> BlockContext {
        BlockContext {
            computed_value_count: 0,
        }
    }

    fn new_computed_value(&mut self, typ: Type) -> ir::Computed {
        let index = self.computed_value_count;
        self.computed_value_count += 1;
        ir::Computed {
            name: format!("v{index}"),
            typ: typ,
        }
    }
}

fn build_instrs_for_expr(
    expr: &ast::Expr,
    ctx: &mut BlockContext,
) -> Result<(Vec<ir::Instr>, ir::Value), Error> {
    match expr {
        ast::Expr::BoolLit(lit) => Ok((vec![], translate_bool_lit(lit))),
        ast::Expr::IntLit(lit) => Ok((vec![], translate_int_lit(lit))),
        ast::Expr::BinaryExpr(binary_expr) => build_instrs_for_binary_expr(&binary_expr, ctx),
    }
}

fn translate_bool_lit(lit: &ast::BoolLit) -> ir::Value {
    ir::Value::Constant(ir::Constant::Bool(match lit {
        &ast::BoolLit::False => false,
        &ast::BoolLit::True => true,
    }))
}

fn translate_int_lit(lit: &ast::IntLit) -> ir::Value {
    ir::Value::Constant(ir::Constant::Int(lit.value))
}

fn build_instrs_for_binary_expr(
    expr: &ast::BinaryExpr,
    ctx: &mut BlockContext,
) -> Result<(Vec<ir::Instr>, ir::Value), Error> {
    let (operand_a_instrs, operand_a) = build_instrs_for_expr(&expr.operand_a, ctx)?;
    let (operand_b_instrs, operand_b) = build_instrs_for_expr(&expr.operand_b, ctx)?;
    use ast::BinaryOp::*;
    use ir::Instr::*;
    let instr = match (operand_a.typ(), operand_b.typ(), expr.op) {
        (Bool, Bool, Or | And | Equal | NotEqual) => {
            BoolBinaryInstr(build_bool_binary_instr(operand_a, operand_b, expr.op, ctx))
        }
        (
            Int,
            Int,
            Equal | NotEqual | LessThan | LessThanOrEqual | GreaterThanOrEqual | GreaterThan,
        ) => IntComparisonInstr(build_int_comparison_instr(
            operand_a, operand_b, expr.op, ctx,
        )),
        (
            Int,
            Int,
            Add | Subtract | Multipy | Divide | Remainder | ShiftLeft | ShiftRight | BitwiseOr
            | BitwiseXor | BitwiseAnd,
        ) => IntBinaryInstr(build_int_binary_instr(operand_a, operand_b, expr.op, ctx)),
        _ => return Err(Error::IncompatibleBinaryOperandsAndOperation),
    };
    let result = (*instr.results().first().unwrap()).clone();
    Ok((
        operand_a_instrs
            .into_iter()
            .chain(operand_b_instrs)
            .chain([instr])
            .collect(),
        ir::Value::Computed(result),
    ))
}

fn build_bool_binary_instr(
    operand_a: Value,
    operand_b: Value,
    op: ast::BinaryOp,
    ctx: &mut BlockContext,
) -> ir::BoolBinaryInstr {
    let result = ctx.new_computed_value(Bool);
    ir::BoolBinaryInstr {
        operand_a: operand_a,
        operand_b: operand_b,
        op: translate_bool_binary_op(op),
        result,
    }
}

fn translate_bool_binary_op(op: ast::BinaryOp) -> ir::BoolBinaryOp {
    match op {
        ast::BinaryOp::Or => ir::BoolBinaryOp::Or,
        ast::BinaryOp::And => ir::BoolBinaryOp::And,
        ast::BinaryOp::Equal => ir::BoolBinaryOp::Equal,
        ast::BinaryOp::NotEqual => ir::BoolBinaryOp::NotEqual,
        _ => panic!("unexpected int comparison op: {:?}", op),
    }
}

fn build_int_comparison_instr(
    operand_a: Value,
    operand_b: Value,
    op: ast::BinaryOp,
    ctx: &mut BlockContext,
) -> ir::IntComparisonInstr {
    let result = ctx.new_computed_value(Bool);
    ir::IntComparisonInstr {
        operand_a,
        operand_b,
        op: translate_int_comparison_op(op),
        result,
    }
}

fn translate_int_comparison_op(op: ast::BinaryOp) -> ir::IntComparisonOp {
    match op {
        ast::BinaryOp::Equal => ir::IntComparisonOp::Equal,
        ast::BinaryOp::NotEqual => ir::IntComparisonOp::NotEqual,
        ast::BinaryOp::LessThan => ir::IntComparisonOp::LessThan,
        ast::BinaryOp::LessThanOrEqual => ir::IntComparisonOp::LessThanOrEqual,
        ast::BinaryOp::GreaterThanOrEqual => ir::IntComparisonOp::GreaterThanOrEqual,
        ast::BinaryOp::GreaterThan => ir::IntComparisonOp::GreaterThan,
        _ => panic!("unexpected int comparison op: {:?}", op),
    }
}

fn build_int_binary_instr(
    operand_a: Value,
    operand_b: Value,
    op: ast::BinaryOp,
    ctx: &mut BlockContext,
) -> ir::IntBinaryInstr {
    let result = ctx.new_computed_value(Int);
    ir::IntBinaryInstr {
        operand_a,
        operand_b,
        op: translate_int_binary_op(op),
        result,
    }
}

fn translate_int_binary_op(op: ast::BinaryOp) -> ir::IntBinaryOp {
    match op {
        ast::BinaryOp::Add => ir::IntBinaryOp::Add,
        ast::BinaryOp::Subtract => ir::IntBinaryOp::Subtract,
        ast::BinaryOp::Multipy => ir::IntBinaryOp::Multipy,
        ast::BinaryOp::Divide => ir::IntBinaryOp::Divide,
        ast::BinaryOp::Remainder => ir::IntBinaryOp::Remainder,
        ast::BinaryOp::ShiftLeft => ir::IntBinaryOp::ShiftLeft,
        ast::BinaryOp::ShiftRight => ir::IntBinaryOp::ShiftRight,
        ast::BinaryOp::BitwiseOr => ir::IntBinaryOp::BitwiseOr,
        ast::BinaryOp::BitwiseXor => ir::IntBinaryOp::BitwiseXor,
        ast::BinaryOp::BitwiseAnd => ir::IntBinaryOp::BitwiseAnd,
        _ => panic!("unexpected int binary op: {:?}", op),
    }
}
