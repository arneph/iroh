use std::collections::HashSet;
use std::fmt::Display;

use crate::ast;
use crate::ir;
use crate::ir::InstrAttributes;
use crate::types::Type::*;
use crate::types::*;

pub enum Error {
    IncompatibleBinaryOperandsAndOperation,
    IncompatibleIfExprBranchTypes,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Error::*;
        match self {
            IncompatibleBinaryOperandsAndOperation => {
                write!(f, "incompatible binary operands and operation")
            }
            IncompatibleIfExprBranchTypes => {
                write!(f, "incompatible if expr branch types")
            }
        }
    }
}

pub fn build_func(ast_func: ast::Func) -> Result<ir::Func, Error> {
    let mut ir_func = ir::Func::new(ast_func.name, vec![Int]);
    let mut ctx = FuncContext::new(&ir_func);
    let (result, block_index) = build_expr(&ast_func.result, &mut ctx, &mut ir_func, 0)?;
    ir_func.blocks[block_index].cont = ir::Continuation::Return(ir::Return {
        results: vec![result],
    });
    Ok(ir_func)
}

struct FuncContext {
    block_names: HashSet<String>,
    computed_value_count: u64,
}

impl FuncContext {
    fn new(func: &ir::Func) -> FuncContext {
        FuncContext {
            block_names: HashSet::from_iter(func.blocks.iter().map(|b| b.name.clone())),
            computed_value_count: 0,
        }
    }

    fn new_block_name(&mut self, desired: &str) -> String {
        let mut i = 1;
        while self.block_names.contains(&format!("{}{}", desired, i)) {
            i += 1;
        }
        let name = format!("{}{}", desired, i);
        self.block_names.insert(name.clone());
        name
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

fn build_expr<'a, 'b>(
    expr: &ast::Expr,
    ctx: &'a mut FuncContext,
    func: &'a mut ir::Func,
    block_index: ir::BlockIndex,
) -> Result<(ir::Value, ir::BlockIndex), Error> {
    match expr {
        ast::Expr::BoolLit(lit) => Ok((translate_bool_lit(lit), block_index)),
        ast::Expr::IntLit(lit) => Ok((translate_int_lit(lit), block_index)),
        ast::Expr::BinaryExpr(binary_expr) => {
            build_binary_expr(&binary_expr, ctx, func, block_index)
        }
        ast::Expr::IfExpr(if_expr) => build_if_expr(if_expr, ctx, func, block_index),
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

fn build_binary_expr<'a, 'b>(
    expr: &ast::BinaryExpr,
    ctx: &'a mut FuncContext,
    func: &'a mut ir::Func,
    block_index: ir::BlockIndex,
) -> Result<(ir::Value, ir::BlockIndex), Error> {
    let (operand_a, block_index) = build_expr(&expr.operand_a, ctx, func, block_index)?;
    let (operand_b, block_index) = build_expr(&expr.operand_b, ctx, func, block_index)?;
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
    func.blocks[block_index].instrs.push(instr);
    Ok((ir::Value::Computed(result), block_index))
}

fn build_bool_binary_instr(
    operand_a: ir::Value,
    operand_b: ir::Value,
    op: ast::BinaryOp,
    ctx: &mut FuncContext,
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
    operand_a: ir::Value,
    operand_b: ir::Value,
    op: ast::BinaryOp,
    ctx: &mut FuncContext,
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
    operand_a: ir::Value,
    operand_b: ir::Value,
    op: ast::BinaryOp,
    ctx: &mut FuncContext,
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

fn build_if_expr<'a>(
    expr: &ast::IfExpr,
    ctx: &'a mut FuncContext,
    func: &'a mut ir::Func,
    block_index: ir::BlockIndex,
) -> Result<(ir::Value, ir::BlockIndex), Error> {
    let (cond, block_index) = build_expr(&expr.cond, ctx, func, block_index)?;

    let block_name_if_true = ctx.new_block_name("expr_if_true");
    let block_name_if_false = ctx.new_block_name("expr_if_false");
    let follow_block_name = ctx.new_block_name("if_follow");

    let block_index_if_true = func.add_block(&block_name_if_true, Vec::new());
    let (value_if_true, block_index_if_true) =
        build_expr(&expr.expr_if_true, ctx, func, block_index_if_true)?;

    let block_index_if_false = func.add_block(&block_name_if_false, Vec::new());
    let (value_if_false, block_index_if_false) =
        build_expr(&expr.expr_if_false, ctx, func, block_index_if_false)?;

    if value_if_true.typ() != value_if_false.typ() {
        return Err(Error::IncompatibleIfExprBranchTypes);
    }
    let result_type = value_if_true.typ();

    func.blocks[block_index].cont = ir::Continuation::CondJump(ir::CondJump {
        cond: cond,
        jump_if_true: ir::Jump {
            dest: block_name_if_true,
            args: Vec::new(),
        },
        jump_if_false: ir::Jump {
            dest: block_name_if_false,
            args: Vec::new(),
        },
    });
    func.blocks[block_index_if_true].cont = ir::Continuation::Jump(ir::Jump {
        dest: follow_block_name.clone(),
        args: vec![value_if_true],
    });
    func.blocks[block_index_if_false].cont = ir::Continuation::Jump(ir::Jump {
        dest: follow_block_name.clone(),
        args: vec![value_if_false],
    });

    let result = ctx.new_computed_value(result_type);
    let follow_block_index = func.add_block(&follow_block_name, vec![result.clone()]);
    Ok((ir::Value::Computed(result), follow_block_index))
}
