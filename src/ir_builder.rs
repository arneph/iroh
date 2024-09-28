use std::collections::BTreeMap;
use std::collections::HashSet;
use std::fmt::Display;
use std::iter::Iterator;

use crate::ast;
use crate::ir;
use crate::ir::InstrAttributes;
use crate::types::Type::*;
use crate::types::*;

pub enum Error {
    ExprHasNoValue,
    IncompatibleBinaryOperandsAndOperation,
    IncompatibleIfExprBranchTypes,
    UndefinedIdentifier,
    UndefinedCallee,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Error::*;
        match self {
            ExprHasNoValue => {
                write!(f, "expression has no value")
            }
            IncompatibleBinaryOperandsAndOperation => {
                write!(f, "incompatible binary operands and operation")
            }
            IncompatibleIfExprBranchTypes => {
                write!(f, "incompatible if expr branch types")
            }
            UndefinedIdentifier => {
                write!(f, "undefined identifier")
            }
            UndefinedCallee => {
                write!(f, "undefined callee")
            }
        }
    }
}

pub fn build_program(ast_program: &ast::Program) -> Result<ir::Program, Error> {
    let ir_funcs = ast_program
        .funcs
        .iter()
        .map(|ast_func| build_func(ast_program, ast_func))
        .collect::<Result<Vec<ir::Func>, Error>>()?;
    Ok(ir::Program { funcs: ir_funcs })
}

fn build_func(ast_program: &ast::Program, ast_func: &ast::Func) -> Result<ir::Func, Error> {
    let mut ir_func = ir::Func::new(ast_func.name, ast_func.result.clone());
    let mut func_ctx = FuncContext::new(&ir_func);
    let entry_block = ir_func.entry_block_mut();
    for ast_arg in &ast_func.args {
        entry_block.args.push(ir::Computed {
            name: ast_arg.name.to_string(),
            typ: ast_arg.typ.clone(),
        })
    }
    let block_ctx = BlockContext::new(&entry_block);
    let (result, block_index, _) = build_expr(
        ast_program,
        &ast_func.body,
        &mut func_ctx,
        block_ctx,
        &mut ir_func,
        0,
    )?;
    ir_func.blocks[block_index].cont = ir::Continuation::Return(ir::Return { result });
    Ok(ir_func)
}

struct FuncContext {
    block_names: HashSet<String>,
}

impl FuncContext {
    fn new(func: &ir::Func) -> FuncContext {
        FuncContext {
            block_names: HashSet::from_iter(func.blocks.iter().map(|b| b.name.clone())),
        }
    }

    fn new_block_name(&mut self, desired: &str) -> String {
        let mut i = 1;
        while self.block_names.contains(&format!("{desired}{i}")) {
            i += 1;
        }
        let name = format!("{}{}", desired, i);
        self.block_names.insert(name.clone());
        name
    }
}

struct BlockContext {
    computed_value_count: u64,
    vars_to_computed_values: BTreeMap<String, ir::Computed>,
}

impl BlockContext {
    fn new(entry_block: &ir::Block) -> BlockContext {
        let mut vars_to_computed_values = BTreeMap::new();
        for arg in &entry_block.args {
            vars_to_computed_values.insert(arg.name.to_string(), arg.clone());
        }
        BlockContext {
            computed_value_count: vars_to_computed_values.len() as u64,
            vars_to_computed_values: vars_to_computed_values,
        }
    }

    fn merged_from(a: &BlockContext, b: &BlockContext) -> BlockContext {
        assert!(a
            .vars_to_computed_values
            .keys()
            .eq(b.vars_to_computed_values.keys()));
        BlockContext {
            computed_value_count: a.vars_to_computed_values.len() as u64,
            vars_to_computed_values: a.vars_to_computed_values.clone(),
        }
    }

    fn child_context(&self) -> BlockContext {
        BlockContext {
            computed_value_count: self.vars_to_computed_values.len() as u64,
            vars_to_computed_values: self.vars_to_computed_values.clone(),
        }
    }

    fn computed_value_for_var(&self, name: &str) -> Option<&ir::Computed> {
        self.vars_to_computed_values.get(name)
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

fn build_exprs<'a>(
    ast_program: &ast::Program,
    exprs: &[ast::Expr],
    func_ctx: &'a mut FuncContext,
    block_ctx: BlockContext,
    func: &'a mut ir::Func,
    block_index: ir::BlockIndex,
) -> Result<(Vec<ir::Value>, ir::BlockIndex, BlockContext), Error> {
    let mut block_index = block_index;
    let mut block_ctx = block_ctx;
    let mut values = Vec::<ir::Value>::new();
    for expr in exprs {
        let (value, i, ctx) =
            build_expr(ast_program, expr, func_ctx, block_ctx, func, block_index)?;
        values.push(value.ok_or(Error::ExprHasNoValue)?);
        block_index = i;
        block_ctx = ctx;
    }
    Ok((values, block_index, block_ctx))
}

fn build_expr<'a>(
    ast_program: &ast::Program,
    expr: &ast::Expr,
    func_ctx: &'a mut FuncContext,
    block_ctx: BlockContext,
    func: &'a mut ir::Func,
    block_index: ir::BlockIndex,
) -> Result<(Option<ir::Value>, ir::BlockIndex, BlockContext), Error> {
    match expr {
        ast::Expr::BoolLit(lit) => Ok((Some(translate_bool_lit(lit)), block_index, block_ctx)),
        ast::Expr::IntLit(lit) => Ok((Some(translate_int_lit(lit)), block_index, block_ctx)),
        ast::Expr::Identifier(ident) => {
            let result = translate_identifier(ident, &block_ctx)?;
            Ok((Some(result), block_index, block_ctx))
        }
        ast::Expr::BinaryExpr(binary_expr) => {
            let (result, block_index, block_ctx) = build_binary_expr(
                ast_program,
                &binary_expr,
                func_ctx,
                block_ctx,
                func,
                block_index,
            )?;
            Ok((Some(result), block_index, block_ctx))
        }
        ast::Expr::IfExpr(if_expr) => {
            build_if_expr(ast_program, if_expr, func_ctx, block_ctx, func, block_index)
        }
        ast::Expr::Call(call) => {
            build_call_expr(ast_program, call, func_ctx, block_ctx, func, block_index)
        }
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

fn translate_identifier(ident: &ast::Identifier, ctx: &BlockContext) -> Result<ir::Value, Error> {
    ctx.computed_value_for_var(ident.name)
        .cloned()
        .map(|c| ir::Value::Computed(c))
        .ok_or(Error::UndefinedIdentifier)
}

fn build_binary_expr<'a>(
    ast_program: &ast::Program,
    expr: &ast::BinaryExpr,
    func_ctx: &'a mut FuncContext,
    block_ctx: BlockContext,
    func: &'a mut ir::Func,
    block_index: ir::BlockIndex,
) -> Result<(ir::Value, ir::BlockIndex, BlockContext), Error> {
    let (operand_a, block_index, block_ctx) = build_expr(
        ast_program,
        &expr.operand_a,
        func_ctx,
        block_ctx,
        func,
        block_index,
    )?;
    let operand_a = operand_a.ok_or(Error::ExprHasNoValue)?;
    let (operand_b, block_index, mut block_ctx) = build_expr(
        ast_program,
        &expr.operand_b,
        func_ctx,
        block_ctx,
        func,
        block_index,
    )?;
    let operand_b = operand_b.ok_or(Error::ExprHasNoValue)?;
    use ast::BinaryOp::*;
    use ir::Instr::*;
    let instr = match (operand_a.typ(), operand_b.typ(), expr.op) {
        (Bool, Bool, Or | And | Equal | NotEqual) => BoolBinaryInstr(build_bool_binary_instr(
            operand_a,
            operand_b,
            expr.op,
            &mut block_ctx,
        )),
        (
            Int,
            Int,
            Equal | NotEqual | LessThan | LessThanOrEqual | GreaterThanOrEqual | GreaterThan,
        ) => IntComparisonInstr(build_int_comparison_instr(
            operand_a,
            operand_b,
            expr.op,
            &mut block_ctx,
        )),
        (
            Int,
            Int,
            Add | Subtract | Multipy | Divide | Remainder | ShiftLeft | ShiftRight | BitwiseOr
            | BitwiseXor | BitwiseAnd,
        ) => IntBinaryInstr(build_int_binary_instr(
            operand_a,
            operand_b,
            expr.op,
            &mut block_ctx,
        )),
        _ => return Err(Error::IncompatibleBinaryOperandsAndOperation),
    };
    let result = (*instr.results().first().unwrap()).clone();
    func.blocks[block_index].instrs.push(instr);
    Ok((ir::Value::Computed(result), block_index, block_ctx))
}

fn build_bool_binary_instr(
    operand_a: ir::Value,
    operand_b: ir::Value,
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
    operand_a: ir::Value,
    operand_b: ir::Value,
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
    operand_a: ir::Value,
    operand_b: ir::Value,
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

fn build_if_expr<'a>(
    ast_program: &ast::Program,
    expr: &ast::IfExpr,
    func_ctx: &'a mut FuncContext,
    block_ctx: BlockContext,
    func: &'a mut ir::Func,
    block_index: ir::BlockIndex,
) -> Result<(Option<ir::Value>, ir::BlockIndex, BlockContext), Error> {
    let (cond, block_index, mut block_ctx) = build_expr(
        ast_program,
        &expr.cond,
        func_ctx,
        block_ctx,
        func,
        block_index,
    )?;
    let cond = cond.ok_or(Error::ExprHasNoValue)?;

    let block_name_if_true = func_ctx.new_block_name("expr_if_true");
    let block_name_if_false = func_ctx.new_block_name("expr_if_false");
    let follow_block_name = func_ctx.new_block_name("if_follow");

    let block_ctx_if_true = block_ctx.child_context();
    let block_index_if_true = func.add_block(
        &block_name_if_true,
        block_ctx_if_true
            .vars_to_computed_values
            .values()
            .cloned()
            .collect(),
    );
    let (value_if_true, block_index_if_true, block_ctx_if_true) = build_expr(
        ast_program,
        &expr.expr_if_true,
        func_ctx,
        block_ctx_if_true,
        func,
        block_index_if_true,
    )?;

    let block_ctx_if_false = block_ctx.child_context();
    let block_index_if_false = func.add_block(
        &block_name_if_false,
        block_ctx_if_false
            .vars_to_computed_values
            .values()
            .cloned()
            .collect(),
    );
    let (value_if_false, block_index_if_false, block_ctx_if_false) = build_expr(
        ast_program,
        &expr.expr_if_false,
        func_ctx,
        block_ctx_if_false,
        func,
        block_index_if_false,
    )?;

    let result_type = match (&value_if_true, &value_if_false) {
        (Some(true_type), Some(false_type)) => {
            if true_type.typ() != false_type.typ() {
                return Err(Error::IncompatibleIfExprBranchTypes);
            }
            Some(true_type.typ())
        }
        (None, None) => None,
        _ => {
            return Err(Error::IncompatibleIfExprBranchTypes);
        }
    };

    func.blocks[block_index].cont = ir::Continuation::CondJump(ir::CondJump {
        cond: cond,
        jump_if_true: ir::Jump {
            dest: block_name_if_true,
            args: block_ctx
                .vars_to_computed_values
                .values()
                .cloned()
                .map(|c| ir::Value::Computed(c))
                .collect(),
        },
        jump_if_false: ir::Jump {
            dest: block_name_if_false,
            args: block_ctx
                .vars_to_computed_values
                .values()
                .cloned()
                .map(|c| ir::Value::Computed(c))
                .collect(),
        },
    });
    func.blocks[block_index_if_true].cont = ir::Continuation::Jump(ir::Jump {
        dest: follow_block_name.clone(),
        args: value_if_true.into_iter().collect(),
    });
    func.blocks[block_index_if_false].cont = ir::Continuation::Jump(ir::Jump {
        dest: follow_block_name.clone(),
        args: value_if_false.into_iter().collect(),
    });

    let result = result_type
        .as_ref()
        .map(|r| block_ctx.new_computed_value(r.clone()));
    let follow_block_index =
        func.add_block(&follow_block_name, result.clone().into_iter().collect());
    let follow_block_ctx = BlockContext::merged_from(&block_ctx_if_true, &block_ctx_if_false);
    Ok((
        result.map(|r| ir::Value::Computed(r)),
        follow_block_index,
        follow_block_ctx,
    ))
}

fn build_call_expr<'a>(
    ast_program: &ast::Program,
    expr: &ast::Call,
    func_ctx: &'a mut FuncContext,
    block_ctx: BlockContext,
    func: &'a mut ir::Func,
    block_index: ir::BlockIndex,
) -> Result<(Option<ir::Value>, ir::BlockIndex, BlockContext), Error> {
    let callee = expr.func_name.to_string();
    let ast_func = ast_program
        .func_with_name(&callee)
        .ok_or(Error::UndefinedCallee)?;
    let (args, block_index, mut block_ctx) = build_exprs(
        ast_program,
        &expr.args,
        func_ctx,
        block_ctx,
        func,
        block_index,
    )?;
    let result = ast_func
        .result
        .as_ref()
        .map(|r| block_ctx.new_computed_value(r.clone()));
    let results = result.clone().into_iter().collect();
    let instr = ir::CallInstr {
        callee,
        args,
        results,
    };
    func.blocks[block_index]
        .instrs
        .push(ir::Instr::CallInstr(instr));
    Ok((
        result.map(|r| ir::Value::Computed(r)),
        block_index,
        block_ctx,
    ))
}
