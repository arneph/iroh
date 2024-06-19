use crate::ast;
use crate::ir;

pub fn build_func(func: ast::Func) -> ir::Func {
    let mut ctx = BlockContext::new();
    let (instrs, result) = build_instrs_for_expr(&func.result, &mut ctx);
    let block = ir::Block { instrs, result };
    ir::Func {
        name: func.name.to_string(),
        blocks: vec![block],
    }
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

    fn new_computed_value(&mut self) -> ir::Computed {
        let index = self.computed_value_count;
        self.computed_value_count += 1;
        ir::Computed {
            name: format!("v{index}"),
        }
    }
}

fn build_instrs_for_expr(expr: &ast::Expr, ctx: &mut BlockContext) -> (Vec<ir::Instr>, ir::Value) {
    match expr {
        ast::Expr::IntLit(lit) => (vec![], ir::Value::Constant(lit.value)),
        ast::Expr::BinaryExpr(binary_expr) => build_instrs_for_binary_expr(&binary_expr, ctx),
    }
}

fn build_instrs_for_binary_expr(
    expr: &ast::BinaryExpr,
    ctx: &mut BlockContext,
) -> (Vec<ir::Instr>, ir::Value) {
    let (operand_a_instrs, operand_a) = build_instrs_for_expr(&expr.operand_a, ctx);
    let (operand_b_instrs, operand_b) = build_instrs_for_expr(&expr.operand_b, ctx);
    let op = translate_binary_op(expr.op);
    let result = ctx.new_computed_value();
    let instr = ir::Instr::BinaryInstr(ir::BinaryInstr {
        operand_a,
        operand_b,
        op,
        result: result.clone(),
    });
    (
        operand_a_instrs
            .into_iter()
            .chain(operand_b_instrs)
            .chain([instr])
            .collect(),
        ir::Value::Computed(result),
    )
}

fn translate_binary_op(op: ast::BinaryOp) -> ir::BinaryOp {
    match op {
        ast::BinaryOp::Add => ir::BinaryOp::Add,
        ast::BinaryOp::Subtract => ir::BinaryOp::Subtract,
        ast::BinaryOp::Multipy => ir::BinaryOp::Multipy,
        ast::BinaryOp::Divide => ir::BinaryOp::Divide,
        ast::BinaryOp::Remainder => ir::BinaryOp::Remainder,
    }
}
