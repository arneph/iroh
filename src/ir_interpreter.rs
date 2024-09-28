use std::collections::HashMap;
use std::fmt::Display;
use std::iter::zip;

use crate::ir::*;
use crate::types::Type;

pub enum Error {
    ProgramHasNoMainFunc,
    MainDidNotReturnResult,
    UnexpectedNumberOfArgs(String, String, usize, usize),
    UnexpectedNumberOfResults(String, usize, usize),
    UndefinedBlock(String, String),
    UndefinedCallee(String),
    UndefinedComputed(String),
    RedefinedComputed(String),
    UnexpectedType(Type, Type),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Error::*;
        match self {
            ProgramHasNoMainFunc => write!(f, "Program has no main function"),
            MainDidNotReturnResult => write!(f, "Main did not return a result"),
            UnexpectedNumberOfArgs(func_name, block_name, expected, actual) => {
                write!(
                    f,
                    "Function '{func_name}''s block '{block_name}' expected {expected} args, got {actual}"
                )
            }
            UnexpectedNumberOfResults(func_name, expected, actual) => {
                write!(
                    f,
                    "function '{func_name}' expected {expected} results, got {actual}"
                )
            }
            UndefinedBlock(func_name, block_name) => write!(
                f,
                "Function '{func_name}' has no block named '{block_name}'"
            ),
            UndefinedCallee(callee_name) => write!(f, "undefined callee '{callee_name}'"),
            UndefinedComputed(computed_name) => {
                write!(f, "Undefined computed value: '{computed_name}'")
            }
            RedefinedComputed(computed_name) => {
                write!(f, "Redefined computed value: '{computed_name}'")
            }
            UnexpectedType(expected, actual) => {
                write!(f, "Expected type {expected:?}, got {actual:?}")
            }
        }
    }
}

pub fn interpret_program<'a>(program: &'a Program) -> Result<Constant, Error> {
    let main_func = program.main_func().ok_or(Error::ProgramHasNoMainFunc)?;
    interpret_func(program, main_func, Vec::new())?.ok_or(Error::MainDidNotReturnResult)
}

fn interpret_func<'a>(
    program: &'a Program,
    func: &'a Func,
    args: Vec<Constant>,
) -> Result<Option<Constant>, Error> {
    let mut block = func.entry_block();
    let mut args: Vec<Constant> = args;
    loop {
        let result = interpret_block(program, &func.name, block, args)?;
        use BlockResult::*;
        match result {
            Return(result) => {
                return match (&func.result_type, result) {
                    (Some(expected_result_type), Some(result)) => {
                        if expected_result_type != &result.typ() {
                            return Err(Error::UnexpectedType(
                                expected_result_type.clone(),
                                result.typ(),
                            ));
                        }
                        Ok(Some(result))
                    }
                    (None, None) => Ok(None),
                    (_, _) => Err(Error::UnexpectedNumberOfResults(
                        func.name.clone(),
                        func.result_type.iter().len(),
                        result.iter().len(),
                    )),
                };
            }
            Jump(dest, dest_args) => {
                block = func
                    .block_with_name(dest)
                    .ok_or_else(|| Error::UndefinedBlock(func.name.clone(), dest.to_string()))?;
                args = dest_args;
            }
        };
    }
}

struct Context {
    computed_values: HashMap<String, Constant>,
}

impl Context {
    fn get_bool(&self, value: &Value) -> Result<bool, Error> {
        let v = self.get_value(value)?;
        match v {
            Constant::Bool(v) => Ok(v),
            _ => Err(Error::UnexpectedType(Type::Bool, v.typ())),
        }
    }

    fn get_int(&self, value: &Value) -> Result<i64, Error> {
        let v = self.get_value(value)?;
        match v {
            Constant::Int(v) => Ok(v),
            _ => Err(Error::UnexpectedType(Type::Int, v.typ())),
        }
    }

    fn get_value(&self, value: &Value) -> Result<Constant, Error> {
        match value {
            Value::Constant(v) => Ok(*v),
            Value::Computed(computed) => self.get_computed(&computed.name),
        }
    }

    fn get_values(&self, values: &[Value]) -> Result<Vec<Constant>, Error> {
        values.iter().map(|value| self.get_value(value)).collect()
    }

    fn get_computed(&self, name: &str) -> Result<Constant, Error> {
        self.computed_values
            .get(name)
            .ok_or_else(|| Error::UndefinedComputed(name.to_string()))
            .copied()
    }

    fn set_computed(&mut self, name: &str, value: Constant) -> Result<(), Error> {
        match self.computed_values.insert(name.to_string(), value) {
            None => Ok(()),
            Some(_) => Err(Error::RedefinedComputed(name.to_string())),
        }
    }
}

enum BlockResult<'a> {
    Return(Option<Constant>),
    Jump(&'a str, Vec<Constant>),
}

fn interpret_block<'a>(
    program: &'a Program,
    func_name: &str,
    block: &'a Block,
    args: Vec<Constant>,
) -> Result<BlockResult<'a>, Error> {
    if block.args.len() != args.len() {
        return Err(Error::UnexpectedNumberOfArgs(
            func_name.to_string(),
            block.name.clone(),
            block.args.len(),
            args.len(),
        ));
    }
    let mut ctx = Context {
        computed_values: HashMap::new(),
    };
    for (arg, value) in zip(&block.args, args) {
        ctx.set_computed(&arg.name, value)?;
    }
    for instr in &block.instrs {
        interpret_instr(program, instr, &mut ctx)?
    }
    use Continuation::*;
    Ok(match &block.cont {
        Jump(jmp) => BlockResult::Jump(&jmp.dest, ctx.get_values(&jmp.args)?),
        CondJump(cjmp) => {
            if ctx.get_bool(&cjmp.cond)? {
                BlockResult::Jump(
                    &cjmp.jump_if_true.dest,
                    ctx.get_values(&cjmp.jump_if_true.args)?,
                )
            } else {
                BlockResult::Jump(
                    &cjmp.jump_if_false.dest,
                    ctx.get_values(&cjmp.jump_if_false.args)?,
                )
            }
        }
        Return(ret) => BlockResult::Return(
            ret.result
                .as_ref()
                .map_or_else(|| Ok(None), |v| ctx.get_value(v).map(|c| Some(c)))?,
        ),
    })
}

fn interpret_instr<'a>(
    program: &'a Program,
    instr: &'a Instr,
    ctx: &'a mut Context,
) -> Result<(), Error> {
    use Instr::*;
    match instr {
        BoolBinaryInstr(instr) => interpret_bool_binary_instr(instr, ctx),
        IntComparisonInstr(instr) => interpret_int_comparison_instr(instr, ctx),
        IntBinaryInstr(instr) => interpret_int_binary_instr(instr, ctx),
        CallInstr(instr) => interpret_call_instr(program, instr, ctx),
    }
}

fn interpret_bool_binary_instr<'a>(
    instr: &'a BoolBinaryInstr,
    ctx: &'a mut Context,
) -> Result<(), Error> {
    let a = ctx.get_bool(&instr.operand_a)?;
    let b = ctx.get_bool(&instr.operand_b)?;
    use BoolBinaryOp::*;
    let result = match instr.op {
        Or => a || b,
        And => a && b,
        Equal => a == b,
        NotEqual => a != b,
    };
    ctx.set_computed(&instr.result.name, Constant::Bool(result))
}

fn interpret_int_comparison_instr<'a>(
    instr: &'a IntComparisonInstr,
    ctx: &'a mut Context,
) -> Result<(), Error> {
    let a = ctx.get_int(&instr.operand_a)?;
    let b = ctx.get_int(&instr.operand_b)?;
    use IntComparisonOp::*;
    let result = match instr.op {
        Equal => a == b,
        NotEqual => a != b,
        LessThan => a < b,
        LessThanOrEqual => a <= b,
        GreaterThanOrEqual => a >= b,
        GreaterThan => a > b,
    };
    ctx.set_computed(&instr.result.name, Constant::Bool(result))
}

fn interpret_int_binary_instr<'a>(
    instr: &'a IntBinaryInstr,
    ctx: &'a mut Context,
) -> Result<(), Error> {
    let a = ctx.get_int(&instr.operand_a)?;
    let b = ctx.get_int(&instr.operand_b)?;
    use IntBinaryOp::*;
    let result = match instr.op {
        Add => a + b,
        Subtract => a - b,
        Multipy => a * b,
        Divide => a / b,
        Remainder => a % b,
        ShiftLeft => a << b,
        ShiftRight => a >> b,
        BitwiseOr => a | b,
        BitwiseXor => a ^ b,
        BitwiseAnd => a & b,
    };
    ctx.set_computed(&instr.result.name, Constant::Int(result))
}

fn interpret_call_instr<'a>(
    program: &'a Program,
    instr: &'a CallInstr,
    ctx: &'a mut Context,
) -> Result<(), Error> {
    let func = program
        .func_with_name(&instr.callee)
        .ok_or_else(|| Error::UndefinedCallee(instr.callee.clone()))?;
    let args = ctx.get_values(&instr.args)?;
    let results = interpret_func(program, func, args)?;
    for (c, v) in zip(&instr.results, results) {
        ctx.set_computed(&c.name, v)?;
    }
    Ok(())
}
