use std::collections::HashMap;
use std::fmt::Display;

use crate::ir::*;
use crate::types::Type;

pub enum Error {
    FuncHasNoBlocks(String),
    UndefinedComputed(String),
    RedefinedComputed(String),
    UnexpectedType(Type, Type),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Error::*;
        match self {
            FuncHasNoBlocks(func_name) => write!(f, "Function '{}' has no blocks", func_name),
            UndefinedComputed(computed_name) => {
                write!(f, "Undefined computed value: '{}'", computed_name)
            }
            RedefinedComputed(computed_name) => {
                write!(f, "Redefined computed value: '{}'", computed_name)
            }
            UnexpectedType(expected, actual) => {
                write!(f, "Expected type {:?}, got {:?}", expected, actual)
            }
        }
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

pub fn interpret_func<'a>(func: &'a Func) -> Result<Constant, Error> {
    let block = match func.blocks.first() {
        None => Err(Error::FuncHasNoBlocks(func.name.clone())),
        Some(block) => Ok(block),
    }?;
    interpret_block(block)
}

fn interpret_block<'a>(block: &'a Block) -> Result<Constant, Error> {
    let mut ctx = Context {
        computed_values: HashMap::new(),
    };
    for instr in &block.instrs {
        interpret_instr(instr, &mut ctx)?
    }
    ctx.get_value(&block.result)
}

fn interpret_instr<'a>(instr: &'a Instr, ctx: &'a mut Context) -> Result<(), Error> {
    use Instr::*;
    match instr {
        BoolBinaryInstr(instr) => interpret_bool_binary_instr(instr, ctx),
        IntComparisonInstr(instr) => interpret_int_comparison_instr(instr, ctx),
        IntBinaryInstr(instr) => interpret_int_binary_instr(instr, ctx),
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
