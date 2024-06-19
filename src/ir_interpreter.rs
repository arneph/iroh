use std::collections::HashMap;
use std::fmt::Display;

use crate::ir::Value::*;
use crate::ir::*;

pub enum Error {
    FuncHasNoBlocks(String),
    UndefinedComputed(String),
    RedefinedComputed(String),
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
        }
    }
}

struct Context {
    computed_values: HashMap<String, i64>,
}

impl Context {
    fn get_value(&self, value: &Value) -> Result<i64, Error> {
        match value {
            Constant(v) => Ok(*v),
            Computed(computed) => self.get_computed(&computed.name),
        }
    }
    fn get_computed(&self, name: &str) -> Result<i64, Error> {
        self.computed_values
            .get(name)
            .ok_or_else(|| Error::UndefinedComputed(name.to_string()))
            .copied()
    }
    fn set_computed(&mut self, name: &str, value: i64) -> Result<(), Error> {
        match self.computed_values.insert(name.to_string(), value) {
            None => Ok(()),
            Some(_) => Err(Error::RedefinedComputed(name.to_string())),
        }
    }
}

pub fn interpret_func<'a>(func: &'a Func) -> Result<i64, Error> {
    let block = match func.blocks.first() {
        None => Err(Error::FuncHasNoBlocks(func.name.clone())),
        Some(block) => Ok(block),
    }?;
    interpret_block(block)
}

fn interpret_block<'a>(block: &'a Block) -> Result<i64, Error> {
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
        BinaryInstr(instr) => interpret_binary_instr(instr, ctx),
    }
}

fn interpret_binary_instr<'a>(instr: &'a BinaryInstr, ctx: &'a mut Context) -> Result<(), Error> {
    let a = ctx.get_value(&instr.operand_a)?;
    let b = ctx.get_value(&instr.operand_b)?;
    use BinaryOp::*;
    let result = match instr.op {
        Add => a + b,
        Subtract => a - b,
        Multipy => a * b,
        Divide => a / b,
        Remainder => a % b,
    };
    ctx.set_computed(&instr.result.name, result)
}
