use std::fmt::Debug;

use enum_dispatch::enum_dispatch;

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub blocks: Vec<Block>,
}

#[derive(Debug)]
pub struct Block {
    pub instrs: Vec<Instr>,
    pub result: Value,
}

pub trait InstrValues {
    fn arguments(&self) -> Vec<&Value>;
    fn results(&self) -> Vec<&Computed>;
}

#[derive(Debug)]
#[enum_dispatch(InstrValues)]
pub enum Instr {
    BinaryInstr(BinaryInstr),
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multipy,
    Divide,
    Remainder,
}

#[derive(Debug)]
pub struct BinaryInstr {
    pub operand_a: Value,
    pub operand_b: Value,
    pub op: BinaryOp,
    pub result: Computed,
}

impl InstrValues for BinaryInstr {
    fn arguments(&self) -> Vec<&Value> {
        vec![&self.operand_a, &self.operand_b]
    }
    fn results(&self) -> Vec<&Computed> {
        vec![&self.result]
    }
}

#[derive(Debug)]
pub enum Value {
    Constant(i64),
    Computed(Computed),
}

#[derive(Clone, Debug)]
pub struct Computed {
    pub name: String,
}
