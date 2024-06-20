use std::fmt::Display;

use crate::types::*;
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

#[enum_dispatch]
pub trait InstrValues {
    fn arguments(&self) -> Vec<&Value>;
    fn results(&self) -> Vec<&Computed>;
}

#[derive(Debug)]
#[enum_dispatch(InstrValues)]
pub enum Instr {
    BoolBinaryInstr(BoolBinaryInstr),
    IntComparisonInstr(IntComparisonInstr),
    IntBinaryInstr(IntBinaryInstr),
}

#[derive(Debug)]
pub enum BoolBinaryOp {
    Or,
    And,
    Equal,
    NotEqual,
}

#[derive(Debug)]
pub struct BoolBinaryInstr {
    pub operand_a: Value,
    pub operand_b: Value,
    pub op: BoolBinaryOp,
    pub result: Computed,
}

impl InstrValues for BoolBinaryInstr {
    fn arguments(&self) -> Vec<&Value> {
        vec![&self.operand_a, &self.operand_b]
    }
    fn results(&self) -> Vec<&Computed> {
        vec![&self.result]
    }
}

#[derive(Debug)]
pub enum IntComparisonOp {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    GreaterThan,
}

#[derive(Debug)]
pub struct IntComparisonInstr {
    pub operand_a: Value,
    pub operand_b: Value,
    pub op: IntComparisonOp,
    pub result: Computed,
}

impl InstrValues for IntComparisonInstr {
    fn arguments(&self) -> Vec<&Value> {
        vec![&self.operand_a, &self.operand_b]
    }
    fn results(&self) -> Vec<&Computed> {
        vec![&self.result]
    }
}

#[derive(Debug)]
pub enum IntBinaryOp {
    Add,
    Subtract,
    Multipy,
    Divide,
    Remainder,
    ShiftLeft,
    ShiftRight,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
}

#[derive(Debug)]
pub struct IntBinaryInstr {
    pub operand_a: Value,
    pub operand_b: Value,
    pub op: IntBinaryOp,
    pub result: Computed,
}

impl InstrValues for IntBinaryInstr {
    fn arguments(&self) -> Vec<&Value> {
        vec![&self.operand_a, &self.operand_b]
    }
    fn results(&self) -> Vec<&Computed> {
        vec![&self.result]
    }
}

#[derive(Debug)]
pub enum Value {
    Constant(Constant),
    Computed(Computed),
}

impl Value {
    pub fn typ(&self) -> Type {
        use Value::*;
        match self {
            Constant(c) => c.typ(),
            Computed(c) => c.typ.clone(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Constant {
    Bool(bool),
    Int(i64),
}

impl Constant {
    pub fn typ(&self) -> Type {
        match self {
            Constant::Bool(_) => Type::Bool,
            Constant::Int(_) => Type::Int,
        }
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Constant::*;
        match self {
            Bool(v) => write!(f, "{}", v),
            Int(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Computed {
    pub name: String,
    pub typ: Type,
}
