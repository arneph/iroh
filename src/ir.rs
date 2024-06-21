use std::fmt::{Debug, Display};

use crate::types::*;
use enum_dispatch::enum_dispatch;

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub blocks: Vec<Block>,
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} {{", self.name)?;
        for block in self.blocks.iter() {
            write!(f, "{}", block)?;
        }
        writeln!(f, "}}")
    }
}

#[derive(Debug)]
pub struct Block {
    pub name: String,
    pub instrs: Vec<Instr>,
    pub result: Value,
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, " {}:", self.name)?;
        for instr in self.instrs.iter() {
            writeln!(f, "  {}", instr)?;
        }
        writeln!(f, "  ret {}", self.result)
    }
}

#[enum_dispatch]
pub trait InstrAttributes {
    fn name(&self) -> &'static str;
    fn arguments(&self) -> Vec<&Value>;
    fn results(&self) -> Vec<&Computed>;
}

#[derive(Debug)]
#[enum_dispatch(InstrAttributes)]
pub enum Instr {
    BoolBinaryInstr(BoolBinaryInstr),
    IntComparisonInstr(IntComparisonInstr),
    IntBinaryInstr(IntBinaryInstr),
}

impl Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let results = self.results();
        for (i, result) in results.iter().enumerate() {
            write!(f, "{}", result)?;
            if i < results.len() - 1 {
                write!(f, ", ")?;
            } else {
                write!(f, " = ")?;
            }
        }
        write!(f, "{}", self.name())?;
        let args = self.arguments();
        for (i, arg) in args.iter().enumerate() {
            if i == 0 {
                write!(f, " ")?;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum BoolBinaryOp {
    Or,
    And,
    Equal,
    NotEqual,
}

impl BoolBinaryOp {
    fn name(&self) -> &'static str {
        use BoolBinaryOp::*;
        match self {
            Or => "or",
            And => "and",
            Equal => "eq",
            NotEqual => "ne",
        }
    }
}

#[derive(Debug)]
pub struct BoolBinaryInstr {
    pub operand_a: Value,
    pub operand_b: Value,
    pub op: BoolBinaryOp,
    pub result: Computed,
}

impl InstrAttributes for BoolBinaryInstr {
    fn name(&self) -> &'static str {
        self.op.name()
    }
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

impl IntComparisonOp {
    fn name(&self) -> &'static str {
        use IntComparisonOp::*;
        match self {
            Equal => "eq",
            NotEqual => "ne",
            LessThan => "lt",
            LessThanOrEqual => "le",
            GreaterThanOrEqual => "ge",
            GreaterThan => "gt",
        }
    }
}

#[derive(Debug)]
pub struct IntComparisonInstr {
    pub operand_a: Value,
    pub operand_b: Value,
    pub op: IntComparisonOp,
    pub result: Computed,
}

impl InstrAttributes for IntComparisonInstr {
    fn name(&self) -> &'static str {
        self.op.name()
    }
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

impl IntBinaryOp {
    fn name(&self) -> &'static str {
        use IntBinaryOp::*;
        match self {
            Add => "add",
            Subtract => "sub",
            Multipy => "mul",
            Divide => "div",
            Remainder => "rem",
            ShiftLeft => "shl",
            ShiftRight => "shr",
            BitwiseOr => "or",
            BitwiseXor => "xor",
            BitwiseAnd => "and",
        }
    }
}

#[derive(Debug)]
pub struct IntBinaryInstr {
    pub operand_a: Value,
    pub operand_b: Value,
    pub op: IntBinaryOp,
    pub result: Computed,
}

impl InstrAttributes for IntBinaryInstr {
    fn name(&self) -> &'static str {
        self.op.name()
    }
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

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Constant(c) => write!(f, "{}", c),
            Value::Computed(c) => write!(f, "{}", c),
        }
    }
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

impl Display for Computed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.name, self.typ)
    }
}
