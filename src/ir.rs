use std::{
    fmt::{Debug, Display},
    iter::once,
};

use crate::types::*;
use enum_dispatch::enum_dispatch;

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub result_types: Vec<Type>,
    pub blocks: Vec<Block>,
}

impl Func {
    pub fn new(name: &str, result_types: Vec<Type>) -> Func {
        Func {
            name: name.to_string(),
            result_types: result_types,
            blocks: vec![Block::new("entry", Vec::new())],
        }
    }

    pub fn args(&self) -> &[Computed] {
        self.entry_block().args.as_slice()
    }

    pub fn entry_block(&self) -> &Block {
        self.blocks.first().unwrap()
    }

    pub fn block_with_name<'a>(&'a self, name: &'a str) -> Option<&'a Block> {
        self.blocks.iter().find(|b| b.name == name)
    }

    pub fn add_block(&mut self, name: &str, args: Vec<Computed>) -> BlockIndex {
        self.blocks.push(Block::new(name, args));
        self.blocks.len() - 1
    }
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

pub type BlockIndex = usize;

#[derive(Debug)]
pub struct Block {
    pub name: String,
    pub args: Vec<Computed>,
    pub instrs: Vec<Instr>,
    pub cont: Continuation,
}

impl Block {
    pub fn new(name: &str, args: Vec<Computed>) -> Block {
        Block {
            name: name.to_string(),
            args: args,
            instrs: Vec::new(),
            cont: Continuation::Return(Return {
                results: Vec::new(),
            }),
        }
    }

    pub fn instrs_and_cont(&self) -> Vec<&dyn InstrAttributes> {
        self.instrs
            .iter()
            .map(|i| i as &dyn InstrAttributes)
            .chain(once(&self.cont as &dyn InstrAttributes))
            .collect()
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, " {}(", self.name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        writeln!(f, "):")?;
        for instr in self.instrs.iter() {
            writeln!(f, "  {}", instr)?;
        }
        writeln!(f, "  {}", self.cont)
    }
}

#[derive(Debug)]
#[enum_dispatch(InstrAttributes)]
pub enum Continuation {
    Jump(Jump),
    CondJump(CondJump),
    Return(Return),
}

impl Display for Continuation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Continuation::*;
        match self {
            Jump(jmp) => write!(f, "{}", jmp),
            CondJump(cjmp) => write!(f, "{}", cjmp),
            Return(ret) => write!(f, "{}", ret),
        }
    }
}

#[derive(Debug)]
pub struct Jump {
    pub dest: String,
    pub args: Vec<Value>,
}

impl Display for Jump {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "jmp {}(", self.dest)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, ")")
    }
}

impl InstrAttributes for Jump {
    fn name(&self) -> &'static str {
        "jmp"
    }
    fn arguments(&self) -> Vec<&Value> {
        self.args.iter().collect()
    }
    fn results(&self) -> Vec<&Computed> {
        vec![]
    }
}

#[derive(Debug)]
pub struct CondJump {
    pub cond: Value,
    pub jump_if_false: Jump,
    pub jump_if_true: Jump,
}

impl Display for CondJump {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "cjmp {}, {}(", self.cond, self.jump_if_true.dest)?;
        for (i, arg) in self.jump_if_true.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, "), {}(", self.jump_if_false.dest)?;
        for (i, arg) in self.jump_if_false.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, ")")
    }
}

impl InstrAttributes for CondJump {
    fn name(&self) -> &'static str {
        "cjmp"
    }
    fn arguments(&self) -> Vec<&Value> {
        vec![&self.cond]
            .into_iter()
            .chain(self.jump_if_false.arguments())
            .chain(self.jump_if_true.arguments())
            .collect()
    }
    fn results(&self) -> Vec<&Computed> {
        vec![]
    }
}

#[derive(Debug)]
pub struct Return {
    pub results: Vec<Value>,
}

impl Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ret")?;
        for (i, result) in self.results.iter().enumerate() {
            if i == 0 {
                write!(f, " ")?;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", result)?;
        }
        Ok(())
    }
}

impl InstrAttributes for Return {
    fn name(&self) -> &'static str {
        "ret"
    }
    fn arguments(&self) -> Vec<&Value> {
        self.results.iter().collect()
    }
    fn results(&self) -> Vec<&Computed> {
        vec![]
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
