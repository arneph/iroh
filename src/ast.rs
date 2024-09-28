use crate::types::Type;

#[derive(Debug)]
pub struct Program<'a> {
    pub funcs: Vec<Func<'a>>,
}

impl<'a> Program<'a> {
    pub fn func_with_name(&'a self, name: &str) -> Option<&'a Func> {
        self.funcs.iter().find(|f| f.name == name)
    }
}

#[derive(Debug)]
pub struct Func<'a> {
    pub name: &'a str,
    pub args: Vec<Arg<'a>>,
    pub result: Option<Type>,
    pub body: Expr<'a>,
}

#[derive(Debug)]
pub struct Arg<'a> {
    pub name: &'a str,
    pub typ: Type,
}

#[derive(Debug)]
pub enum Expr<'a> {
    BoolLit(BoolLit),
    IntLit(IntLit),
    Identifier(Identifier<'a>),
    BinaryExpr(BinaryExpr<'a>),
    IfExpr(IfExpr<'a>),
    Call(Call<'a>),
}

#[derive(Debug)]
pub struct Call<'a> {
    pub func_name: &'a str,
    pub args: Vec<Expr<'a>>,
}

#[derive(Debug)]
pub struct IfExpr<'a> {
    pub cond: Box<Expr<'a>>,
    pub expr_if_true: Box<Expr<'a>>,
    pub expr_if_false: Box<Expr<'a>>,
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOp {
    Or,
    And,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    GreaterThan,
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
pub struct BinaryExpr<'a> {
    pub operand_a: Box<Expr<'a>>,
    pub operand_b: Box<Expr<'a>>,
    pub op: BinaryOp,
}

#[derive(Debug)]
pub struct Identifier<'a> {
    pub name: &'a str,
}

#[derive(Debug)]
pub enum BoolLit {
    False,
    True,
}

#[derive(Debug)]
pub struct IntLit {
    pub value: i64,
}
