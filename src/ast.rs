#[derive(Debug)]
pub struct Func<'a> {
    pub name: &'a str,
    pub result: Expr,
}

#[derive(Debug)]
pub enum Expr {
    IntLit(IntLit),
    BinaryExpr(Box<BinaryExpr>),
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multipy,
    Divide,
    Remainder,
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub operand_a: Expr,
    pub operand_b: Expr,
    pub op: BinaryOp,
}

#[derive(Debug)]
pub struct IntLit {
    pub value: i64,
}
