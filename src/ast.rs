#[derive(Debug)]
pub struct Func<'a> {
    pub name: &'a str,
    pub result: Expr,
}

#[derive(Debug)]
pub enum Expr {
    BoolLit(BoolLit),
    IntLit(IntLit),
    BinaryExpr(Box<BinaryExpr>),
    IfExpr(IfExpr),
}

#[derive(Debug)]
pub struct IfExpr {
    pub cond: Box<Expr>,
    pub expr_if_true: Box<Expr>,
    pub expr_if_false: Box<Expr>,
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
pub struct BinaryExpr {
    pub operand_a: Expr,
    pub operand_b: Expr,
    pub op: BinaryOp,
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
