use std::fmt::Display;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Bool,
    Int,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Type::*;
        match self {
            Bool => write!(f, "bool"),
            Int => write!(f, "int"),
        }
    }
}
