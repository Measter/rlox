use std::io::Write;

#[derive(Clone, PartialEq)]
pub enum Value {
    Boolean(bool),
    Number(f64),
    Nil,
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Number(v) => write!(f, "{}", v),
            Self::Nil => write!(f, "Nil"),
        }
    }
}

impl Value {
    pub fn display(&self, w: &mut impl Write) {
        match self {
            Value::Boolean(b) => write!(w, "{}", b).unwrap(),
            Value::Number(num) => write!(w, "{}", num).unwrap(),
            Value::Nil => write!(w, "nil").unwrap(),
        }
    }

    pub fn is_falsy(&self) -> bool {
        matches!(self, Value::Nil | Value::Boolean(false))
    }

    pub fn kind(&self) -> &'static str {
        match self {
            Value::Boolean(_) => "Boolean",
            Value::Number(_) => "Number",
            Value::Nil => "Nil",
        }
    }
}
