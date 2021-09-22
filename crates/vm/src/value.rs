use std::{io::Write, rc::Rc};

use lasso::Rodeo;

use crate::object::Obj;

pub enum Value {
    Boolean(bool),
    Number(f64),
    Nil,
    Obj(Rc<dyn Obj>),
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Self::Boolean(arg0) => Self::Boolean(*arg0),
            Self::Number(arg0) => Self::Number(*arg0),
            Self::Nil => Self::Nil,
            Self::Obj(arg0) => Self::Obj(arg0.clone()),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Number(v) => write!(f, "{}", v),
            Self::Nil => write!(f, "Nil"),
            Self::Obj(obj) => obj.fmt(f),
        }
    }
}

impl Value {
    pub fn display(&self, w: &mut impl Write, interner: &Rodeo, is_dump: bool) {
        match self {
            Value::Boolean(b) => write!(w, "{}", b).unwrap(),
            Value::Number(num) => write!(w, "{}", num).unwrap(),
            Value::Nil => write!(w, "nil").unwrap(),
            Value::Obj(obj) => obj.display(w, interner, is_dump),
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
            Value::Obj(obj) => obj.kind_str(),
        }
    }

    pub fn eq(&self, rhs: &Self, interner: &Rodeo) -> bool {
        match (self, rhs) {
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Nil, Value::Nil) => todo!(),
            (Value::Obj(a), Value::Obj(b)) => a.eq(&**b, interner),
            _ => false,
        }
    }
}
