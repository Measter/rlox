use std::{io::Write, rc::Rc};

use lasso::{Rodeo, Spur};

use crate::object::Obj;

#[derive(Debug, Clone)]
pub enum StringObject {
    Literal(Spur),
    Runtime(Rc<str>),
}

impl StringObject {
    fn dump(&self, w: &mut dyn Write, interner: &Rodeo) {
        let val = match self {
            StringObject::Literal(key) => interner.resolve(key),
            StringObject::Runtime(val) => val,
        };
        write!(w, "\"{}\"", val).unwrap();
    }

    fn display(&self, w: &mut dyn Write, interner: &Rodeo) {
        let val = match self {
            StringObject::Literal(key) => interner.resolve(key),
            StringObject::Runtime(val) => val,
        };
        w.write_all(val.as_bytes()).unwrap();
        w.write_all(b"\n").unwrap();
    }

    fn eq(&self, rhs: &StringObject, interner: &Rodeo) -> bool {
        match (self, rhs) {
            (StringObject::Literal(a), StringObject::Literal(b)) => a == b,
            (StringObject::Runtime(a), StringObject::Runtime(b)) => a == b,
            (StringObject::Literal(a), StringObject::Runtime(b)) => {
                let a = interner.resolve(a);
                a == &**b
            }
            (StringObject::Runtime(a), StringObject::Literal(b)) => {
                let b = interner.resolve(b);
                &**a == b
            }
        }
    }

    pub fn concatinate_strings(&self, rhs: &StringObject, interner: &Rodeo) -> StringObject {
        let lhs_str = match self {
            StringObject::Literal(key) => interner.resolve(key),
            StringObject::Runtime(val) => (&**val),
        };

        let rhs_str = match rhs {
            StringObject::Literal(key) => interner.resolve(key),
            StringObject::Runtime(val) => val,
        };

        if lhs_str.is_empty() {
            return rhs.clone();
        }

        if rhs_str.is_empty() {
            return self.clone();
        }

        let mut lhs = lhs_str.to_owned();
        lhs.push_str(rhs_str);

        StringObject::Runtime(lhs.into_boxed_str().into())
    }
}

pub enum Value {
    Boolean(bool),
    Number(f64),
    Nil,
    Obj(Rc<dyn Obj>),
    String(StringObject),
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Self::Boolean(arg0) => Self::Boolean(*arg0),
            Self::Number(arg0) => Self::Number(*arg0),
            Self::Nil => Self::Nil,
            Self::Obj(arg0) => Self::Obj(arg0.clone()),
            Self::String(arg0) => Self::String(arg0.clone()),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Number(v) => write!(f, "{}", v),
            Self::Nil => write!(f, "Nil"),
            Self::String(s) => s.fmt(f),
            Self::Obj(obj) => obj.fmt(f),
        }
    }
}

impl Value {
    pub fn display(&self, w: &mut impl Write, interner: &Rodeo) {
        match self {
            Value::Boolean(b) => writeln!(w, "{}", b).unwrap(),
            Value::Number(num) => writeln!(w, "{}", num).unwrap(),
            Value::Nil => writeln!(w, "nil").unwrap(),
            Value::Obj(obj) => obj.display(w, interner),
            Value::String(s) => s.display(w, interner),
        }
    }

    pub fn dump(&self, w: &mut impl Write, interner: &Rodeo) {
        match self {
            Value::Boolean(b) => write!(w, "{}", b).unwrap(),
            Value::Number(num) => write!(w, "{}", num).unwrap(),
            Value::Nil => write!(w, "nil").unwrap(),
            Value::Obj(obj) => obj.dump(w, interner),
            Value::String(s) => s.dump(w, interner),
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
            Value::String(_) => "String",
        }
    }

    pub fn eq(&self, rhs: &Self, interner: &Rodeo) -> bool {
        match (self, rhs) {
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Nil, Value::Nil) => todo!(),
            (Value::Obj(a), Value::Obj(b)) => a.eq(&**b, interner),
            (Value::String(a), Value::String(b)) => a.eq(b, interner),
            _ => false,
        }
    }
}
