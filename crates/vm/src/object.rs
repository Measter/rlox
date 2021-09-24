use std::{any::Any, io::Write, rc::Rc};

use lasso::{Rodeo, Spur};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObjKind {
    String,
}

pub trait Obj: std::fmt::Debug + Any {
    fn kind(&self) -> ObjKind;
    fn display(&self, w: &mut dyn Write, interner: &Rodeo, is_dump: bool);
    fn kind_str(&self) -> &'static str;
    fn eq(&self, rhs: &dyn Obj, interner: &Rodeo) -> bool;
    fn concatinate_strings(&self, rhs: &dyn Obj, _: &Rodeo) -> ObjString {
        panic!(
            "ICE: Attempted to concatinate a {:?} and {:?}",
            self.kind(),
            rhs.kind()
        );
    }

    fn as_any(&self) -> &dyn Any;
}

#[derive(Debug, Clone)]
pub enum ObjString {
    Literal(Spur),
    Runtime(Rc<str>),
}

impl Obj for ObjString {
    fn kind(&self) -> ObjKind {
        ObjKind::String
    }

    fn display(&self, w: &mut dyn Write, interner: &Rodeo, is_dump: bool) {
        let val = match self {
            ObjString::Literal(key) => interner.resolve(key),
            ObjString::Runtime(val) => val,
        };
        if is_dump {
            write!(w, "\"{}\"", val).unwrap();
        } else {
            w.write_all(val.as_bytes()).unwrap();
        }
    }

    fn kind_str(&self) -> &'static str {
        "String"
    }

    fn eq(&self, rhs: &dyn Obj, interner: &Rodeo) -> bool {
        if let Some(rhs) = rhs.as_any().downcast_ref::<ObjString>() {
            match (self, rhs) {
                (ObjString::Literal(a), ObjString::Literal(b)) => a == b,
                (ObjString::Runtime(a), ObjString::Runtime(b)) => a == b,
                (ObjString::Literal(a), ObjString::Runtime(b)) => {
                    let a = interner.resolve(a);
                    a == &**b
                }
                (ObjString::Runtime(a), ObjString::Literal(b)) => {
                    let b = interner.resolve(b);
                    &**a == b
                }
            }
        } else {
            false
        }
    }

    fn concatinate_strings(&self, rhs: &dyn Obj, interner: &Rodeo) -> ObjString {
        let rhs = rhs
            .as_any()
            .downcast_ref::<ObjString>()
            .expect("ICE: Attempted to concatinate_string a non-string");

        let lhs_str = match self {
            ObjString::Literal(key) => interner.resolve(key),
            ObjString::Runtime(val) => (&**val),
        };

        let rhs_str = match rhs {
            ObjString::Literal(key) => interner.resolve(key),
            ObjString::Runtime(val) => val,
        };

        if lhs_str.is_empty() {
            return rhs.clone();
        }

        if rhs_str.is_empty() {
            return self.clone();
        }

        let mut lhs = lhs_str.to_owned();
        lhs.push_str(rhs_str);

        ObjString::Runtime(lhs.into_boxed_str().into())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
