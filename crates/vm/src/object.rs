use std::{any::Any, io::Write};

use lasso::Rodeo;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObjKind {}

pub trait Obj: std::fmt::Debug + Any {
    fn kind(&self) -> ObjKind;
    fn dump(&self, w: &mut dyn Write, interner: &Rodeo);
    fn display(&self, w: &mut dyn Write, interner: &Rodeo);
    fn kind_str(&self) -> &'static str;
    fn eq(&self, rhs: &dyn Obj, interner: &Rodeo) -> bool;

    fn as_any(&self) -> &dyn Any;
}
