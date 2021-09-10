use std::{
    cell::RefCell,
    io::Write,
    ops::Range,
    rc::Rc,
    time::{Duration, SystemTime},
};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use fnv::FnvHashMap as HashMap;
use lasso::{Rodeo, Spur};

use crate::{
    lox_callable::{
        Callable, LoxClassConstructor, LoxClassDefinition, LoxClassInstance, NativeCallable,
    },
    source_file::FileId,
    token::Token,
};

#[derive(Debug)]
pub enum Object {
    String(Rc<String>),
    Number(f64),
    Boolean(bool),
    Nil,
    Uninitialized,
    Callable(Rc<dyn Callable + 'static>),
    LoxClassSuper(Rc<LoxClassDefinition>),
    LoxClassConstructor(Rc<LoxClassConstructor>),
    LoxClassInstance(Rc<RefCell<LoxClassInstance>>),
}

impl Clone for Object {
    fn clone(&self) -> Self {
        match self {
            Self::Callable(arg0) => Self::Callable(arg0.clone()),
            Self::LoxClassConstructor(arg0) => Self::LoxClassConstructor(arg0.clone()),
            Self::LoxClassInstance(arg0) => Self::LoxClassInstance(arg0.clone()),
            Self::LoxClassSuper(arg0) => Self::LoxClassSuper(arg0.clone()),
            Self::Boolean(arg0) => Self::Boolean(*arg0),
            Self::Nil => Self::Nil,
            Self::Number(arg0) => Self::Number(*arg0),
            Self::String(arg0) => Self::String(arg0.clone()),
            Self::Uninitialized => Self::Uninitialized,
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (
                Self::Callable(_) | Self::LoxClassConstructor(_),
                Self::Callable(_) | Self::LoxClassConstructor(_),
            ) => false,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Object {
    pub fn kind(&self) -> &'static str {
        match self {
            Object::Callable(_) | Object::LoxClassConstructor(_) => "Function",
            Object::LoxClassInstance(_) => "Class Instance",
            Object::LoxClassSuper(_) => "Class Super",
            Object::Boolean(_) => "Boolean",
            Object::Nil => "Nil",
            Object::Number(_) => "Number",
            Object::String(_) => "String",
            Object::Uninitialized => "Uninitialized",
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Object::Nil | Object::Boolean(false))
    }

    pub fn display(&self, f: &mut impl Write, interner: &Rodeo) -> std::io::Result<()> {
        match self {
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Callable(callable) => {
                f.write_all(b"<")?;
                f.write_all(callable.kind().as_bytes())?;
                f.write_all(b" ")?;
                f.write_all(callable.name(interner).as_bytes())?;
                f.write_all(b">")
            }
            Object::LoxClassConstructor(callable) => {
                f.write_all(b"<")?;
                f.write_all(callable.kind().as_bytes())?;
                f.write_all(b" ")?;
                f.write_all(callable.name(interner).as_bytes())?;
                f.write_all(b">")
            }
            Object::LoxClassInstance(class) => {
                let class = class.borrow();
                f.write_all(b"<class ")?;
                f.write_all(interner.resolve(&class.definition.name.lexeme).as_bytes())?;
                f.write_all(b" instance>")
            }
            Object::LoxClassSuper(def) => {
                f.write_all(b"<class ")?;
                f.write_all(interner.resolve(&def.name.lexeme).as_bytes())?;
                f.write_all(b" definition>")
            }
            Object::Nil => f.write_all(b"nil"),
            Object::Number(n) => write!(f, "{}", n),
            Object::String(s) => f.write_all(s.as_bytes()),
            Object::Uninitialized => f.write_all(b"uninitialized"),
        }
    }
}

#[derive(Debug)]
struct StoredVariable {
    value: Object,
    source_range: Range<usize>,
    source_id: FileId,
}

impl Default for StoredVariable {
    fn default() -> Self {
        Self {
            value: Object::Uninitialized,
            source_range: Default::default(),
            source_id: FileId::blank(),
        }
    }
}

#[derive(Debug, Default)]
pub struct Environment {
    stored_variables: HashMap<Spur, StoredVariable>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            stored_variables: HashMap::default(),
            parent: None,
        }
    }

    pub fn new_global(interner: &mut Rodeo) -> Self {
        let mut globals = Self::new();
        globals.stored_variables.insert(
            interner.get_or_intern_static("clock"),
            StoredVariable {
                value: Object::Callable(Rc::new(NativeCallable {
                    func: |_| {
                        Ok(Object::Number(
                            SystemTime::now()
                                .duration_since(SystemTime::UNIX_EPOCH)
                                .unwrap_or(Duration::ZERO)
                                .as_secs_f64(),
                        ))
                    },
                    name: "clock",
                    arity: 0,
                })),
                source_range: 0..0,
                source_id: FileId::blank(),
            },
        );
        globals
    }

    pub fn with_parent(parent: Rc<RefCell<Environment>>) -> Self {
        Self {
            stored_variables: HashMap::default(),
            parent: Some(parent),
        }
    }

    pub fn parent(&self) -> Option<&Rc<RefCell<Environment>>> {
        self.parent.as_ref()
    }

    pub fn define(&mut self, name: Token, expression_range: Option<Range<usize>>, value: Object) {
        let full_range = expression_range
            .map(|exp_range| {
                let name_range = name.source_range();
                name_range.start..exp_range.end
            })
            .unwrap_or_else(|| name.source_range());

        let variable = self.stored_variables.entry(name.lexeme).or_default();
        variable.value = value;
        variable.source_range = full_range;
        variable.source_id = name.source_id;
    }

    pub fn assign(
        &mut self,
        name: Token,
        expression_range: Range<usize>,
        value: Object,
        interner: &Rodeo,
    ) -> Result<(), Diagnostic<FileId>> {
        match self.stored_variables.get_mut(&name.lexeme) {
            Some(v) => {
                v.value = value;
                v.source_range = name.source_start..expression_range.end;
                v.source_id = name.source_id;
                Ok(())
            }
            None => {
                let name_lexeme = interner.resolve(&name.lexeme);
                let diag = Diagnostic::error()
                    .with_message(format!("undefined variable `{}`", name_lexeme))
                    .with_labels(vec![Label::primary(name.source_id, name.source_range())]);

                Err(diag)
            }
        }
    }

    pub fn assign_at(
        &mut self,
        name: Token,
        depth: usize,
        expression_range: Range<usize>,
        value: Object,
        interner: &Rodeo,
    ) -> Result<(), Diagnostic<FileId>> {
        if let Some(ancestor) = self.ancestor(depth) {
            let mut ancestor = ancestor.borrow_mut();
            ancestor.assign(name, expression_range, value, interner)
        } else {
            self.assign(name, expression_range, value, interner)
        }
    }

    pub fn get(
        &self,
        name: Token,
        interner: &Rodeo,
    ) -> Result<(Object, Option<Diagnostic<FileId>>), Diagnostic<FileId>> {
        match self.stored_variables.get(&name.lexeme) {
            Some(v) if v.value == Object::Uninitialized => {
                let name_lexeme = interner.resolve(&name.lexeme);
                let diag = Diagnostic::warning()
                    .with_message(format!("variable `{}` was uninitialized", name_lexeme))
                    .with_labels(vec![
                        Label::primary(name.source_id, name.source_range()),
                        Label::secondary(v.source_id, v.source_range.clone())
                            .with_message("variable defined here"),
                    ]);

                Ok((Object::Nil, Some(diag)))
            }
            Some(v) => Ok((v.value.clone(), None)),
            None => {
                let name_lexeme = interner.resolve(&name.lexeme);
                let diag = Diagnostic::error()
                    .with_message(format!("undefined variable `{}`", name_lexeme))
                    .with_labels(vec![Label::primary(name.source_id, name.source_range())]);

                Err(diag)
            }
        }
    }

    pub fn get_at(
        &self,
        name: Token,
        depth: usize,
        interner: &Rodeo,
    ) -> Result<(Object, Option<Diagnostic<FileId>>), Diagnostic<FileId>> {
        if let Some(ancestor) = self.ancestor(depth) {
            let ancester = ancestor.borrow();
            ancester.get(name, interner)
        } else {
            self.get(name, interner)
        }
    }

    fn ancestor(&self, depth: usize) -> Option<Rc<RefCell<Environment>>> {
        if depth == 0 {
            return None;
        }

        let mut env = self
            .parent
            .as_ref()
            .map(Clone::clone)
            .expect("expected at least one ancestor");

        for _ in 1..depth {
            env = {
                let parent = env.borrow();
                parent
                    .parent
                    .as_ref()
                    .map(Clone::clone)
                    .expect("expected at least one ancestor")
            };
        }

        Some(env)
    }

    pub fn source(&self, name: Token) -> Option<(FileId, Range<usize>)> {
        self.stored_variables
            .get(&name.lexeme)
            .map(|v| (v.source_id, v.source_range.clone()))
    }
}
