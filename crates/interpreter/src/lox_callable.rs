use std::{cell::RefCell, rc::Rc};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use fnv::FnvHashMap as HashMap;
use lasso::{Rodeo, Spur};

use crate::{
    ast::Function,
    environment::{Environment, Object},
    interpreter::{EvaluateResult, Interpreter},
    token::Token,
    DiagnosticEmitter,
};

pub trait Callable: std::fmt::Debug {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        emitter: &mut DiagnosticEmitter<'_>,
        interner: &Rodeo,
        args: Vec<Object>,
    ) -> EvaluateResult;

    fn arity(&self, interner: &Rodeo) -> usize;

    fn name<'a>(&self, interner: &'a Rodeo) -> &'a str;

    fn kind(&self) -> &'static str;
}

#[derive(Debug)]
pub(crate) struct NativeCallable {
    pub func: fn(Vec<Object>) -> EvaluateResult,
    pub name: &'static str,
    pub arity: usize,
}

impl Callable for NativeCallable {
    fn call(
        &self,
        _: &mut Interpreter,
        _: &mut DiagnosticEmitter<'_>,
        _: &Rodeo,
        args: Vec<Object>,
    ) -> EvaluateResult {
        (self.func)(args)
    }

    fn arity(&self, _: &Rodeo) -> usize {
        self.arity
    }

    fn name<'a>(&self, _: &'a Rodeo) -> &'a str {
        self.name
    }

    fn kind(&self) -> &'static str {
        "native fn"
    }
}

pub struct LoxCallable {
    pub declaration: Rc<Function>,
    pub closure: Rc<RefCell<Environment>>,
    pub is_initializer: bool,
}

impl LoxCallable {
    pub fn bind(&self, instance: Rc<RefCell<LoxClassInstance>>, interner: &Rodeo) -> LoxCallable {
        let mut closure = Environment::with_parent(self.closure.clone());
        let this_token = Token::make_ident(
            interner.get("this").unwrap(),
            self.declaration.name.source_id,
            self.declaration.name.source_range(),
        );
        closure.define(this_token, None, Object::LoxClassInstance(instance));

        Self {
            declaration: self.declaration.clone(),
            closure: Rc::new(RefCell::new(closure)),
            is_initializer: self.is_initializer,
        }
    }
}

impl std::fmt::Debug for LoxCallable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LoxCallable")
            .field("declaration", &self.declaration.name.lexeme)
            .finish()
    }
}

impl Callable for LoxCallable {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        emitter: &mut DiagnosticEmitter<'_>,
        interner: &Rodeo,
        args: Vec<Object>,
    ) -> EvaluateResult {
        let parent = self.closure.clone();
        let mut env = Environment::with_parent(parent);

        for (param, arg) in self.declaration.parameters.iter().zip(args) {
            env.define(*param, None, arg);
        }

        let old_env = std::mem::replace(&mut interpreter.environment, Rc::new(RefCell::new(env)));

        let ret_val =
            match interpreter.evaluate_statement_block(&self.declaration.body, emitter, interner) {
                Ok(Some(obj)) => obj,
                Ok(None) => Object::Nil,
                Err(diag) => {
                    interpreter.environment = old_env;
                    interpreter.had_runtime_error = true;
                    return Err(diag);
                }
            };

        interpreter.environment = old_env;
        if self.is_initializer {
            let this_token = Token::make_ident(
                interner.get("this").unwrap(),
                self.declaration.name.source_id,
                self.declaration.name.source_range(),
            );
            let closure = self.closure.borrow();
            let (ret_val, _) = closure.get_at(this_token, 0, interner)?;

            Ok(ret_val)
        } else {
            Ok(ret_val)
        }
    }

    fn arity(&self, _: &Rodeo) -> usize {
        self.declaration.parameters.len()
    }

    fn name<'a>(&self, interner: &'a Rodeo) -> &'a str {
        interner.resolve(&self.declaration.name.lexeme)
    }

    fn kind(&self) -> &'static str {
        "fn"
    }
}

#[derive(Debug)]
pub struct LoxClassDefinition {
    pub name: Token,
    pub methods: HashMap<Spur, Rc<LoxCallable>>,
    pub superclass: Option<Rc<LoxClassDefinition>>,
}

impl LoxClassDefinition {
    pub fn find_method(&self, name: Token) -> Option<&Rc<LoxCallable>> {
        if let Some(method) = self.methods.get(&name.lexeme) {
            Some(method)
        } else if let Some(superclass) = self.superclass.as_ref() {
            superclass.find_method(name)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct LoxClassConstructor {
    pub definition: Rc<LoxClassDefinition>,
}

impl Callable for LoxClassConstructor {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        emitter: &mut DiagnosticEmitter<'_>,
        interner: &Rodeo,
        args: Vec<Object>,
    ) -> EvaluateResult {
        let instance = LoxClassInstance {
            definition: self.definition.clone(),
            fields: HashMap::default(),
        };

        let instance = Rc::new(RefCell::new(instance));

        let init_token = Token::make_ident(
            interner.get("init").unwrap(),
            self.definition.name.source_id,
            self.definition.name.source_range(),
        );
        if let Some(init) = self.definition.find_method(init_token) {
            init.bind(instance.clone(), interner)
                .call(interpreter, emitter, interner, args)?;
        }

        Ok(Object::LoxClassInstance(instance))
    }

    fn arity(&self, interner: &Rodeo) -> usize {
        let init_token = Token::make_ident(
            interner.get("init").unwrap(),
            self.definition.name.source_id,
            self.definition.name.source_range(),
        );
        self.definition
            .find_method(init_token)
            .map(|init| init.arity(interner))
            .unwrap_or(0)
    }

    fn name<'a>(&self, interner: &'a Rodeo) -> &'a str {
        interner.resolve(&self.definition.name.lexeme)
    }

    fn kind(&self) -> &'static str {
        "class"
    }
}

#[derive(Debug)]
pub struct LoxClassInstance {
    pub definition: Rc<LoxClassDefinition>,
    pub fields: HashMap<Spur, Object>,
}

impl LoxClassInstance {
    pub fn get(this: &Rc<RefCell<Self>>, name: Token, interner: &Rodeo) -> EvaluateResult {
        let this_borrow = this.borrow();
        if let Some(obj) = this_borrow.fields.get(&name.lexeme) {
            return Ok(obj.clone());
        }

        if let Some(method) = this_borrow.definition.find_method(name) {
            let func = method.bind(this.clone(), interner);
            return Ok(Object::Callable(Rc::new(func)));
        }

        let property_name = interner.resolve(&name.lexeme);
        let diag = Diagnostic::error()
            .with_message(format!("undefined property '{}'", property_name))
            .with_labels(vec![Label::primary(name.source_id, name.source_range())]);

        Err(diag)
    }

    pub fn set(&mut self, name: Token, value: Object) {
        self.fields.insert(name.lexeme, value);
    }
}
