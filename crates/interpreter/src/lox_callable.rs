use std::{cell::RefCell, rc::Rc};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use fnv::FnvHashMap as HashMap;
use lasso::{Rodeo, Spur};
use rlox::{
    program::{FunctionId, Program},
    token::Token,
};

use crate::{
    environment::{Environment, Object},
    interpreter::{EvaluateResult, Interpreter},
    DiagnosticEmitter,
};

pub trait Callable: std::fmt::Debug {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        emitter: &mut DiagnosticEmitter<'_>,
        interner: &Rodeo,
        program: &Program,
        args: Vec<Object>,
    ) -> EvaluateResult;

    fn arity(&self, interner: &Rodeo, program: &Program) -> usize;

    fn name<'a>(&self, interner: &'a Rodeo) -> &'a str;

    fn kind(&self) -> &'static str;

    // Functions need to be compared by identity, but comparing a trait object
    // results in comparing the vtable pointers.
    // We need to compare data pointers instead, which means erasing the type.
    fn as_void_ptr(&self) -> *const u8;
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
        _: &Program,
        args: Vec<Object>,
    ) -> EvaluateResult {
        (self.func)(args)
    }

    fn arity(&self, _: &Rodeo, _: &Program) -> usize {
        self.arity
    }

    fn name<'a>(&self, _: &'a Rodeo) -> &'a str {
        self.name
    }

    fn kind(&self) -> &'static str {
        "native fn"
    }

    fn as_void_ptr(&self) -> *const u8 {
        self as *const Self as *const u8
    }
}

pub struct LoxCallable {
    pub declaration: FunctionId,
    pub closure: Rc<RefCell<Environment>>,
    pub is_initializer: bool,
    pub arity: usize,
    pub name: Spur,
}

impl LoxCallable {
    pub fn bind(&self, instance: Rc<RefCell<LoxClassInstance>>, program: &Program) -> LoxCallable {
        let mut closure = Environment::with_parent(self.closure.clone());
        let func_def = &program[self.declaration];
        let this_token = Token::make_ident(
            program.this_lexeme(),
            func_def.name.location.file_id,
            func_def.name.source_range(),
        );
        closure.define(this_token, None, Object::LoxClassInstance(instance));

        Self {
            declaration: self.declaration,
            closure: Rc::new(RefCell::new(closure)),
            is_initializer: self.is_initializer,
            name: func_def.name.lexeme,
            arity: func_def.parameters.len(),
        }
    }
}

impl std::fmt::Debug for LoxCallable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LoxCallable")
            .field("declaration", &self.name)
            .finish()
    }
}

impl Callable for LoxCallable {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        emitter: &mut DiagnosticEmitter<'_>,
        interner: &Rodeo,
        program: &Program,
        args: Vec<Object>,
    ) -> EvaluateResult {
        let parent = self.closure.clone();
        let mut env = Environment::with_parent(parent);

        let func_def = &program[self.declaration];
        for (param, arg) in func_def.parameters.iter().zip(args) {
            env.define(*param, None, arg);
        }

        let old_env = std::mem::replace(&mut interpreter.environment, Rc::new(RefCell::new(env)));

        let ret_val = match interpreter.evaluate_statement_block(
            &func_def.body,
            emitter,
            interner,
            program,
        ) {
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
                program.this_lexeme(),
                func_def.name.location.file_id,
                func_def.name.source_range(),
            );
            let closure = self.closure.borrow();
            let (ret_val, _) = closure.get_at(this_token, 0, interner)?;

            Ok(ret_val)
        } else {
            Ok(ret_val)
        }
    }

    fn arity(&self, _: &Rodeo, _: &Program) -> usize {
        self.arity
    }

    fn name<'a>(&self, interner: &'a Rodeo) -> &'a str {
        interner.resolve(&self.name)
    }

    fn kind(&self) -> &'static str {
        "fn"
    }

    fn as_void_ptr(&self) -> *const u8 {
        self as *const Self as *const u8
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
        program: &Program,
        args: Vec<Object>,
    ) -> EvaluateResult {
        let instance = LoxClassInstance {
            definition: self.definition.clone(),
            fields: HashMap::default(),
        };

        let instance = Rc::new(RefCell::new(instance));

        let init_token = Token::make_ident(
            program.init_lexeme(),
            self.definition.name.location.file_id,
            self.definition.name.source_range(),
        );
        if let Some(init) = self.definition.find_method(init_token) {
            init.bind(instance.clone(), program).call(
                interpreter,
                emitter,
                interner,
                program,
                args,
            )?;
        }

        Ok(Object::LoxClassInstance(instance))
    }

    fn arity(&self, interner: &Rodeo, program: &Program) -> usize {
        let init_token = Token::make_ident(
            program.init_lexeme(),
            self.definition.name.location.file_id,
            self.definition.name.source_range(),
        );
        self.definition
            .find_method(init_token)
            .map(|init| init.arity(interner, program))
            .unwrap_or(0)
    }

    fn name<'a>(&self, interner: &'a Rodeo) -> &'a str {
        interner.resolve(&self.definition.name.lexeme)
    }

    fn kind(&self) -> &'static str {
        "class"
    }

    fn as_void_ptr(&self) -> *const u8 {
        self as *const Self as *const u8
    }
}

#[derive(Debug)]
pub struct LoxClassInstance {
    pub definition: Rc<LoxClassDefinition>,
    pub fields: HashMap<Spur, Object>,
}

impl LoxClassInstance {
    pub fn get(
        this: &Rc<RefCell<Self>>,
        name: Token,
        interner: &Rodeo,
        program: &Program,
    ) -> EvaluateResult {
        let this_borrow = this.borrow();
        if let Some(obj) = this_borrow.fields.get(&name.lexeme) {
            return Ok(obj.clone());
        }

        if let Some(method) = this_borrow.definition.find_method(name) {
            let func = method.bind(this.clone(), program);
            return Ok(Object::Callable(Rc::new(func)));
        }

        let property_name = interner.resolve(&name.lexeme);
        let diag = Diagnostic::error()
            .with_message(format!("undefined property '{}'", property_name))
            .with_labels(vec![Label::primary(
                name.location.file_id,
                name.source_range(),
            )]);

        Err(diag)
    }

    pub fn set(&mut self, name: Token, value: Object) {
        self.fields.insert(name.lexeme, value);
    }
}
