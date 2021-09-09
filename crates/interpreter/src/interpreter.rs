use std::{cell::RefCell, fmt::Write as _, io::Stdout, ops::Range, rc::Rc};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use fnv::FnvHashMap as HashMap;
use lasso::Rodeo;

use crate::{
    ast::{Expression, ExpressionId, Function, Statement},
    environment::{Environment, Object},
    lox_callable::{LoxCallable, LoxClassConstructor, LoxClassDefinition, LoxClassInstance},
    source_file::FileId,
    token::{Token, TokenKind},
    DiagnosticEmitter,
};

pub type EvaluateResult = Result<Object, Diagnostic<FileId>>;

fn get_literal(kind: TokenKind, interner: &Rodeo) -> Object {
    match kind {
        TokenKind::String(s) => {
            let s = interner.resolve(&s);
            Object::String(Rc::new(s.to_owned()))
        }
        TokenKind::Number(n) => Object::Number(n),
        TokenKind::Boolean(b) => Object::Boolean(b),
        TokenKind::Nil => Object::Nil,
        _ => panic!("ICE: Expected literal, found {:?}", kind),
    }
}

fn make_secondary_variable_label(
    expr: &Expression,
    obj: &Object,
    env: &RefCell<Environment>,
) -> Label<FileId> {
    if let Expression::Variable { name, .. } = &expr {
        let env = env.borrow();
        let (source_id, source_range) = env
            .source(*name)
            .expect("missing variable after defined check");

        Label::secondary(source_id, source_range)
            .with_message(format!("{} assigned here", obj.kind()))
    } else {
        Label::secondary(expr.source_id(), expr.source_range()).with_message(obj.kind())
    }
}

pub struct Interpreter {
    pub had_runtime_error: bool,
    pub globals: Rc<RefCell<Environment>>,
    pub locals: HashMap<ExpressionId, usize>,
    pub environment: Rc<RefCell<Environment>>,
    pub did_print: bool,
    pub stdout: Stdout,
}

impl Interpreter {
    pub fn new(interner: &mut Rodeo) -> Self {
        let globals = Rc::new(RefCell::new(Environment::new_global(interner)));

        Self {
            had_runtime_error: false,
            environment: globals.clone(),
            globals,
            locals: HashMap::default(),
            did_print: false,
            stdout: std::io::stdout(),
        }
    }

    pub fn did_print(&self) -> bool {
        self.did_print
    }

    fn nest_scope(&mut self) {
        let parent = self.environment.clone();
        self.environment = Rc::new(RefCell::new(Environment::with_parent(parent)));
    }

    fn pop_scope(&mut self) {
        self.environment = {
            let env = self.environment.borrow();
            env.parent()
                .map(Clone::clone)
                .expect("ICE: attempted to pop global scope")
        };
    }

    pub fn resolve(&mut self, expr_id: ExpressionId, depth: usize) {
        self.locals.insert(expr_id, depth);
    }

    pub fn interpret(
        &mut self,
        program: &[Statement],
        emitter: &mut DiagnosticEmitter<'_>,
        interner: &Rodeo,
    ) -> Result<(), Diagnostic<FileId>> {
        self.did_print = false;

        for stmnt in program {
            self.evaluate_statement(stmnt, emitter, interner)?;
        }

        Ok(())
    }

    fn evaluate_statement(
        &mut self,
        stmnt: &Statement,
        emitter: &mut DiagnosticEmitter<'_>,
        interner: &Rodeo,
    ) -> Result<Option<Object>, Diagnostic<FileId>> {
        match stmnt {
            Statement::Block { statements } => {
                if let Some(obj) = self.evaluate_statement_block(statements, emitter, interner)? {
                    return Ok(Some(obj));
                };
            }
            Statement::Class {
                name,
                methods,
                source_range,
            } => {
                self.evaluate_statement_class(*name, methods, source_range.clone(), interner)?;
            }
            Statement::Expression(expr) => {
                self.evaluate_expression(expr, emitter, interner)?;
            }
            Statement::Function(func) => {
                self.evaluate_statement_function(func.clone(), interner)?
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let res = self.evaluate_expression(condition, emitter, interner)?;
                let obj = if res.is_truthy() {
                    self.evaluate_statement(then_branch, emitter, interner)?
                } else if let Some(else_branch) = else_branch.as_deref() {
                    self.evaluate_statement(else_branch, emitter, interner)?
                } else {
                    None
                };

                if obj.is_some() {
                    return Ok(obj);
                }
            }
            Statement::Print(expr) => self.evaluate_statement_print(expr, emitter, interner)?,
            Statement::Return { value, .. } => {
                let obj = value
                    .as_ref()
                    .map(|e| self.evaluate_expression(e, emitter, interner))
                    .transpose()?
                    .unwrap_or(Object::Nil);

                return Ok(Some(obj));
            }
            Statement::Variable { name, initializer } => {
                self.evaluate_statement_variable(*name, initializer.as_ref(), emitter, interner)?
            }
            Statement::While { condition, body } => {
                while self
                    .evaluate_expression(condition, emitter, interner)?
                    .is_truthy()
                {
                    if let Some(obj) = self.evaluate_statement(body, emitter, interner)? {
                        return Ok(Some(obj));
                    }
                }
            }
        }

        Ok(None)
    }

    pub fn evaluate_statement_block(
        &mut self,
        statements: &[Statement],
        emitter: &mut DiagnosticEmitter<'_>,
        interner: &Rodeo,
    ) -> Result<Option<Object>, Diagnostic<FileId>> {
        self.nest_scope();
        for statement in statements {
            match self.evaluate_statement(statement, emitter, interner) {
                Ok(Some(obj)) => {
                    self.pop_scope();
                    return Ok(Some(obj));
                }
                Ok(None) => {}
                Err(diag) => {
                    self.pop_scope();
                    self.had_runtime_error = true;
                    return Err(diag);
                }
            }
        }
        self.pop_scope();

        Ok(None)
    }

    pub fn evaluate_statement_class(
        &mut self,
        name: Token,
        methods: &[Rc<Function>],
        source_range: Range<usize>,
        interner: &Rodeo,
    ) -> Result<(), Diagnostic<FileId>> {
        let mut env = self.environment.borrow_mut();
        env.define(name, None, Object::Uninitialized);
        let mut def_methods = HashMap::default();

        for method in methods {
            let function = LoxCallable {
                declaration: method.clone(),
                closure: self.environment.clone(),
                is_initializer: method.name.lexeme == interner.get("init").unwrap(),
            };

            def_methods.insert(method.name.lexeme, Rc::new(function));
        }

        let class = LoxClassDefinition {
            name,
            methods: def_methods,
        };
        let constructor = LoxClassConstructor {
            definition: Rc::new(class),
        };

        env.assign(
            name,
            source_range,
            Object::Callable(Rc::new(constructor)),
            interner,
        )?;
        Ok(())
    }

    fn evaluate_statement_function(
        &mut self,
        func: Rc<Function>,
        interner: &Rodeo,
    ) -> Result<(), Diagnostic<FileId>> {
        let name = func.name;
        let function = Rc::new(LoxCallable {
            declaration: func,
            closure: self.environment.clone(),
            is_initializer: name.lexeme == interner.get("init").unwrap(),
        });
        let obj = Object::Callable(function);
        let mut env = self.environment.borrow_mut();
        env.define(name, None, obj);

        Ok(())
    }

    fn evaluate_statement_print(
        &mut self,
        expr: &Expression,
        emitter: &mut DiagnosticEmitter<'_>,
        interner: &Rodeo,
    ) -> Result<(), Diagnostic<FileId>> {
        let res = self.evaluate_expression(expr, emitter, interner)?;
        res.display(&mut self.stdout, interner)
            .expect("ICE: failed to print to stdout");
        println!();
        self.did_print = true;

        Ok(())
    }

    fn evaluate_statement_variable(
        &mut self,
        name: Token,
        initializer: Option<&Expression>,
        emitter: &mut DiagnosticEmitter<'_>,
        interner: &Rodeo,
    ) -> Result<(), Diagnostic<FileId>> {
        let value = initializer
            .as_ref()
            .map(|e| self.evaluate_expression(e, emitter, interner))
            .unwrap_or(Ok(Object::Uninitialized))?;

        let expression_source_range = initializer.map(|e| e.source_range());

        let mut env = self.environment.borrow_mut();
        env.define(name, expression_source_range, value);

        Ok(())
    }

    fn evaluate_expression(
        &mut self,
        expr: &Expression,
        emitter: &mut DiagnosticEmitter<'_>,
        interner: &Rodeo,
    ) -> EvaluateResult {
        match expr {
            Expression::Assign { name, value, id } => {
                self.evaluate_expr_assign(*name, *id, value, emitter, interner)
            }
            Expression::Binary {
                left,
                operator,
                right,
                ..
            } => self.evaluate_expr_binary(left, *operator, right, emitter, interner),
            Expression::Call {
                callee,
                arguments,
                source_range,
                ..
            } => {
                self.evaluate_expr_call(callee, arguments, source_range.clone(), emitter, interner)
            }
            Expression::Grouping { expression, .. } => {
                self.evaluate_expression(expression, emitter, interner)
            }
            Expression::Get { object, name, .. } => {
                let obj = self.evaluate_expression(object, emitter, interner)?;

                if let Object::LoxClass(instance) = obj {
                    LoxClassInstance::get(&instance, *name, interner)
                } else {
                    let diag = Diagnostic::error()
                        .with_message("only instances have properties")
                        .with_labels(vec![Label::primary(name.source_id, name.source_range())]);

                    Err(diag)
                }
            }
            Expression::Literal { value, .. } => Ok(get_literal(value.kind, interner)),
            Expression::Logical {
                left,
                operator,
                right,
                ..
            } => self.evaluate_expr_logical(left, *operator, right, emitter, interner),
            Expression::Set {
                object,
                name,
                value,
                ..
            } => {
                let obj = self.evaluate_expression(object, emitter, interner)?;
                if let Object::LoxClass(instance) = obj {
                    let value = self.evaluate_expression(value, emitter, interner)?;

                    let mut instance = instance.borrow_mut();
                    instance.set(*name, value.clone());
                    Ok(value)
                } else {
                    let diag = Diagnostic::error()
                        .with_message("only instances have properites")
                        .with_labels(vec![Label::primary(name.source_id, name.source_range())]);

                    Err(diag)
                }
            }

            Expression::Unary {
                operator, right, ..
            } => self.evaluate_expr_unary(*operator, right, emitter, interner),
            Expression::This { keyword: name, id } | Expression::Variable { name, id } => {
                self.evaluate_expr_variable(*name, *id, emitter, interner)
            }
        }
    }

    fn evaluate_expr_assign(
        &mut self,
        name: Token,
        expr_id: ExpressionId,
        value_expr: &Expression,
        emitter: &mut DiagnosticEmitter<'_>,
        interner: &Rodeo,
    ) -> EvaluateResult {
        let value = self.evaluate_expression(value_expr, emitter, interner)?;

        let value_range = value_expr.source_range();
        let res = if let Some(depth) = self.locals.get(&expr_id).copied() {
            let mut env = self.environment.borrow_mut();
            env.assign_at(name, depth, value_range, value.clone(), interner)
        } else {
            let mut env = self.globals.borrow_mut();
            env.assign(name, value_range, value.clone(), interner)
        };

        self.had_runtime_error |= res.is_err();
        res?;
        Ok(value)
    }

    fn evaluate_expr_call(
        &mut self,
        callee: &Expression,
        arguments: &[Expression],
        source_range: Range<usize>,
        emitter: &mut DiagnosticEmitter<'_>,
        interner: &Rodeo,
    ) -> EvaluateResult {
        let callee_obj = self.evaluate_expression(callee, emitter, interner)?;
        let mut evaluated_args = Vec::with_capacity(arguments.len());
        for arg in arguments {
            evaluated_args.push(self.evaluate_expression(arg, emitter, interner)?);
        }

        let callee_function = if let Object::Callable(callee) = callee_obj {
            callee
        } else {
            let diag = Diagnostic::error()
                .with_message("can only call functions and classes")
                .with_labels(vec![
                    Label::primary(callee.source_id(), source_range),
                    Label::secondary(callee.source_id(), callee.source_range())
                        .with_message(callee_obj.kind()),
                ]);
            return Err(diag);
        };

        if evaluated_args.len() != callee_function.arity(interner) {
            let diag = Diagnostic::error()
                .with_message(format!(
                    "expected {} arguments but got {}",
                    callee_function.arity(interner),
                    evaluated_args.len()
                ))
                .with_labels(vec![Label::primary(callee.source_id(), source_range)]);
            return Err(diag);
        }

        callee_function.call(self, emitter, interner, evaluated_args)
    }

    fn evaluate_expr_logical(
        &mut self,
        left_expr: &Expression,
        operator: Token,
        right_expr: &Expression,
        emitter: &mut DiagnosticEmitter<'_>,
        interner: &Rodeo,
    ) -> EvaluateResult {
        let maybe_invert: fn(bool) -> bool = match operator.kind {
            TokenKind::And => |b| !b,
            TokenKind::Or => |b| b,
            _ => {
                let operator_lexeme = interner.resolve(&operator.lexeme);
                let diag = Diagnostic::bug()
                    .with_message(format!(
                        "ICE: tried to `evaluate_logical` with operator `{}`",
                        operator_lexeme
                    ))
                    .with_labels(vec![Label::primary(
                        operator.source_id,
                        operator.source_range(),
                    )]);

                return Err(diag);
            }
        };

        let left = self.evaluate_expression(left_expr, emitter, interner)?;
        if maybe_invert(left.is_truthy()) {
            Ok(left)
        } else {
            self.evaluate_expression(right_expr, emitter, interner)
        }
    }

    fn evaluate_expr_unary(
        &mut self,
        operator: Token,
        right_expr: &Expression,
        emitter: &mut DiagnosticEmitter<'_>,
        interner: &Rodeo,
    ) -> EvaluateResult {
        let right = self.evaluate_expression(right_expr, emitter, interner)?;
        match (&operator.kind, right) {
            (TokenKind::Minus, Object::Number(n)) => Ok(Object::Number(-n)),
            (TokenKind::Bang, obj) => Ok(Object::Boolean(!obj.is_truthy())),

            (TokenKind::Minus, right) => {
                let range_start = operator.source_start;
                let range_end = right_expr.source_range().end;
                let diag = Diagnostic::error()
                    .with_message(format!("unable to negate a {}", right.kind()))
                    .with_labels(vec![Label::primary(
                        operator.source_id,
                        range_start..range_end,
                    )]);

                Err(diag)
            }
            _ => {
                let operator_lexeme = interner.resolve(&operator.lexeme);
                let diag = Diagnostic::bug()
                    .with_message(format!(
                        "ICE: tried to evaluate unary with operator `{}`",
                        operator_lexeme
                    ))
                    .with_labels(vec![Label::primary(
                        operator.source_id,
                        operator.source_range(),
                    )]);

                Err(diag)
            }
        }
    }

    fn evaluate_expr_binary(
        &mut self,
        left_expr: &Expression,
        operator: Token,
        right_expr: &Expression,
        emitter: &mut DiagnosticEmitter<'_>,
        interner: &Rodeo,
    ) -> EvaluateResult {
        let left = self.evaluate_expression(left_expr, emitter, interner)?;
        let right = self.evaluate_expression(right_expr, emitter, interner)?;

        let res = match (operator.kind, left, right) {
            // Arithmetic operators
            (TokenKind::Minus, Object::Number(left), Object::Number(right)) => {
                Object::Number(left - right)
            }
            (TokenKind::Slash, Object::Number(left), Object::Number(right)) => {
                Object::Number(left / right)
            }
            (TokenKind::Percent, Object::Number(left), Object::Number(right)) => {
                Object::Number(left % right)
            }
            (TokenKind::Star, Object::Number(left), Object::Number(right)) => {
                Object::Number(left * right)
            }
            (TokenKind::Plus, Object::Number(left), Object::Number(right)) => {
                Object::Number(left + right)
            }

            // String concatination
            (TokenKind::Plus, Object::String(left), Object::String(right)) => {
                let mut left = left.as_str().to_owned();
                left += right.as_str();
                Object::String(Rc::new(left))
            }

            // Comparison operators
            (TokenKind::Greater, Object::Number(left), Object::Number(right)) => {
                Object::Boolean(left > right)
            }
            (TokenKind::GreaterEqual, Object::Number(left), Object::Number(right)) => {
                Object::Boolean(left >= right)
            }
            (TokenKind::Less, Object::Number(left), Object::Number(right)) => {
                Object::Boolean(left < right)
            }
            (TokenKind::LessEqual, Object::Number(left), Object::Number(right)) => {
                Object::Boolean(left <= right)
            }
            (TokenKind::BangEqual, left, right) => Object::Boolean(left != right),
            (TokenKind::EqualEqual, left, right) => Object::Boolean(left == right),

            // Unsupported operations
            (
                TokenKind::Minus
                | TokenKind::Plus
                | TokenKind::Slash
                | TokenKind::Star
                | TokenKind::Greater
                | TokenKind::GreaterEqual
                | TokenKind::Less
                | TokenKind::LessEqual,
                left,
                right,
            ) => {
                let operator_lexeme = interner.resolve(&operator.lexeme);
                let mut msg = format!(
                    "unable to apply operator `{}` to {}",
                    operator_lexeme,
                    left.kind()
                );
                if left.kind() != right.kind() {
                    write!(&mut msg, " and {}", right.kind()).unwrap();
                }

                let labels = vec![
                    Label::primary(operator.source_id, operator.source_range()),
                    make_secondary_variable_label(left_expr, &left, &self.environment),
                    make_secondary_variable_label(right_expr, &right, &self.environment),
                ];

                return Err(Diagnostic::error().with_message(msg).with_labels(labels));
            }

            _ => unimplemented!(),
        };

        Ok(res)
    }

    fn evaluate_expr_variable(
        &mut self,
        name: Token,
        expr_id: ExpressionId,
        emitter: &mut DiagnosticEmitter<'_>,
        interner: &Rodeo,
    ) -> EvaluateResult {
        let res = if let Some(depth) = self.locals.get(&expr_id).copied() {
            let env = self.environment.borrow();
            env.get_at(name, depth, interner)
        } else {
            let env = self.globals.borrow();
            env.get(name, interner)
        };

        self.had_runtime_error |= res.is_err();
        let (value, diag) = res?;
        if let Some(diag) = diag {
            emitter.emit_diagnostic(&diag).unwrap();
        }

        Ok(value)
    }
}
