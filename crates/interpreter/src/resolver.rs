use std::{collections::HashMap, ops::Range, rc::Rc};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use lasso::{Rodeo, Spur};

use crate::{
    ast::{Expression, ExpressionId, Function, Statement},
    interpreter::Interpreter,
    source_file::FileId,
    token::Token,
};

pub type ResolveResult = Result<(), Diagnostic<FileId>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum InitState {
    PreDeclare,
    Declared,
}

struct Variable {
    init_state: InitState,
    source_range: Range<usize>,
    source_id: FileId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClassType {
    None,
    Class,
    SubClass,
}

pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    interner: &'a mut Rodeo,
    scopes: Vec<HashMap<Spur, Variable>>,
    current_function: FunctionType,
    current_class: ClassType,
    file_id: FileId,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter, file_id: FileId, interner: &'a mut Rodeo) -> Self {
        Self {
            interpreter,
            scopes: Vec::new(),
            current_function: FunctionType::None,
            current_class: ClassType::None,
            file_id,
            interner,
        }
    }

    pub fn resolve(&mut self, statements: &[Statement]) -> ResolveResult {
        for statement in statements {
            self.resolve_statement(statement)?;
        }

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(Default::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: Token) -> ResolveResult {
        if let Some(scope) = self.scopes.last_mut() {
            let var = Variable {
                init_state: InitState::PreDeclare,
                source_range: name.source_range(),
                source_id: name.source_id,
            };
            if let Some(already_contained) = scope.insert(name.lexeme, var) {
                let diag = Diagnostic::error()
                    .with_message("a variable with this name already exists in this scope")
                    .with_labels(vec![
                        Label::primary(name.source_id, name.source_range()),
                        Label::secondary(
                            already_contained.source_id,
                            already_contained.source_range,
                        )
                        .with_message("previously defined here"),
                    ]);

                return Err(diag);
            }
        }

        Ok(())
    }

    fn define(&mut self, name: Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(
                name.lexeme,
                Variable {
                    init_state: InitState::Declared,
                    source_range: name.source_range(),
                    source_id: name.source_id,
                },
            );
        }
    }

    fn resolve_local(&mut self, expr_id: ExpressionId, name: Token) -> ResolveResult {
        for (depth, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(&name.lexeme) {
                self.interpreter.resolve(expr_id, depth);
                break;
            }
        }

        Ok(())
    }

    fn resolve_statement(&mut self, statement: &Statement) -> ResolveResult {
        match statement {
            Statement::Block { statements } => {
                self.begin_scope();
                self.resolve(statements)?;
                self.end_scope();
            }
            Statement::Class {
                name,
                methods,
                superclass,
                ..
            } => {
                self.declare(*name)?;
                self.define(*name);

                self.resolve_class(*name, superclass.as_ref(), methods)?;
            }
            Statement::Expression(expr) | Statement::Print(expr) => {
                self.resolve_expression(expr)?
            }
            Statement::Function(func) => {
                self.declare(func.name)?;
                self.define(func.name);

                self.resolve_function(func, FunctionType::Function)?;
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expression(condition)?;
                self.resolve_statement(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.resolve_statement(else_branch)?;
                }
            }
            Statement::Return { value, keyword } => {
                let source_range = if let Some(value) = value.as_ref() {
                    let end = value.source_range().end;
                    keyword.source_range().start..end
                } else {
                    keyword.source_range()
                };

                if self.current_function == FunctionType::None {
                    let diag = Diagnostic::error()
                        .with_message("can't return from top-level code")
                        .with_labels(vec![Label::primary(keyword.source_id, source_range)]);
                    return Err(diag);
                }

                if let Some(value) = value.as_ref() {
                    if self.current_function == FunctionType::Initializer {
                        let diag = Diagnostic::error()
                            .with_message("can't return a value from a class initializer")
                            .with_labels(vec![Label::primary(keyword.source_id, source_range)]);
                        return Err(diag);
                    }
                    self.resolve_expression(value)?;
                }
            }
            Statement::Variable { name, initializer } => {
                self.declare(*name)?;
                if let Some(init) = initializer {
                    self.resolve_expression(init)?;
                }
                self.define(*name);
            }
            Statement::While { condition, body } => {
                self.resolve_expression(condition)?;
                self.resolve_statement(body)?;
            }
        }

        Ok(())
    }

    fn resolve_class(
        &mut self,
        name: Token,
        superclass: Option<&Expression>,
        methods: &[Rc<Function>],
    ) -> ResolveResult {
        let enclosing_class = std::mem::replace(&mut self.current_class, ClassType::Class);
        match superclass.as_ref() {
            Some(Expression::Variable {
                name: super_name, ..
            }) => {
                self.current_class = ClassType::SubClass;
                let expr = superclass.unwrap();
                self.resolve_expression(expr)?;
                if name.lexeme == super_name.lexeme {
                    let diag = Diagnostic::error()
                        .with_message("A class can't inherit from itself")
                        .with_labels(vec![Label::primary(
                            super_name.source_id,
                            super_name.source_range(),
                        )]);
                    return Err(diag);
                }
            }
            Some(e) => {
                panic!("ICE: expected superclass to be a Variable, found `{:?}`", e);
            }
            None => {}
        }

        if superclass.is_some() {
            self.begin_scope();

            self.scopes.last_mut().unwrap().insert(
                self.interner.get("super").unwrap(),
                Variable {
                    init_state: InitState::Declared,
                    source_range: name.source_range(),
                    source_id: name.source_id,
                },
            );
        }

        self.begin_scope();
        self.scopes.last_mut().unwrap().insert(
            self.interner.get_or_intern_static("this"),
            Variable {
                init_state: InitState::Declared,
                source_range: 0..0,
                source_id: self.file_id,
            },
        );

        for method in methods {
            let declaration = if method.name.lexeme == self.interner.get("init").unwrap() {
                FunctionType::Initializer
            } else {
                FunctionType::Method
            };
            self.resolve_function(method, declaration)?;
        }
        self.end_scope();

        if superclass.is_some() {
            self.end_scope();
        }
        self.current_class = enclosing_class;

        Ok(())
    }

    fn resolve_function(&mut self, func: &Function, func_type: FunctionType) -> ResolveResult {
        let enclosing_function = std::mem::replace(&mut self.current_function, func_type);
        self.begin_scope();

        for &param in &func.parameters {
            self.declare(param)?;
            self.define(param);
        }

        // There's a bit of a difference in how the author's code handles blocks and mine
        // which results one scope being introduced for the function arguments, and a second
        // for the function body.
        // Fixing that would make the interpreter a bit more complex, so I'm "fixing" it here.
        self.begin_scope();
        self.resolve(&func.body)?;
        self.end_scope();

        self.end_scope();
        self.current_function = enclosing_function;

        Ok(())
    }

    fn resolve_expression(&mut self, expression: &Expression) -> ResolveResult {
        match expression {
            Expression::Assign { name, value, id } => {
                self.resolve_expression(value)?;
                self.resolve_local(*id, *name)?;
            }
            Expression::Binary { left, right, .. } | Expression::Logical { left, right, .. } => {
                self.resolve_expression(left)?;
                self.resolve_expression(right)?;
            }
            Expression::Call {
                callee, arguments, ..
            } => {
                self.resolve_expression(callee)?;
                for arg in arguments {
                    self.resolve_expression(arg)?;
                }
            }
            Expression::Grouping { expression, .. }
            | Expression::Get {
                object: expression, ..
            }
            | Expression::Unary {
                right: expression, ..
            } => self.resolve_expression(expression)?,

            Expression::Literal { .. } => {}
            Expression::Set { value, object, .. } => {
                self.resolve_expression(value)?;
                self.resolve_expression(object)?;
            }
            Expression::Super { keyword, id, .. } => {
                if self.current_class != ClassType::SubClass {
                    let diag = Diagnostic::error()
                        .with_message("can't use `super` outside of a subclass")
                        .with_labels(vec![Label::primary(
                            keyword.source_id,
                            keyword.source_range(),
                        )]);
                    return Err(diag);
                }

                self.resolve_variable(*keyword, *id)?;
            }
            Expression::This { keyword, id } => {
                if self.current_class == ClassType::None {
                    let diag = Diagnostic::error()
                        .with_message("can't use `this` outside of a class")
                        .with_labels(vec![Label::primary(
                            keyword.source_id,
                            keyword.source_range(),
                        )]);
                    return Err(diag);
                }

                self.resolve_variable(*keyword, *id)?;
            }
            Expression::Variable { name, id } => {
                self.resolve_variable(*name, *id)?;
            }
        }

        Ok(())
    }

    fn resolve_variable(&mut self, name: Token, id: ExpressionId) -> ResolveResult {
        let var = self
            .scopes
            .last()
            .and_then(|s| s.get(&name.lexeme))
            .map(|v| v.init_state);

        if let Some(InitState::PreDeclare) = var {
            let diag = Diagnostic::error()
                .with_message("can't read local variable in its own initializer")
                .with_labels(vec![Label::primary(self.file_id, name.source_range())]);
            return Err(diag);
        }

        self.resolve_local(id, name)
    }
}
