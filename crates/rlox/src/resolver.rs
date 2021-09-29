use codespan_reporting::diagnostic::{Diagnostic, Label};
use fnv::FnvHashMap as HashMap;
use lasso::{Rodeo, Spur};

use crate::{
    ast::{ExpressionKind, Statement},
    program::{ExpressionId, FunctionId, Program, StatementId},
    source_file::{FileId, SourceLocation},
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
    location: SourceLocation,
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
    interner: &'a mut Rodeo,
    program: &'a Program,
    scopes: Vec<HashMap<Spur, Variable>>,
    local_depths: HashMap<ExpressionId, usize>,
    current_function: FunctionType,
    current_class: ClassType,
    file_id: FileId,
}

impl<'a> Resolver<'a> {
    fn new(file_id: FileId, interner: &'a mut Rodeo, program: &'a Program) -> Self {
        Self {
            scopes: Vec::new(),
            local_depths: HashMap::default(),
            current_function: FunctionType::None,
            current_class: ClassType::None,
            file_id,
            interner,
            program,
        }
    }

    pub fn resolve(
        file_id: FileId,
        interner: &'a mut Rodeo,
        program: &'a mut Program,
        statements: &[StatementId],
    ) -> ResolveResult {
        let locals = {
            let mut resolver = Resolver::new(file_id, interner, program);
            resolver.resolve_statement_list(statements)?;
            resolver.local_depths
        };

        program.resolve_locals(locals);

        Ok(())
    }

    fn resolve_statement_list(&mut self, statements: &[StatementId]) -> ResolveResult {
        for &statement in statements {
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
                location: name.location,
            };
            if let Some(already_contained) = scope.insert(name.lexeme, var) {
                let diag = Diagnostic::error()
                    .with_message("a variable with this name already exists in this scope")
                    .with_labels(vec![
                        Label::primary(name.location.file_id, name.source_range()),
                        Label::secondary(
                            already_contained.location.file_id,
                            already_contained.location.range(),
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
                    location: name.location,
                },
            );
        }
    }

    fn resolve_local(&mut self, expr_id: ExpressionId, name: Token) -> ResolveResult {
        for (depth, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(&name.lexeme) {
                self.local_depths.insert(expr_id, depth);
                break;
            }
        }

        Ok(())
    }

    fn resolve_statement(&mut self, statement: StatementId) -> ResolveResult {
        match &self.program[statement] {
            Statement::Block { statements } => {
                self.begin_scope();
                self.resolve_statement_list(statements)?;
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

                self.resolve_class(*name, *superclass, methods)?;
            }
            Statement::Expression(expr) | Statement::Print(expr) => {
                self.resolve_expression(*expr)?
            }
            Statement::Function(func) => {
                let func_def = &self.program[*func];
                self.declare(func_def.name)?;
                self.define(func_def.name);

                self.resolve_function(*func, FunctionType::Function)?;
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expression(*condition)?;
                self.resolve_statement(*then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.resolve_statement(*else_branch)?;
                }
            }
            Statement::Return { value, keyword } => {
                let source_range = if let Some(value) = value.as_ref() {
                    let end = self.program[*value].source_range(self.program).end;
                    keyword.source_range().start..end
                } else {
                    keyword.source_range()
                };

                if self.current_function == FunctionType::None {
                    let diag = Diagnostic::error()
                        .with_message("can't return from top-level code")
                        .with_labels(vec![Label::primary(keyword.location.file_id, source_range)]);
                    return Err(diag);
                }

                if let Some(value) = value.as_ref() {
                    if self.current_function == FunctionType::Initializer {
                        let diag = Diagnostic::error()
                            .with_message("can't return a value from a class initializer")
                            .with_labels(vec![Label::primary(
                                keyword.location.file_id,
                                source_range,
                            )]);
                        return Err(diag);
                    }
                    self.resolve_expression(*value)?;
                }
            }
            Statement::Variable { name, initializer } => {
                self.declare(*name)?;
                if let Some(init) = initializer {
                    self.resolve_expression(*init)?;
                }
                self.define(*name);
            }
            Statement::While { condition, body } => {
                self.resolve_expression(*condition)?;
                self.resolve_statement(*body)?;
            }
        }

        Ok(())
    }

    fn resolve_class(
        &mut self,
        name: Token,
        superclass: Option<ExpressionId>,
        methods: &[FunctionId],
    ) -> ResolveResult {
        let enclosing_class = std::mem::replace(&mut self.current_class, ClassType::Class);
        match superclass.map(|id| &self.program[id].kind) {
            Some(ExpressionKind::Variable {
                name: super_name, ..
            }) => {
                self.current_class = ClassType::SubClass;
                let expr = superclass.unwrap();
                self.resolve_expression(expr)?;
                if name.lexeme == super_name.lexeme {
                    let diag = Diagnostic::error()
                        .with_message("A class can't inherit from itself")
                        .with_labels(vec![Label::primary(
                            super_name.location.file_id,
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
                    location: name.location,
                },
            );
        }

        self.begin_scope();
        self.scopes.last_mut().unwrap().insert(
            self.interner.get_or_intern_static("this"),
            Variable {
                init_state: InitState::Declared,
                location: SourceLocation::new(self.file_id, 0..0),
            },
        );

        for method in methods {
            let method_name = self.program[*method].name.lexeme;
            let declaration = if method_name == self.interner.get("init").unwrap() {
                FunctionType::Initializer
            } else {
                FunctionType::Method
            };
            self.resolve_function(*method, declaration)?;
        }
        self.end_scope();

        if superclass.is_some() {
            self.end_scope();
        }
        self.current_class = enclosing_class;

        Ok(())
    }

    fn resolve_function(&mut self, func: FunctionId, func_type: FunctionType) -> ResolveResult {
        let enclosing_function = std::mem::replace(&mut self.current_function, func_type);
        self.begin_scope();

        let func = &self.program[func];
        for &param in &func.parameters {
            self.declare(param)?;
            self.define(param);
        }

        self.resolve_statement_list(&func.body)?;

        self.end_scope();
        self.current_function = enclosing_function;

        Ok(())
    }

    fn resolve_expression(&mut self, expr_id: ExpressionId) -> ResolveResult {
        match &self.program[expr_id].kind {
            ExpressionKind::Assign { name, value } => {
                self.resolve_expression(*value)?;
                self.resolve_local(expr_id, *name)?;
            }
            ExpressionKind::Binary { left, right, .. }
            | ExpressionKind::Logical { left, right, .. } => {
                self.resolve_expression(*left)?;
                self.resolve_expression(*right)?;
            }
            ExpressionKind::Call {
                callee, arguments, ..
            } => {
                self.resolve_expression(*callee)?;
                for arg in arguments {
                    self.resolve_expression(*arg)?;
                }
            }
            ExpressionKind::Grouping { expression, .. }
            | ExpressionKind::Get {
                object: expression, ..
            }
            | ExpressionKind::Unary {
                right: expression, ..
            } => self.resolve_expression(*expression)?,

            ExpressionKind::Literal { .. } => {}
            ExpressionKind::Set { value, object, .. } => {
                self.resolve_expression(*value)?;
                self.resolve_expression(*object)?;
            }
            ExpressionKind::Super { keyword, .. } => {
                if self.current_class != ClassType::SubClass {
                    let diag = Diagnostic::error()
                        .with_message("can't use `super` outside of a subclass")
                        .with_labels(vec![Label::primary(
                            keyword.location.file_id,
                            keyword.source_range(),
                        )]);
                    return Err(diag);
                }

                self.resolve_variable(*keyword, expr_id)?;
            }
            ExpressionKind::This { keyword } => {
                if self.current_class == ClassType::None {
                    let diag = Diagnostic::error()
                        .with_message("can't use `this` outside of a class")
                        .with_labels(vec![Label::primary(
                            keyword.location.file_id,
                            keyword.source_range(),
                        )]);
                    return Err(diag);
                }

                self.resolve_variable(*keyword, expr_id)?;
            }
            ExpressionKind::Variable { name } => {
                self.resolve_variable(*name, expr_id)?;
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
