use std::ops::Index;

use lasso::Spur;

use crate::{
    ast::{Expression, ExpressionKind, Function, Statement},
    source_file::FileId,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExpressionId {
    file_id: FileId,
    expr_id: usize,
}

impl ExpressionId {
    pub fn file_id(self) -> FileId {
        self.file_id
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StatementId {
    file_id: FileId,
    stmt_id: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionId {
    file_id: FileId,
    func_id: usize,
}

#[derive(Debug, Default)]
pub struct SourceStore {
    functions: Vec<Function>,
    expressions: Vec<Expression>,
    statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct Program {
    programs: Vec<SourceStore>,
    this_lexeme: Spur,
    init_lexeme: Spur,
    super_lexeme: Spur,
}

impl Program {
    pub fn new(this_lexeme: Spur, init_lexeme: Spur, super_lexeme: Spur) -> Self {
        Self {
            programs: Vec::new(),
            this_lexeme,
            init_lexeme,
            super_lexeme,
        }
    }

    pub fn this_lexeme(&self) -> Spur {
        self.this_lexeme
    }

    pub fn init_lexeme(&self) -> Spur {
        self.init_lexeme
    }

    pub fn super_lexeme(&self) -> Spur {
        self.super_lexeme
    }

    fn ensure_file(&mut self, id: FileId) {
        let new_len = self.programs.len().max(id.id() + 1);
        self.programs.resize_with(new_len, Default::default);
    }

    pub fn add_expression(&mut self, file_id: FileId, expr_kind: ExpressionKind) -> ExpressionId {
        self.ensure_file(file_id);

        let source_store = &mut self.programs[file_id.id()];
        let expr_id = ExpressionId {
            file_id,
            expr_id: source_store.expressions.len(),
        };
        source_store.expressions.push(Expression {
            id: expr_id,
            kind: expr_kind,
        });

        expr_id
    }

    pub fn add_statement(&mut self, file_id: FileId, statement: Statement) -> StatementId {
        self.ensure_file(file_id);

        let source_store = &mut self.programs[file_id.id()];
        let stmt_id = StatementId {
            file_id,
            stmt_id: source_store.statements.len(),
        };

        source_store.statements.push(statement);
        stmt_id
    }

    pub fn add_function(&mut self, file_id: FileId, function: Function) -> FunctionId {
        self.ensure_file(file_id);

        let source_store = &mut self.programs[file_id.id()];
        let func_id = FunctionId {
            file_id,
            func_id: source_store.functions.len(),
        };
        source_store.functions.push(function);

        func_id
    }
}

impl Index<ExpressionId> for Program {
    type Output = Expression;

    fn index(&self, index: ExpressionId) -> &Self::Output {
        // SAFETY: The only way to get an ExpressionId is by adding an expression to the
        // storage, which can only create expressions, not remove them.
        unsafe {
            self.programs
                .get_unchecked(index.file_id.id())
                .expressions
                .get_unchecked(index.expr_id)
        }
    }
}

impl Index<StatementId> for Program {
    type Output = Statement;

    fn index(&self, index: StatementId) -> &Self::Output {
        // SAFETY: The only way to get a StatementId is through the add_statement function
        // which only creates valid IDs.
        unsafe {
            self.programs
                .get_unchecked(index.file_id.id())
                .statements
                .get_unchecked(index.stmt_id)
        }
    }
}

impl Index<FunctionId> for Program {
    type Output = Function;

    fn index(&self, index: FunctionId) -> &Self::Output {
        // SAFETY: The only way to get a FunctionId is through the add_function function
        // which only creates valid IDs.
        unsafe {
            self.programs
                .get_unchecked(index.file_id.id())
                .functions
                .get_unchecked(index.func_id)
        }
    }
}
