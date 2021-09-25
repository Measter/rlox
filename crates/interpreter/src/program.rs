use std::ops::Index;

use rlox::source_file::FileId;

use crate::ast::{Expression, ExpressionKind, Function, Statement};

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

#[derive(Debug, Default)]
pub struct SourceStore {
    functions: Vec<Function>,
    expressions: Vec<Expression>,
    statements: Vec<Statement>,
}

#[derive(Debug, Default)]
pub struct Program {
    programs: Vec<SourceStore>,
}

impl Program {
    pub fn new() -> Self {
        Self::default()
    }

    fn ensure_file(&mut self, id: FileId) {
        let new_len = self.programs.len().max(id.id() + 1);
        self.programs.resize_with(new_len, Default::default);
    }

    pub fn add_expression(&mut self, file_id: FileId, expr_kind: ExpressionKind) -> ExpressionId {
        self.ensure_file(file_id);

        // SAFETY: ensure_file will create a SourceStore at this file index.
        let source_store = unsafe { self.programs.get_unchecked_mut(file_id.id()) };
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
