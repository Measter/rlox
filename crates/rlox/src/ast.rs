use std::ops::Range;

use crate::{
    program::{ExpressionId, FunctionId, Program, StatementId},
    token::Token,
    FileId,
};

#[derive(Debug)]
pub enum ExpressionKind {
    Assign {
        name: Token,
        value: ExpressionId,
    },
    Binary {
        left: ExpressionId,
        operator: Token,
        right: ExpressionId,
    },
    Call {
        callee: ExpressionId,
        arguments: Vec<ExpressionId>,
        source_range: Range<usize>,
    },
    Get {
        object: ExpressionId,
        name: Token,
    },
    Grouping {
        expression: ExpressionId,
        // A bit leaky, but I see no other way to hold this beyond
        // holding on to the paren tokens.
        source_range: Range<usize>,
    },
    Literal {
        // INVARIANT: it's expected that this only contains:
        //  * String
        //  * Number
        //  * Boolean
        //  * Nil
        value: Token,
    },
    Logical {
        left: ExpressionId,
        operator: Token,
        right: ExpressionId,
    },
    Set {
        object: ExpressionId,
        name: Token,
        value: ExpressionId,
    },
    This {
        keyword: Token,
    },
    Super {
        keyword: Token,
        method: Token,
    },
    Unary {
        operator: Token,
        right: ExpressionId,
    },
    Variable {
        name: Token,
    },
}

impl ExpressionKind {
    pub fn assign(name: Token, value: ExpressionId) -> Self {
        ExpressionKind::Assign { name, value }
    }

    pub fn binary(left: ExpressionId, operator: Token, right: ExpressionId) -> Self {
        assert!(operator.kind.is_binary());

        ExpressionKind::Binary {
            left,
            right,
            operator,
        }
    }

    pub fn call(
        callee: ExpressionId,
        arguments: Vec<ExpressionId>,
        source_range: Range<usize>,
    ) -> Self {
        ExpressionKind::Call {
            callee,
            arguments,
            source_range,
        }
    }

    pub fn grouping(expression: ExpressionId, source_range: Range<usize>) -> Self {
        ExpressionKind::Grouping {
            expression,
            source_range,
        }
    }

    pub fn get(object: ExpressionId, name: Token) -> Self {
        ExpressionKind::Get { object, name }
    }

    pub fn literal(value: Token) -> Self {
        assert!(value.kind.is_literal());

        ExpressionKind::Literal { value }
    }

    pub fn logical(left: ExpressionId, operator: Token, right: ExpressionId) -> Self {
        assert!(operator.kind.is_logical());

        ExpressionKind::Logical {
            left,
            operator,
            right,
        }
    }

    pub fn set(object: ExpressionId, name: Token, value: ExpressionId) -> Self {
        ExpressionKind::Set {
            object,
            name,
            value,
        }
    }

    pub fn this(keyword: Token) -> Self {
        ExpressionKind::This { keyword }
    }

    pub fn super_keyword(keyword: Token, method: Token) -> Self {
        ExpressionKind::Super { keyword, method }
    }

    pub fn unary(operator: Token, right: ExpressionId) -> Self {
        assert!(operator.kind.is_unary());

        ExpressionKind::Unary { operator, right }
    }

    pub fn variable(name: Token) -> Self {
        ExpressionKind::Variable { name }
    }
}

#[derive(Debug)]
pub struct Expression {
    pub id: ExpressionId,
    pub kind: ExpressionKind,
}

impl Expression {
    #[inline(always)]
    pub fn source_id(&self) -> FileId {
        self.id.file_id()
    }

    pub fn source_range(&self, program: &Program) -> Range<usize> {
        match &self.kind {
            ExpressionKind::Assign { name, value, .. } => {
                let name_range = name.source_range();
                let value_range = program[*value].source_range(program);

                name_range.start..value_range.end
            }
            ExpressionKind::Binary { left, right, .. }
            | ExpressionKind::Logical { left, right, .. } => {
                let left_range = program[*left].source_range(program);
                let right_range = program[*right].source_range(program);

                left_range.start..right_range.end
            }
            ExpressionKind::Call { source_range, .. } => source_range.clone(),
            ExpressionKind::Grouping { source_range, .. } => source_range.clone(),
            ExpressionKind::Get { name, object, .. } => {
                let start = program[*object].source_range(program).start;
                let end = name.source_range().end;
                start..end
            }
            ExpressionKind::Literal { value, .. } => value.source_range(),
            ExpressionKind::Set { object, value, .. } => {
                let start = program[*object].source_range(program).start;
                let end = program[*value].source_range(program).end;
                start..end
            }
            ExpressionKind::Super { keyword, .. } | ExpressionKind::This { keyword, .. } => {
                keyword.source_range()
            }
            ExpressionKind::Unary {
                operator, right, ..
            } => {
                let op_range = operator.source_range();
                let right_range = program[*right].source_range(program);

                op_range.start..right_range.end
            }
            ExpressionKind::Variable { name, .. } => name.source_range(),
        }
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: Token,
    pub parameters: Vec<Token>,
    pub body: Vec<StatementId>,
}

#[derive(Debug)]
pub enum Statement {
    Block {
        statements: Vec<StatementId>,
    },
    Class {
        name: Token,
        methods: Vec<FunctionId>,
        source_range: Range<usize>,
        superclass: Option<ExpressionId>,
    },
    Expression(ExpressionId),
    Function(FunctionId),
    If {
        condition: ExpressionId,
        then_branch: StatementId,
        else_branch: Option<StatementId>,
    },
    Print {
        keyword: Token,
        expr: ExpressionId,
    },
    Return {
        keyword: Token,
        value: Option<ExpressionId>,
    },
    Variable {
        name: Token,
        initializer: Option<ExpressionId>,
    },
    While {
        condition: ExpressionId,
        body: StatementId,
    },
}
