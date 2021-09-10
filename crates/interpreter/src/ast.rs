use std::{ops::Range, rc::Rc};

use crate::{token::Token, FileId};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct ExpressionId {
    expr_id: usize,
    file_id: FileId,
}

impl ExpressionId {
    pub fn new(expr_id: usize, file_id: FileId) -> Self {
        Self { expr_id, file_id }
    }
}

#[derive(Debug)]
pub enum Expression {
    Assign {
        name: Token,
        value: Box<Expression>,
        id: ExpressionId,
    },
    Binary {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
        id: ExpressionId,
    },
    Call {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
        source_range: Range<usize>,
        id: ExpressionId,
    },
    Get {
        object: Box<Expression>,
        name: Token,
        id: ExpressionId,
    },
    Grouping {
        expression: Box<Expression>,
        // A bit leaky, but I see no other way to hold this beyond
        // holding on to the paren tokens.
        source_range: Range<usize>,
        id: ExpressionId,
    },
    Literal {
        // INVARIANT: it's expected that this only contains:
        //  * String
        //  * Number
        //  * Boolean
        //  * Nil
        value: Token,
        id: ExpressionId,
    },
    Logical {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
        id: ExpressionId,
    },
    Set {
        object: Box<Expression>,
        name: Token,
        value: Box<Expression>,
        id: ExpressionId,
    },
    This {
        keyword: Token,
        id: ExpressionId,
    },
    Super {
        keyword: Token,
        method: Token,
        id: ExpressionId,
    },
    Unary {
        operator: Token,
        right: Box<Expression>,
        id: ExpressionId,
    },
    Variable {
        name: Token,
        id: ExpressionId,
    },
}

impl Expression {
    pub fn assign(name: Token, value: Expression, id: ExpressionId) -> Self {
        Self::Assign {
            name,
            value: Box::new(value),
            id,
        }
    }

    pub fn binary(left: Expression, operator: Token, right: Expression, id: ExpressionId) -> Self {
        assert!(operator.kind.is_binary());

        Self::Binary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
            id,
        }
    }

    pub fn call(
        callee: Expression,
        arguments: Vec<Expression>,
        source_range: Range<usize>,
        id: ExpressionId,
    ) -> Self {
        Self::Call {
            callee: Box::new(callee),
            arguments,
            source_range,
            id,
        }
    }

    pub fn grouping(expr: Expression, range: Range<usize>, id: ExpressionId) -> Self {
        Self::Grouping {
            expression: Box::new(expr),
            source_range: range,
            id,
        }
    }

    pub fn get(expr: Expression, name: Token, id: ExpressionId) -> Self {
        Self::Get {
            object: Box::new(expr),
            name,
            id,
        }
    }

    pub fn literal(value: Token, id: ExpressionId) -> Self {
        assert!(value.kind.is_literal());

        Self::Literal { value, id }
    }

    pub fn logical(left: Expression, operator: Token, right: Expression, id: ExpressionId) -> Self {
        assert!(operator.kind.is_logical());

        Self::Logical {
            left: Box::new(left),
            operator,
            right: Box::new(right),
            id,
        }
    }

    pub fn set(object: Expression, name: Token, value: Expression, id: ExpressionId) -> Self {
        Self::Set {
            object: Box::new(object),
            name,
            value: Box::new(value),
            id,
        }
    }

    pub fn this(keyword: Token, id: ExpressionId) -> Self {
        Self::This { keyword, id }
    }

    pub fn super_keyword(keyword: Token, method: Token, id: ExpressionId) -> Self {
        Self::Super {
            keyword,
            method,
            id,
        }
    }

    pub fn unary(operator: Token, right: Expression, id: ExpressionId) -> Self {
        assert!(operator.kind.is_unary());

        Self::Unary {
            operator,
            right: Box::new(right),
            id,
        }
    }

    pub fn variable(name: Token, id: ExpressionId) -> Self {
        Self::Variable { name, id }
    }

    pub fn source_id(&self) -> FileId {
        match self {
            Expression::Assign { id, .. }
            | Expression::Binary { id, .. }
            | Expression::Call { id, .. }
            | Expression::Grouping { id, .. }
            | Expression::Get { id, .. }
            | Expression::Literal { id, .. }
            | Expression::Logical { id, .. }
            | Expression::Set { id, .. }
            | Expression::Super { id, .. }
            | Expression::This { id, .. }
            | Expression::Unary { id, .. }
            | Expression::Variable { id, .. } => id.file_id,
        }
    }

    pub fn source_range(&self) -> Range<usize> {
        match self {
            Expression::Assign { name, value, .. } => {
                let name_range = name.source_range();
                let value_range = value.source_range();

                name_range.start..value_range.end
            }
            Expression::Binary { left, right, .. } | Expression::Logical { left, right, .. } => {
                let left_range = left.source_range();
                let right_range = right.source_range();

                left_range.start..right_range.end
            }
            Expression::Call { source_range, .. } => source_range.clone(),
            Expression::Grouping { source_range, .. } => source_range.clone(),
            Expression::Get { name, object, .. } => {
                object.source_range().start..name.source_range().end
            }
            Expression::Literal { value, .. } => value.source_range(),
            Expression::Set { object, value, .. } => {
                let start = object.source_range().start;
                let end = value.source_range().end;
                start..end
            }
            Expression::Super { keyword, .. } | Expression::This { keyword, .. } => {
                keyword.source_range()
            }
            Expression::Unary {
                operator, right, ..
            } => {
                let op_range = operator.source_range();
                let right_range = right.source_range();

                op_range.start..right_range.end
            }
            Expression::Variable { name, .. } => name.source_range(),
        }
    }

    pub fn id(&self) -> ExpressionId {
        match self {
            Expression::Assign { id, .. }
            | Expression::Binary { id, .. }
            | Expression::Call { id, .. }
            | Expression::Grouping { id, .. }
            | Expression::Get { id, .. }
            | Expression::Literal { id, .. }
            | Expression::Logical { id, .. }
            | Expression::Set { id, .. }
            | Expression::Super { id, .. }
            | Expression::This { id, .. }
            | Expression::Unary { id, .. }
            | Expression::Variable { id, .. } => *id,
        }
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: Token,
    pub parameters: Vec<Token>,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Block {
        statements: Vec<Statement>,
    },
    Class {
        name: Token,
        methods: Vec<Rc<Function>>,
        source_range: Range<usize>,
        superclass: Option<Expression>,
    },
    Expression(Expression),
    Function(Rc<Function>),
    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    Print(Expression),
    Return {
        keyword: Token,
        value: Option<Expression>,
    },
    Variable {
        name: Token,
        initializer: Option<Expression>,
    },
    While {
        condition: Expression,
        body: Box<Statement>,
    },
}
