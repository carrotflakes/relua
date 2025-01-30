use crate::r#type::{ConstData, Type};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span(pub std::ops::Range<usize>);

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn none(node: T) -> Self {
        Spanned {
            node,
            span: Span(0..0),
        }
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(SpannedExpression),
    Let(Vec<(String, Option<Type>)>, Vec<SpannedExpression>),
    Fn {
        name: String,
        function: Function,
    },
    Assignment {
        vars: Vec<LValue>,
        exprs: Vec<SpannedExpression>,
    },
    If {
        condition: SpannedExpression,
        then: Vec<SpannedStatement>,
        else_: Vec<SpannedStatement>,
    },
    While {
        condition: SpannedExpression,
        body: Vec<SpannedStatement>,
    },
    ForNumeric {
        variable: String,
        start: SpannedExpression,
        end: SpannedExpression,
        step: Option<SpannedExpression>,
        body: Vec<SpannedStatement>,
    },
    ForGeneric {
        variables: Vec<(String, Option<Type>)>,
        exprs: Vec<SpannedExpression>,
        body: Vec<SpannedStatement>,
    },
    Return(Vec<SpannedExpression>),
    Break,
    TypeAlias(String, Type),
}

pub type SpannedStatement = Spanned<Statement>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Nil,
    Variable(String),
    Call {
        function: Box<SpannedExpression>,
        arguments: Vec<SpannedExpression>,
    },
    Index {
        table: Box<SpannedExpression>,
        index: Box<SpannedExpression>,
    },
    Table(Vec<(TableKey, SpannedExpression)>),
    Fn(Function),
    LogicalAnd(Box<SpannedExpression>, Box<SpannedExpression>),
    LogicalOr(Box<SpannedExpression>, Box<SpannedExpression>),
    LogicalNot(Box<SpannedExpression>),
    TypeResolve(Box<SpannedExpression>, Vec<Type>),
}

pub type SpannedExpression = Spanned<Expression>;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub type_params: Vec<String>,
    pub parameters: Vec<(String, Type)>,
    pub return_types: Option<Vec<Type>>,
    pub body: Vec<SpannedStatement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LValue {
    Variable(String),
    Index(Box<SpannedExpression>, Box<SpannedExpression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TableKey {
    Literal(Literal),
    Expression(SpannedExpression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(f64),
    String(String),
    Bool(bool),
}

impl Expression {
    pub fn as_string(&self) -> Option<&str> {
        match self {
            Expression::Literal(Literal::String(s)) => Some(s),
            _ => None,
        }
    }

    pub fn as_variable(&self) -> Option<&str> {
        match self {
            Expression::Variable(s) => Some(s),
            _ => None,
        }
    }

    /// Returns the indexing path of the expression.
    pub fn indexing_path(&self) -> Option<(String, Vec<ConstData>)> {
        match self {
            Expression::Variable(s) => Some((s.clone(), vec![])),
            Expression::Index { table, index } => {
                if let Expression::Literal(k) = &***index {
                    let mut path = table.indexing_path()?;
                    path.1.push(k.to_const_data());
                    return Some(path);
                }
                None
            }
            _ => None,
        }
    }
}

impl Literal {
    pub fn to_const_data(&self) -> ConstData {
        match self {
            Literal::Number(n) => ConstData::try_from_f64(*n).unwrap(),
            Literal::String(s) => ConstData::String(s.clone()),
            Literal::Bool(b) => ConstData::Bool(*b),
        }
    }
}
