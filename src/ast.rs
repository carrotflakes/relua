use crate::r#type::{ConstData, Type};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(Expression),
    Let(Variable),
    Fn {
        name: String,
        function: Function,
    },
    Assignment {
        vars: Vec<LValue>,
        exprs: Vec<Expression>,
    },
    If {
        condition: Expression,
        then: Vec<Statement>,
        else_: Vec<Statement>,
    },
    While {
        condition: Expression,
        body: Vec<Statement>,
    },
    ForNumeric {
        variable: String,
        start: Expression,
        end: Expression,
        step: Option<Expression>,
        body: Vec<Statement>,
    },
    // ForGeneric {
    //     variables: Vec<String>,
    //     iter: Expression,
    //     body: Vec<Statement>,
    // },
    Return(Option<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Variable(String),
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Index {
        table: Box<Expression>,
        index: Box<Expression>,
    },
    Table(Vec<(TableKey, Expression)>),
    Fn(Function),
    LogicalAnd(Box<Expression>, Box<Expression>),
    LogicalOr(Box<Expression>, Box<Expression>),
    LogicalNot(Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub parameters: Vec<(String, Type)>,
    pub return_type: Option<Type>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: String,
    pub type_: Option<Type>,
    pub expr: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LValue {
    Variable(String),
    Index(Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TableKey {
    Literal(Literal),
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(f64),
    String(String),
    Bool(bool),
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
