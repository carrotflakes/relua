use crate::r#type::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Definition {
    Function { name: String, function: Function },
    Variable(Variable),
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub parameters: Vec<(String, Type)>,
    pub return_type: Type,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: String,
    pub type_: Option<Type>,
    pub expr: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(Expression),
    Let(Variable),
    Assignment {
        target: String,
        e: Expression,
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
    Return(Option<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Variable(String),
    Call {
        function: String,
        arguments: Vec<Expression>,
    },
    Index {
        array: Box<Expression>,
        index: Box<Expression>,
    },
    Tuple(Vec<Expression>),
    Table(Vec<TableEntry>),
    Fn(Function),
    LogicalAnd(Box<Expression>, Box<Expression>),
    LogicalOr(Box<Expression>, Box<Expression>),
    LogicalNot(Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TableEntry {
    Field(String, Expression),
    Value(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(f64),
    String(String),
    Bool(bool),
}
