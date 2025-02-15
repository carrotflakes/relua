use crate::ast;
use crate::r#type::{ConstData, Type, TypeTable};

#[derive(Clone, Debug, PartialEq)]
pub enum TypeFilter {
    Item(String, Vec<ConstData>, TypeFilterItem),
    Not(Box<TypeFilter>),
    And(Box<TypeFilter>, Box<TypeFilter>),
    Or(Box<TypeFilter>, Box<TypeFilter>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeFilterItem {
    Number,
    String,
    Bool,
    Nil,
    Table,
    Function,
    Const(ConstData),
    Truthy,
}

impl TypeFilterItem {
    pub fn to_type(&self, neg: bool) -> Type {
        if !neg {
            match self {
                TypeFilterItem::Number => Type::Number,
                TypeFilterItem::String => Type::String,
                TypeFilterItem::Bool => Type::Bool,
                TypeFilterItem::Nil => Type::Nil,
                TypeFilterItem::Table => Type::Table(TypeTable::any()),
                TypeFilterItem::Function => Type::Function(vec![], vec![]),
                TypeFilterItem::Const(const_data) => Type::Const(const_data.clone()),
                TypeFilterItem::Truthy => Type::Union(vec![
                    Type::Number,
                    Type::String,
                    Type::Table(TypeTable::any()),
                    Type::Function(vec![], vec![]),
                    Type::Const(ConstData::Bool(true)),
                ]),
            }
        } else {
            match self {
                TypeFilterItem::Number => Type::Union(vec![
                    Type::String,
                    Type::Bool,
                    Type::Nil,
                    Type::Table(TypeTable::any()),
                    Type::Function(vec![], vec![]),
                ]),
                TypeFilterItem::String => Type::Union(vec![
                    Type::Number,
                    Type::Bool,
                    Type::Nil,
                    Type::Table(TypeTable::any()),
                    Type::Function(vec![], vec![]),
                ]),
                TypeFilterItem::Bool => Type::Union(vec![
                    Type::Number,
                    Type::String,
                    Type::Nil,
                    Type::Table(TypeTable::any()),
                    Type::Function(vec![], vec![]),
                ]),
                TypeFilterItem::Nil => Type::Union(vec![
                    Type::Number,
                    Type::String,
                    Type::Bool,
                    Type::Table(TypeTable::any()),
                    Type::Function(vec![], vec![]),
                ]),
                TypeFilterItem::Table => Type::Union(vec![
                    Type::Number,
                    Type::String,
                    Type::Bool,
                    Type::Nil,
                    Type::Function(vec![], vec![]),
                ]),
                TypeFilterItem::Function => Type::Union(vec![
                    Type::Number,
                    Type::String,
                    Type::Bool,
                    Type::Nil,
                    Type::Table(TypeTable::any()),
                ]),
                TypeFilterItem::Const(_) => Type::Any, // ?
                TypeFilterItem::Truthy => {
                    Type::Union(vec![Type::Nil, Type::Const(ConstData::Bool(false))])
                }
            }
        }
    }
}

pub fn expression_to_type_filter(expr: &ast::SpannedExpression) -> Option<TypeFilter> {
    match &**expr {
        ast::Expression::LogicalAnd(a, b) => {
            match (expression_to_type_filter(a), expression_to_type_filter(b)) {
                (Some(a), Some(b)) => Some(TypeFilter::And(Box::new(a), Box::new(b))),
                (Some(a), None) => Some(a),
                (None, Some(b)) => Some(b),
                _ => None,
            }
        }
        ast::Expression::LogicalOr(a, b) => Some(TypeFilter::Or(
            Box::new(expression_to_type_filter(a)?),
            Box::new(expression_to_type_filter(b)?),
        )),
        ast::Expression::LogicalNot(a) => {
            Some(TypeFilter::Not(Box::new(expression_to_type_filter(a)?)))
        }
        ast::Expression::Call {
            function,
            arguments,
        } => match function.as_string() {
            Some("__eq") => process_eq(arguments),
            Some("__ne") => process_eq(arguments).map(|value| TypeFilter::Not(Box::new(value))),
            _ => None,
        },
        ast::Expression::Index { .. } => None,
        ast::Expression::Fn(_) => None,
        ast::Expression::Table(_) => None,
        ast::Expression::Literal(_) => None,
        ast::Expression::Nil => None,
        ast::Expression::Variable(name) => Some(TypeFilter::Item(
            name.clone(),
            vec![],
            TypeFilterItem::Truthy,
        )),
        ast::Expression::TypeResolve(_, _) => {
            None // TODO
        }
    }
}

fn process_eq(args: &Vec<ast::Spanned<ast::Expression>>) -> Option<TypeFilter> {
    for (i1, i2) in [(0, 1), (1, 0)] {
        // Check: type(x) == "number"
        if let ast::Expression::Call {
            function,
            arguments: args2,
        } = &*args[i1]
        {
            // FIXME: We should check the type of the function
            if function.as_variable() == Some(&"type") {
                if let Some(path) = args2[0].indexing_path() {
                    match args[i2].as_string() {
                        Some("number") => {
                            return Some(TypeFilter::Item(path.0, path.1, TypeFilterItem::Number))
                        }
                        Some("string") => {
                            return Some(TypeFilter::Item(path.0, path.1, TypeFilterItem::String))
                        }
                        Some("boolean") => {
                            return Some(TypeFilter::Item(path.0, path.1, TypeFilterItem::Bool))
                        }
                        Some("nil") => {
                            return Some(TypeFilter::Item(path.0, path.1, TypeFilterItem::Nil))
                        }
                        Some("table") => {
                            return Some(TypeFilter::Item(path.0, path.1, TypeFilterItem::Table))
                        }
                        Some("function") => {
                            return Some(TypeFilter::Item(path.0, path.1, TypeFilterItem::Function))
                        }
                        _ => {}
                    }
                }
            }
        }

        // Check: x == "foo"
        if let Some(path) = args[i1].indexing_path() {
            if let Some(s) = args[i2].as_string() {
                return Some(TypeFilter::Item(
                    path.0,
                    path.1,
                    TypeFilterItem::Const(ConstData::String(s.to_owned())),
                ));
            }
        }
    }
    None
}

pub fn type_filter_to_dnf(
    tf: &TypeFilter,
) -> Vec<Vec<(String, Vec<ConstData>, TypeFilterItem, bool)>> {
    match tf {
        TypeFilter::Item(name, path, i) => {
            vec![vec![(name.clone(), path.clone(), i.clone(), false)]]
        }

        TypeFilter::Not(sub) => match &**sub {
            TypeFilter::Item(name, path, i) => {
                vec![vec![(name.clone(), path.clone(), i.clone(), true)]]
            }
            TypeFilter::Not(xx) => type_filter_to_dnf(xx),
            TypeFilter::And(x, y) => type_filter_to_dnf(&TypeFilter::Or(
                Box::new(TypeFilter::Not(x.clone())),
                Box::new(TypeFilter::Not(y.clone())),
            )),
            TypeFilter::Or(x, y) => type_filter_to_dnf(&TypeFilter::And(
                Box::new(TypeFilter::Not(x.clone())),
                Box::new(TypeFilter::Not(y.clone())),
            )),
        },

        TypeFilter::And(a, b) => {
            let dnf_a = type_filter_to_dnf(a);
            let dnf_b = type_filter_to_dnf(b);

            let mut result = Vec::new();
            for clause_a in dnf_a {
                for clause_b in &dnf_b {
                    let mut combined = clause_a.clone();
                    combined.extend(clause_b.clone());
                    result.push(combined);
                }
            }
            result
        }

        TypeFilter::Or(a, b) => {
            let mut dnf_a = type_filter_to_dnf(a);
            let dnf_b = type_filter_to_dnf(b);

            dnf_a.extend(dnf_b);
            dnf_a
        }
    }
}

pub fn type_filter_apply(
    tf: &Vec<Vec<(String, Vec<ConstData>, TypeFilterItem, bool)>>,
    name: &str,
    type_: &Type,
    neg: bool,
) -> Type {
    let mut result = Vec::new();
    for clause in tf {
        let mut clause_result = type_.clone();
        for (name_, path, item, neg_) in clause {
            if name_ != name {
                continue;
            }

            let neg = *neg_ ^ neg;

            clause_result = if !path.is_empty() {
                if let Type::Union(types) = clause_result {
                    Type::Union(
                        types
                            .into_iter()
                            .flat_map(|t| {
                                if t.indexing(path).intersect(&item.to_type(neg)).is_never() {
                                    None
                                } else {
                                    Some(t)
                                }
                            })
                            .collect(),
                    )
                    .normalize()
                } else {
                    if clause_result
                        .indexing(path)
                        .intersect(&item.to_type(neg))
                        .is_never()
                    {
                        Type::never()
                    } else {
                        clause_result
                    }
                }
            } else {
                clause_result.intersect(&item.to_type(neg))
            };
        }
        result.push(clause_result);
    }
    Type::Union(result).normalize()
}
