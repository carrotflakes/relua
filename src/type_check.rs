use std::collections::HashMap;
use std::ops::Deref;

use crate::ast;
use crate::r#type::{ConstData, Type, TypeTable};

enum ReturnType {
    Fixed(Type),
    Infer(Vec<Type>),
}

pub fn check_program(
    bindings: HashMap<String, Type>,
    stmts: &[ast::Statement],
) -> Result<Type, String> {
    let mut return_type = ReturnType::Infer(vec![]);

    check_statements(bindings, stmts, &mut return_type)?;

    let return_type = match return_type {
        ReturnType::Fixed(t) => t,
        ReturnType::Infer(types) => Type::from_types(types).unwrap_or(Type::Nil),
    };

    Ok(return_type)
}

fn check_function(
    mut bindings: HashMap<String, Type>,
    function: &ast::Function,
) -> Result<Type, String> {
    for (name, type_) in &function.parameters {
        bindings.insert(name.clone(), type_.clone());
    }

    let mut ret_type = match &function.return_type {
        Some(t) => ReturnType::Fixed(t.clone()),
        None => ReturnType::Infer(vec![]),
    };

    check_statements(bindings, &function.body, &mut ret_type)?;

    let ret_type = match ret_type {
        ReturnType::Fixed(t) => t,
        ReturnType::Infer(types) => Type::from_types(types).unwrap_or(Type::Nil),
    };

    Ok(ret_type)
}

fn check_statements(
    mut bindings: HashMap<String, Type>,
    stmts: &[ast::Statement],
    return_type: &mut ReturnType,
) -> Result<(), String> {
    for stmt in stmts {
        match stmt {
            ast::Statement::Expression(expression) => {
                check_expression(bindings.clone(), expression)?;
            }
            ast::Statement::Fn { name, function } => {
                // Add function to bindings to allow recursion
                bindings.insert(
                    name.clone(),
                    Type::Function(
                        function.parameters.iter().map(|(_, t)| t.clone()).collect(),
                        Box::new(function.return_type.clone().unwrap_or(Type::Unknown)),
                    ),
                );

                let ret_type = check_function(bindings.clone(), function)?;
                bindings.insert(
                    name.clone(),
                    Type::Function(
                        function.parameters.iter().map(|(_, t)| t.clone()).collect(),
                        Box::new(ret_type),
                    ),
                );
            }
            ast::Statement::Let(variable) => {
                let actual = check_expression(bindings.clone(), &variable.expr)?;
                if let Some(expect) = &variable.type_ {
                    type_match(expect, &actual)?;
                    bindings.insert(variable.name.clone(), expect.clone());
                } else {
                    bindings.insert(variable.name.clone(), actual);
                }
            }
            ast::Statement::Assignment { vars, exprs } => {
                let mut var_types = vec![];
                for var in vars {
                    let var_type = match var {
                        ast::LValue::Variable(name) => bindings
                            .get(name)
                            .ok_or_else(|| format!("Variable not found: {}", name))?
                            .clone(),
                        ast::LValue::Index(table, index) => {
                            let table_type = check_expression(bindings.clone(), table)?;
                            let Type::Table(table_type) = table_type else {
                                return Err("Not a table".to_string());
                            };
                            let index_type = check_expression(bindings.clone(), index)?;
                            check_table(&table_type, &index_type)?
                        }
                    };
                    var_types.push(var_type);
                }
                for i in 0..exprs.len() {
                    let expr_type = check_expression(bindings.clone(), &exprs[i])?;
                    type_match(&var_types[i], &expr_type)?;
                }
            }
            ast::Statement::If {
                condition,
                then,
                else_,
            } => {
                check_expression(bindings.clone(), condition)?;
                check_statements(bindings.clone(), then, return_type)?;
                check_statements(bindings.clone(), else_, return_type)?;
            }
            ast::Statement::While { condition, body } => {
                check_expression(bindings.clone(), condition)?;
                check_statements(bindings.clone(), body, return_type)?;
            }
            ast::Statement::ForNumeric {
                variable,
                start,
                end,
                step,
                body,
            } => {
                let start_type = check_expression(bindings.clone(), start)?;
                type_match(&Type::Number, &start_type)?;
                let end_type = check_expression(bindings.clone(), end)?;
                type_match(&Type::Number, &end_type)?;
                if let Some(step) = step {
                    let step_type = check_expression(bindings.clone(), step)?;
                    type_match(&Type::Number, &step_type)?;
                }
                bindings.insert(variable.clone(), Type::Number);
                check_statements(bindings.clone(), body, return_type)?;
            }
            ast::Statement::Return(expression) => {
                if let Some(expression) = expression {
                    let actual = check_expression(bindings.clone(), expression)?;
                    match return_type {
                        ReturnType::Fixed(expect) => {
                            type_match(expect, &actual)?;
                        }
                        ReturnType::Infer(types) => {
                            types.push(actual);
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

fn check_expression(
    bindings: HashMap<String, Type>,
    expr: &ast::Expression,
) -> Result<Type, String> {
    Ok(match expr {
        ast::Expression::Literal(literal) => match literal {
            ast::Literal::Number(n) => ConstData::try_from_f64(*n)
                .map(Type::Const)
                .unwrap_or(Type::Number),
            ast::Literal::Bool(b) => Type::Const(ConstData::Bool(*b)),
            ast::Literal::String(s) => Type::Const(ConstData::String(s.clone())),
        },
        ast::Expression::Variable(name) => bindings
            .get(name)
            .ok_or_else(|| format!("Variable not found: {}", name))?
            .clone(),
        ast::Expression::Call {
            function,
            arguments,
        } => {
            let func_type =
                if let ast::Expression::Literal(ast::Literal::String(f)) = function.deref() {
                    match f.as_str() {
                        "__eq" => Type::Function(vec![Type::Any, Type::Any], Box::new(Type::Bool)),
                        "__lt" | "__le" => {
                            Type::Function(vec![Type::Number, Type::Number], Box::new(Type::Bool))
                        }
                        "__add" | "__sub" | "__mul" | "__div" | "__idiv" | "__mod" | "__pow" => {
                            Type::Function(vec![Type::Number, Type::Number], Box::new(Type::Number))
                        }
                        "__neg" => Type::Function(vec![Type::Number], Box::new(Type::Number)),
                        "__len" => Type::Function(
                            vec![Type::Union(vec![
                                Type::String,
                                Type::Table(TypeTable::any()),
                            ])],
                            Box::new(Type::Number),
                        ),
                        _ => check_expression(bindings.clone(), function)?,
                    }
                } else {
                    check_expression(bindings.clone(), function)?
                };

            if let Type::Any = func_type {
                return Ok(Type::Any);
            }

            if let Type::Function(params, return_type) = func_type {
                if params.len() != arguments.len() {
                    return Err("Argument count mismatch".to_string());
                }
                for (param, arg) in params.iter().zip(arguments.iter()) {
                    let actual = check_expression(bindings.clone(), arg)?;
                    type_match(param, &actual)?;
                }
                *return_type
            } else {
                return Err("Not a function".to_string());
            }
        }
        ast::Expression::Index { table, index } => {
            let table_type = check_expression(bindings.clone(), table)?;
            let Type::Table(table_type) = table_type else {
                return Err("Not a table".to_string());
            };
            let index_type = check_expression(bindings.clone(), index)?;
            check_table(&table_type, &index_type)?
        }
        ast::Expression::Fn(function) => {
            let ret_type = check_function(bindings.clone(), function)?;
            Type::Function(
                function.parameters.iter().map(|(_, t)| t.clone()).collect(),
                Box::new(ret_type),
            )
        }
        ast::Expression::Table(vec) => {
            let mut consts = vec![];
            let mut number = vec![];
            let mut string = vec![];
            let mut bool = vec![];
            let mut table = vec![];
            let mut functions = vec![];

            for (key, value) in vec {
                let value_type = check_expression(bindings.clone(), value)?;
                match key {
                    ast::TableKey::Literal(literal) => {
                        consts.push((literal.to_const_data(), value_type))
                    }
                    ast::TableKey::Expression(e) => {
                        let key_type = check_expression(bindings.clone(), e)?;
                        match key_type {
                            Type::Number => number.push(value_type),
                            Type::String => string.push(value_type),
                            Type::Bool => bool.push(value_type),
                            Type::Const(const_data) => consts.push((const_data, value_type)),
                            Type::Table(_) => table.push(value_type),
                            Type::Function(_, _) => functions.push(value_type),
                            t => return Err(format!("Invalid table key type: {}", t)),
                        }
                    }
                }
            }

            Type::Table(TypeTable {
                consts,
                number: Type::from_types(number).map(Box::new),
                string: Type::from_types(string).map(Box::new),
                bool: Type::from_types(bool).map(Box::new),
                table: Type::from_types(table).map(Box::new),
                function: Type::from_types(functions).map(Box::new),
            })
        }
        ast::Expression::LogicalAnd(a, b) => {
            let a = check_expression(bindings.clone(), a)?;
            let b = check_expression(bindings.clone(), b)?;
            Type::from_types(vec![a, b]).unwrap()
        }
        ast::Expression::LogicalOr(a, b) => {
            let a = check_expression(bindings.clone(), a)?;
            let b = check_expression(bindings.clone(), b)?;
            Type::from_types(vec![a, b]).unwrap()
        }
        ast::Expression::LogicalNot(e) => {
            check_expression(bindings.clone(), e)?;
            Type::Bool
        }
    })
}

fn check_table(table_type: &TypeTable, index_type: &Type) -> Result<Type, String> {
    match index_type {
        Type::Number => {
            if let Some(t) = &table_type.number {
                return Ok((**t).clone());
            }
        }
        Type::String => {
            if let Some(t) = &table_type.string {
                return Ok((**t).clone());
            }
        }
        Type::Bool => {
            if let Some(t) = &table_type.bool {
                return Ok((**t).clone());
            }
        }
        Type::Table(_) => {
            if let Some(t) = &table_type.table {
                return Ok((**t).clone());
            }
        }
        Type::Function(_, _) => {
            if let Some(t) = &table_type.function {
                return Ok((**t).clone());
            }
        }
        Type::Const(const_data) => {
            for (cd, ty) in table_type.consts.iter() {
                if cd == const_data {
                    return Ok(ty.clone());
                }
            }
            return check_table(table_type, &const_data.r#type());
        }
        Type::Union(ts) => {
            let ts = ts
                .iter()
                .map(|t| check_table(table_type, t))
                .collect::<Result<Vec<_>, _>>()?;
            return Ok(Type::from_types(ts).unwrap());
        }
        _ => {}
    }
    Err(format!("Invalid index type: {}", index_type))
}

fn type_match(expect: &Type, actual: &Type) -> Result<(), String> {
    if expect.include(actual) {
        Ok(())
    } else {
        Err(format!(
            "Type mismatch, expected {}, got {}",
            expect, actual
        ))
    }
}
