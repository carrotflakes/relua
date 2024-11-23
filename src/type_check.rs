use std::collections::HashMap;
use std::ops::Deref;

use crate::ast;
use crate::r#type::{ConstData, Type, TypeTable};

pub fn check_definitions(defs: &[ast::Definition]) -> Result<(), String> {
    let mut bindings: HashMap<String, Type> = vec![("print", vec![Type::Unknown], Type::Nil)]
        .into_iter()
        .map(|(name, params, ret)| (name.to_owned(), Type::Function(params, Box::new(ret))))
        .collect();

    for def in defs.iter() {
        match def {
            ast::Definition::Function { name, function } => {
                check_function(bindings.clone(), function)?;
                bindings.insert(
                    name.clone(),
                    Type::Function(
                        function.parameters.iter().map(|(_, t)| t.clone()).collect(),
                        Box::new(function.return_type.clone()),
                    ),
                );
            }
            ast::Definition::Variable(var) => {
                let actual = check_expression(bindings.clone(), &var.expr)?;
                if let Some(expect) = &var.type_ {
                    type_match(expect, &actual)?;
                    bindings.insert(var.name.clone(), expect.clone());
                } else {
                    bindings.insert(var.name.clone(), actual);
                }
            }
            ast::Definition::Expression(expr) => {
                check_expression(bindings.clone(), expr)?;
            }
        }
    }
    Ok(())
}

fn check_function(
    mut bindings: HashMap<String, Type>,
    function: &ast::Function,
) -> Result<(), String> {
    for (name, type_) in &function.parameters {
        bindings.insert(name.clone(), type_.clone());
    }
    for stmt in &function.body {
        match stmt {
            ast::Statement::Expression(expression) => {
                check_expression(bindings.clone(), expression)?;
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
            ast::Statement::Assignment { target, e } => {
                let var_type = bindings.get(target).ok_or("Variable not found")?.clone();
                let actual = check_expression(bindings.clone(), e)?;
                type_match(&var_type, &actual)?;
            }
            ast::Statement::If {
                condition,
                then,
                else_,
            } => todo!(),
            ast::Statement::While { condition, body } => todo!(),
            ast::Statement::Return(expression) => {
                if let Some(expression) = expression {
                    let actual = check_expression(bindings.clone(), expression)?;
                    type_match(&function.return_type, &actual)?;
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
        ast::Expression::Variable(name) => bindings.get(name).ok_or("Variable not found")?.clone(),
        ast::Expression::Call {
            function,
            arguments,
        } => {
            match function.as_str() {
                "__eq" => {
                    if arguments.len() != 2 {
                        return Err("Argument count mismatch".to_string());
                    }
                    return Ok(Type::Bool);
                }
                "__add" | "__mul" => {
                    if arguments.len() != 2 {
                        return Err("Argument count mismatch".to_string());
                    }
                    for arg in arguments {
                        type_match(&Type::Number, &check_expression(bindings.clone(), arg)?)?;
                    }
                    return Ok(Type::Number);
                }
                _ => {}
            }
            let func_type = bindings.get(function).ok_or("Function not found")?.clone();
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
            check_function(bindings.clone(), function)?;
            Type::Function(
                function.parameters.iter().map(|(_, t)| t.clone()).collect(),
                Box::new(function.return_type.clone()),
            )
        }
        ast::Expression::Table(vec) => {
            let mut consts = vec![];
            let mut number = vec![];
            let mut string = vec![];
            let mut bool = vec![];

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
            })
        }
        ast::Expression::LogicalAnd(a, b) => todo!(),
        ast::Expression::LogicalOr(a, b) => todo!(),
        ast::Expression::LogicalNot(e) => todo!(),
    })
}

fn check_table(table_type: &TypeTable, index_type: &Type) -> Result<Type, String> {
    match index_type {
        Type::Number => table_type
            .number
            .as_ref()
            .map(|t| (**t).clone())
            .ok_or_else(|| "Invalid index".to_string()),
        Type::String => table_type
            .string
            .as_ref()
            .map(|t| (**t).clone())
            .ok_or_else(|| "Invalid index".to_string()),
        Type::Bool => table_type
            .bool
            .as_ref()
            .map(|t| (**t).clone())
            .ok_or_else(|| "Invalid index".to_string()),
        Type::Const(const_data) => {
            for (cd, ty) in table_type.consts.iter() {
                if cd == const_data {
                    return Ok(ty.clone());
                }
            }
            check_table(table_type, &const_data.r#type())
        }
        Type::Union(ts) => {
            let ts = ts
                .iter()
                .map(|t| check_table(table_type, t))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Type::Union(ts).normalize())
        }
        _ => Err(format!("Invalid index type: {}", index_type))?,
    }
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
