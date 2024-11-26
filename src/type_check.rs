use std::collections::HashMap;
use std::ops::Deref;

use crate::ast;
use crate::r#type::{ConstData, Type, TypeTable};

enum ReturnType {
    Fixed(Vec<Type>),
    Infer(Vec<Vec<Type>>),
}

pub fn check_program(
    bindings: HashMap<String, Type>,
    stmts: &[ast::Statement],
) -> Result<Vec<Type>, String> {
    let mut return_types = ReturnType::Infer(vec![]);

    check_statements(bindings, stmts, &mut return_types)?;

    Ok(return_types.into_types())
}

fn check_function(
    mut bindings: HashMap<String, Type>,
    function: &ast::Function,
) -> Result<Vec<Type>, String> {
    for (name, type_) in &function.parameters {
        bindings.insert(name.clone(), type_.clone());
    }

    let mut ret_types = match &function.return_types {
        Some(t) => ReturnType::Fixed(t.clone()),
        None => ReturnType::Infer(vec![]),
    };

    check_statements(bindings, &function.body, &mut ret_types)?;

    Ok(ret_types.into_types())
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
                        function.return_types.clone().unwrap_or_else(|| vec![]), // unknown?
                    ),
                );

                let ret_types = check_function(bindings.clone(), function)?;
                bindings.insert(
                    name.clone(),
                    Type::Function(
                        function.parameters.iter().map(|(_, t)| t.clone()).collect(),
                        ret_types,
                    ),
                );
            }
            ast::Statement::Let(vars, exprs) => {
                if exprs.len() == 1 {
                    let actual = check_expression(bindings.clone(), &exprs[0])?;
                    for i in 0..vars.len() {
                        let var = &vars[i];
                        let actual = actual.get(i).unwrap_or(&Type::Nil).clone();
                        if let Some(expect) = &var.1 {
                            type_match(expect, &actual)?;
                            bindings.insert(var.0.clone(), expect.clone());
                        } else {
                            bindings.insert(var.0.clone(), actual);
                        }
                    }
                } else {
                    for i in 0..vars.len() {
                        let var = &vars[i];
                        let actual = check_expression(bindings.clone(), &exprs[i])?;
                        if let Some(expect) = &var.1 {
                            type_match(expect, &actual[0])?;
                            bindings.insert(var.0.clone(), expect.clone());
                        } else {
                            bindings.insert(var.0.clone(), actual[0].clone());
                        }
                    }
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
                            let table_type = check_expression(bindings.clone(), table)?[0].clone();
                            let Type::Table(table_type) = table_type else {
                                return Err("Not a table".to_string());
                            };
                            let index_type = check_expression(bindings.clone(), index)?[0].clone();
                            check_table(&table_type, &index_type)?
                        }
                    };
                    var_types.push(var_type);
                }
                if exprs.len() == 1 {
                    let actual = check_expression(bindings.clone(), &exprs[0])?;
                    for i in 0..var_types.len() {
                        type_match(&var_types[i], actual.get(i).unwrap_or(&Type::Nil))?;
                    }
                } else {
                    for i in 0..var_types.len().max(exprs.len()) {
                        let actual = if i < exprs.len() {
                            check_expression(bindings.clone(), &exprs[i])?[0].clone()
                        } else {
                            Type::Nil
                        };
                        type_match(&var_types[i], &actual)?;
                    }
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
                let start_type = check_expression(bindings.clone(), start)?[0].clone();
                type_match(&Type::Number, &start_type)?;
                let end_type = check_expression(bindings.clone(), end)?[0].clone();
                type_match(&Type::Number, &end_type)?;
                if let Some(step) = step {
                    let step_type = check_expression(bindings.clone(), step)?[0].clone();
                    type_match(&Type::Number, &step_type)?;
                }
                bindings.insert(variable.clone(), Type::Number);
                check_statements(bindings.clone(), body, return_type)?;
            }
            ast::Statement::Return(exprs) => {
                if exprs.len() == 1 {
                    let actual = check_expression(bindings.clone(), &exprs[0])?;
                    match return_type {
                        ReturnType::Fixed(expect) => {
                            for i in 0..expect.len() {
                                type_match(&expect[i], actual.get(i).unwrap_or(&Type::Nil))?;
                            }
                        }
                        ReturnType::Infer(typess) => {
                            typess.push(actual);
                        }
                    }
                } else {
                    let mut actuals = vec![];
                    for expr in exprs {
                        let actual = check_expression(bindings.clone(), expr)?;
                        actuals.push(actual[0].clone());
                    }
                    match return_type {
                        ReturnType::Fixed(expect) => {
                            for i in 0..expect.len() {
                                type_match(&expect[i], actuals.get(i).unwrap_or(&Type::Nil))?;
                            }
                        }
                        ReturnType::Infer(typess) => {
                            typess.push(actuals);
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
) -> Result<Vec<Type>, String> {
    Ok(match expr {
        ast::Expression::Literal(literal) => {
            let val = match literal {
                ast::Literal::Number(n) => ConstData::try_from_f64(*n)
                    .map(Type::Const)
                    .unwrap_or(Type::Number),
                ast::Literal::Bool(b) => Type::Const(ConstData::Bool(*b)),
                ast::Literal::String(s) => Type::Const(ConstData::String(s.clone())),
            };
            vec![val]
        }
        ast::Expression::Variable(name) => {
            let val = bindings
                .get(name)
                .ok_or_else(|| format!("Variable not found: {}", name))?
                .clone();
            vec![val]
        }
        ast::Expression::Call {
            function,
            arguments,
        } => {
            let func_type =
                if let ast::Expression::Literal(ast::Literal::String(f)) = function.deref() {
                    match f.as_str() {
                        "__eq" => Type::Function(vec![Type::Any, Type::Any], vec![Type::Bool]),
                        "__lt" | "__le" => {
                            Type::Function(vec![Type::Number, Type::Number], vec![Type::Bool])
                        }
                        "__add" | "__sub" | "__mul" | "__div" | "__idiv" | "__mod" | "__pow" => {
                            Type::Function(vec![Type::Number, Type::Number], vec![Type::Number])
                        }
                        "__neg" => Type::Function(vec![Type::Number], vec![Type::Number]),
                        "__len" => Type::Function(
                            vec![Type::Union(vec![
                                Type::String,
                                Type::Table(TypeTable::any()),
                            ])],
                            vec![Type::Number],
                        ),
                        _ => check_expression(bindings.clone(), function)?[0].clone(),
                    }
                } else {
                    check_expression(bindings.clone(), function)?[0].clone()
                };

            if let Type::Any = func_type {
                for arg in arguments {
                    check_expression(bindings.clone(), arg)?;
                }
                return Ok(vec![Type::Any]); // TODO
            }

            if let Type::Function(params, return_types) = func_type {
                if arguments.len() == 1 {
                    let actual = check_expression(bindings.clone(), &arguments[0])?;
                    for i in 0..params.len() {
                        type_match(&params[i], actual.get(i).unwrap_or(&Type::Nil))?;
                    }
                } else {
                    for i in 0..params.len() {
                        let actual = if let Some(arg) = arguments.get(i) {
                            check_expression(bindings.clone(), arg)?[0].clone()
                        } else {
                            Type::Nil
                        };
                        type_match(&params[i], &actual)?;
                    }
                }
                return_types
            } else {
                return Err("Not a function".to_string());
            }
        }
        ast::Expression::Index { table, index } => {
            let table_types = check_expression(bindings.clone(), table)?;
            let table_type = &table_types[0];
            let Type::Table(table_type) = table_type else {
                return Err("Not a table".to_string());
            };
            let index_types = check_expression(bindings.clone(), index)?;
            let index_type = &index_types[0];
            vec![check_table(&table_type, &index_type)?]
        }
        ast::Expression::Fn(function) => {
            let ret_types = check_function(bindings.clone(), function)?;
            vec![Type::Function(
                function.parameters.iter().map(|(_, t)| t.clone()).collect(),
                ret_types,
            )]
        }
        ast::Expression::Table(vec) => {
            let mut consts = vec![];
            let mut number = vec![];
            let mut string = vec![];
            let mut bool = vec![];
            let mut table = vec![];
            let mut functions = vec![];

            for (key, value) in vec {
                let value_types = check_expression(bindings.clone(), value)?;
                let value_type = value_types[0].clone();
                match key {
                    ast::TableKey::Literal(literal) => {
                        consts.push((literal.to_const_data(), value_type))
                    }
                    ast::TableKey::Expression(e) => {
                        let key_types = check_expression(bindings.clone(), e)?;
                        let key_type = key_types[0].clone();
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

            vec![Type::Table(TypeTable {
                consts,
                number: Type::from_types(number).map(Box::new),
                string: Type::from_types(string).map(Box::new),
                bool: Type::from_types(bool).map(Box::new),
                table: Type::from_types(table).map(Box::new),
                function: Type::from_types(functions).map(Box::new),
            })]
        }
        ast::Expression::LogicalAnd(a, b) => {
            let a = check_expression(bindings.clone(), a)?;
            let b = check_expression(bindings.clone(), b)?;
            vec![Type::from_types(vec![a[0].clone(), b[0].clone()]).unwrap()]
        }
        ast::Expression::LogicalOr(a, b) => {
            let a = check_expression(bindings.clone(), a)?;
            let b = check_expression(bindings.clone(), b)?;
            vec![Type::from_types(vec![a[0].clone(), b[0].clone()]).unwrap()]
        }
        ast::Expression::LogicalNot(e) => {
            check_expression(bindings.clone(), e)?;
            vec![Type::Bool]
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

impl ReturnType {
    fn into_types(self) -> Vec<Type> {
        match self {
            ReturnType::Fixed(ts) => ts,
            ReturnType::Infer(typess) => {
                let mut types = vec![];
                for i in 0..typess.iter().map(|ts| ts.len()).max().unwrap_or(0) {
                    let ts = typess
                        .iter()
                        .map(|types| types.get(i).cloned().unwrap_or(Type::Nil))
                        .collect();
                    types.push(Type::from_types(ts).unwrap());
                }
                types
            }
        }
    }
}
