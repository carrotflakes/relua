use std::collections::HashMap;

use crate::ast;
use crate::r#type::Type;

pub fn check_definitions(defs: &[ast::Definition]) -> Result<(), String> {
    let mut bindings: HashMap<String, Type> = vec![
        ("__eq", vec![Type::Number, Type::Number], Type::Bool),
        ("__add", vec![Type::Number, Type::Number], Type::Number),
        ("__mul", vec![Type::Number, Type::Number], Type::Number),
    ]
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
                type_match(&var.type_, &actual)?;
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
                type_match(&variable.type_, &actual)?;
                bindings.insert(variable.name.clone(), variable.type_.clone());
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
            ast::Literal::Number(_) => Type::Number,
            ast::Literal::Bool(_) => Type::Bool,
            ast::Literal::String(_) => Type::String,
        },
        ast::Expression::Variable(name) => bindings.get(name).ok_or("Variable not found")?.clone(),
        ast::Expression::Call {
            function,
            arguments,
        } => {
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
        ast::Expression::Index { array, index } => {
            if let Type::Array(t) = check_expression(bindings.clone(), array)? {
                type_match(&Type::Number, &check_expression(bindings.clone(), index)?)?;
                *t
            } else {
                return Err("Not an array".to_string());
            }
        }
        ast::Expression::Tuple(vec) => {
            let types: Vec<_> = vec
                .iter()
                .map(|expr| check_expression(bindings.clone(), expr))
                .collect::<Result<_, _>>()?;
            Type::Tuple(types)
        }
        ast::Expression::Fn(function) => {
            check_function(bindings.clone(), function)?;
            Type::Function(
                function.parameters.iter().map(|(_, t)| t.clone()).collect(),
                Box::new(function.return_type.clone()),
            )
        }
        ast::Expression::Table(vec) => todo!(),
        ast::Expression::LogicalAnd(a, b) => todo!(),
        ast::Expression::LogicalOr(a, b) => todo!(),
        ast::Expression::LogicalNot(e) => todo!(),
    })
}

fn type_match(expect: &Type, actual: &Type) -> Result<(), String> {
    if expect.include(actual) {
        Ok(())
    } else {
        Err(format!(
            "Type mismatch, expected {:?}, got {:?}",
            expect, actual
        ))
    }
}
