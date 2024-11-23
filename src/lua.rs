use std::fmt::Write;

use crate::ast;

pub fn definitions(writer: &mut impl Write, defs: &[ast::Definition]) -> std::fmt::Result {
    for def in defs {
        match def {
            ast::Definition::Function { name, function } => {
                writer.write_str(&format!("function {}(", name))?;
                for (i, param) in function.parameters.iter().enumerate() {
                    if i > 0 {
                        writer.write_str(", ")?;
                    }
                    writer.write_str(&format!("{}", &param.0))?;
                }
                writer.write_str(")\n")?;
                statements(writer, &function.body)?;
                writer.write_str("end\n")?;
            }
            ast::Definition::Variable(variable) => {
                writer.write_str(&format!("local {} = ", variable.name))?;
                expression(writer, &variable.expr)?;
                writer.write_str("\n")?;
            }
            ast::Definition::Expression(e) => {
                expression(writer, e)?;
                writer.write_str("\n")?;
            }
        }
    }
    Ok(())
}

fn statements(writer: &mut impl Write, stmts: &[ast::Statement]) -> std::fmt::Result {
    for stmt in stmts {
        match stmt {
            ast::Statement::Return(expr) => {
                if let Some(expr) = expr {
                    writer.write_str("return ")?;
                    expression(writer, expr)?;
                    writer.write_str("\n")?;
                } else {
                    writer.write_str("return\n")?;
                }
            }
            ast::Statement::Expression(expr) => {
                expression(writer, expr)?;
                writer.write_str("\n")?;
            }
            ast::Statement::Let(variable) => {
                writer.write_str(&format!("local {} = ", variable.name))?;
                expression(writer, &variable.expr)?;
                writer.write_str("\n")?;
            }
            ast::Statement::Assignment { target, e } => todo!(),
            ast::Statement::If {
                condition,
                then,
                else_,
            } => todo!(),
            ast::Statement::While { condition, body } => todo!(),
        }
    }
    Ok(())
}

fn expression(writer: &mut impl Write, expr: &ast::Expression) -> std::fmt::Result {
    match expr {
        ast::Expression::Literal(l) => {
            literal(writer, l)?;
        }
        ast::Expression::Variable(v) => {
            writer.write_str(&format!("{}", v))?;
        }
        ast::Expression::Call {
            function,
            arguments,
        } => {
            match function.as_str() {
                "__eq" => {
                    writer.write_str("(")?;
                    expression(writer, &arguments[0])?;
                    writer.write_str("==")?;
                    expression(writer, &arguments[1])?;
                    writer.write_str(")")?;
                    return Ok(());
                }
                "__add" => {
                    writer.write_str("(")?;
                    expression(writer, &arguments[0])?;
                    writer.write_str("+")?;
                    expression(writer, &arguments[1])?;
                    writer.write_str(")")?;
                    return Ok(());
                }
                "__mul" => {
                    writer.write_str("(")?;
                    expression(writer, &arguments[0])?;
                    writer.write_str("*")?;
                    expression(writer, &arguments[1])?;
                    writer.write_str(")")?;
                    return Ok(());
                }
                _ => {}
            }
            writer.write_str(function)?;
            writer.write_str("(")?;
            for (i, arg) in arguments.iter().enumerate() {
                if i > 0 {
                    writer.write_str(", ")?;
                }
                expression(writer, arg)?;
            }
            writer.write_str(")")?;
        }
        ast::Expression::Index { table, index } => {
            writer.write_str("(")?;
            expression(writer, table)?;
            writer.write_str(")")?;
            writer.write_str("[")?;
            expression(writer, index)?;
            writer.write_str("]")?;
        }
        ast::Expression::Fn(function) => todo!(),
        ast::Expression::Table(vec) => {
            writer.write_str("{")?;
            for (i, (key, value)) in vec.iter().enumerate() {
                if i > 0 {
                    writer.write_str(", ")?;
                }
                match key {
                    ast::TableKey::Literal(l) => {
                        if let Some(name) = literal_to_name(l) {
                            writer.write_str(&name)?;
                        } else {
                            writer.write_str("[")?;
                            literal(writer, l)?;
                            writer.write_str("]")?;
                        }
                    }
                    ast::TableKey::Expression(e) => {
                        writer.write_str("[")?;
                        expression(writer, e)?;
                        writer.write_str("]")?;
                    }
                }
                writer.write_str(" = ")?;
                expression(writer, value)?;
            }
            writer.write_str("}")?;
        }
        ast::Expression::LogicalAnd(a, b) => todo!(),
        ast::Expression::LogicalOr(a, b) => todo!(),
        ast::Expression::LogicalNot(e) => todo!(),
    }
    Ok(())
}

fn literal(writer: &mut impl Write, literal: &ast::Literal) -> std::fmt::Result {
    match literal {
        ast::Literal::Number(n) => {
            writer.write_str(&format!("{}", n))?;
        }
        ast::Literal::String(s) => {
            writer.write_str(&format!("{:?}", s))?;
        }
        ast::Literal::Bool(b) => {
            writer.write_str(&format!("{}", b))?;
        }
    }
    Ok(())
}

fn literal_to_name(literal: &ast::Literal) -> Option<String> {
    match literal {
        ast::Literal::String(s) => {
            if s.chars().next().map(|c| c.is_ascii_digit()) != Some(false) {
                return None;
            }
            if s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') {
                return Some(s.clone());
            }
            None
        }
        _ => None,
    }
}
