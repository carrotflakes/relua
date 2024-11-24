use std::{fmt::Write, ops::Deref};

use crate::ast::{self, Expression, Literal};

pub fn write_lua(writer: &mut impl Write, defs: &[ast::Definition]) -> std::fmt::Result {
    definitions(writer, defs)
}

fn definitions(writer: &mut impl Write, defs: &[ast::Definition]) -> std::fmt::Result {
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
            ast::Statement::Assignment { target, expr } => {
                match target {
                    ast::LValue::Variable(v) => {
                        writer.write_str(v)?;
                    }
                    ast::LValue::Index(table, index) => {
                        expression(writer, table)?;
                        writer.write_str("[")?;
                        expression(writer, index)?;
                        writer.write_str("]")?;
                    }
                }
                writer.write_str(" = ")?;
                expression(writer, expr)?;
                writer.write_str("\n")?;
            }
            ast::Statement::If {
                condition,
                then,
                else_,
            } => {
                writer.write_str("if ")?;
                expression(writer, condition)?;
                writer.write_str(" then\n")?;
                statements(writer, then)?;
                if !else_.is_empty() {
                    writer.write_str("else\n")?;
                    statements(writer, else_)?;
                }
                writer.write_str("end\n")?;
            }
            ast::Statement::While { condition, body } => {
                writer.write_str("while ")?;
                expression(writer, condition)?;
                writer.write_str(" do\n")?;
                statements(writer, body)?;
                writer.write_str("end\n")?;
            }
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
            let op = if let Expression::Literal(Literal::String(f)) = function.deref() {
                match f.as_str() {
                    "__len" => {
                        writer.write_str("#")?;
                        expression(writer, &arguments[0])?;
                        return Ok(());
                    }
                    "__eq" => Some("=="),
                    "__lt" => Some("<"),
                    "__le" => Some("<="),
                    "__add" => Some("+"),
                    "__sub" => Some("-"),
                    "__mul" => Some("*"),
                    "__div" => Some("/"),
                    "__idiv" => Some("//"),
                    "__mod" => Some("%"),
                    "__pow" => Some("^"),
                    _ => None,
                }
            } else {
                None
            };
            if let Some(op) = op {
                writer.write_str("(")?;
                expression(writer, &arguments[0])?;
                writer.write_str(op)?;
                expression(writer, &arguments[1])?;
                writer.write_str(")")?;
                return Ok(());
            }
            expression(writer, &function)?;
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
        ast::Expression::Fn(function) => {
            writer.write_str("function(")?;
            for (i, param) in function.parameters.iter().enumerate() {
                if i > 0 {
                    writer.write_str(", ")?;
                }
                writer.write_str(&param.0)?;
            }
            writer.write_str(")\n")?;
            statements(writer, &function.body)?;
            writer.write_str("end")?;
        }
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
        ast::Expression::LogicalAnd(a, b) => {
            writer.write_str("(")?;
            expression(writer, a)?;
            writer.write_str(" and ")?;
            expression(writer, b)?;
            writer.write_str(")")?;
        }
        ast::Expression::LogicalOr(a, b) => {
            writer.write_str("(")?;
            expression(writer, a)?;
            writer.write_str(" or ")?;
            expression(writer, b)?;
            writer.write_str(")")?;
        }
        ast::Expression::LogicalNot(e) => {
            writer.write_str("not ")?;
            expression(writer, e)?;
        }
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
