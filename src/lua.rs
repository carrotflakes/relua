use std::fmt::Write;

use crate::ast::{self, Expression, Literal};

pub fn write_lua(writer: &mut impl Write, stmts: &[ast::SpannedStatement]) -> std::fmt::Result {
    statements(writer, stmts)
}

fn statements(writer: &mut impl Write, stmts: &[ast::SpannedStatement]) -> std::fmt::Result {
    for stmt in stmts {
        match &**stmt {
            ast::Statement::Return(expr) => {
                if expr.is_empty() {
                    writer.write_str("return\n")?;
                } else {
                    writer.write_str("return ")?;
                    for (i, e) in expr.iter().enumerate() {
                        if i > 0 {
                            writer.write_str(", ")?;
                        }
                        expression(writer, e)?;
                    }
                    writer.write_str("\n")?;
                }
            }
            ast::Statement::Break => {
                writer.write_str("break\n")?;
            }
            ast::Statement::Expression(expr) => {
                expression(writer, expr)?;
                writer.write_str(";\n")?;
            }
            ast::Statement::Fn { name, function } => {
                writer.write_str(&format!("local function {}(", &**name))?;
                for (i, param) in function.parameters.iter().enumerate() {
                    if i > 0 {
                        writer.write_str(", ")?;
                    }
                    writer.write_str(&param.0)?;
                }
                writer.write_str(")\n")?;
                statements(writer, &function.body)?;
                writer.write_str("end\n")?;
            }
            ast::Statement::Let(vars, exprs) => {
                writer.write_str(&format!("local "))?;
                for (i, (var, _)) in vars.iter().enumerate() {
                    if i > 0 {
                        writer.write_str(", ")?;
                    }
                    writer.write_str(var)?;
                }
                writer.write_str(" = ")?;
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        writer.write_str(", ")?;
                    }
                    expression(writer, expr)?;
                }
                writer.write_str(";\n")?;
            }
            ast::Statement::Assignment { vars, exprs } => {
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        writer.write_str(", ")?;
                    }
                    match var {
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
                }
                writer.write_str(" = ")?;
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        writer.write_str(", ")?;
                    }
                    expression(writer, expr)?;
                }
                writer.write_str(";\n")?;
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
            ast::Statement::ForNumeric {
                variable,
                start,
                end,
                step,
                body,
            } => {
                writer.write_str("for ")?;
                writer.write_str(variable)?;
                writer.write_str(" = ")?;
                expression(writer, start)?;
                writer.write_str(", ")?;
                expression(writer, end)?;
                if let Some(step) = step {
                    writer.write_str(", ")?;
                    expression(writer, step)?;
                }
                writer.write_str(" do\n")?;
                statements(writer, body)?;
                writer.write_str("end\n")?;
            }
            ast::Statement::ForGeneric {
                variables,
                exprs,
                body,
            } => {
                writer.write_str("for ")?;
                for (i, (var, _)) in variables.iter().enumerate() {
                    if i > 0 {
                        writer.write_str(", ")?;
                    }
                    writer.write_str(var)?;
                }
                writer.write_str(" in ")?;
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        writer.write_str(", ")?;
                    }
                    expression(writer, expr)?;
                }
                writer.write_str(" do\n")?;
                statements(writer, body)?;
                writer.write_str("end\n")?;
            }
            ast::Statement::TypeAlias(_, _) => {}
            ast::Statement::DeclareLet(_) => {}
        }
    }
    Ok(())
}

fn parened_expression(
    writer: &mut impl Write,
    precedence: u8,
    expr: &ast::Expression,
) -> std::fmt::Result {
    let p = expr.precedence() < precedence;
    if p {
        writer.write_str("(")?;
    }
    expression(writer, expr)?;
    if p {
        writer.write_str(")")?;
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
            let op = if let Expression::Literal(Literal::String(f)) = &***function {
                match f.as_str() {
                    "__len" => {
                        writer.write_str("#")?;
                        parened_expression(writer, 7, &arguments[0])?;
                        return Ok(());
                    }
                    "__neg" => {
                        writer.write_str("-")?;
                        parened_expression(writer, 7, &arguments[0])?;
                        return Ok(());
                    }
                    "__eq" => Some(("==", true)),
                    "__ne" => Some(("~=", true)),
                    "__lt" => Some(("<", true)),
                    "__le" => Some(("<=", true)),
                    "__gt" => Some((">", true)),
                    "__ge" => Some((">=", true)),
                    "__add" => Some(("+", true)),
                    "__sub" => Some(("-", true)),
                    "__mul" => Some(("*", true)),
                    "__div" => Some(("/", true)),
                    "__idiv" => Some(("//", true)),
                    "__mod" => Some(("%", true)),
                    "__pow" => Some(("^", false)),
                    _ => None,
                }
            } else {
                None
            };
            let p = expr.precedence();
            if let Some((op, left)) = op {
                if left {
                    parened_expression(writer, p, &arguments[0])?;
                    writer.write_str(op)?;
                    parened_expression(writer, p + 1, &arguments[1])?;
                } else {
                    parened_expression(writer, p + 1, &arguments[0])?;
                    writer.write_str(op)?;
                    parened_expression(writer, p, &arguments[1])?;
                }
                return Ok(());
            }
            parened_expression(writer, 10, &function)?;
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
            parened_expression(writer, 10, table)?;
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
            parened_expression(writer, 2, a)?;
            writer.write_str(" and ")?;
            parened_expression(writer, 2, b)?;
        }
        ast::Expression::LogicalOr(a, b) => {
            parened_expression(writer, 1, a)?;
            writer.write_str(" or ")?;
            parened_expression(writer, 1, b)?;
        }
        ast::Expression::LogicalNot(e) => {
            writer.write_str("not ")?;
            parened_expression(writer, 7, e)?;
        }
        ast::Expression::TypeResolve(e, _) => {
            expression(writer, e)?;
        }
        Expression::As(e, _) => {
            expression(writer, e)?;
        }
        Expression::Nil => {
            writer.write_str("nil")?;
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

impl Expression {
    pub fn precedence(&self) -> u8 {
        match self {
            Expression::LogicalOr(_, _) => 1,
            Expression::LogicalAnd(_, _) => 2,
            Expression::Call {
                function,
                arguments: _,
            } => {
                if let Expression::Literal(Literal::String(op)) = &***function {
                    match op.as_str() {
                        "__eq" => 3,
                        "__ne" => 3,
                        "__lt" => 3,
                        "__le" => 3,
                        "__gt" => 3,
                        "__ge" => 3,
                        "__add" => 5,
                        "__sub" => 5,
                        "__mul" => 6,
                        "__div" => 6,
                        "__idiv" => 6,
                        "__mod" => 6,
                        "__len" => 7,
                        "__neg" => 7,
                        "__pow" => 8,
                        _ => panic!("unknown operator: {}", op),
                    }
                } else {
                    10
                }
            }
            Expression::LogicalNot(_) => 7,
            Expression::Table(_) => 9,
            Expression::Index { .. } => 10, //?
            Expression::Fn(_) => 10,
            Expression::Variable(_) => 10,
            Expression::Literal(_) => 10,
            Expression::Nil => 10,

            Expression::As(e, _) => e.precedence(),
            Expression::TypeResolve(e, _) => e.precedence(),
        }
    }
}

#[test]
fn test() {
    use crate::front::parser;

    fn test_case(src: &str) {
        let prog = parser::program(src).unwrap();
        let mut res = String::new();
        write_lua(&mut res, &prog).unwrap();
        gilder::assert_golden!(res);
    }

    test_case(r#"let x = 1 * 2 + (3 - 4)"#);
    test_case(r#"let x = 1 * (2 + -3) ^ (4 + 5)"#);
    test_case(r#"let x = 1 ^ 2 ^ 3"#);
    test_case(r#"let x = 1 ^ (2 ^ 3)"#);
    test_case(r#"let x = (1 ^ 2) ^ 3"#);
    test_case(r#"let x = (1 + 2) > 3 - 4"#);
    test_case(r#"let x = !(1 + (2 + 3)(4)[5](6))"#);
    test_case(r#"let x = {} || () || {}"#);
}
