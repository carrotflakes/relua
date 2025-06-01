pub mod ast;
pub mod front;
pub mod lua;
#[cfg(test)]
mod tests;
pub mod r#type;
pub mod type_check;

use std::collections::HashMap;

pub fn compile(src: &str) -> Result<String, String> {
    let prog = front::parser::program(src).map_err(|e| e.to_string())?;

    type_check::Context::from_symbol_table(default_bindings())
        .check_program(&prog)
        .map_err(|es| format_errors(es, src))?;

    let mut res = String::new();
    lua::write_lua(&mut res, &prog).map_err(|e| e.to_string())?;
    Ok(res)
}

pub fn compile_with_bindings(
    bindings: HashMap<String, r#type::Type>,
    src: &str,
) -> Result<String, String> {
    let prog = front::parser::program(src).map_err(|e| e.to_string())?;

    type_check::Context::from_symbol_table(bindings)
        .check_program(&prog)
        .map_err(|es| format_errors(es, src))?;

    let mut res = String::new();
    lua::write_lua(&mut res, &prog).map_err(|e| e.to_string())?;
    Ok(res)
}

pub fn format_errors(errors: Vec<type_check::Error>, src: &str) -> String {
    errors
        .into_iter()
        .map(|e| format!("{} at {:?}", e.message, row_and_col(src, e.location.start)))
        .collect::<Vec<_>>()
        .join("\n")
}

fn row_and_col(src: &str, pos: usize) -> (usize, usize) {
    let mut row = 1;
    let mut col = 1;
    for (i, c) in src.char_indices() {
        if i == pos {
            break;
        }
        if c == '\n' {
            row += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (row, col)
}

pub fn default_bindings() -> HashMap<String, r#type::Type> {
    use r#type::{ConstData, Type, TypeTable, Variable};

    let bindings: HashMap<String, Type> = vec![
        (
            "type",
            Type::Function(
                vec![Type::Unknown],
                vec![Type::Union(vec![
                    Type::Const(ConstData::String("nil".to_owned())),
                    Type::Const(ConstData::String("boolean".to_owned())),
                    Type::Const(ConstData::String("number".to_owned())),
                    Type::Const(ConstData::String("string".to_owned())),
                    Type::Const(ConstData::String("userdata".to_owned())),
                    Type::Const(ConstData::String("function".to_owned())),
                    Type::Const(ConstData::String("thread".to_owned())),
                    Type::Const(ConstData::String("table".to_owned())),
                ])],
            ),
        ),
        ("print", Type::Function(vec![Type::Unknown], vec![])),
        ("pairs", {
            let t = Variable::from_spanned_str(ast::Spanned::none("T".to_owned()));
            Type::Generic(
                vec![t.clone()],
                Box::new(Type::Function(
                    vec![Type::Variable(t.clone())],
                    vec![
                        Type::Function(
                            vec![Type::Variable(t.clone()), Type::Unknown],
                            vec![Type::Any, Type::Any],
                        ),
                        Type::Variable(t.clone()),
                        Type::Nil,
                    ],
                )),
            )
        }),
        ("ipairs", {
            let t = Variable::from_spanned_str(ast::Spanned::none("T".to_owned()));
            Type::Generic(
                vec![t.clone()],
                Box::new(Type::Function(
                    vec![Type::Variable(t.clone())],
                    vec![
                        Type::Function(
                            vec![Type::Variable(t.clone()), Type::Number],
                            vec![Type::Number, Type::Any],
                        ),
                        Type::Variable(t.clone()),
                        Type::Const(ConstData::try_from_f64(0.0).unwrap()),
                    ],
                )),
            )
        }),
        (
            "math",
            Type::Table(TypeTable {
                consts: vec![],
                number: Box::new(Type::Nil),
                string: Box::new(Type::Any),
                bool: Box::new(Type::Nil),
                table: Box::new(Type::Nil),
                function: Box::new(Type::Nil),
            }),
        ),
        (
            "table",
            Type::Table(TypeTable {
                consts: vec![],
                number: Box::new(Type::Nil),
                string: Box::new(Type::Any),
                bool: Box::new(Type::Nil),
                table: Box::new(Type::Nil),
                function: Box::new(Type::Nil),
            }),
        ),
        (
            "string",
            Type::Table(TypeTable {
                consts: vec![],
                number: Box::new(Type::Nil),
                string: Box::new(Type::Any),
                bool: Box::new(Type::Nil),
                table: Box::new(Type::Nil),
                function: Box::new(Type::Nil),
            }),
        ),
        (
            "io",
            Type::Table(TypeTable {
                consts: vec![],
                number: Box::new(Type::Nil),
                string: Box::new(Type::Any),
                bool: Box::new(Type::Nil),
                table: Box::new(Type::Nil),
                function: Box::new(Type::Nil),
            }),
        ),
        (
            "os",
            Type::Table(TypeTable {
                consts: vec![],
                number: Box::new(Type::Nil),
                string: Box::new(Type::Any),
                bool: Box::new(Type::Nil),
                table: Box::new(Type::Nil),
                function: Box::new(Type::Nil),
            }),
        ),
        (
            "debug",
            Type::Table(TypeTable {
                consts: vec![],
                number: Box::new(Type::Nil),
                string: Box::new(Type::Any),
                bool: Box::new(Type::Nil),
                table: Box::new(Type::Nil),
                function: Box::new(Type::Nil),
            }),
        ),
        (
            "coroutine",
            Type::Table(TypeTable {
                consts: vec![],
                number: Box::new(Type::Nil),
                string: Box::new(Type::Any),
                bool: Box::new(Type::Nil),
                table: Box::new(Type::Nil),
                function: Box::new(Type::Nil),
            }),
        ),
    ]
    .into_iter()
    .map(|(name, value)| (name.to_owned(), value))
    .collect();
    bindings
}
