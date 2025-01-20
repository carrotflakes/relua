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

    type_check::Context::from_symbol_table(default_bindings()).check_program(&prog, src)?;

    let mut res = String::new();
    lua::write_lua(&mut res, &prog).map_err(|e| e.to_string())?;
    Ok(res)
}

pub fn compile_with_bindings(
    bindings: HashMap<String, r#type::Type>,
    src: &str,
) -> Result<String, String> {
    let prog = front::parser::program(src).map_err(|e| e.to_string())?;

    type_check::Context::from_symbol_table(bindings).check_program(&prog, src)?;

    let mut res = String::new();
    lua::write_lua(&mut res, &prog).map_err(|e| e.to_string())?;
    Ok(res)
}

pub fn default_bindings() -> HashMap<String, r#type::Type> {
    use r#type::{ConstData, Type, TypeTable};

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
        (
            "pairs",
            Type::Function(vec![Type::Table(TypeTable::any())], vec![]),
        ),
        (
            "ipairs",
            Type::Function(vec![Type::Table(TypeTable::any())], vec![]),
        ),
        (
            "math",
            Type::Table(TypeTable {
                consts: vec![],
                number: None,
                string: Some(Box::new(Type::Any)),
                bool: None,
                table: None,
                function: None,
            }),
        ),
        (
            "table",
            Type::Table(TypeTable {
                consts: vec![],
                number: None,
                string: Some(Box::new(Type::Any)),
                bool: None,
                table: None,
                function: None,
            }),
        ),
        (
            "string",
            Type::Table(TypeTable {
                consts: vec![],
                number: None,
                string: Some(Box::new(Type::Any)),
                bool: None,
                table: None,
                function: None,
            }),
        ),
        (
            "io",
            Type::Table(TypeTable {
                consts: vec![],
                number: None,
                string: Some(Box::new(Type::Any)),
                bool: None,
                table: None,
                function: None,
            }),
        ),
        (
            "os",
            Type::Table(TypeTable {
                consts: vec![],
                number: None,
                string: Some(Box::new(Type::Any)),
                bool: None,
                table: None,
                function: None,
            }),
        ),
        (
            "debug",
            Type::Table(TypeTable {
                consts: vec![],
                number: None,
                string: Some(Box::new(Type::Any)),
                bool: None,
                table: None,
                function: None,
            }),
        ),
    ]
    .into_iter()
    .map(|(name, value)| (name.to_owned(), value))
    .collect();
    bindings
}
