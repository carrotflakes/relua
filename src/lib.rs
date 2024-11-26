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

    type_check::check_program(default_bindings(), &prog)?;

    let mut res = String::new();
    lua::write_lua(&mut res, &prog).map_err(|e| e.to_string())?;
    Ok(res)
}

pub fn compile_with_bindings(
    bindings: HashMap<String, r#type::Type>,
    src: &str,
) -> Result<String, String> {
    let prog = front::parser::program(src).map_err(|e| e.to_string())?;

    type_check::check_program(bindings, &prog)?;

    let mut res = String::new();
    lua::write_lua(&mut res, &prog).map_err(|e| e.to_string())?;
    Ok(res)
}

pub fn default_bindings() -> HashMap<String, r#type::Type> {
    use r#type::{Type, TypeTable};

    let bindings: HashMap<String, Type> = vec![
        (
            "print",
            Type::Function(vec![Type::Unknown], vec![]),
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
