pub mod ast;
pub mod front;
pub mod lua;
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
            Type::Function(vec![Type::Unknown], Box::new(Type::Nil)),
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

#[test]
fn test() {
    let srcs = [
        r#"
fn main(a: num) -> num {
    let b: num = 1
    return a * 2 + b
}
"#,
        r#"
fn main() {
    let b: num = 1 + 2
}
"#,
        r#"
let a: {1, 2} = {1, 2}
"#,
        r#"
let a: num | str = 1 || "a" && 2
"#,
        r#"
let a: {f: (num) -> num} = {
    f: fn(a: num) -> num {
        return a * 2
    }
}"#,
        r#"
fn f() -> () {
    f()
}"#,
        r#"
fn f() {
    len!{1, 2, 3}
}"#,
        r#"
let t: {[table]: num, [fn]: str} = {}
t[{1, 2}] = 3
t[fn() -> () {}] = "a"
"#,
        r#"
{1: "1", [2]: "2", ["3"]: "3"}
"#,
        r#"
let f: (num) -> () = fn(a: num, b: num) -> () {}
let f: (num, num | ()) -> () = fn(a: num) -> () {}
"#,
    ];
    for src in &srcs {
        let prog = front::parser::program(src).unwrap();
        let res = type_check::check_program(default_bindings(), &prog);
        gilder::assert_golden!(format!("{:?}", res));
    }
    for src in &srcs {
        let prog = front::parser::program(src).unwrap();
        let mut res = String::new();
        lua::write_lua(&mut res, &prog).unwrap();
        gilder::assert_golden!(res);
    }

    let srcs = [
        r#"let a: num = "a""#,
        r#"let a: {1, 2} = {1, 3}"#,
        r#"let a: {1, 2} = {1}"#,
        r#"let a: {1, 2} = {1, 2, 3}"#,
        r#"let a: {1, [num]: num} = {1, 2, 3}"#,
        r#"let a: {1, [num]: str} = {1, 2, 3}"#,
        r#"let a: {1, [str]: num} = {1, 2, 3}"#,
        r#"let f: (num, num) -> () = fn(a: num) -> () {}"#,
    ];
    for src in &srcs {
        let prog = front::parser::program(src).unwrap();
        let res = type_check::check_program(default_bindings(), &prog);
        gilder::assert_golden!(format!("{:?}", res));
    }
}
