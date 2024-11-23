pub mod ast;
pub mod front;
pub mod lua;
pub mod r#type;
pub mod type_check;

pub fn compile(src: &str) -> Result<String, String> {
    let prog = front::parser::program(src).map_err(|e| e.to_string())?;

    type_check::check_definitions(&prog)?;

    let mut res = String::new();
    lua::definitions(&mut res, &prog).map_err(|e| e.to_string())?;
    Ok(res)
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
    ];
    for src in &srcs {
        let defs = front::parser::program(src).unwrap();
        assert_eq!(type_check::check_definitions(&defs), Ok(()));
    }
    for src in &srcs {
        let defs = front::parser::program(src).unwrap();
        let mut res = String::new();
        lua::definitions(&mut res, &defs).unwrap();
        gilder::assert_golden!(res);
    }
}
