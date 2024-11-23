pub mod ast;
pub mod front;
pub mod lua;
pub mod r#type;
pub mod type_check;

#[test]
fn test() {
    let defs = front::parser::program(
        r#"
fn main(a: num) -> num {
    let b: num = 1
    return a * 2 + b
}
        "#,
    )
    .unwrap();
    type_check::check_definitions(&defs).unwrap();
}
