fn main() {
    let src = r#"
fn main() {
    let x: num = 5
    let y: num = 10
    let z: num = x + y
    print(z)
}
main()
"#;
    let src = relua::compile(src).unwrap();

    let lua = mlua::Lua::new();
    lua.load(&src).exec().unwrap();
}
