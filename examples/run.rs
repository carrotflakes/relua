fn main() {
    let src = r#"
fn main() {
    let x: num = 5
    let y: num = 10
    let z: num = x + y
    if z > 8 {
        z = z - 8
    }
    print({2: true, z}[1])
}
main()
"#;
    let src = relua::compile(src).unwrap();

    println!("{}", src);

    let lua = mlua::Lua::new();
    lua.load(&src).exec().unwrap();
}
