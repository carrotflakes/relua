fn main() {
    let src = r#"
fn fib(n: num) -> num {
    if n <= 1 {
        return n
    }
    return fib(n - 1) + fib(n - 2)
}

fn main() {
    let x: num = 5
    let y: num = 10
    let z: num = x + y
    if z > 8 {
        z = z - 8
    }
    print({2: true, z}[1])

    let i: num = 1
    while i < 10 {
        if i % 2 == 0 {
            print("even")
        } else {
            print(i)
        }
        i = i + 1
    }

    print(fib(10))
}
main()

let a: {num, num} = {1, 2}
"#;
    let src = relua::compile(src).unwrap();

    println!("{}", src);

    let lua = mlua::Lua::new();
    lua.load(&src).exec().unwrap();
}
