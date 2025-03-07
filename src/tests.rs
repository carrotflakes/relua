use crate::{default_bindings, front::parser, lua::write_lua, type_check::Context};

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
        r#"
for i = 1, 10 {
    print(i)
}
for i = 1, 10, 2 {
    print(i)
}
"#,
        r#"
for k, v in pairs<{1:1,2:2,3:3}>({1, 2, 3}) {
    print(k, v)
}
"#,
        r#"
let a: num, b: num = 1, 2
b, a = a, b
"#,
        r#"
fn f(a: num, b: str) -> (num, str) {
    return a, b
}
let a: num, b: str = f(1, "2")
let a: num, b: bool = f(1, "2"), true
"#,
        r#"
type User = {type: "user", name: str}
let a: User = {type: "user", name: "carrotflakes"}
"#,
        r#"
fn f<T>(a: T) -> T {
    return a
}
let a: num = f<num>(1)
let a: str = f<str>("a")
"#,
        r#"
fn f<T>(a: T) -> {get: () -> T} {
    return {
        get: fn() -> T {
            return a
        }
    }
}
let a = f<num>(1)
print(a.get())
"#,
        r#"
type T = num
fn f<T>(a: T) -> T {
    return a
}
f<num>(1)
f("1")
"#,
    ];
    for src in &srcs {
        let prog = parser::program(src).unwrap();
        let res = Context::from_symbol_table(default_bindings())
            .check_program(&prog)
            .map_err(|es| crate::format_errors(es, src));
        gilder::assert_golden!(format!("{:?}", res));
    }
    for src in &srcs {
        let prog = parser::program(src).unwrap();
        let mut res = String::new();
        write_lua(&mut res, &prog).unwrap();
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
        r#"let f: (num, str) -> (num, str) = fn(a: num) {return 1}"#,
        r#"let f: (num) -> (num) = fn(a: num, b: str) {return 1, "2"}"#,
        r#"let a: {type: "user", name: str} = {type: "user", name: "carrotflakes"}"#,
        r#"(fn<T>(a: T) -> T {return a})<num>("a")"#,
        r#"let x: num | () = 1 let y: num = x"#,
        r#"let x: num | () = 1 if x { let y: num = x }"#,
        r#"let x: num | () = 1 if !x { let y: num = x }"#,
        r#"let x: num | str = 1 if type(x) == "number" { let y: num = x }"#,
        r#"let x: num | str = 1 if type(x) == "number" { let y: str = x }"#,
        r#"let x: {type: "a", a: 1} | {type: "b", b: 2} = {type:"a", a: 1} if x.type == "a" {let y: num = x.a}"#,
        r#"let x: {type: "a", a: 1} | {type: "b", b: 2} = {type:"a", a: 1} if x.type == "a" {let y: num = x.b}"#,
        r#"let x: num | () = 1 x = 2 let y: num = x"#,
        r#"let x: num | () = 1 if !x {x=2} let y: num = x"#,
        r#"let x: num = "1" as any as num"#,
        r#"let x: num | () = 1 if x {fn() -> () {let y: num = x}}"#,
        r#"let x: num | () = 1 fn() -> () {if x {let y: num = x}}"#,
    ];
    for src in &srcs {
        let prog = parser::program(src).unwrap();
        let res = Context::from_symbol_table(default_bindings())
            .check_program(&prog)
            .map_err(|es| crate::format_errors(es, src));
        gilder::assert_golden!(format!("{:?}", res));
    }
}
