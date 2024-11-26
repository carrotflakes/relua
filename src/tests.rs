use crate::{default_bindings, front::parser, lua::write_lua, type_check::check_program};

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
for k, v in pairs({1, 2, 3}) {
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
"#
    ];
    for src in &srcs {
        let prog = parser::program(src).unwrap();
        let res = check_program(default_bindings(), &prog);
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
    ];
    for src in &srcs {
        let prog = parser::program(src).unwrap();
        let res = check_program(default_bindings(), &prog);
        gilder::assert_golden!(format!("{:?}", res));
    }
}
