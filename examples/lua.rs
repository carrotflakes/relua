fn main() {
    let src = r#"
local function f()
    return 1, 2
end
local function g(a, b, c)
    return a + 1, b + 1, c + 1
end
local a, b, c = 0, 0
a, b, c = f(), 3
print(a, b, c)
print(g(f(1, 2), 3, 4))

"#;
    let lua = mlua::Lua::new();
    lua.load(src).exec().unwrap();
}
