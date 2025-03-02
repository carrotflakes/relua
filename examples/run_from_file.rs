use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        return;
    }

    let file_path = &args[1];
    let src = fs::read_to_string(file_path).expect("Unable to read file");

    let src = relua::compile(&src).unwrap();

    println!("{}", src);

    let lua = mlua::Lua::new();
    lua.load(&src).exec().unwrap();
}
