use std::io::Read;

use relua::compile;

fn main() {
    let mut src = String::new();
    std::io::stdin().read_to_string(&mut src).unwrap();

    let output = compile(&src).unwrap();

    println!("{}", output);
}
