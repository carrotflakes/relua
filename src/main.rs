use std::io::Read;

use relua::compile;

fn main() {
    let mut src = String::new();
    std::io::stdin().read_to_string(&mut src).unwrap();
    match compile(&src) {
        Ok(output) => {
            println!("{}", output);
        }
        Err(err) => {
            eprintln!("{}", err);
        }
    }
}
