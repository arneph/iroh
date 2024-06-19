use std::env;
use std::fs;

mod ast;
mod ir;
mod ir_builder;
mod ir_interpreter;
mod parse;
mod scan;
mod tokens;

use tokens::Token::EndOfCode;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    let mut code = "".to_string();
    for file_path in args {
        let contents =
            fs::read_to_string(file_path).expect("Should have been able to read the file");
        code += &contents;
        code += "\n";
    }

    let mut tokens = vec![];
    let mut remaining_code = code.as_str();
    loop {
        let (t, c) = scan::next(&remaining_code);
        tokens.push(t);
        remaining_code = c;
        if tokens.last() == Some(&EndOfCode) {
            break;
        }
    }

    let ast_func = match parse::parse_func(tokens.as_slice()) {
        Some(func) => func,
        None => {
            println!("failed to parse");
            return;
        }
    };

    let ir_func = ir_builder::build_func(ast_func);

    let result = match ir_interpreter::interpret_func(&ir_func) {
        Ok(v) => v,
        Err(error) => {
            println!("failed to interpret: {error}");
            return;
        }
    };
    println!("{result}");
}
