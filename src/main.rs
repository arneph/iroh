use std::fs;

mod ast;
mod ir;
mod ir_builder;
mod ir_interpreter;
mod parse;
mod scan;
mod tokens;
mod types;

use tokens::Token::EndOfCode;

use clap::Parser;

#[derive(Parser)]
struct IrohArgs {
    file_paths: Vec<String>,
    #[clap(long, action, default_value_t = false)]
    print_ir: bool,
}

fn main() {
    // let args: Vec<String> = env::args().skip(1).collect();
    let args = IrohArgs::parse();
    let mut code = "".to_string();
    for file_path in args.file_paths {
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

    let ir_func = match ir_builder::build_func(ast_func) {
        Ok(func) => func,
        Err(error) => {
            println!("failed to build IR: {error}");
            return;
        }
    };
    if args.print_ir {
        print!("{}", ir_func)
    }

    let result = match ir_interpreter::interpret_func(&ir_func, Vec::new()) {
        Ok(v) => {
            if v.len() != 1 {
                println!("main func did not return one result");
            }
            v[0]
        }
        Err(error) => {
            println!("failed to interpret IR: {error}");
            return;
        }
    };
    println!("{result}");
}
