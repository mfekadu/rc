#[cfg(test)]
mod testing;
mod tokenizer;
mod parser;
mod uniquify;

use tokenizer::*;
use parser::r1::*;
use uniquify::*;

use std::collections::HashMap;

/// a compiler for the R1 langauge
fn main() {
    println!("Hello, RC!");

    let input = "(program () (let ([x 2])(+ x (let {{x (+ 3 x)}} x))".to_string();

    println!("{:?}", tokenizer(input.clone()));

    println!("{:?}", parse(tokenizer(input.clone())).unwrap());

    // TODO: prevent panic! when not Ok
    match parse(tokenizer(input.clone())).unwrap() {
        Program{ info: _, exp } => {
            uniquify(exp, &mut HashMap::new());
        }
    }
}
