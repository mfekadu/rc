#[cfg(test)]
mod testing;
mod tokenizer;
mod parser;
mod uniquify;

// TODO am I able to put this in the testing file?
mod test_helpers;

use tokenizer::*;
use parser::r1::*;
use uniquify::*;

use std::collections::HashMap;

/// a compiler for the R1 langauge
fn main() -> Result<(), UniquifyError> {
    println!("Hello, RC!");

    // let input = "(program () (let ([x 2])(+ x (let {{x (+ 3 x)}} x))".to_string();

    let input = "(program () (+ 2 2))".to_string();

    println!("{:?}", tokenizer(input.clone()));

    println!("{:?}", parse(tokenizer(input.clone())).unwrap());

    // TODO: prevent panic! when not Ok
    match parse(tokenizer(input.clone())).unwrap() {
        Program{ info: _, exp } => {
            println!("{:?}", uniquify(exp, &mut HashMap::new())?);
        }
    }
    Ok(())
}
