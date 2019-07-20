// TODO <1 john> am I able to put this in the testing file?
#[macro_use]
mod test_helpers;

#[cfg(test)]
mod testing;
mod tokenizer;
mod parser;
mod uniquify;
mod rco;



use tokenizer::*;
use parser::r1::*;
use uniquify::*;

use std::collections::{HashMap, VecDeque};

/// a compiler for the R1 langauge
fn main() -> Result<(), UniquifyError> {
    println!("Hello, RC! {:?}", vecdec![1,2,3]);

    // let input = "(program () (let ([x 2])(+ x (let {{x (+ 3 x)}} x))".to_string();

    let input = "(program () (+ 2 2))".to_string();

    println!("{:?}", tokenizer(input.clone()));

    println!("{:?}", parse(tokenizer(input.clone())).unwrap());

    match parse(tokenizer(input.clone())).unwrap() {
        Program{ info: _, exp } => {
            println!("{:?}", uniquify(exp, &mut HashMap::new())?);
        }
    }
    Ok(())
}
