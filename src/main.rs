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
use rco::*;
use uniquify::*;
use rustyline::Editor;

use std::collections::{HashMap, VecDeque};

/// a compiler for the R1 langauge
fn main() -> Result<(), UniquifyError> {

    let mut r1 = Editor::<()>::new();

    loop {
        let raw_input = r1.readline("rc> ").unwrap();
        let parse_out = parse(tokenizer(raw_input)).unwrap();

        match parse_out {
            Program{ info: _, exp } => {
                println!("{:?}", rco_exp(exp).unwrap());
            }
        }
    }

    Ok(())
}


// TODO: <3> test cases
/*
(program () 1)
(program () (- 1))
(program () (+ 2 2))

(program () (+ (- 2) 2) )

(program () (+ (- 2) (- 2)) )

(program () (+ (- 2) (+ 2 2)) )

(program ()  (let ([a 42]) (let ([b a]) b)) )
(program ()  (+ (let ([x 42]) x) 2) )

(program ()  (+ (let ([x 42]) (+ x 1)) 2) )

(program ()  (+ (let ([x 42]) x) (+ 2 3) ) )

(program () (let ([x (+ 4 (- 5))]) x) )
*/
