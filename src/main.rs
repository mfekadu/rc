#[cfg(test)]
mod testing;
mod tokenizer;
mod parser;

use tokenizer::*;
use parser::*;

/// given an abstract syntax tree, output an AST with unique variable names
// fn uniqify_r1(_ : R1Expr) -> R1Expr {
//     R1Expr::Num(42)
// }

/// a compiler for the R1 langauge
fn main() {
    println!("Hello, RC!");

    let input = "(program () (let ([x 2])(+ x (let {{x 3}} x))".to_string();

    println!("{:?}", tokenizer(input.clone()));

    // TODO: prevent panic! when not Ok
    match R1::parse(tokenizer(input.clone())).unwrap() {
        // R1::Expr::Num(v) => { println!("matched number with value = {}", v) }
        _ => { println!("idk") }
    }
}
