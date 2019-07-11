#[cfg(test)]
mod testing;
mod tokenizer;

use tokenizer::*;

/// given an abstract syntax tree, output an AST with unique variable names
fn uniqify_r1(_ : R1Expr) -> R1Expr {
    R1Expr::Num(42)
}

/// a compiler for the R1 langauge
fn main() {
    println!("Hello, RC!");

    let input = "(program () (let ([x 2])(+ x (let {{x 3}} x))".to_string();

    println!("{:?}", tokenizer(input.clone()));

    match uniqify_r1(parse_r1(tokenizer(input.clone()))) {
        R1Expr::Num(v) => { println!("matched number with value = {}", v) }
        _ => { println!("idk") }
    }
}

