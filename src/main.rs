use std::collections::VecDeque;

enum Keyword {
    Let,
    Program
}

enum Operation {
    Plus,
    Negation,
    Read
}

/// an expression type for the R1 language
/// exp ::= int | (read) | (- exp) | (+ exp exp) | var | (let ([var exp]) exp)
/// R1 ::= (program info exp)
enum R1Expr {
    Num { v: u64 }, // TODO: check book for what range int has
    // Read, // e.g. (read)
    // Negation { exp: Box<R1Expr> }, // e.g. (- 2)
    // Addition { exp1: Box<R1Expr>, exp2: Box<R1Expr> }, // e.g. (+ 2 2)
    NullaryOperation { op: Operation }, // e.g. (read)
    UnaryOperation { op: Operation, e1: Box<R1Expr> }, // e.g. (- 2)
    BinaryOperation { op: Operation, e1: Box<R1Expr>, e2: Box<R1Expr> }, // e.g. (+ 2 2)
    // Function { op: Operation, exp: Vec<Box<R1Expr>> }
    Binding, // e.g. (let ([x 10]) (+ x 2))
    // TODO: is this even what a var should be like?
    Var { symbol: String } // e.g. x
    // Var { v: &'static str },
}

/// abstract syntax tree


/// given an S-expression as a string, output a stream of tokens
/// e.g. (program () (+ 2 2))
fn tokenizer(s : String) -> VecDeque<String> {
    let mut v = VecDeque::new();
    v.push_back(s);
    v
}

/// given tokens, output an abstract syntax tree of R1
fn parse_r1(_tokens : VecDeque<String>) -> R1Expr {
    R1Expr::Num{value: 42}
}

/// given an abstract syntax tree, output an AST with unique variable names
fn uniqify_r1(_ : R1Expr) -> R1Expr {
    R1Expr::Num{value: 42}
}

/// a compiler for the R1 langauge
fn main() {
    println!("Hello, RC!");

    match uniqify_r1(parse_r1(tokenizer("applesauce".to_string()))) {
        R1Expr::Num{v} => { println!("matched number with value = {}", v) }
        _ => { println!("idk") }
    }
}
