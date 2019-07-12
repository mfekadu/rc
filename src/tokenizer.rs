use std::collections::{VecDeque, HashMap};

pub enum Operation {
    Plus,
    Negation,
    Read
}

/// an expression type for the R1 language
/// exp ::= int | (read) | (- exp) | (+ exp exp) | var | (let ([var exp]) exp)
/// R1 ::= (program info exp)
pub enum R1Expr {
    Num (u64), // TODO: check book for what range int has
    NullaryOperation( Operation ), // e.g. (read)
    UnaryOperation{ op: Operation, e: Box<R1Expr> }, // e.g. (- 2)
    BinaryOperation { op: Operation, e1: Box<R1Expr>, e2: Box<R1Expr> }, // e.g. (+ 2 2)
    // Function { op: Operation, exp: Vec<R1Expr> } // TODO: for R2
    Binding { // e.g. (let ([x 10]) (+ x 2))
        var: Box<R1Expr>,
        value: Box<R1Expr>,
        body: Box<R1Expr>
    },
    Var( String ) // e.g. x
}

/// the program
pub struct R1 {
    info: HashMap<String, String>,
    exp: R1Expr
}

/// given an S-expression as a string, output a stream of tokens
/// e.g. (program () (+ 2 2))
pub fn tokenizer(s : String) -> VecDeque<String> {

    let mut tokens = VecDeque::new();

    let mut token = String::new();

    let specials = "(){}[]"; // TODO: consider comments via ';'

    for c in s.chars() {
        if specials.contains(c) {
            // we found a special
            // lets look for words
            if token.len() > 0 {
                tokens.push_back(token.clone()); // TODO: avoid clone
                token.clear();
            }
            tokens.push_back(c.to_string());
        } else if " ".contains(c) {
            if token.len() > 0 {
                tokens.push_back(token.clone()); // TODO: avoid clone
                token.clear();
            }
        } else {
            token.push(c);
        }
    }
    tokens
}

/// given tokens, output an abstract syntax tree of R1
pub fn parse_r1(_tokens : VecDeque<String>) -> R1Expr {
    R1Expr::Num(42)
}
