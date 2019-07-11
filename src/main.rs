use std::collections::VecDeque;
use std::collections::HashMap;

enum Operation {
    Plus,
    Negation,
    Read
}

/// an expression type for the R1 language
/// exp ::= int | (read) | (- exp) | (+ exp exp) | var | (let ([var exp]) exp)
/// R1 ::= (program info exp)
enum R1Expr {
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
struct R1 {
    info: HashMap<String, String>,
    exp: R1Expr
}


/// given an S-expression as a string, output a stream of tokens
/// e.g. (program () (+ 2 2))
fn tokenizer(s : String) -> VecDeque<String> {

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
fn parse_r1(_tokens : VecDeque<String>) -> R1Expr {
    R1Expr::Num(42)
}

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

#[cfg(test)]
mod tests {
    #[test]
    fn test_tokenizer() {
        let given = "(program () (let ([x 2])(+ x (let {{x 3}} x))".to_string();
        let expect = crate::tokenizer(given.clone());
        let output = crate::tokenizer(given.clone());
        assert_eq!(expect, output);
    }
}