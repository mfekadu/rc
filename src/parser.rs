pub mod R1 {
    use std::collections::{VecDeque, HashMap};
    /// the program
    pub struct Program {
        info: HashMap<String, String>,
        exp: Expr
    }

    #[derive(PartialEq, Debug)]
    pub enum Operation {
    }

    /// an expression type for the R1 language
    /// exp ::= int | (read) | (- exp) | (+ exp exp) | var | (let ([var exp]) exp)
    /// R1 ::= (program info exp)
    #[derive(PartialEq, Debug)]
    pub enum Expr {
        Num (u64), // TODO: check book for what range int has
        Read,     // e.g. (read)
        Negation, // e.g. (- 2)
        Plus,     // e.g. (+ 2 2)
        Let,      // e.g. (let ([x 2]) (+ 2 x))
        Var( String ), // e.g. x
        List(Vec<Expr>), // e.g. (2 2) OR (+ 2 2) ...
    }

    #[derive(Debug)]
    pub enum ExprError {
        GenericError
    }

    /// given tokens, output an abstract syntax tree of R1
    /// helper for parse_r1
    fn parse_expr(mut tokens : VecDeque<String>) -> Result<Expr, ExprError> {

        match tokens.pop_front().as_ref() {
            // e.g. (+ 2 2)
            // e.g. (program () (+ 2 2))
            "(" => parse_expr(tokens), // call parse on the rest
            "read" => Ok(Expr::NullaryOperation(Operation::Read)),
            "-" => Ok(Expr::UnaryOperation{
                    op: Operation::Negation,
                    e: parse_expr(tokens)?
                }),
            "+" => Ok(Expr::BinaryOperation{
                    op: Operation::Plus,
                    e1: parse_expr(tokens),
                    e2: Operation::Plus,
                }),
            // "let" => Ok(Expr::Binding()),
            ")" => {panic!("idk... tbh.");},
            _ =>  { panic!("idk... tbh."); }
        }
    }

    pub fn parse(mut tokens : VecDeque<String>) -> Result<Program, ExprError> {
    }
    //
}