pub mod R1 {
    use std::collections::{VecDeque, HashMap};
    /// the program
    pub struct Program {
        info: HashMap<String, String>,
        exp: Expr
    }

    #[derive(PartialEq, Debug)]
    pub enum Operation {
        Plus,
        Negation,
        Read
    }

    /// an expression type for the R1 language
    /// exp ::= int | (read) | (- exp) | (+ exp exp) | var | (let ([var exp]) exp)
    /// R1 ::= (program info exp)
    #[derive(PartialEq, Debug)]
    pub enum Expr {
        Num (u64), // TODO: check book for what range int has
        NullaryOperation( Operation ), // e.g. (read)
        UnaryOperation{ op: Operation, e: Box<Expr> }, // e.g. (- 2)
        BinaryOperation { op: Operation, e1: Box<Expr>, e2: Box<Expr> }, // e.g. (+ 2 2)
        // Function { op: Operation, exp: Vec<Expr> } // TODO: for R2
        Binding { // e.g. (let ([x 10]) (+ x 2))
            var: Box<Expr>,
            value: Box<Expr>,
            body: Box<Expr>
        },
        Var( String ) // e.g. x
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