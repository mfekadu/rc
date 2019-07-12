pub mod r1 {
    use std::collections::{VecDeque, HashMap};
    /// the program
    pub struct Program {
        info: HashMap<String, String>,
        exp: Expr
    }

    #[derive(PartialEq, Debug)]
    pub enum Operation {
    }

    /// an expression type for the r1 language
    /// exp ::= int | (read) | (- exp) | (+ exp exp) | var | (let ([var exp]) exp)
    /// r1 ::= (program info exp)
    #[derive(PartialEq, Debug)]
    pub enum Expr {
        Num (u64),            // TODO: check book for what range int has
        Read,                 // e.g. (read)
        Negation,             // e.g. (- 2)
        Plus,                 // e.g. (+ 2 2)
        Let,                  // e.g. (let ([x 2]) (+ 2 x))
        Var( String ),        // e.g. x
        List(VecDeque<Expr>), // e.g. (2 2) OR (+ 2 2) ...
    }

    #[derive(Debug)]
    pub enum ExprError {
        GenericError
    }

    /// given tokens, output an abstract syntax tree of r1
    /// helper for parse_r1
    fn parse_expr(tokens : &mut VecDeque<String>) -> Result<Expr, ExprError> {
        if tokens.len() == 0 {
            return Err(ExprError::GenericError);
        }
        // we can always unwrap because we check the length
        match tokens.pop_front().unwrap().as_ref() {
            "(" => {
                let mut temp = VecDeque::new();
                if tokens.len() == 0 {
                    return Err(ExprError::GenericError);
                }
                // we can always unwrap because we check the length
                while tokens.pop_front().unwrap() != ")" {
                    temp.push_back(parse_expr(tokens)?);
                }
                Ok(Expr::List(temp))
            }, // call parse on the rest
            "read" => Ok(Expr::Read),
            "-" => Ok(Expr::Negation),
            "+" => Ok(Expr::Plus),
            "let" => Ok(Expr::Let),
            ")" => Err(ExprError::GenericError),
            other =>  { // parse Var or Num
                match other.parse::<u64>() {
                    Ok(value) => Ok(Expr::Num(value)),
                    Err(_) => Ok(Expr::Var(String::from(other))),
                }
            }
        }
    }

    pub fn parse(mut tokens : VecDeque<String>) -> Result<Program, ExprError> {
        assert_ne!(tokens.len(), 0);
        assert_eq!(tokens.pop_front().unwrap(), "(");
        assert_ne!(tokens.len(), 0);
        assert_eq!(tokens.pop_front().unwrap(), "program");
        assert_ne!(tokens.len(), 0);
        assert_eq!(tokens.pop_front().unwrap(), "(");
        assert_ne!(tokens.len(), 0);
        assert_eq!(tokens.pop_front().unwrap(), ")");
        Ok(Program{
            info: HashMap::new(),
            exp: parse_expr(&mut tokens)?
        })
    }
    //
}