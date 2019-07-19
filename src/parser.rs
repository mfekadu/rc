pub mod r1 {
    use std::collections::{VecDeque, HashMap};
    /// the program
    #[derive(PartialEq, Debug)]
    pub struct Program {
        pub info: HashMap<String, String>,
        pub exp: Expr
    }

    type ExprResult = Result<Expr, ExprError>;
    type ProgramResult = Result<Program, ExprError>;

    /// an expression type for the r1 language
    /// exp ::= int | (read) | (- exp) | (+ exp exp) | var | (let ([var exp]) exp)
    /// r1 ::= (program info exp)
    #[derive(PartialEq, Debug, Clone, Hash, Eq)]
    pub enum Expr {
        Num (i64),            // e.g. 2, or -2, or 1849000000000000
        Read,                 // e.g. (read)
        Negation,             // e.g. (- 2)
        Plus,                 // e.g. (+ 2 2)
        Binding{ var: Box<Expr>, value: Box<Expr> }, // e.g. (let ([x 2]) (+ 2 x)) // TODO <1> change this to {var, val}
        Var( String ),        // e.g. x
        List(VecDeque<Expr>), // e.g. (2 2) OR (+ 2 2) ...
    }

    /// error enum
    #[derive(Debug)]
    pub enum ExprError {
        GenericError
    }

    fn expect_opener(token: String) -> Result<(), ExprError> {
        match token.as_ref() {
            "{" | "(" | "[" => Ok(()),
            _ => Err(ExprError::GenericError),
        }
    }

    fn expect_closer(token: String) -> Result<(), ExprError> {
        match token.as_ref() {
            "}" | ")" | "]" => Ok(()),
            _ => Err(ExprError::GenericError),
        }
    }

    /// given tokens, output an abstract syntax tree of r1
    /// helper for parse_r1
    fn parse_expr(tokens : &mut VecDeque<String>) -> ExprResult {
        if tokens.len() == 0 {
            return Err(ExprError::GenericError);
        }
        println!("tokens {:?}", tokens);
        // we can always unwrap because we check the length
        match tokens.pop_front().unwrap().as_ref() {
            "(" | "{" | "[" => {
                // println!("OPEN!");
                let mut temp = VecDeque::new();
                if tokens.len() == 0 {
                    return Err(ExprError::GenericError);
                }
                // avoid pop here because will miss tokens otherwise. pop only once.
                while tokens[0] != ")" && tokens[0] != "}" && tokens[0] != "]" {
                    println!("\twhile_tokens {:?}", tokens);
                    temp.push_back(parse_expr(tokens)?);

                }
                println!("\t\tafter_while_tokens {:?}", tokens);

                // need to pop off closer because if this expression is nested, it can terminate
                // early
                if tokens.len() == 0 { // safe pop
                    return Err(ExprError::GenericError);
                }
                tokens.pop_front();
                println!("\t\t\tafter_after_while_tokens {:?}", tokens);
                Ok(Expr::List(temp))
            }, // call parse on the rest
            "read" => Ok(Expr::Read),
            "-" => Ok(Expr::Negation),
            "+" => Ok(Expr::Plus),
            "let" => {
                if tokens.len() == 0 {
                    return Err(ExprError::GenericError);
                }
                // TODO: <3> safe popping(mutable borrow)/checking length before popping
                // ?? pop.or_else???
                expect_opener(tokens.pop_front().unwrap())?;
                expect_opener(tokens.pop_front().unwrap())?;
                let mut first_token_vec = VecDeque::new();
                first_token_vec.push_back(tokens.pop_front().unwrap());
                let var_str = if let Expr::Var(v) = parse_expr(&mut first_token_vec)? {
                    v
                } else {
                    return Err(ExprError::GenericError);
                };
                Ok(Expr::Binding{
                    var: Box::new(Expr::Var(var_str)),
                    value: {
                        // TODO <3> don't panic
                        assert_ne!(tokens.len(), 0); // make sure there's a body
                        assert_ne!(tokens[0], ")"); // make sure there's a body
                        let res = Box::new(parse_expr(tokens).unwrap());
                        expect_closer(tokens.pop_front().unwrap())?;
                        expect_closer(tokens.pop_front().unwrap())?;
                        res
                    }
                })
            },
            ")" | "}" | "]" => Err(ExprError::GenericError),
            other =>  { // parse Var or Num
                match other.parse::<i64>() {
                    Ok(value) => Ok(Expr::Num(value)),
                    Err(_) => Ok(Expr::Var(String::from(other))),
                }
            }
        }
    }

    #[test]
    pub fn test_private_parse_expr() {
        use crate::parser::*;
        use r1::*;   // allows Expr instead of r1::Expr
        use Expr::*; // allows List instead of r1::Expr::List
        use std::collections::VecDeque;

        let mut input: VecDeque<String> = VecDeque::from(vec!["(", "let", "(", "[", "x", "2", "]", ")", "(", "+", "x", "4", ")", ")"]
                                        .into_iter()
                                        .map(|x| x.to_string())
                                        .collect::<Vec<String>>());

        let plus_expr = List( VecDeque::from(vec![Plus, Var("x".to_string()), Num(4)]));
        let binding = Binding {var: Box::new(Var("x".to_string())), value: Box::new(Num(2)) };
        let expect = List( VecDeque::from(vec![ binding, plus_expr ]));
        let output = parse_expr(&mut input);
        assert_eq!(output.unwrap(), expect);
    }

    /// parser
    pub fn parse(mut tokens : VecDeque<String>) -> ProgramResult {
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
}
