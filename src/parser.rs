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
    #[derive(PartialEq, Debug)]
    pub enum Expr {
        Num (u64),            // TODO: check book for what range int has
        Read,                 // e.g. (read)
        Negation,             // e.g. (- 2)
        Plus,                 // e.g. (+ 2 2)
        Binding{ var: Box<Expr>, exp: Box<Expr> }, // e.g. (let ([x 2]) (+ 2 x))
        Var( String ),        // e.g. x
        List(VecDeque<Expr>), // e.g. (2 2) OR (+ 2 2) ...
    }

    /// error enum
    #[derive(Debug)]
    pub enum ExprError {
        GenericError
    }

    /// given tokens, output an abstract syntax tree of r1
    /// helper for parse_r1
    fn parse_expr(tokens : &mut VecDeque<String>) -> ExprResult {
        if tokens.len() == 0 {
            return Err(ExprError::GenericError);
        }
        let openers = "({[";
        let closers = ")}]";
        // we can always unwrap because we check the length
        match tokens.pop_front().unwrap().as_ref() {
            "(" => {
                // println!("OPEN!");
                let mut temp = VecDeque::new();
                if tokens.len() == 0 {
                    return Err(ExprError::GenericError);
                }
                // avoid pop here because will miss tokens otherwise. pop only once.
                while tokens[0] != ")" {
                    temp.push_back(parse_expr(tokens)?);
                }
                Ok(Expr::List(temp))
            }, // call parse on the rest
            "read" => Ok(Expr::Read),
            "-" => Ok(Expr::Negation),
            "+" => Ok(Expr::Plus),
            "let" => {
                if tokens.len() == 0 {
                    return Err(ExprError::GenericError);
                }
                // TODO: handle errors
                println!("panic? {}", tokens[0]);
                assert!(tokens.pop_front().unwrap().contains(openers));
                println!("panic?");
                assert!(tokens.pop_front().unwrap().contains(openers));
                println!("panic?");
                let var_str = tokens.pop_front().unwrap();
                Ok(Expr::Binding{
                    var: Box::new(Expr::Var(var_str)),
                    exp: {
                        assert_ne!(tokens.len(), 0); // make sure there's a body
                        assert_ne!(tokens[0], ")"); // make sure there's a body
                        let res = Box::new(parse_expr(tokens).unwrap());
                        println!("panic?");
                        assert!(tokens.pop_front().unwrap().contains(closers));
                        println!("panic?");
                        assert!(tokens.pop_front().unwrap().contains(closers));
                        println!("panic?");
                        res
                    }
                })
            },
            ")" => Err(ExprError::GenericError),
            other =>  { // parse Var or Num
                println!("~~Other~~ {}", other);
                match other.parse::<u64>() {
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
        let expect = List(VecDeque::from(vec![Plus, Num(2), Num(2)]));
        // TODO: part-2 consider if possible... function pointer instead of parse_expr?
        // to allow for reusing tests on parse / parse_expr
        // while keeping parse_expr private?
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