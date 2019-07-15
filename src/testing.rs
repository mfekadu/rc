mod tokenizer_tests {
    use crate::tokenizer::*;

    #[test]
    fn test_add() {
        let input = String::from("(+ 2 2)");
        assert_eq!(tokenizer(input), vec!["(", "+", "2", "2", ")"]);
    }

    #[test]
    fn test_let() {
        let input = String::from("(let ([x 2]) (+ x 4))");
        let output = tokenizer(input);
        let expect = vec!["(", "let", "(", "[", "x", "2", "]", ")", "(", "+", "x", "4", ")", ")"];
        assert_eq!(output, expect);
    }

    #[test]
    fn test_full_program() {
        let given = "(program () (let ([x 2])(+ x (let {{x 3}} x))".to_string();
        let expect = vec!["(", "program", "(", ")", "(", "let", "(", "[", "x", "2", "]", ")", "(",
        "+", "x", "(", "let", "{", "{", "x", "3","}", "}", "x", ")", ")"];
        let output = tokenizer(given);
        // fun fact: dont do assert_eq!(expect, output);
        // because &str does not impl PartialEq but String does
        assert_eq!(output, expect);
    }
}

mod parser_tests {
    use crate::parser::*;
    use r1::*;   // allows Expr instead of r1::Expr
    use Expr::*; // allows List instead of r1::Expr::List
    use std::collections::{VecDeque, HashMap};

    #[test]
    fn test_parse_expr() {
        test_private_parse_expr();
    }

    /// given vector of string, return concatenated vector with program syntax
    fn make_prog_vec(given_vec: Vec<&'static str>) -> Vec<&'static str> {
        let prog_vec = vec!["(", "program", "(", ")"];
        // overshadow prog_vec with fully setup program vector
        // also append the last paren
        return vec![prog_vec, given_vec, vec![")"]].concat();
    }

    #[test]
    fn test_add() {
        let exp_vec = vec!["(", "+", "2", "2", ")"];
        let prog_vec = make_prog_vec(exp_vec);
        println!("{:?}", prog_vec);
        let input: VecDeque<String> = VecDeque::from(prog_vec
                                        .into_iter()                // change to iterator
                                        .map(|x| x.to_string())     // apply function to all elements in iterator
                                        .collect::<Vec<String>>()); // collect back to vector with type annotation
                                                                    // see
                                                                    // https://stackoverflow.com/questions/30026893/using-map-with-vectors
        let expect = Program{
            info: HashMap::new(),
            exp: List(VecDeque::from(vec![Plus, Num(2), Num(2)]))
        };
        // TODO: part-1 consider if possible... function pointer instead of parse_expr?
        // to allow for reusing tests on parse / parse_expr
        // while keeping parse_expr private?
        let output = parse(input);
        assert_eq!(output.unwrap(), expect);
    }

    #[test]
    fn test_let() {
        let exp_vec = vec!["(", "let", "(", "[", "x", "2", "]", ")", "(", "+", "x", "4", ")", ")"];
        let prog_vec = make_prog_vec(exp_vec);
        let input: VecDeque<String> = VecDeque::from(prog_vec
                                        .into_iter()
                                        .map(|x| x.to_string())
                                        .collect::<Vec<String>>());

        // (+ x 4)
        let plus_expr = List( VecDeque::from(vec![Plus, Var("x".to_string()), Num(4)]));
        let binding = Binding {var: Box::new(Var("x".to_string())), value: Box::new(Num(2)) };
        let the_whole_let_expr = List( VecDeque::from(vec![ binding, plus_expr ]));
        let expect = Program { info: HashMap::new(), exp: the_whole_let_expr};

        let output = parse(input);
        assert_eq!(output.unwrap(), expect);
    }
}

mod integration_tests {
    use crate::parser::*;
    use crate::tokenizer::*;
    use r1::*;   // allows Expr instead of r1::Expr
    use Expr::*; // allows List instead of r1::Expr::List
    use std::collections::{VecDeque, HashMap};

    #[test]
    fn test_tokenizer_with_parser() {
        let input = String::from("(program () (+ 2 2))");
        let expect = Program { info: HashMap::new(), exp: List( VecDeque::from(vec![Plus, Num(2), Num(2)]))};
        // TODO: make a func that can make exprs or Programs or whatever 
        // TODO: do the TODOs hahaha
        let output = parse(tokenizer(input));
        assert_eq!(output.unwrap(), expect);
    }
}