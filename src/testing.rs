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
        let expect = Program(
            info: HashMap::new(),
            exp: List(VecDeque::from(
                vec![Let,
                    List(VecDeque::from(
                        vec![
                            List(VecDeque::from(
                                vec![Var("x"), Num(2)]
                            )
                        ]
                    )
                ]
            ))
        )
        R1Expr::Binding {
            var: Box::new(R1Expr::Var(String::from("x"))),
            value: Box::new(R1Expr::Num(2)),
            body: Box::new(R1Expr::BinaryOperation {
                op: Operation::Plus,
                e1: Box::new(R1Expr::Var(String::from("x"))),
                e2: Box::new(R1Expr::Num(4)),
            }),
        };
        let output = parse_r1(input);
        assert_eq!(output, expect);
    }
}

mod integration_tests {
    use crate::parser::*;
    use crate::tokenizer::*;

    #[test]
    fn test_tokenizer_with_parser() {
        let input = String::from("(+ 2 2)");
        let expect =  R1Expr::BinaryOperation {
            op: Operation::Plus,
            e1: Box::new(R1Expr::Num(2)),
            e2: Box::new(R1Expr::Num(2)),
        };
        let output = parse_r1(tokenizer(input));
        assert_eq!(output, expect);
    }
}
