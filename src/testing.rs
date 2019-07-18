// TODO <2> split up into multiple files

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

    /// given vector of string, return concatenated vector with program syntax
    fn make_prog_vec(given_vec: Vec<&'static str>) -> Vec<&'static str> {
        let prog_vec = vec!["(", "program", "(", ")"];
        // overshadow prog_vec with fully setup program vector
        // also append the last paren
        return vec![prog_vec, given_vec, vec![")"]].concat();
    }

    #[test]
    fn test_add_with_negative() {
        let exp_vec = vec!["(", "+", "-2", "2", ")"];
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
            exp: List(VecDeque::from(vec![Plus, Num(-2), Num(2)]))
        };
        let output = parse(input);
        assert_eq!(output.unwrap(), expect);
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
        let output = parse(input);
        assert_eq!(output.unwrap(), expect);
    }

    #[test]
    fn test_nested_add() {
        let exp_vec = vec!["(", "+", "(", "+", "2", "2", ")", "4", ")"];
        let prog_vec = make_prog_vec(exp_vec);
        println!("{:?}", prog_vec);
        let input: VecDeque<String> = VecDeque::from(prog_vec
                                        .into_iter()                // change to iterator
                                        .map(|x| x.to_string())     // apply function to all elements in iterator
                                        .collect::<Vec<String>>()); // collect back to vector with type annotation
                                                                    // see
                                                                    // https://stackoverflow.com/questions/30026893/using-map-with-vectors
        let nested = List(VecDeque::from(vec![Plus, Num(2), Num(2)]));
        let expect = Program{
            info: HashMap::new(),
            exp: List(VecDeque::from(vec![Plus, nested, Num(4)]))
        };
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

    #[test]
    fn test_let_2() {
        let exp_vec = vec!["(", "let", "(", "[", "x", "2", "]", ")", "(", "+", "x", "(", "let", "{", "{", "x", "(", "+", "3", "x", ")", "}", "}", "x", ")", ")", ")"];
        let prog_vec = make_prog_vec(exp_vec);
        let input: VecDeque<String> = VecDeque::from(prog_vec
                                        .into_iter()
                                        .map(|x| x.to_string())
                                        .collect::<Vec<String>>());

        // (+ x 4)
        // let input = "(program () (let ([x 2])(+ x (let {{x (+ 3 x)}} x))))".to_string();
        let first_binding = Binding {var: Box::new(Var("x".to_string())), value: Box::new(Num(2)) };
        let second_binding_val = List( VecDeque::from(vec![Plus, Num(3), Var("x".to_string())]));
        let second_binding = Binding {var: Box::new(Var("x".to_string())), value: Box::new(second_binding_val) };
        let second_binding = List(VecDeque::from(vec![second_binding, Var("x".to_string())]));

        let body = List(VecDeque::from(vec![Plus, Var("x".to_string()), second_binding]));
        let the_whole_let_expr = List( VecDeque::from(vec![ first_binding, body ]));
        let expect = Program { info: HashMap::new(), exp: the_whole_let_expr};
        let output = parse(input);
        assert_eq!(output.unwrap(), expect);
    }

}

mod uniquify_tests {
    use crate::parser::*;
    use crate::uniquify::*;
    use crate::test_helpers::*;
    use std::collections::{VecDeque, HashMap};
    use r1::*;
    use Expr::*;

    #[test]
    fn test_add() {
        let input = List(VecDeque::from(vec![Plus, Num(2), Num(2)]));
        let expect = List(VecDeque::from(vec![Plus, Num(2), Num(2)]));
        let output = uniquify(input, &mut HashMap::new());
        assert_eq!(output.unwrap(), expect);
    }

    #[test]
    fn test_single_let() {
        let input = List(VecDeque::from(vec![generate_binding("x", 2),
                                             List(VecDeque::from(vec![Plus, Num(2), Var("x".to_string())]))]));

        let expect = List(VecDeque::from(vec![generate_binding("x1", 2),
                                              List(VecDeque::from(vec![Plus, Num(2), Var("x1".to_string())]))]));

        let output = uniquify(input, &mut HashMap::new());
        assert_eq!(output.unwrap(), expect);
    }

    #[test]
    fn test_shadow() {
        // input should be (let ([x 1]) (let ([x 3]) (+ x x)))
        let nested_let_expr = vec![generate_binding("x", 3),
                                   List(VecDeque::from(vec![Plus,
                                                            Var("x".to_string()),
                                                            Var("x".to_string())]))];
        let input = List(VecDeque::from(vec![generate_binding("x", 1),
                                             List(VecDeque::from(nested_let_expr))]));

        let nested_let_expr_out = vec![generate_binding("x2", 3),
                                   List(VecDeque::from(vec![Plus,
                                                            Var("x2".to_string()),
                                                            Var("x2".to_string())]))];
        let expect = List(VecDeque::from(vec![generate_binding("x1", 1),
                                              List(VecDeque::from(nested_let_expr_out))]));
        let output = uniquify(input, &mut HashMap::new());
        assert_eq!(output.unwrap(), expect);
    }

    #[test]
    fn test_shadow_inside_value() {
        // input should be (let ([x 1]) (let ([x (+ x x)]) (+ x x)))
        // output should be (let ([x.1 1]) (let ([x.2 (+ x.1 x.1)]) (+ x.2 x.2) ))
        let plus_x_x = List(VecDeque::from(vec![Plus, Var("x".to_string()), Var("x".to_string())]));

        let nested_let_expr = vec![generate_binding_expr("x", plus_x_x.clone()), plus_x_x];

        let input = List(VecDeque::from(vec![generate_binding("x", 1),
                                             List(VecDeque::from(nested_let_expr))]));


        let plus_x1_x1 = List(VecDeque::from(vec![Plus, Var("x1".to_string()), Var("x1".to_string())]));

        let nested_let_expr_out = vec![generate_binding_expr("x2", plus_x1_x1),
                                   List(VecDeque::from(vec![Plus,
                                                            Var("x2".to_string()),
                                                            Var("x2".to_string())]))];
        let expect = List(VecDeque::from(vec![generate_binding("x1", 1),
                                              List(VecDeque::from(nested_let_expr_out))]));
        let output = uniquify(input, &mut HashMap::new());
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
        // TODO: <1> make a func that can make exprs or Programs or whatever
        // also move the make_prog_expr into test_helpers
        // TODO: do the TODOs hahaha... NEVER!!
        let output = parse(tokenizer(input));
        assert_eq!(output.unwrap(), expect);
    }
}

