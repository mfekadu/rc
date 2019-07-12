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
    use std::collections::VecDeque;

    #[test]
    fn test_add() {
        let input: VecDeque<String> = VecDeque::from(vec!["(", "+", "2", "2", ")"]
                                        .into_iter()                // change to iterator
                                        .map(|x| x.to_string())     // apply function to all elements in iterator
                                        .collect::<Vec<String>>()); // collect back to vector with type annotation
                                                                    // see
                                                                    // https://stackoverflow.com/questions/30026893/using-map-with-vectors
        let expect = R1Expr::BinaryOperation {
            op: Operation::Plus,
            e1: Box::new(R1Expr::Num(2)),
            e2: Box::new(R1Expr::Num(2)),
        };

        let output = parse_r1(input);
        assert_eq!(output, expect);
    }

    #[test]
    fn test_let() {
        let input: VecDeque<String> = VecDeque::from(vec!["(", "let", "(", "[", "x", "2", "]", ")", "(", "+", "x", "4", ")", ")"]
                                        .into_iter()
                                        .map(|x| x.to_string())
                                        .collect::<Vec<String>>());
        let expect = R1Expr::Binding {
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
