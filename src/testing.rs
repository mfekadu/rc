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

    #[test]
    fn test_add() {
        let input = vec!["(", "+", "2", "2", ")"];
        let expect = R1Expr::BinaryOperation {
            op: Operation::Plus,
            e1: Box::new(R1Expr::Num(2)),
            e2: Box::new(R1Expr::Num(2)),
        };

        let output = R1Expr::BinaryOperation {
            op: Operation::Plus,
            e1: Box::new(R1Expr::Num(2)),
            e2: Box::new(R1Expr::Num(2)),
        };

        assert_eq!(output, expect);
    }
}

mod integration_tests {

}
