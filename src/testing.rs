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
}


