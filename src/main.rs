use std::collections::VecDeque;

enum SomeEnum { Haha }

/// tokenizer
/// given an S-expression as a string, output a stream of tokens
fn tokenizer(s : String) -> VecDeque<String> {
    let mut v = VecDeque::new();
    v.push_back(s);
    v
}

/// parser
/// given tokens, output an abstract syntax tree
fn parser(_s : VecDeque<String>) -> SomeEnum {
    SomeEnum::Haha
}

/// uniqify
/// given an abstract syntax tree, output an AST with unique variable names
fn uniqify(_s : SomeEnum) -> SomeEnum {
    SomeEnum::Haha
}

/// main
/// a compiler for the R1 langauge
fn main() {
    println!("Hello, RC!");

    uniqify(parser(tokenizer("applesauce".to_string())));
}
