use std::collections::VecDeque;


/// tokenizer
/// given an S-expression as a string, output a stream of tokens
fn tokenizer(s : String) -> VecDeque<String> {
    let mut v = VecDeque::new();
    v.push_back(s);
    v
}

fn main() {
    println!("Hello, world!");
    // todo: tokenizer
    // given (+ 2 2) >> ["(", "+", "2", "2", ")"]
    // use VecDeque
    tokenizer("applesauce".to_string());
}
