use std::collections::VecDeque;

/// given an S-expression as a string, output a stream of tokens
/// e.g. (program () (+ 2 2))
pub fn tokenizer(s : String) -> VecDeque<String> {
    let mut tokens = VecDeque::new();
    let mut token = String::new();
    let specials = "(){}[]";
    for c in s.chars() {
        if specials.contains(c) {
            // we found a special
            // lets look for words
            if token.len() > 0 {
                tokens.push_back(token.clone());
                token.clear();
            }
            tokens.push_back(c.to_string());
        } else if " ".contains(c) {
            if token.len() > 0 {
                tokens.push_back(token.clone());
                token.clear();
            }
        } else {
            token.push(c);
        }
    }

    // handle single atoms
    if (token.len() > 0) { tokens.push_back(token); }

    tokens
}


