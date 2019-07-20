#[cfg(test)]
mod testing;
mod tokenizer;
mod parser;
mod uniquify;
mod rco;

// TODO <1 john> am I able to put this in the testing file?
mod test_helpers;

use tokenizer::*;
use parser::r1::*;
use uniquify::*;

use std::collections::{HashMap, VecDeque};

mod macros {
    use std::collections::VecDeque;
    use super::parser::r1::*;

    #[macro_export]
    macro_rules! vecdec {
        ( $( $x:expr ),* ) => {
            {
                let mut temp_vec = VecDeque::new();
                $(
                    temp_vec.push_back($x);
                )*
                temp_vec
            }
        };
    }

    #[test]
    fn test_vecdec() {
        println!("{:?}", vecdec![1, 2, 3]);
        assert_eq!(VecDeque::from(vec![1,2,3]), vecdec![1,2,3]);
    }

    #[macro_export]
    macro_rules! list {
        ( $( $x:expr ),* ) => {
            {
                let mut temp_vec = VecDeque::new();
                $(
                    temp_vec.push_back($x);
                )*
                Expr::List(temp_vec)
            }
        };
    }

    #[test]
    fn test_list() {
        let x = list![Expr::Num(1), Expr::Num(2), Expr::Num(3)];
        println!("{:?}", x);
        assert_eq!(Expr::List(VecDeque::from(vec![Expr::Num(1), Expr::Num(2), Expr::Num(3)])), x);
    }
}




/// a compiler for the R1 langauge
fn main() -> Result<(), UniquifyError> {
    println!("Hello, RC! {:?}", vecdec![1,2,3]);

    // let input = "(program () (let ([x 2])(+ x (let {{x (+ 3 x)}} x))".to_string();

    let input = "(program () (+ 2 2))".to_string();

    println!("{:?}", tokenizer(input.clone()));

    println!("{:?}", parse(tokenizer(input.clone())).unwrap());

    match parse(tokenizer(input.clone())).unwrap() {
        Program{ info: _, exp } => {
            println!("{:?}", uniquify(exp, &mut HashMap::new())?);
        }
    }
    Ok(())
}
