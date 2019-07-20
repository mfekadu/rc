use std::collections::{VecDeque, HashMap};
use super::parser::r1::*;
use Expr::*;
use super::vecdec;
use super::list;

#[derive(Debug)]
pub enum RcoError {
    GenericError,
}

type Alist = HashMap<Expr, Expr>;

fn rco_arg(expr: Expr, alist: &mut Alist) -> Result<Expr, RcoError> {
    match expr {
        Expr::List(v) => {
            let tmp = Expr::Var(generate_unique_name());
            alist.insert(tmp.clone(), rco_exp(Expr::List(v))?);
            Ok(tmp)
        },
        other => Ok(other)
    }
}

/// given (+ (- 2) 2)
/// expect (let ([tmp1 (- 2)]) (+ tmp1 2))
fn rco_exp(expr: Expr) -> Result<Expr, RcoError>  {
    match expr {
        Expr::List(vec) => {
            let mut body = VecDeque::new();
            let mut alist = Alist::new();
            for v in vec {
                body.push_back(rco_arg(v, &mut alist)?);
            }
            //let bindings = VecDeque::new();
            // for v in bindings: if alist.contains_key(v)
            // TODO do something with body
            Ok(Expr::Num(42))
        },
        other => Ok(other)
    }
}

fn generate_unique_name() -> String {
    static mut COUNT :isize = 0;
    let mut ret_string = String::from("tmp");
    unsafe {
        COUNT += 1;
        ret_string.push_str(&COUNT.to_string());
    }
    ret_string
}


#[test]
fn unique_name_test() {
    assert_eq!(generate_unique_name(), "tmp1");
    assert_eq!(generate_unique_name(), "tmp2");
}


#[test]
fn test_nested_negation() {
    // // // given (- (- 2))
    // // // expect (let ([tmp1 (- 2)]) (- tmp1))
    // // let neg_2 = List(VecDeque::from(vec![Negation, Num(2)]));
    // // let neg_2 = List(VecDeque::from(vec![Negation, Num(2)]));
    // // let the_binding = Binding{var: Var("tmp1"), val: }
    // // let expect = List(VecDeque::from(vec![, Num(2), Num(2)]));
    // // let output = rco_exp(input);
    // // assert_eq!(output.unwrap(), expect);
    println!("{:?}", vecdec![1, 2, 3]);
    let x = list![Expr::Num(1), Expr::Num(2), Expr::Num(3)];
    println!("{:?}", x);
}