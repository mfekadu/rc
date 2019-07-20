
use super::parser::r1::*;
use std::collections::{HashMap, VecDeque};


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
        }
        other => Ok(other),
    }
}

/// given (+ (- 2) 2)
/// expect (let ([tmp1 (- 2)]) (+ tmp1 2))
fn rco_exp(expr: Expr) -> Result<Expr, RcoError> {
    match expr {
        Expr::List(vec) => {
            let mut body: VecDeque<Expr> = VecDeque::new();
            let mut alist = Alist::new();
            for v in vec {
                body.push_back(rco_arg(v, &mut alist)?);
            }
            // let mut the_list = List(body);

            let mut bindings = VecDeque::new();

            for x in &body {
                if (alist.contains_key(x)) {
                    let value = alist.get(x).unwrap().clone();
                    bindings.push_back(Binding {
                        var: Box::new(x.clone()),
                        val: Box::new(value),
                    })
                }
            }

            for (key, value) in alist {
                let mut v = VecDeque::new();
                v.push_back(Binding {
                    var: Box::new(key),
                    val: Box::new(value),
                });
                v.push_back(List(body));
                body = v;
            }

            //let bindings = VecDeque::new();
            // for v in bindings: if alist.contains_key(v)
            // TODO do something with body
            Ok(List(body))
        }
        other => Ok(other),
    }
}

fn generate_unique_name() -> String {
    static mut COUNT: isize = 0;
    let mut ret_string = String::from("tmp");
    unsafe {
        COUNT += 1;
        ret_string.push_str(&COUNT.to_string());
    }
    ret_string
}

mod rco_test {

    use crate::parser::r1::*;

    use crate::rco::*;
    use crate::test_helpers::*;
    use std::collections::{HashMap, VecDeque};
    use Expr::*;
    #[test]
    fn unique_name_test() {
        // first one should be tmp1 or something like that
        println!("{}", generate_unique_name());
        // second one should increment by 1
        println!("{}", generate_unique_name());
        assert_ne!(generate_unique_name(), generate_unique_name());
        println!("{}", generate_unique_name());
    }


    #[test]
    fn test_nested_negation() {
        // given (- (- 2))
        let neg_2 = list![Negation, Num(2)];
        let input = list![Negation, neg_2.clone()];

        // expect (let ([tmp1 (- 2)]) (- tmp1))
        let the_binding = generate_binding_expr("tmp1", neg_2);
        let expect = list![the_binding, list![Negation, Var("tmp1".to_string())]];

        let output = rco_exp(input);
        assert_eq!(output.unwrap(), expect);
    }


    #[test]
    fn test_nested_negation_addition() {
        // given (- (+  (- 3) (- 4)))
        let neg_3 = list![Negation, Num(3)];
        let neg_4 = list![Negation, Num(4)];
        let addition = list![Plus, neg_3, neg_4];
        let input = list![Negation, addition.clone()];

        println!("output: {:?}", rco_exp(input).unwrap());

        // // expect (let ([tmp1 (- 2)]) (- tmp1))
        // let the_binding = generate_binding_expr("tmp1", neg_2);
        // let expect = list![the_binding, list![Negation, Var("tmp1".to_string())]];

        // let output = rco_exp(input);
        // assert_eq!(output.unwrap(), expect);
    }
}
