
use super::parser::r1::*;
use std::collections::{HashMap, VecDeque};
use Expr::*;

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
pub fn rco_exp(expr: Expr) -> Result<Expr, RcoError> {
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

/* RefCell does dynamic borrow checking instead of at compile time */
use std::cell::RefCell;

// TODO: learn thread magic
thread_local! {
    static COUNT: RefCell<isize> = RefCell::new(0);
}

pub fn reset_count() {
    COUNT.with( |count_cell| { *count_cell.borrow_mut() = 0 });
}

fn generate_unique_name() -> String {
    let mut ret_string = String::from("tmp");

    COUNT.with( |count_cell| {
        *count_cell.borrow_mut() += 1;
        ret_string.push_str(&count_cell.borrow_mut().to_string());
    });



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
        // setup test
        reset_count();

        // first one should be tmp1 or something like that
        println!("{}", generate_unique_name());
        // second one should increment by 1
        println!("{}", generate_unique_name());
        assert_ne!(generate_unique_name(), generate_unique_name());
        // should now have been incremented twice
        println!("{}", generate_unique_name());
    }

    #[test]
    fn unique_name_reset_test() {
        // setup test
        reset_count();

        // first one should be tmp1 or something like that
        println!("{}", generate_unique_name());
        // second one should increment by 1
        println!("{}", generate_unique_name());
        assert_ne!(generate_unique_name(), generate_unique_name());
        // should now have been incremented twice
        println!("{}", generate_unique_name());

        println!("do reset && assert tmp1"); reset_count();
        assert_eq!(generate_unique_name(), "tmp1");
        println!("{}", generate_unique_name());
        assert_ne!(generate_unique_name(), generate_unique_name());
        println!("{}", generate_unique_name());
        println!("{}", generate_unique_name());
    }

    #[test]
    fn test_nested_negation() {
        // setup test
        reset_count();

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
        // setup test
        reset_count();

        // given (- (+  (- 3) (- 4)))
        let neg_3 = list![Negation, Num(3)];
        let neg_4 = list![Negation, Num(4)];
        let addition = list![Plus, neg_3, neg_4];
        let input = list![Negation, addition.clone()];

        let bind3 = Binding {
            var: Box::new(Var("tmp3".to_string())),
            val: Box::new(list![Negation, Num(4)])
        };

        let bind2 = Binding {
            var: Box::new(Var("tmp2".to_string())),
            val: Box::new(list![Negation, Num(3)])
        };

        // we learned that the for (key, value) in HashMap
        // does not guarantee a particular order
        // so let's make the test flexible for that
        let bind_opt_1 = Binding {
            var: Box::new(Var("tmp1".to_string())),
            val: Box::new(list![
                bind3.clone(), // notice the swap
                list![
                    bind2.clone(), // notice the swap
                    list![Plus, Var("tmp2".to_string()), Var("tmp3".to_string())]]])
        };

        let bind_opt_2 = Binding {
            var: Box::new(Var("tmp1".to_string())),
            val: Box::new(list![
                bind2, // notice the swap
                list![
                    bind3, // notice the swap
                    list![Plus, Var("tmp2".to_string()), Var("tmp3".to_string())]]])
        };

        let expect_opt_1 = list![bind_opt_1, list![Negation, Var("tmp1".to_string())]];
        let expect_opt_2 = list![bind_opt_2, list![Negation, Var("tmp1".to_string())]];

        let output = rco_exp(input).unwrap();
        assert!(
            output == expect_opt_1 ||
            output == expect_opt_2
        );
    }

    #[test]
    fn test_nested_let() {
        // setup test
        reset_count();
        let innermost_bind = Binding {
            var: Box::new(Var("x2".to_string())),
            val: Box::new(Num(22)),
        };

        let inner_bind = Binding {
            var: Box::new(Var("x1".to_string())),
            val: Box::new(Num(20)),
        };

        let innermost_expr = list![innermost_bind.clone(), Var("x2".to_string())];

        let inner_body = list![Plus, Var("x1".to_string()), innermost_expr];
        let inner_expr = list![inner_bind.clone(), inner_body];

        let outer_bind = Binding {
            var: Box::new(Var("y".to_string())),
            val: Box::new(inner_expr),
        };

        let input = list![outer_bind, Var("y".to_string())];

        let out_innermost_body = list![Plus, Var("x1".to_string()), Var("x2".to_string())];
        let out_inner_expr = list![innermost_bind, out_innermost_body];
        let out_inner_bind = list![inner_bind, out_inner_expr];

        let out_outer_bind = Binding {
            var: Box::new(Var("y".to_string())),
            val: Box::new(out_inner_bind),
        };
        let expect = list![out_outer_bind, Var("y".to_string())];

        let output = rco_exp(input).unwrap();
        assert_eq!(output, expect);

    }
}
