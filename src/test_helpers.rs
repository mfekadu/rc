use super::parser::r1::Expr;
use std::collections::VecDeque;

// This allow prevents the compiler from spitting out warnings during a 'cargo build'
// since it's only used during testing
#[allow(dead_code)]
pub fn generate_binding(var: &str, val: i64) -> Expr {
    Expr::Binding {
        var: Box::new(Expr::Var(var.to_string())),
        val: Box::new(Expr::Num(val)),
    }
}

#[allow(dead_code)]
pub fn generate_binding_expr(var: &str, expr: Expr) -> Expr {
    Expr::Binding {
        var: Box::new(Expr::Var(var.to_string())),
        val: Box::new(expr),
    }
}

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

