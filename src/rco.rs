use std::collections::VecDeque;
use super::parser::r1::*;

#[derive(Debug)]
pub enum RcoError {
    GenericError,
}

type Alist = HashMap<String, usize>;

fn rco_arg(expr: Expr) -> Result<Expr, RcoError> {
    match expr {
        Expr::Binding => {},
        Expr::List(v) => {
            rco_exp(v)
        },
        other => Ok(other)

    }
}


/// given (+ (- 2) 2)
// rco_exp(given)
// match list, loop over it
// ..1 call rco_arg(+) >> +
// ..2 call rco_arg( (- 2) ) >>>
// >>> call rco_exp( (- 2) ) ; loop over it
// ....1 call rco_arg( - ) >> -
// ....2 call rco_arg( 2 ) >> 2
// ....done looping return 42
// <<< rco_arg() also returns 42
// ..3 call rco_arg( 2 ) >> 2
// ....done looping return 42

/// given (+ (- 2) 2)
/// expect (let ([tmp1 (- 2)]) (+ tmp1 2))
fn rco_exp(expr: Expr) -> Result<Expr, RcoError>  {
    match expr {
        Expr::List(vec) => {
            for v in vec {
                println!("{:?}", rco_arg(v)?);
            }
            Ok(Expr::Num(42))
        },
        other => Ok(other)
    }
}