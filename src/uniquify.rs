use std::collections::{HashMap, VecDeque};
use super::parser::r1::*;

type Alist = HashMap<String, usize>;

// TODO more descriptive errors
pub enum UniquifyError {
    GenericError,
}

fn lookup(s: &String, alist: &Alist) -> Result<usize, UniquifyError> {
    match alist.get(s) {
        Some(n) => Ok(n.clone()),
        None => Err(UniquifyError::GenericError),
    } 
}

fn create_name(s: &String, n: usize) -> String {
    let mut ret = String::from(s);
    ret.push_str(&n.to_string());
    ret
}

/// Given a variable name (var) and hashmap (alist), update the count of var in alist
/// This should never fail because insert is cool like that :)
fn update_alist(var: String, alist: &mut Alist) {
    match alist.get(&var) {
        Some(n) => {
            alist.insert(var, n+1);  // alist.insert() returns an Option, but we throw it away
        },
        None => {
            alist.insert(var, 1);
        }
    }
}

pub fn uniquify(expr: Expr, alist: &mut Alist) -> Result<Expr, UniquifyError> {
    match expr {
        Expr::Num(n) => Ok(Expr::Num(n)),
        Expr::Plus => Ok(Expr::Plus), 
        Expr::Negation => Ok(Expr::Negation),
        Expr::Read => Ok(Expr::Read),
        Expr::Var(v) => Ok(Expr::Var(create_name(&v, lookup(&v, alist)?))),
        Expr::Binding { var, value } => {
            let new_val = uniquify(*value, alist)?;
            //TODO fixmeeeeee
            match &*var {
                Expr::Var(v) => {
                    update_alist(v.to_string(), alist);
                },
                _ => {
                    return Err(UniquifyError::GenericError);
                }
            }
            let new_var = uniquify(*var, alist)?; // uniquify var only after updating alist
            // because binding creates a new scope so this variable is now unique
            Ok(Expr::Binding {
                var: Box::new(new_var),
                value: Box::new(new_val),
            })
        },
        Expr::List(list) => {
            let mut new_list = VecDeque::new();
            for e in list {
                 new_list.push_back(uniquify(e, alist)?);
            }
            Ok(Expr::List(new_list))
        }
    }
}
