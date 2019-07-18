use super::parser::r1::Expr;

// This allow prevents the compiler from spitting out warnings during a 'cargo build'
// since it's only used during testing
#[allow(dead_code)]
pub fn generate_binding(var: &str, value: u64) -> Expr {
    Expr::Binding {
        var: Box::new(Expr::Var(var.to_string())),
        value: Box::new(Expr::Num(value)),
    }
}

#[allow(dead_code)]
pub fn generate_binding_expr(var: &str, expr: Expr) -> Expr {
    Expr::Binding {
        var: Box::new(Expr::Var(var.to_string())),
        value: Box::new(expr),
    }
}
