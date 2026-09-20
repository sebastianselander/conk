type Expr {
    Lit(int),
    Add(Expr, Expr),
    Mul(Expr, Expr),
}

def eval(e: Expr) -> int {
    match e {
        Lit(n) => n,
        Add(e1,e2) => eval(e1) + eval(e2),
        Mul(e1,e2) => eval(e1) * eval(e2),
    }
}

def main() {
    let expr = Add(Mul(Lit(6), Lit(10)), Lit(9));
    std.printInt(eval(expr));
    std.printString("\n");
}
