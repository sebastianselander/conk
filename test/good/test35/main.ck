def apply<A>(f: fn(A) -> A, x: A) -> A {
    f(x)
}

def main() {
    let inc = \(x: int) -> x + 1;
    let x = apply(inc, 1);
    std::printInt(x);
    std::printChar('\n')
}
