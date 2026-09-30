def compose<A,B,C>(f: fn(B) -> C, g: fn(A) -> B, x: A) -> C {
    f(g(x))
}

def inc(x: int) -> int {
    x + 1
}

def show(x: bool) -> string {
    if x { "yes" } else { "no" }
}

def main() {
    let y = compose(show, \(n: int) -> n > 0, 3);
    std::printString(y);
    std::printChar('\n')
}
