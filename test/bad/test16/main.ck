def apply<A,B>(f: fn(A) -> B, x: A) -> B {
    f(x)
}

def id<A>(x: A) -> A {
    x
}

def main() {
    let f = id;
    let x = apply(f, "hej\n");
    let y = apply(f, 123);
    std::printString(x);
    std::printInt(y);
    std::printChar('\n');
}
