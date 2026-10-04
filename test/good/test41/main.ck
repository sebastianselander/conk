def apply<A,B>(f: fn(A) -> B, x: A) -> B {
    f(x)
}

def id<A>(x: A) -> A {
    x
}

def main() {
    let x = apply(id, "hej\n");
    std::printString(x);
}
