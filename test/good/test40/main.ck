type Box {
    Box(int)
}

def apply<A, B>(f: fn(A) -> B, a: A) -> B {
    f(a)
}

def main() {
    let boxed = apply(Box, 123);
    match boxed {
        Box(n) => std::printInt(n),
    };
    std::printChar('\n')
}
