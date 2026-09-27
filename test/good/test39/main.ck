def id<A>(x: A) -> A {
    x
}

def main() {
    let a = id(123);
    let b = id("hej\n");
    std::printInt(a);
    std::printString(b);
}
