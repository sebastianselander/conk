def id<A>(x: A) -> A {
    x
}

def main() {
    let x = id(123);
    let y = id("hej\n");
    std::printInt(x);
    std::printString(y);
}
