def id<A>(x: A) -> A {
    x
}

def main() {
    let b = id("hej\n");
    std::printString(b);
}
