def const<A,B>(x: A, y: B) -> A {
    x
}

def main() {
    std::printString(const(const("bar\n", 123), "foo"))
}
