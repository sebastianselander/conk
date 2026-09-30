def const<A>(x: A) -> fn(A) -> A {
    return \_y -> x;
}

def main() {
    std::printString(const("foo\n")("hej"));
}
