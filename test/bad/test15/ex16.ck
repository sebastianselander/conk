def main() {
    foo(20)(20)(29);
}

def foo(a: int) -> fn(int) -> (fn(int) -> ()) {
    return \b -> \c -> { std::printInt(a + b + c); std::printString("\n"); }
}
