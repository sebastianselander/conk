def foo() -> fn(int) -> int {
    return loop {
        break \x -> x;
    }
}

def main() {
    std::printInt(foo()(123));
    std::printString("\n");
}
