def foo() -> fn(int) -> int {
    return {
        \x -> x
    }
}

def main() {
    std::printInt(foo()(123));
    std::printString("\n");
}
