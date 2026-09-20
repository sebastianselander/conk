def main() {
    let f: fn(int, int) -> int = \x y -> x + x;
    std::printInt(f(420, 69));
    std::printString("\n");
}
