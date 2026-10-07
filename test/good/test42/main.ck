def main() {
    let f = \x -> x + 1;
    std::printInt(f(123));
    std::printInt(f(321));
    std::printChar('\n');
}
