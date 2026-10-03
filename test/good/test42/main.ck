def apply<A,B>(f: fn(A) -> B, x: A) -> B {
    f(x)
}
def main() {
    let x = apply(\(x: int) -> x + 321, 123);
    std::printInt(x);
    std::printChar('\n')
}
