def apply<A,B>(f: fn(A) -> B, x: A) -> B {
    f(x)
}
def main() {
    let y = 321;
    let x = apply(\(x: int) -> x + y, 123);
    std::printInt(x);
    std::printChar('\n')
}
