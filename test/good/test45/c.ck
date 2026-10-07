import a;
def c(x: int) -> int {
    if x > 1000 {
        return x
    };
    std::printString("inside c ");
    std::printInt(x);
    std::printChar('\n');
    a::a(x + 1)
}
