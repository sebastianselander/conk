import c;
def b(x: int) -> int {
    if x > 1000 {
        return x
    };
    std::printString("inside b ");
    std::printInt(x);
    std::printChar('\n');
    c::c(x + 1)
}
