import b;
def a(x: int) -> int {
    if x > 1000 {
        return x
    };
    std::printString("inside a ");
    std::printInt(x);
    std::printChar('\n');
    b::b(x + 1)
}
