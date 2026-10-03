def foo(x: int) -> fn(int,int,int,int) -> int {
    return \a b c d -> a + b + c + d
}

def main() {
    std::printInt(foo(62)(1,1,2,3));
    std::printString("\n");
}
