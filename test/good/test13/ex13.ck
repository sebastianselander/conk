// BUG AS FUCK, it works with 1 fewer arguments to lambda
def foo(x: int) -> fn(int,int,int,int) -> int {
    return \a b c d -> a + b + c + d + x
}

def main() {
    std::printInt(foo(62)(1,1,2,3));
    std::printString("\n");
}
