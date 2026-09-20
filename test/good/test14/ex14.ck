def foo(x: int) -> fn(int, string, bool, int) -> () {
    return \a b c d -> {
        std::printInt(a);
        std::printString("\n");
        std::printString(b);
        std::printString("\n");
        printBool(c);
        std::printString("\n");
        std::printInt(d+x);
        std::printString("\n");
    }
}

def main() {
    let f = foo(370);
    f(10,"yoo",true,50);
}


def printBool(b: bool) {
    if b {
        std::printString("true")
    } else {
        std::printString("false")
    }
}
