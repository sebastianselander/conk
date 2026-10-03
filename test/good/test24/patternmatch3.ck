// declare an algebraic data type
type Foo {
    Bar,
    Foo,
    Baz(int, string, int, int, bool),
}

def main() {
    let var = Baz(420, "hej", 69, 1337, false);
    // pattern match
    let x = match var {
        Baz(n,str, m, k, b) => {
            std::printInt(n);
            std::printString("\n");
            std::printString(str);
            std::printString("\n");
            std::printInt(m);
            std::printString("\n");
            std::printInt(k);
            std::printString("\n");
            printBool(b);
            std::printString("\n");
        }
    };
}
 
def printBool(b: bool) -> () {
    if b {
        std::printString("true")
    } else {
        std::printString("false")
    }
}
