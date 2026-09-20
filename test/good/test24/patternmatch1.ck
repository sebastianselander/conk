// declare an algebraic data type
type Foo {
    Bar,
    Foo,
    Baz(int, Foo),
}

def main() {
    let var = Baz(420, Baz(69, Bar));
    // pattern match
    let x = match var {
        Baz(n,m) => {
            std::printInt(n); 
            std::printString("\n");
            match m {
                Baz(k, x) => {
                    std::printInt(k);
                    std::printString("\n");
                    match x {
                        Bar => std::printString("got bar\n")
                    }
                }
            }
        }
    };
}
