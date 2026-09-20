type Foo {
    Foo(int, int),
}

def main() {
    match Foo(3, 3) {
        Foo(x,x) => std::printInt(x),
    };
}
