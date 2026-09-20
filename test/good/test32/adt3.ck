type Foo {
    Bar,
    Baz(),
}

def fun(f: fn() -> Foo) {
    let a: Foo = f();
    std::printString("got thing of type foo\n");
}

def main() {
    let b = Bar;
    let f = Baz;
    fun(f)
}
