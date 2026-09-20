type Foo {
    Bar,
    Baz(),
}

def fun(a: Foo) {
    std.printString("got thing of type foo\n");
}

def main() {
    let b = Bar;
    let f = Baz;
    let thing: Foo = f();
    fun(thing)
}
