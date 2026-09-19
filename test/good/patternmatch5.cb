type Foo {
    Foo(int)
}

def main() {
    let thing = Foo(1337);
    let f: fn() -> () =
        \ -> match thing {
            Foo(foonumber) => {
                std.printInt(foonumber);
                std.printString("\n")
            },
        };
    f()
}
