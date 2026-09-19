def main() {
    let f: fn(int) -> int = \x -> x + 1;
    let g: fn(int) -> int = \x -> x + 1;
    let h: fn(int) -> int = \x -> x + 1;
    let i: fn(int) -> int = \x -> x + 1;
    std.printInt(f(g(h(i(1)))));
    std.printString("\n");

    let f: fn(int) -> fn(int) -> fn(int) -> fn(int) -> fn(int) -> int = \a -> \b -> \c -> \d -> \e -> a + b + c + d + e;
    std.printInt(f(1)(1)(1)(1)(1));
    std.printString("\n")
}
