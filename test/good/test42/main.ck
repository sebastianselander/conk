def apply<A,B>(f: fn(A) -> B, x: A) -> B {
    f(x)
}

def id<A>(x: A) -> A {
    x
}

def main() {
    let f = id;
    let two = f(69);
    let o = id('o');
    let hej = apply(id,"hej\n");
    let unit = apply(f, ());
    std::printInt(two);
    std::printChar(o);
    std::printString(hej);
}

