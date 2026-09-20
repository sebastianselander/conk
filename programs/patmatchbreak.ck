type List {
    Nil,
    Cons(int, List),
}

def main() {
    let mut list = Cons(3, Cons(2, Cons(1, Nil)));
    loop {
        match list {
            Nil => {
                printString("empty\n");
                break;
            },
            Cons(x,xs) => {
                if x == 4 {
                    break
                };
                printInt(x);
                printString("\n");
                list = xs;
            },
        }
    }
}
