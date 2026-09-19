// declare an algebraic data type
type List {
    Nil,
    Cons(int, List),
}

def main() {
    let ls = Cons(420, Cons(58, Cons(69, Cons(1337, Nil))));
    let len = length(ls);
    std.printString("length is: ");
    std.printInt(len);
    std.printString("\n");
    let sum = sum(ls);
    std.printString("sum is: ");
    std.printInt(sum);
    std.printString("\n");
    std.printString("head: ");
    std.printInt(head(ls));
    std.printString("\n");
    printList(ls);
    std.printString("\n");
}

def length(xs: List) -> int {
   match xs {
       Cons(x, xs) => {
           1 + length(xs)
       },
       Nil => 0,
   }
}


def sum(xs: List) -> int {
   match xs {
       Cons(x, xs) => x + sum(xs),
       Nil => 0,
   }
}

def head(xs: List) -> int {
    match xs {
        Cons(x,tail) => x
    }
}

def printList(xs: List) {
    std.printString("[");
    match xs {
        Nil => (),
        Cons(x,xs) => {
            std.printInt(x);
            _printList(xs);
        },
    };
    std.printString("]");
}
def _printList(xs: List) {
    match xs {
        Nil => (),
        Cons(x, xs) => {
            std.printString(", ");
            std.printInt(x);
            _printList(xs);
        } 
    }
}
