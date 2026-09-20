// declare an algebraic data type
type List {
    Nil,
    Cons(int, List),
}

def main() {
    let ls = Cons(420, Cons(58, Cons(69, Cons(1337, Nil))));
    let len = length(ls);
    printString("length is: ");
    printInt(len);
    printString("\n");
    let sum = sum(ls);
    printString("sum is: ");
    printInt(sum);
    printString("\n");
    printString("head:");
    printInt(head(ls));
    printString("\n");
    printList(ls);
    printString("\n");
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
    printString("[");
    match xs {
        Nil => (),
        Cons(x,xs) => {
            printInt(x);
            _printList(xs);
        },
    };
    printString("]");
}
def _printList(xs: List) {
    match xs {
        Nil => (),
        Cons(x, xs) => {
            printString(", ");
            printInt(x);
            _printList(xs);
        } 
    }
}
