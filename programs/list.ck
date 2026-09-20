type List {
    Nil,
    Cons(int, List),
}

def map(f: fn(int) -> int, xs: List) -> List {
   match xs {
       Nil => Nil,
       Cons(head, tail) => Cons(f(head), map(f, tail)),
   } 
}

def from_to(from: int, to: int) -> List {
    let mut x = from;
    let mut list = Nil;
    while x < to {
        list = Cons(x, list);
        x += 1;
    };
    reverse(list)
}

def reverse(xs: List) -> List {
    _reverse(Nil, xs)
}

def _reverse(acc: List, xs: List) -> List {
    match xs {
        Nil => acc,
        Cons(head, tail) => _reverse(Cons(head, acc), tail)
    }
}

def head(xs: List) -> int {
    match xs {
        Cons(x, tail) => x,
    }
}

def tail(xs: List) -> List {
    match xs {
        Cons(x, tail) => tail,
    }
}

def filter(p: fn(int) -> bool, xs: List) -> List {
    match xs {
        Nil => Nil,
        Cons(head, tail) => {
            if p(head) {
                Cons(head, filter(p, tail))
            } else {
                filter(p, tail)
            }
        }
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
    printString("]\n");
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

def main() {
    let list = from_to(1, 10);
    let val = 4;
    printList(list);
    printList(map(\x -> x + 1, list));
    printList(map(\x -> x + val, list));
    printList(tail(list));
    printInt(head(list));
    printString("\n");
    printList(filter(\x -> x % 2 == 0, list));
    printList(filter(\x -> x % 2 == 1, list));
}
