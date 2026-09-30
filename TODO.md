# BUG

Named functions are not passed as closures -- they are missing environments


This program for instance:

```rust
const static_string_0: [4 x char] = yes\00

const static_string_1: [3 x char] = no\00

def lambda_0 (env: void*,x$1: ptr) -> ptr {
     let f$1 = env[0]
     let g$1 = env[1]
     let named_0: ptr = get(g$1, 0)(get(g$1, 1),x$1)
     let named_1: ptr = get(f$1, 0)(get(f$1, 1),named_0)
     return named_1
}

def lambda_1 (env: void*,n$1: int64) -> () {
     let named_6: int64 = 0
     return n$1 > named_6
}

def compose (env: void*,
f$1: {fn(ptr) -> ptr, void*},
g$1: {fn(ptr) -> ptr, void*}) -> {fn(ptr) -> ptr, void*} {
     return {lambda_0, [f$1, g$1]}
}

def inc (env: void*,x$2: int64) -> int64 {
     let named_2: int64 = 1
     return x$2 + named_2
}

def show (env: void*,x$3: ()) -> string {
     let declare_0: string
     if x$3 {
         let named_3: string = static_string_0
         declare_0: string = named_3
     } else {
         let named_4: string = static_string_1
         declare_0: string = named_4
     }
     let named_5: string = declare_0
     return named_5
}

def main () -> () {
     let named_7: {fn(int64)
     ->
     string, void*} = compose(null,show,{lambda_1, []})
     let f$2: {fn(int64) -> string, void*} = named_7
     let named_8: int64 = 3
     let named_9: string = get(f$2, 0)(get(f$2, 1),named_8)
     let y$1: string = named_9
     let named_10: () = printString(null,y$1)
     let named_11: char = '\n'
     let named_12: () = printChar(null,named_11)
     named_12
}
```

the second argument to the call of `compose` passes `show`, it has to be passed like `{show, []}`.
