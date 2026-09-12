# conk

## Installation

### Dependencies
- ghc >= 9.8.2
- cabal >= 3.10
- llvm >= 18

If Nix is available the `nix develop` will open a development shell with all
currently neccesary dependencies.

### Build

```
git clone --depth 1 git@github.com:sebastianselander/conk.git
cd conk
cabal build
```

On compiling a program no executable is generated, but rather the LLVM-IR code is written to
a file called "out.ll" in the current working directory.

## Language

Please create an issue if you encounter any bug or unexpected behaviour (which you will).

### Program

A Conk program consists of a list of top level definitions, where a top
level definition is a function.

```python
def main() -> () {
    printInt(add(3,2));
}

def add(a: int, b: int) -> int {
    a+b
}
```
### Functions

A function is declared using the keyword `def`

```c
// These are equivalent

def foo() -> () {
    printInt(1);
}

def bar() -> () {
    printInt(1)
}
def baz() {
    printInt(1);
    return ();
}
```

If the return type of the function is omitted it returns the type `()`, verbally: unit

A function consists of a list of expressions terminated by a semicolon.
If the semicolon for the last expression is omitted then that expression will be returned.

```c
//These are equivalent

def foo() -> int {
    123
}

def foo() -> int {
    return 123;
}
```

### Expressions

- literal: `123`, `true`
- variable: `x`,`y`, `_x`, `x_1` etc.
- binary operators: 
    - `<expr> * <expr>`
    - `<expr> / <expr>`
    - `<expr> % <expr>`
    - `<expr> + <expr>`
    - `<expr> - <expr>`
    - `<expr> <= <expr>`
    - `<expr> >= <expr>`
    - `<expr> < <expr>`
    - `<expr> > <expr>`
    - `<expr> == <expr>`
    - `<expr> != <expr>`
    - `<expr> && <expr>`
    - `<expr> || <expr>`
- prefix operators: `- <expr>`, `! <expr>`
- application: `f(x)`
- variable declaration: `let x = <expr>`, `let mut x = <expr>`, `let x: int = <expr>` or a combination.
- assignment: `x = 3`, however that only works if `x` is explicitly said to be mutable. Also the classic `+=` etc.
- block: 
    - ```{ let x = 0; printInt(x) }```
- return: `return`, `return <expr>`
- break  `break`,`break <expr>`
- if, if-else: `if !true { printInt(123) }`, `if x == 1 { printInt(1) } else { printInt(0) }`
- while: `while <expr> { printInt(1) }`
- loop: `loop { printInt(1) }`

Using loop we can break with values, e.g.
```c
def main() {
    let mut x = 0; //the type of x is inferred to int
    let y = loop {
        if x > 3 {
            break 420;
        }
        x += 1
        printInt(x);
    }
    printInt(x); // 420 is printed
}
```

### Types
Currently existing types are 
- `int`
- `char`
- `string`
- `double`
- `()`
- function types `fn(int, int) -> int`

NOTE: `char`, don't compile correctly yet.

### Identifiers
Identifiers can start with either a lower case letter or an underscore, and can be followed by the same, but also a number.

## Compiler

The compiler currently consists of several stages.
- Parsing: self-explanatory
- Renaming: variable disambiguation.
- Statement check: Check that breaks only exist in loops, all functions have a return statement etc.
- Typechecker: Check for type correctness.
- Desugaring: Transform the tree to a smaller common version, and transform statements to more easily generated.
- Code generation: Generate LLVM-IR code.
