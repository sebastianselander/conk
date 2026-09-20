type Foo {
    Foo,
}
def main() {
    loop {
        match Foo {
            Foo => break,
        }
    }
}
