def foo(x: int) -> fn(int, int, int, string) -> int {
    return \a b c d -> {
        std::printString(d); 
        std::printString("\n");
        a + b + c
    }
}

def main() {
    std::printInt(foo(100)(20,20,29,"thequickbrownfoxjumpsoverthelazydog"));
    std::printString("\n");
}
