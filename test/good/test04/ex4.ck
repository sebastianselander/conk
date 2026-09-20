def foo() -> int {
    loop {
        break loop {
            break 3;
        }
    }
}

def main() {
    std::printInt(foo());
    std::printChar('\n');
}
