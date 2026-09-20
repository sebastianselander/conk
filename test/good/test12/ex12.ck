def apply(f: fn(int) -> int) -> fn(int) -> int {
    return {
        f
    }
}

def main() {
    std::printInt(apply(\x -> x)(123));
    std::printString("\n");
}
