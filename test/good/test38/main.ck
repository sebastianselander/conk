def id<A>(x: A) {
    x
}

def main() {
    std::printString(id("hej"));
    std::printInt(id(69));
    std::printChar(id('\n'));
}
