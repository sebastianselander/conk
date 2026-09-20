def main() {
    let x = 0;
    let g = loop {
        if x > 10 {
            break "done"
        };
        std::printInt(x);
        std::printString("\n");
        x += 1;
    };
    std::printString(g);
    std::printString("\n");
}
