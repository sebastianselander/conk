def main() {
    let i = 0;
    let x = loop {
        if (i < 10) {
            std.printInt(i);
            std.printString("\n");
            i += 1;
        } else {
            break 123
        }
    };
    std.printInt(x);
    std.printString("\n");
}
