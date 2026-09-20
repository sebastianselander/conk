// all break expressions have to have the same type
def foo(x: int) -> int {
        let x = loop {
            if true {
                break 3;
            } else {
                break true;
            };
        };
        3
}
