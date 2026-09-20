def main() {
    let f: fn() -> () = 
        \ -> {
            let a = 1;
            {
                printInt(a);
            }
        };
    f()
}
