type Option<A> {
    Ok(int),
    None,
}

def main() {
    let some_str = Ok("hej");
    let some_int = Ok(123);
    match some_str {
        Ok(x) => printString(x),
        None => printString("<missing string>")
    }
    match some_int {
        Ok(x) => printInt(x),
        None => printString("<missing int>")
    }
}
