import string::*;

struct Both[A, B] {
    a: A,
    b: B,
}

fn main() -> i32 {
    let pair = Both { a: 5, b: "test" };
    pair.a
}
