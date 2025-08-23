import string::*;

type unit = void;

extern fn printf(x: string);

struct Both[A, B] {
    a: A,
    b: B,
}

fn main() -> i32 {
    let pair = Both { a: 5, b: "test" };
    printf(pair.b);

    pair.a
}
