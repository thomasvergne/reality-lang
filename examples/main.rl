const x: i64 = 5;

import string::*;

fn id[T](x: T) -> T {
    x
}

fn main() -> i32 {
    let x = id(9);
    x
}
