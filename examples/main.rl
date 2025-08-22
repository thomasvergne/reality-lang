const x: i64 = 5;

import string::*;

fn id[T](x: T) -> T {
    x
}

fn main() -> i32 {
    let y = id(x);
    let z = id("test");
    80
}
