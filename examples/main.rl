const x: i64 = 5;

import string::*;

fn id[T](x: T) -> T {
    x
}

fn main() -> i32 {
    let id_ = |x| x;
    let y = id(x);
    80
}
