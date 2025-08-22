const x: i64 = 5;

import string::*;

fn id[T](x: T) -> T {
    x
}

fn const[X, Y](x: X) -> fn(Y) -> X {
    |_: Y| x
}

fn main() -> i32 {
    let y = id(x);
    let z = id("test");
    let a = const(5)("test");
    80
}
