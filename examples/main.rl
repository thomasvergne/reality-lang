const x: i64 = 5;

import string::*;

fn id[A](x: A) -> A {
    x
}

fn const[A, B](x: A) -> fn(B) -> A {
    |_: B| id(x)
}

fn main() -> i32 {
    let c = const(5)(5.4);
    c
}
