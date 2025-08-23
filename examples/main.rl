const x: i32 = 5;

import string::*;

const y: string = "test";

fn id[A](x: A) -> A {
    x
}

fn const[A, B](x: A) -> fn(B) -> A {
    |_: B| id(x)
}

fn main() -> i32 {
    let id_ = |x| x;
    let c = const(id_(x))(5.4);
    c
}
