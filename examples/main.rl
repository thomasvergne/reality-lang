const x: i32 = 5;

import imports::*;

fn id[A](x: A) -> A {
    x
}

fn cnst[A, B](x: A) -> fn(B) -> A {
    |_: B| id(x)
}

fn main() -> i32 {
    let id_ = |x| x;
    let c = cnst(id_(x))(5.4);
    c
}
