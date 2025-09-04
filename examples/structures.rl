import string::*;
import list::*;
import actor;

fn factorial(n: i32) -> i32 {
    if n == 0 {
        1
    } else {
        n * factorial(n - 1)
    }
}

fn apply[A](f: fn(A) -> A, x: A) -> A {
    f(x)
}

fn main(argc: i32, argv: *string) -> i32 {
    let a = Actor::init::[i32]();

    let thread = Actor::listen(a, |msg| {
        print(msg)
    });

    Actor::send(a, 42);
    Actor::send(a, 43);

    Actor::close(a);

    pthread_join(thread, 0);

    0
}
