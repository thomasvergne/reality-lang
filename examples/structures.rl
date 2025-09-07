import string::*;
import list::*;
import actor;
import option;

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
    let a = Actor::init();

    let thread = Actor::listen(a, |msg| {
        print(msg)
    });

    Actor::send(a, Some(String::new("Hello from actor!")));
    Actor::send(a, Some(String::new("Another message.")));

    Actor::close(a);

    pthread_join(thread, 0);

    let x = Some::[i32](6);

    print(Option::get_or_else(Option::map(x, factorial), 0));

    let test = Some(String::new("test"));

    print(String::new("test"));

    0
}
