import string::*;
import list::*;

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

fn main() -> i64 {
    let list = List::new();

    List::push(list, |x| x + "test");
    List::push(list, |x| x + "bruh");

    let new_list = List::map(list, |f| {
        |x| f(x)
    });

    let result = List::map(new_list, |f| {
        print(f("test"));
    });

    let id = |x| x;

    print(apply(id, 5));

    print(*result);

    0
}
