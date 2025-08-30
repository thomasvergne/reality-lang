import string::*;
import list::*;

fn factorial(n: i32) -> i32 {
    if n == 0 {
        1
    } else {
        n * factorial(n - 1)
    }
}

fn main(argc: i32) -> i64 {
    let list = List::new();

    List::push(list, String::new("test"));
    List::push(list, String::new("bruh"));

    print((*list)[0]);
    print((*list)[1]);

    0
}
