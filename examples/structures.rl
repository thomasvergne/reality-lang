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

    let new_list = List::map(list, |s| {
        print(s)
    });

    List::map(new_list, |x| {
        print(x)
    });

    0
}
