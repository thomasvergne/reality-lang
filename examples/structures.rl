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
    print("test");
    print(4 + 3);

    let list = List::new::[i32]();

    List::push(list, 4);
    List::push(list, 5);

    print(get_index::[i32](list, 1));

    0
}
