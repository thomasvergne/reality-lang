import string::*;

fn factorial(n: i32) -> i32 {
    if (n == 0) 1
    else n * factorial(n - 1)
}

fn main(argc: i32) -> i64 {
    print("Hello, world!");
    print(1 == 1);
    print(factorial(30));
    0
}
