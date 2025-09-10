import string::*;
import list::*;
import actor;
import option;
import linked_list;

fn factorial(n: u64) -> u64 {
    if n == 0 {
        1u64
    } else {
        n * factorial(n - 1)
    }
}

fn main(args: List[String]) -> i32 {
    args.map(print);

    return 0;
}
