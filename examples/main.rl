const x: i64 = 5;
import string;

fn id[T](x: T) -> T {
    x
}

fn main() -> i32 {
    let id_ = |x| x;
    let x = id(5);
    let y = id_("test");
    let z = id_(8);
    let a = id("test");
    x
}
