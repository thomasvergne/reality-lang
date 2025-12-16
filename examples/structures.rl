import std.prelude;

fn ackermann(m: int, n: int) -> int {
    if m == 0 {
        return n + 1;
    }

    if m > 0 && n == 0 {
        return ackermann(m - 1, 1);
    }
    
    return ackermann(m - 1, ackermann(m, n - 1));
}

fn main(args: List<String>) -> int {
    let m = int(args[1]);
    let n = int(args[2]);

    print(f"Ackermann({m}, {n}) = {ackermann(m, n)}");

    return 0;
}
