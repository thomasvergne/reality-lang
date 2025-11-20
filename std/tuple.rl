import string;

enum Tuple<A, B> {
    Pair(A, B)
}

impl fn (t: Tuple<A, B>) show_prec<A, B>(prec: i32) -> String {
    if t is Pair(let a, let b) {
        "("
            + a.show_prec(prec + 1)
            + ", "
            + b.show_prec(prec + 1)
            + ")"
    } else {
        "<invalid tuple>"
    }
}
