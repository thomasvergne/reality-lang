import string;

enum Option<T> {
    Some(T),
    None
}

impl fn (c: Option<T>) show_prec<T>(i: int) -> String {
    if c is Some(let v) {
        "Some(" + show_prec(v, i + 1) + ")"
    } else {
        "None"
    }
}

mod Option {
    fn map<A, B>(c: Option<A>, f: fn(A) -> B) -> Option<B> {
        if c is Some(let v) {
            Some(f(v))
        } else {
            None
        }
    }

    impl fn (c: Option<A>) or_else<A>(d: Option<A>) -> Option<A> {
        if c is Some(let v) {
            Some(v)
        } else {
            d
        }
    }

    fn get_or_else<A>(c: Option<A>, default: A) -> A {
        if c is Some(let v) {
            v
        } else {
            default
        }
    }

    fn is_some<A>(c: Option<A>) -> bool {
        if c is Some(let x) {
            true
        } else {
            false
        }
    }

    fn is_none<A>(c: Option<A>) -> bool {
        if c is None {
            true
        } else {
            false
        }
    }
}
