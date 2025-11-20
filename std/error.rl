pub enum Error<Success, Failure> {
    Ok(Success),
    Err(Failure)
}

impl fn (self: Error<Success, Failure>) show_prec<Success, Failure>(prec: i32) -> String {
    if self is Ok(let vl) {
        return "Ok(" + vl.show_prec(prec + 1) + ")";
    } else if self is Err(let el) {
        return "Err(" + el.show_prec(prec + 1) + ")";
    }
}

