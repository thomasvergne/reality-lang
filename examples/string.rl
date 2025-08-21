mod string {
    type string = *u8;
}


mod number {
    type number = i32;

    mod float {
        type float = f32;
    }
}


fn add[X](x: X, y: X) -> X {
    x + y
}
