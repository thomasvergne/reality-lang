mod string {
    type string = *char;
}


fn and(a: bool, b: bool) -> bool {
    if a {
        b
    } else {
        false
    }
}


fn or(a: bool, b: bool) -> bool {
    if a {
        true
    } else {
        b
    }
}

fn not(a: bool) -> bool {
    if a {
        false
    } else {
        true
    }
}

property add[A](x: A, y: A) -> A;
property sub[A](x: A, y: A) -> A;
property mul[A](x: A, y: A) -> A;
property div[A](x: A, y: A) -> A;
property modulo[A](x: A, y: A) -> A;
property greater[A](x: A, y: A) -> bool;
property lesser[A](x: A, y: A) -> bool;

property equals[A](x: A, y: A) -> bool;

extern fn number_to_string(n: i32) -> string;
extern fn pointer_to_string[A](p: A) -> string;
extern fn add_number(a: i32, b: i32) -> i32;
extern fn sub_number(a: i32, b: i32) -> i32;
extern fn mul_number(a: i32, b: i32) -> i32;
extern fn div_number(a: i32, b: i32) -> i32;
extern fn equals_number(a: i32, b: i32) -> bool;
extern fn greater_number(a: i32, b: i32) -> bool;
extern fn less_number(a: i32, b: i32) -> bool;
extern fn mod_number(a: i32, b: i32) -> i32;
extern fn string_eq(a: string, b: string) -> bool;

impl fn (x: i32) add(y: i32) -> i32 {
    add_number(x, y)
}

impl fn (x: i32) sub(y: i32) -> i32 {
    sub_number(x, y)
}

impl fn (x: i32) mul(y: i32) -> i32 {
    mul_number(x, y)
}

impl fn (x: i32) div(y: i32) -> i32 {
    div_number(x, y)
}

impl fn (x: i32) equals(y: i32) -> bool {
    equals_number(x, y)
}

impl fn (x: i32) greater(y: i32) -> bool {
    greater_number(x, y)
}

impl fn (x: i32) lesser(y: i32) -> bool {
    less_number(x, y)
}

impl fn (x: i32) modulo(y: i32) -> i32 {
    mod_number(x, y)
}

extern fn add_u64_ext(a: u64, b: u64) -> u64;
extern fn sub_u64_ext(a: u64, b: u64) -> u64;
extern fn mul_u64_ext(a: u64, b: u64) -> u64;
extern fn div_u64_ext(a: u64, b: u64) -> u64;
extern fn equals_u64_ext(a: u64, b: u64) -> bool;
extern fn greater_u64_ext(a: u64, b: u64) -> bool;
extern fn less_u64_ext(a: u64, b: u64) -> bool;
extern fn mod_u64_ext(a: u64, b: u64) -> u64;
extern fn u64_to_string(n: u64) -> string;

impl fn (x: u64) add(y: u64) -> u64 {
    add_u64_ext(x, y)
}
impl fn (x: u64) sub(y: u64) -> u64 {
    sub_u64_ext(x, y)
}
impl fn (x: u64) mul(y: u64) -> u64 {
    mul_u64_ext(x, y)
}
impl fn (x: u64) div(y: u64) -> u64 {
    div_u64_ext(x, y)
}
impl fn (x: u64) equals(y: u64) -> bool {
    equals_u64_ext(x, y)
}
impl fn (x: u64) greater(y: u64) -> bool {
    greater_u64_ext(x, y)
}
impl fn (x: u64) lesser(y: u64) -> bool {
    less_u64_ext(x, y)
}
impl fn (x: u64) modulo(y: u64) -> u64 {
    mod_u64_ext(x, y)
}

fn great_equals[A](x: A, y: A) -> bool {
    or(greater(x, y), equals(x, y))
}

fn less_equals[A](x: A, y: A) -> bool {
    or(lesser(x, y), equals(x, y))
}

fn not_equals[A](x: A, y: A) -> bool {
    not(equals(x, y))
}

struct String {
    data: string,
    length: u64
};

extern fn malloc_string(s: string) -> string;
extern fn strlen(s: string) -> u32;
extern fn strcat(x: string, y: string) -> string;
extern fn concat_strings(a: string, b: string) -> string;

mod String {
    fn new(data: string) -> String {
        let string_mem = malloc_string(data);

        String {
            data: string_mem,
            length: strlen(string_mem)
        }
    }
}

impl fn (x: String) add(y: String) -> String {
    let result = concat_strings(x.data, y.data);

    String {
        data: result,
        length: strlen(result)
    }
}

extern fn printf(s: string) -> i32;

property show_prec[A](x: A, i: i32) -> String;

impl fn (x: String) show_prec(i: i32) -> String {
    if i > 0 {
        String::new("\"" + x.data + "\"")
    } else {
        x
    }
}

impl fn (x: u64) show_prec(_: i32) -> String {
    String::new(u64_to_string(x))
}

impl fn (x: string) show_prec(i: i32) -> String {
    String::new(if i > 0 { "\"" } else { "" } + x + if i > 0 { "\"" } else { "" })
}

impl fn (x: string) add(y: string) -> string {
    (String::new(x) + String::new(y)).data
}

impl fn (x: bool) show_prec(_: i32) -> String {
    String::new(if (x) { "true" } else { "false" })
}

impl fn (x: *A) show_prec[A](i: i32) -> String {
    show_prec(*x, i)
}

fn show[A](x: A) -> String {
    show_prec(x, 0)
}

fn print[A](x: A) -> i32 {
    printf((show(x) + String::new("\n")).data)
}
