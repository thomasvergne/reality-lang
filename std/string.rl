type string = *char;

extern fn ptr_add<A>(ptr: A, offset: int) -> A;
extern fn fetch_ptr<A>(ptr: A, index: int) -> A;

enum unit {
    unit
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

property add<A>(x: A, y: A) -> A;
property sub<A>(x: A, y: A) -> A;
property mul<A>(x: A, y: A) -> A;
property div<A>(x: A, y: A) -> A;
property modulo<A>(x: A, y: A) -> A;
property greater<A>(x: A, y: A) -> bool;
property lesser<A>(x: A, y: A) -> bool;

property equals<A>(x: A, y: A) -> bool;

extern fn number_to_string(n: int) -> string;
extern fn pointer_to_string<A>(p: A) -> string;
extern fn add_number(a: int, b: int) -> int;
extern fn sub_number(a: int, b: int) -> int;
extern fn mul_number(a: int, b: int) -> int;
extern fn div_number(a: int, b: int) -> int;
extern fn equals_number(a: int, b: int) -> bool;
extern fn greater_number(a: int, b: int) -> bool;
extern fn less_number(a: int, b: int) -> bool;
extern fn mod_number(a: int, b: int) -> int;
extern fn string_eq(a: string, b: string) -> bool;

fn int_to_string(n: int) -> string {
    number_to_string(n)
}

impl fn (x: int) add(y: int) -> int {
    add_number(x, y)
}

impl fn (x: int) sub(y: int) -> int {
    sub_number(x, y)
}

impl fn (x: int) mul(y: int) -> int {
    mul_number(x, y)
}

impl fn (x: int) div(y: int) -> int {
    div_number(x, y)
}

impl fn (x: int) equals(y: int) -> bool {
    equals_number(x, y)
}

impl fn (x: int) greater(y: int) -> bool {
    greater_number(x, y)
}

impl fn (x: int) lesser(y: int) -> bool {
    less_number(x, y)
}

impl fn (x: int) modulo(y: int) -> int {
    mod_number(x, y)
}

fn great_equals<A>(x: A, y: A) -> bool {
    or(greater(x, y), equals(x, y))
}

fn less_equals<A>(x: A, y: A) -> bool {
    or(lesser(x, y), equals(x, y))
}

fn not_equals<A>(x: A, y: A) -> bool {
    not(equals(x, y))
}

struct String {
    data: string,
    length: int
};

extern fn malloc_string(s: string) -> string;
extern fn strlen(s: string) -> int;
extern fn strcat(x: string, y: string) -> string;
extern fn concat_strings(a: string, b: string) -> string;
extern fn char_eq(a: char, b: char) -> bool;
extern fn string_to_int(s: string) -> int;

impl fn (x: string) get_index(index: int) -> char {
    let ptr = ptr_add(x, index * sizeof(char));
    *ptr as char
}

impl fn (x: string) get_index_mut(index: int) -> *char {
    let ptr = ptr_add(x, index * sizeof(char));
    ptr as *char
}

impl fn (x: String) get_index(index: int) -> char {
    let ptr = ptr_add(x.data, index * sizeof(char));
    *ptr as char
}

impl fn (x: String) get_index_mut(index: int) -> *char {
    let ptr = ptr_add(x.data, index * sizeof(char));
    ptr as *char
}

mod String {
    fn init(data: string) -> String {
        let string_mem = malloc_string(data);

        struct String {
            data: string_mem,
            length: strlen(string_mem)
        }
    }

    fn from_char(c: char) -> String {
        let s = "0";
        *s.data.get_index_mut(0) = c;

        s
    }
}

impl fn (x: String) add(y: String) -> String {
    let result = concat_strings(x.data, y.data);

    struct String {
        data: result,
        length: strlen(result)
    }
}

extern fn printf(s: string) -> int;

property show_prec<A>(x: A, i: int) -> String;

impl fn (x: String) show_prec(i: int) -> String {
    if i > 0 {
        "\"" + x + "\""
    } else {
        x
    }
}

impl fn (x: int) show_prec(_: int) -> String {
    String.init(int_to_string(x))
}

impl fn (x: int) show_prec(_: int) -> String {
    String.init(number_to_string(x))
}

impl fn (x: bool) show_prec(_: int) -> String {
    if (x) { "true" } else { "false" }
}

impl fn (x: *A) show_prec<A>(i: int) -> String {
    show_prec(*x, i)
}

fn show<A>(x: A) -> String {
    show_prec(x, 0)
}

fn print<A>(x: A) -> int {
    printf((show(x) + "\n").data)
}

impl fn (x: String) equals(y: String) -> bool {
    string_eq(x.data, y.data)
}

impl fn (x: char) equals(y: char) -> bool {
    char_eq(x, y)
}

extern fn int_to_char(x: int) -> char;
extern fn char_to_int(c: char) -> int;

impl fn (x: int) to_char() -> char {
    int_to_char(x)
}

impl fn (x: char) from_char() -> int {
    char_to_int(x)
}

impl fn (x: String) to_int() -> int {
    string_to_int(x.data)
}

property into_int<A>(x: A) -> int;
property into_float<A>(x: A) -> float;

impl fn (x: int) into_int() -> int {
    x
}

impl fn (x: float) into_float() -> float {
    x
}

extern fn int_to_float(x: int) -> float;
extern fn float_to_int(x: float) -> int;

impl fn (x: int) into_float() -> float {
    int_to_float(x)
}

impl fn (x: float) into_int() -> int {
    float_to_int(x)
}

fn float<A>(x: A) -> float {
    return x.into_float();
}

fn int<A>(x: A) -> int {
    return x.into_int();
}

extern fn add_float_(a: float, b: float) -> float;
extern fn sub_float_(a: float, b: float) -> float;
extern fn mul_float_(a: float, b: float) -> float;
extern fn div_float_(a: float, b: float) -> float;
extern fn equals_float_(a: float, b: float) -> bool;
extern fn greater_float_(a: float, b: float) -> bool;
extern fn less_float_(a: float, b: float) -> bool;
extern fn mod_float_(a: float, b: float) -> float;
extern fn float_to_string(f: float) -> string;

impl fn (x: float) add(y: float) -> float {
    add_float_(x, y)
}
impl fn (x: float) sub(y: float) -> float {
    sub_float_(x, y)
}

impl fn (x: float) mul(y: float) -> float {
    mul_float_(x, y)
}

impl fn (x: float) div(y: float) -> float {
    div_float_(x, y)
}

impl fn (x: float) equals(y: float) -> bool {
    equals_float_(x, y)
}

impl fn (x: float) greater(y: float) -> bool {
    greater_float_(x, y)
}

impl fn (x: float) lesser(y: float) -> bool {
    less_float_(x, y)
}

impl fn (x: float) show_prec(i: int) -> String {
    String.init(float_to_string(x))
}

impl fn (x: float) negate() -> float {
    0.0.sub(x)
}
