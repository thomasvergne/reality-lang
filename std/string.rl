import std.internal.gc;

extern fn ptr_add<A>(ptr: A, offset: int) -> A;
extern fn fetch_ptr<A>(ptr: A, index: int) -> A;

impl fn (x: string) length() -> int {
    strlen(x)
}

impl fn (x: string) equals(y: string) -> bool {
    string_eq(x, y)
}

struct String {
    data: string,
    length: int
};


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

extern fn string_to_float(str: string) -> float;
extern fn string_to_int(str: string) -> int;


impl fn (x: int) into_float() -> float {
    int_to_float(x)
}

impl fn (x: float) into_int() -> int {
    float_to_int(x)
}

impl fn (x: String) into_float() -> float {
    string_to_float(x.data)
}

impl fn (x: String) into_int() -> int {
    string_to_int(x.data)
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

mod GC {
    fn red(message: String) -> String {
        "\x1b[31m" + message + "\x1b[0m"
    }

    pub fn panic<A>(message: A) -> never {
        let msg_prefix = GC.red("[Panic]: ");
        let full_message = msg_prefix + message.show();
        panic_ext(full_message.data)

        undefined()
    }
}

impl fn (x: String) repeat(n: int) -> String {
    let result = "";
    let i = 0;

    while i < n {
        result = result + x;
        i = i + 1;
    };

    result
}

impl fn (x: String) trim_left() -> String {
    let start = 0;
    while start < x.length && x.get_index(start) == ' ' {
        start = start + 1;
    };

    let result = "";
    let i = start;
    while i < x.length {
        result = result + String.from_char(x.get_index(i));
        i = i + 1;
    };

    return result;
}

impl fn (x: String) trim_right() -> String {
    let end = x.length - 1;
    while end >= 0 && x.get_index(end) == ' ' {
        end = end - 1;
    };

    let result = "";
    let i = 0;
    while i <= end {
        result = result + String.from_char(x.get_index(i));
        i = i + 1;
    };

    return result;
}

impl fn (x: String) trim() -> String {
    return x.trim_left().trim_right();
}

impl fn (x: String) clone() -> String {
    let result = "";
    let i = 0;
    while i < x.length {
        result = result + String.from_char(x.get_index(i));
        i = i + 1;
    };

    return result;
}

impl fn (x: String) ends_with(suffix: String) -> bool {
    if suffix.length > x.length {
        return false;
    };

    let start = x.length - suffix.length;
    let i = 0;
    while i < suffix.length {
        if x.get_index(start + i) != suffix.get_index(i) {
            return false;
        };

        i = i + 1;
    };

    return true;
}

impl fn (x: String) starts_with(prefix: String) -> bool {
    if prefix.length > x.length {
        return false;
    };

    let i = 0;
    while i < prefix.length {
        if x.get_index(i) != prefix.get_index(i) {
            return false;
        };

        i = i + 1;
    };

    return true;
}

impl fn (x: String) replace(old: String, new_str: String) -> String {
    let result = "";
    let i = 0;

    while i < x.length {
        if x.slice(i, i + old.length).equals(old) {
            result = result + new_str;
            i = i + old.length;
        } else {
            result = result + String.from_char(x.get_index(i));
            i = i + 1;
        };
    };

    return result;
}
