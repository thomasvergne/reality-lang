extern let intrinsic: void;

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


extern fn malloc_string(s: string) -> string;
#[intrinsic] {
    extern fn strcat(x: string, y: string) -> string;
    extern fn printf(s: string) -> int;
    extern fn strlen(s: string) -> int;
}
extern fn concat_strings(a: string, b: string) -> string;
extern fn char_eq(a: char, b: char) -> bool;
extern fn string_to_int(s: string) -> int;

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

mod GC {
    struct GarbageCollector { };
    
    extern fn undefined() -> never;
    extern fn exit_program(code: int) -> never;

    fn exit(code: int) -> never {
        exit_program(code)
    }

    #[intrinsic] {
        extern fn GC_malloc<A>(size: int) -> A;
        extern fn GC_free<A>(ptr: A) -> unit;
        extern fn GC_realloc<A>(ptr: A, size: int) -> A;
    }

    extern fn panic_ext(message: string) -> unit;

    pub fn malloc<A>() -> *A {
        GC_malloc<*A>(sizeof(A))
    }

    pub fn allocate<A>(value: A) -> *A {
        let ptr = GC.malloc<A>();
        *ptr = value;
        ptr
    }

    pub fn calloc<A>(count: int) -> *A {
        GC_malloc(count * sizeof(A))
    }

    pub fn realloc<A>(ptr: *A, count: int) -> *A {
        GC_realloc(ptr, count)
    }

    pub fn free<A>(ptr: *A) -> unit {
        GC_free(ptr)
    }
}
