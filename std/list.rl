import string::*;

struct List[A] {
    data: *A,
    length: u64,
    capacity: u64
};

mod GC {
    struct GarbageCollector { };

    extern fn BDWGC_malloc[A](size: u64) -> A;
    extern fn BDWGC_free[A](ptr: A) -> unit;
    extern fn BDWGC_realloc[A](ptr: A, size: u64) -> A;

    //extern fn GC_INIT(gc: *GC::GarbageCollector, argc_ref: *i32) -> i32;
    extern fn panic_ext(message: string) -> unit;

    pub fn malloc[A]() -> *A {
        BDWGC_malloc::[*A](sizeof(A))
    }

    pub fn allocate[A](value: A) -> *A {
        let ptr = GC::malloc::[A]();
        *ptr = value;
        ptr
    }

    pub fn calloc[A](count: u64) -> *A {
        BDWGC_malloc(count * sizeof(A))
    }

    pub fn realloc[A](ptr: *A, count: u64) -> *A {
        BDWGC_realloc(ptr, count)
    }

    pub fn free[A](ptr: *A) -> unit {
        BDWGC_free(ptr)
    }

    fn red(message: String) -> String {
        "\x1b[31m" + message + "\x1b[0m"
    }

    pub fn panic[A](message: A) -> unit {
        let msg_prefix = GC::red("[Panic]: ");
        let full_message = msg_prefix + show_prec(message, 0);
        panic_ext(full_message.data)
    }
}

property get_index[Item, Container, Index](container: Container, index: Index) -> Item;

extern fn ptr_add[A](ptr: A, offset: u64) -> A;
extern fn fetch_ptr[A](ptr: A, index: u64) -> A;

impl fn (c: *A) get_index[A](index: u64) -> A {
    let ptr = ptr_add(c, index * sizeof(A));
    *ptr
}

impl fn (c: List[A]) get_index[A](index: u64) -> A {
    if index >= c.length {
        GC::panic("Index out of bounds in List::get_index, " 
            + show_prec(index, 0) 
            + " (<index>) must be less than "
            + show_prec(c.length, 0) 
            + " (<length>)"
        );
    };

    get_index(c.data, index)
}

impl fn (x: String) get_index(index: u64) -> char {
    let ptr = ptr_add(x.data, index * sizeof(char));
    *ptr as char
}

property get_index_mut[B, A](container: A, index: u64) -> *B;

impl fn (c: *A) get_index_mut[A](index: u64) -> *A {
    let ptr = ptr_add(c, index * sizeof(A));
    ptr as *A
}

impl fn (c: List[A]) get_index_mut[A](index: u64) -> *A {
    get_index_mut(c.data, index)
}

mod List {
    fn new[A]() -> *List[A] {
        let list = GC::malloc::[List[A]]();

        list->capacity = 10u64;

        list->data = GC::calloc::[A](10);
        list->length = 0u64;

        list
    }

    fn push[A](list: *List[A], value: A) -> *List[A] {
        if list->length == list->capacity {
            list->capacity = list->capacity + 10u64;
            list->data = GC::realloc(list->data, list->capacity + 10);
        };

        *get_index_mut(list->data, list->length) = value;
        list->length = list->length + 1;

        list
    }

    impl fn (list: List[A]) map[A, B](f: fn(A) -> B) -> List[B] {
        let new_list = List::new::[B]();

        let i = 0u64;

        while i < list.length {
            let value = list[i];
            List::push(new_list, f(value));
            i = i + 1;
        };

        *new_list
    }

    impl fn (list: List[A]) extend[A](other: List[A]) -> List[A] {
        let new_list = List::new::[A]();

        let i = 0u64;

        while i < list.length {
            let value = list[i];
            List::push(new_list, value);
            i = i + 1;
        };

        let j = 0u64;

        while j < other.length {
            let value = other[j];
            List::push(new_list, value);
            j = j + 1;
        };

        *new_list
    }

    fn from_pointer[A](ptr: *A, count: u64) -> *List[A] {
        let list = List::new::[A]();

        let i = 0u64;

        while i < count {
            let value = ptr[i];
            List::push(list, value);
            i = i + 1;
        };

        list
    }

    impl fn (list: List[A]) show_prec[A](prec: i32) -> String {
        let result = "[";

        let i = 0u64;

        while i < list.length {
            let value = list[i];
            result = result + show_prec(value, prec + 1);

            if i < list.length - 1 {
                result = result + ", ";
            };

            i = i + 1;
        };

        result = result + "]";

        result
    }

    impl fn (list: List[A]) slice[A](start: u64, end: u64) -> List[A] {
        let new_list = List::new::[A]();

        let i = start;

        while i < end && i < list.length {
            let value = list[i];
            List::push(new_list, value);
            i = i + 1;
        };

        *new_list
    }

    impl fn (list: List[A]) equals[A](other: List[A]) -> bool {
        if list.length != other.length {
            return false;
        };

        let i = 0u64;

        while i < list.length {
            if list[i] != other[i] {
                return false;
            };
            i = i + 1;
        };

        return true;
    }
}

impl fn (x: String) slice(start: u64, end: u64) -> String {
    // for instance "hello".slice(1,4) = "ell"
    let length = if end > x.length { x.length } else { end };
    let result = "";
    let i = start;
    while i < length {
        let c = x[i];
        result = result + String::new(GC::allocate(c));
        i = i + 1;
    };

    result
}

impl fn (x: char) show_prec(_: i32) -> String {
    let data = GC::allocate(x);

    "'" + String::new(data) + "'"
}

pub fn getArgs(argc: i32, argv: *string) -> List[String] {
    List::from_pointer(argv, argc)->map(String::new)
}

mod String {
    fn from_chars(chars: List[char]) -> String {
        let result = "";
        let i = 0u64;
        while i < chars.length {
            let c = chars[i];
            result = result + String::new(GC::allocate(c));
            i = i + 1;
        };
        result
    }
}
