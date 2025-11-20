import string;
import option;

struct List<A> {
    data: *A,
    length: u64,
    capacity: u64
};

type size = u64;

extern let intrinsic: void;

mod GC {
    struct GarbageCollector { };

    #[intrinsic] {
        extern fn GC_malloc<A>(size: u64) -> A;
        extern fn GC_free<A>(ptr: A) -> unit;
        extern fn GC_realloc<A>(ptr: A, size: u64) -> A;
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

    pub fn calloc<A>(count: u64) -> *A {
        GC_malloc(count * sizeof(A))
    }

    pub fn realloc<A>(ptr: *A, count: u64) -> *A {
        GC_realloc(ptr, count)
    }

    pub fn free<A>(ptr: *A) -> unit {
        GC_free(ptr)
    }

    fn red(message: String) -> String {
        "\x1b[31m" + message + "\x1b[0m"
    }

    pub fn panic<A>(message: A) -> unit {
        let msg_prefix = GC.red("[Panic]: ");
        let full_message = msg_prefix + show_prec(message, 0);
        panic_ext(full_message.data)
    }
}

property get_index<Item, Container, Index>(container: Container, index: Index) -> Item;

extern fn ptr_add<A>(ptr: A, offset: u64) -> A;
extern fn fetch_ptr<A>(ptr: A, index: u64) -> A;

impl fn (c: *A) get_index<A>(index: u64) -> A {
    let ptr = ptr_add(c, index * sizeof(A));
    *ptr
}

impl fn (c: List<A>) get_index<A>(index: u64) -> A {
    if index >= c.length {
        GC.panic("Index out of bounds in List.get_index, " 
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

property get_index_mut<B, A>(container: A, index: u64) -> *B;

impl fn (c: *A) get_index_mut<A>(index: u64) -> *A {
    let ptr = ptr_add(c, index * sizeof(A));
    ptr as *A
}

impl fn (c: List<A>) get_index_mut<A>(index: u64) -> *A {
    get_index_mut(c.data, index)
}

mod List {
    fn init<A>() -> *List<A> {
        let list = GC.malloc<List<A>>();

        list->capacity = 10.into();

        list->data = GC.calloc<A>(list->capacity);
        list->length = 0.into();

        list
    }

    impl fn (list: *List<A>) push<A>(value: A) -> *List<A> {
        if list->length == list->capacity {
            list->capacity = list->capacity + 10.into();
            list->data = GC.realloc(list->data, list->capacity + 10.into());
        };

        *get_index_mut(list->data, list->length) = value;
        list->length = list->length + 1.into();

        list
    }

    impl fn (list: List<A>) map<A, B>(f: fn(A) -> B) -> List<B> {
        let init_list = List.init<B>();

        let i = 0.into();

        while i < list.length {
            let value = list[i];
            init_list.push(f(value));
            i = i + 1.into();
        };

        *init_list
    }

    impl fn (list: List<A>) extend<A>(other: List<A>) -> List<A> {
        let init_list = List.init<A>();

        let i = 0.into();

        while i < list.length {
            let value = list[i];
            init_list.push(value);
            i = i + 1.into();
        };

        let j = 0.into();

        while j < other.length {
            let value = other[j];
            init_list.push(value);
            j = j + 1.into();
        };

        *init_list
    }

    fn from_pointer<A>(ptr: *A, count: u64) -> *List<A> {
        let list = List.init<A>();

        let i = 0.into();

        while i < count {
            let value = ptr[i];
            list.push(value);
            i = i + 1.into();
        };

        list
    }

    impl fn (list: List<A>) show_prec<A>(prec: i32) -> String {
        let result = "[";

        let i = 0.into();

        while i < list.length {
            let value = list[i];
            result = result + show_prec(value, prec + 1);

            if i < list.length - 1.into() {
                result = result + ", ";
            };

            i = i + 1.into();
        };

        result = result + "]";

        result
    }

    impl fn (list: List<A>) slice<A>(start: u64, end: u64) -> List<A> {
        let init_list = List.init<A>();

        let i = start;

        while i < end && i < list.length {
            let value = list[i];
            init_list.push(value);
            i = i + 1;
        };

        *init_list
    }

    impl fn (list: List<A>) equals<A>(other: List<A>) -> bool {
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

    impl fn (list: *List<A>) pop<A>() -> Option<A> {
        if list->length == 0.into() {
            return None;
        };

        list->length = list->length - 1.into();
        let value = (list->data)[list->length];

        Some(value)
    }
}

impl fn (x: String) slice(start: u64, end: u64) -> String {
    // for instance "hello".slice(1,4) = "ell"
    let length = if end > x.length { x.length } else { end };
    let result = "";
    let i = start;
    while i < length {
        let c = x[i];
        result = result + String.init(GC.allocate(c));
        i = i + 1;
    };

    result
}

impl fn (x: char) show_prec(_: i32) -> String {
    let data = GC.allocate(x);

    "'" + String.init(data) + "'"
}

pub fn getArgs(argc: i32, argv: *string) -> List<String> {
    List.from_pointer(argv, argc.into())->map(String.init)
}

mod String {
    fn from_chars(chars: List<char>) -> String {
        let result = "";
        let i = 0u64;
        while i < chars.length {
            let c = chars[i];
            result = result + String.init(GC.allocate(c));
            i = i + 1;
        };
        result
    }
}
