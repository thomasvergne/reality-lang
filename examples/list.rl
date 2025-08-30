import string::*;

struct List[A] {
    data: *A,
    length: u64,
    capacity: u64
};

mod GC {
    struct GarbageCollector { };

    extern fn get_gc() -> *GarbageCollector;

    extern fn gc_malloc[A](gc: *GarbageCollector, size: u64) -> A;
    extern fn gc_free[A](gc: *GarbageCollector, ptr: A) -> i32;
    extern fn gc_realloc[A](gc: *GarbageCollector, ptr: A, size: u64) -> A;

    extern fn gc_start(gc: *GarbageCollector, argc_ref: *i32) -> i32;
    extern fn gc_stop(gc: *GarbageCollector) -> i32;

    pub fn malloc[A]() -> *A {
        gc_malloc(get_gc(), sizeof(A))
    }

    pub fn calloc[A](count: u64) -> *A {
        gc_malloc(get_gc(), sizeof(A) * count)
    }

    pub fn realloc[A](ptr: *A, count: u64) -> *A {
        gc_realloc(get_gc(), ptr, sizeof(A) * count)
    }

    pub fn free[A](ptr: *A) -> i32 {
        gc_free(get_gc(), ptr)
    }
}
property get_index[B, A](container: A, index: u64) -> B;

extern fn ptr_add[A](ptr: A, offset: u64) -> A;
extern fn fetch_ptr[A](ptr: A, index: u64) -> A;

impl fn (c: *A) get_index[A](index: u64) -> A {
    let ptr = ptr_add(c, index * sizeof(A));
    *ptr
}

impl fn (c: List[A]) get_index[A](index: u64) -> A {
    get_index(c.data, index)
}

property get_index_mut[B, A](container: A, index: u64) -> *B;

impl fn (c: *A) get_index_mut[A](index: u64) -> *A {
    let ptr = ptr_add(c, index * sizeof(A));
    ptr
}

impl fn (c: List[A]) get_index_mut[A](index: u64) -> *A {
    get_index_mut(c.data, index)
}

mod List {
    fn new[A]() -> *List[A] {
        let list = malloc::[List[A]]();

        list->capacity = 10 as u64;

        list->data = calloc::[A](10);
        list->length = 0 as u64;

        list
    }

    fn push[A](list: *List[A], value: A) -> u64 {
        if list->length == list->capacity {
            list->data = realloc(list->data, list->capacity + 10);
        };

        *get_index_mut(list->data, list->length) = value;
        list->length = list->length + 1;

        0
    }

    fn map[A, B](list: *List[A], f: fn(A) -> B) -> *List[B] {
        let new_list = new::[B]();

        let i = 0 as u64;

        while i < list->length {
            let value = (*list)[i];
            push(new_list, f(value));
            i = i + 1;
        };

        new_list
    }
}

impl fn (list: List[A]) show[A]() -> String {
    String::new("List")
}
