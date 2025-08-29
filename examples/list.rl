import string::*;

struct List[A] {
    data: *A,
    length: u64
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

    pub fn malloc_multiple[A](count: u64) -> *A {
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

impl fn (c: *A) get_index[A](index: u64) -> A {
    let ptr = ptr_add(c, index);
    *ptr
}

impl fn (c: List[A]) get_index[A](index: u64) -> A {
    get_index(c.data, index)
}

property get_index_mut[B, A](container: A, index: u64) -> *B;

impl fn (c: *A) get_index_mut[A](index: u64) -> *A {
    let ptr = ptr_add(c, index);
    ptr
}

impl fn (c: List[A]) get_index_mut[A](index: u64) -> *A {
    get_index_mut(c.data, index)
}

mod List {
    fn new[A]() -> List[A] {
        List[A] {
            data: malloc_multiple(10),
            length: 10
        }
    }

    fn push[A](list: List[A], value: A) -> i32 {
        list.data = realloc(list.data, list.length + 1);
        (*get_index_mut(list.data, list.length)) = value;
        list.length = list.length + 1;

        print(get_index::[A](list, list.length - 1));
        print(list.length);

        0
    }
}

impl fn (list: List[A]) show[A]() -> String {
    String::new("List")
}
