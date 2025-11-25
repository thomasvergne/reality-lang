extern let intrinsic: void;

mod GC {
    struct GarbageCollector { };
    
    extern fn undefined() -> never;

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

    fn red(message: String) -> String {
        "\x1b[31m" + message + "\x1b[0m"
    }

    pub fn panic<B, A>(message: A) -> B {
        let msg_prefix = GC.red("[Panic]: ");
        let full_message = msg_prefix + message.show();
        panic_ext(full_message.data)

        undefined()
    }
}
