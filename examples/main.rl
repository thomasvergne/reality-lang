const x: i32 = 5;

import imports;

extern fn ptr_add[A](c: A, idx: u64) -> A;

property get_index[Cont, Idx, Item](container: Cont, index: Idx) -> Item;

impl fn (container: *A) get_index[A](index: i32) -> *A {
    let ptr = ptr_add(container, index);
    ptr
}

fn main() -> i32 {
    let c = get_index("test", 0);
    0
}
