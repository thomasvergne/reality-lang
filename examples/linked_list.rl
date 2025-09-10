pub enum LinkedList[A] {
    Nil,
    Cons(A, *LinkedList[A])
}

impl fn (c: LinkedList[A]) show_prec[A](i: i32) -> String {
    if c is Cons(let head, let tail) {
        String::new("Cons(")
            + show_prec(head, i + 1)
            + String::new(", ")
            + show_prec(*tail, i + 1)
            + String::new(")")
    } else {
        String::new("Nil")
    }
}

mod LinkedList {
    impl fn (c: LinkedList[A]) map[A, B](f: fn(A) -> B) -> LinkedList[B] {
        if c is Cons(let head, let tail) {
            Cons(f(head), allocate::[LinkedList[B]](tail->map(f)))
        } else {
            Nil
        }
    }

    fn from_pointer[A](ptr: *A, count: u64) -> *LinkedList[A] {
        if count == 0 {
            allocate::[LinkedList[A]](Nil)
        } else {
            let head = ptr[0u64];
            let tail = LinkedList::from_pointer(ptr_add(ptr, 1), count - 1);

            allocate::[LinkedList[A]](Cons(head, tail))
        }
    }

    impl fn (c: LinkedList[A]) to_list[A]() -> *List[A] {
        let list = List::new::[A]();

        let current = c;
        while current is Cons(let head, let tail) {
            List::push(list, head);
            current = *tail;
        };

        list
    }
}
