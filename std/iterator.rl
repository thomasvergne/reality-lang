struct Iterator[A] {
    container: A,
    index: *u64
}

impl fn (it: List[A]) iter[A]() -> Iterator[List[A]] {
    Iterator[List[A]] {
        container: it,
        index: GC::allocate(0u64)
    }
}

impl fn (self: List[A]) at[A](index: u64) -> A {
    self.get_index(index)
}

impl fn (it: Iterator[List[A]]) next[A]() -> Option[A] {
    if *it.index < it.container.length {
        let value = it.container.at(*it.index);
        *it.index = *it.index + 1u64;
        Some(value)
    } else {
        None
    }
}

struct Range {
    start: i32,
    end: i32
}

struct RangeIterator {
    range: Range,
    current: *i32
}

impl fn (r: Range) iter() -> RangeIterator {
    RangeIterator {
        range: r,
        current: GC::allocate(r.start)
    }
}

pub fn range(start: i32, end: i32) -> Range {
    Range {
        start: start,
        end: end
    }
}

impl fn (it: RangeIterator) next() -> Option[i32] {
    if *it.current < it.range.end {
        let value = *it.current;
        *it.current = *it.current + 1i32;
        Some(value)
    } else {
        None
    }
}

impl fn (it: Range) to_list() -> List[i32] {
    let list = List::new::[i32]();

    for value in it {
        List::push(list, value);
    }

    *list
}
