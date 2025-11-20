import std.list;
import std.error;
import std.option;
import std.string;

type FieldMutex = pthread_mutex_t;
type FieldCondition = pthread_cond_t;

type Thread<T> = pthread_t;

#[intrinsic] {
    extern let NULL: *void;
    extern fn pthread_mutex_init<T>(mutex: *FieldMutex, attr: T) -> i32;
    extern fn pthread_mutex_lock(mutex: *FieldMutex) -> i32;
    extern fn pthread_mutex_unlock(mutex: *FieldMutex) -> i32;
    extern fn pthread_cond_init<T>(cond: *FieldCondition, attr: T) -> i32;
    extern fn pthread_cond_wait(cond: *FieldCondition, mutex: *FieldMutex) -> i32;
    extern fn pthread_cond_signal(cond: *FieldCondition) -> i32;
    extern fn pthread_cond_broadcast(cond: *FieldCondition) -> i32;
    extern fn pthread_mutex_destroy(mutex: *FieldMutex) -> i32;
    extern fn pthread_cond_destroy(cond: *FieldCondition) -> i32;
    extern fn pthread_join(thread: pthread_t, attr: i32) -> i32;
    extern fn pthread_mutex_trylock(mutex: *FieldMutex) -> i32;
}

extern fn create_thread<A>(f: fn() -> *A) -> Thread<A>;

enum LinkedList<A> {
    Node(A, *LinkedList<A>),
    Nil
}

impl fn (ll: LinkedList<A>) show_prec<A>(prec: i32) -> String {
    if ll is Nil {
        return "Nil";
    };

    if ll is Node(let value, let next) {
        return "Node(" + show_prec(value, prec + 1) + ", " + show_prec(*next, prec + 1) + ")";
    };
}

impl fn (l1: LinkedList<A>) add<A>(l2: LinkedList<A>) -> LinkedList<A> {
    if l1 is Nil {
        return l2;
    };

    if l1 is Node(let value, let next) {
        return Node(value, GC.allocate((*next).add(l2)));
    };

    return Nil;
}

struct ConcurrentQueue<A> {
    list: *List<A>,
    mutex: FieldMutex,
    not_empty: FieldCondition,
    size: i32
}

fn Queue.init<A>() -> ConcurrentQueue<A> {
    let list = List.init<A>();
    let queue = struct ConcurrentQueue<A> {
        list: list,
        size: 0
    };

    pthread_mutex_init(&queue.mutex, NULL);
    pthread_cond_init(&queue.not_empty, NULL);

    return queue;
}

impl fn (queue: *ConcurrentQueue<A>) enqueue<A>(value: A) -> unit {
    pthread_mutex_lock(&queue->mutex);

    queue->list.push(value);

    // print("Enqueued. Current size: " + show(queue->size));

    pthread_cond_signal(&queue->not_empty);
    pthread_mutex_unlock(&queue->mutex);

    unit
}

impl fn (queue: *ConcurrentQueue<A>) dequeue<A>() -> Option<A> {
    pthread_mutex_lock(&queue->mutex);
    
    let result = queue->list.pop();

    pthread_mutex_unlock(&queue->mutex);

    return result;
}

