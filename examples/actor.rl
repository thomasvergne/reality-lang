import list::*;

type FieldMutex = pthread_mutex_t;
type FieldCondition = pthread_cond_t;

extern let intrinsic: void;

type Thread[T] = pthread_t;

enum Error[T] {
    Error(String),
    Success(T)
}

#[intrinsic] {
    extern fn pthread_mutex_init[T](mutex: *FieldMutex, attr: T) -> i32;
    extern fn pthread_mutex_lock(mutex: *FieldMutex) -> i32;
    extern fn pthread_mutex_unlock(mutex: *FieldMutex) -> i32;
    extern fn pthread_cond_init[T](cond: *FieldCondition, attr: T) -> i32;
    extern fn pthread_cond_wait(cond: *FieldCondition, mutex: *FieldMutex) -> i32;
    extern fn pthread_cond_signal(cond: *FieldCondition) -> i32;
    extern fn pthread_cond_broadcast(cond: *FieldCondition) -> i32;
    extern fn pthread_mutex_destroy(mutex: *FieldMutex) -> i32;
    extern fn pthread_cond_destroy(cond: *FieldCondition) -> i32;
    extern fn pthread_join(thread: pthread_t, attr: i32) -> i32;
}

extern fn create_thread(f: fn() -> i32) -> pthread_t;

struct Actor[T] {
    mailbox: *List[T],
    head: u64,
    tail: u64,

    closed: bool,

    mutex: FieldMutex,
    not_full: FieldCondition,
    not_empty: FieldCondition
}

mod Actor {
    fn init[T]() -> *Actor[T] {
        let actor = malloc::[Actor[T]]();

        actor->mailbox = List::new::[T]();
        actor->head = 0;
        actor->tail = 0;

        actor->closed = false;

        pthread_mutex_init(&actor->mutex, 0);
        pthread_cond_init(&actor->not_full, 0);
        pthread_cond_init(&actor->not_empty, 0);

        actor
    }

    fn close[T](actor: *Actor[T]) -> i32 {
        pthread_mutex_lock(&actor->mutex);

        actor->closed = true;
        pthread_cond_broadcast(&actor->not_empty);
        pthread_cond_broadcast(&actor->not_full);

        pthread_mutex_unlock(&actor->mutex);
        0
    }

    fn send[T](actor: *Actor[T], message: T) -> i32 {
        pthread_mutex_lock(&actor->mutex);
        while actor->tail - actor->head == actor->mailbox->capacity && not(actor->closed) {
            pthread_cond_wait(&actor->not_full, &actor->mutex);
        }
        if actor->closed {
            pthread_mutex_unlock(&actor->mutex);
            -1 // Err("Actor is closed")
        } else {
            List::push(actor->mailbox, message);
            actor->tail = actor->tail + 1;
            pthread_cond_signal(&actor->not_empty);
            pthread_mutex_unlock(&actor->mutex);
            0 // Ok()
        }
    }

    fn recv[T](actor: *Actor[T]) -> Error[T] {
        pthread_mutex_lock(&actor->mutex);
        while actor->tail == actor->head && not(actor->closed) {
            pthread_cond_wait(&actor->not_empty, &actor->mutex);
        }

        if actor->tail == actor->head && actor->closed {
            pthread_mutex_unlock(&actor->mutex);
            // return Error.Closed
            // Here we should handle the closed case properly.
            // For simplicity, we'll just return a default value.
            // In a real implementation, consider using Option or Result types.

            Error(String::new("Actor is closed"))
        } else {
            let message = (*(actor->mailbox))[actor->head];
            actor->head = actor->head + 1;
            pthread_cond_signal(&actor->not_full);
            pthread_mutex_unlock(&actor->mutex);

            Success(message)
        }
    }

    fn listen[T](actor: *Actor[T], handler: fn(T) -> i32) -> Thread[i32] {
        create_thread(|| {
            let result = 0;
            while not(actor->closed) || actor->head < actor->tail {
                let msg = Actor::recv(actor);

                if msg is Success(let value) {
                    result = handler(value);
                }
            }
            result
        })
    }
}
