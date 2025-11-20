import std.list;
import std.error;
import std.option;
import std.string;
import concurrent_queue;

struct Actor<T> {
    mailbox: *ConcurrentQueue<T>,
    closed: bool,
    thread: pthread_t,
    opened: bool
}

mod Actor {
    pub impl fn (actor: *Actor<T>) send<T>(message: T) -> unit {
        if actor->closed {
            GC.panic("Cannot send message to closed actor");
        };

        while not(actor->opened) {
            // Busy wait until the actor thread is opened
            print("");
        };

        actor->mailbox.enqueue(message);
    }

    pub impl fn (actor: *Actor<T>) receive<T>() -> Option<T> {
        return (actor->mailbox).dequeue();
    }

    pub impl fn (actor: *Actor<T>) close<T>() -> bool {
        actor->closed = true;

        return true;
    }

    pub fn create<T>(f: fn(*Actor<T>, T) -> unit) -> *Actor<T> {
        let actor = 
            new struct Actor<T> {
                mailbox: new Queue.init<T>(),
                closed: false
            }
        
        let actor_ptr = actor;  // For closure capture
        let thread = create_thread(|| {
            actor->opened = true;
            while not(actor_ptr->closed) {
                if actor_ptr.receive() is Some(let msg) {
                    f(actor_ptr, msg);
                }
            };
            &unit
        });
        
        actor->thread = thread;
        
        // Option: Add synchronization here to ensure thread starts
        
        return actor;
    }

    pub impl fn (act: *Actor<T>) wait<T>() -> unit {
        pthread_join(act->thread, 0);

        unit
    }
}
