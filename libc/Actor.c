#include <pthread.h>
#include <stdlib.h>

typedef struct {
    void* environment;
    void* (*function)(void*);
} generic_function;

pthread_t create_thread(generic_function* f) {
    pthread_t thread;
    pthread_create(&thread, NULL, f->function, f->environment);

    return thread;
}
