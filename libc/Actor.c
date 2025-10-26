#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct {
    void* environment;
    void* (*function)(void*);
} generic_function;

pthread_t create_thread(generic_function* f) {
    pthread_t thread;
    pthread_create(&thread, NULL, f->function, f->environment);

    return thread;
}

char* read_file_ext(const char* path) {
    FILE* file = fopen(path, "rb");
    if (!file) {
        return "";
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    char* buffer = (char*)malloc(length + 1);
    if (buffer) {
        fread(buffer, 1, length, file);
        buffer[length] = '\0';
    }

    fclose(file);
    return buffer;
}
