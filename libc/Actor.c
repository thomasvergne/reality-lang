#define GC_THREADS
#include <gc.h>
#include <pthread.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <unistd.h>

void undefined() {
    return;
}

void pthread_yield() {
    sched_yield();
}

void wait_time(int32_t milliseconds) {
    usleep(milliseconds * 1000);
}

typedef struct {
  void* (*function)(void*);
  void* environment;
} generic_function;

static void* thread_trampoline(void* arg) {
  GC_register_my_thread(NULL);
  generic_function* f = (generic_function*)arg;
  void* result = f->function(f->environment);
  return result;
}

pthread_t create_thread(generic_function* f) {
  pthread_t thread;
  pthread_create(&thread, NULL, thread_trampoline, f);
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

int string_to_int(char* str) {
    return atoi(str);
}

void exit_program(int code) {
    exit(code);
}
