#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <gc.h>
#include <ctype.h>

bool string_eq(char* a, char* b) {
    return strcmp(a, b) == 0;
}

char* malloc_string(const char* content) {
    size_t length = strlen(content);
    char* copy = (char*)GC_MALLOC(length + 1);
    if (copy) {
        strcpy(copy, content);
    }
    return copy;
}

char* u64_to_string(uint64_t number) {
    size_t length = snprintf(NULL, 0, "%llu", number);

    char* result = (char*)GC_MALLOC(length + 1);
    if (result) {
        snprintf(result, length + 1, "%llu", number);
    }
    return result;
}

char* number_to_string(int number) {
    size_t length = snprintf(NULL, 0, "%d", number);

    char* result = (char*)GC_MALLOC(length + 1);
    if (result) {
        snprintf(result, length + 1, "%d", number);
    }
    return result;
}

char* pointer_to_string(void* ptr) {
    size_t length = snprintf(NULL, 0, "%p", ptr);

    char* result = (char*)GC_MALLOC(length + 1);
    if (result) {
        snprintf(result, length + 1, "%p", ptr);
    }
    return result;
}

int add_number(int a, int b) {
    return a + b;
}

int mul_number(int a, int b) {
    return a * b;
}

int div_number(int a, int b) {
    if (b != 0) {
        return a / b;
    }
    // Handle division by zero case
    return 0;
}

int sub_number(int a, int b) {
    return a - b;
}

bool equals_number(int a, int b) {
    return a == b;
}

uint64_t add_u64_ext(uint64_t a, uint64_t b) {
    return a + b;
}

uint64_t sub_u64_ext(uint64_t a, uint64_t b) {
    return a - b;
}

uint64_t mul_u64_ext(uint64_t a, uint64_t b) {
    return a * b;
}

uint64_t div_u64_ext(uint64_t a, uint64_t b) {
    if (b != 0) {
        return a / b;
    }
    // Handle division by zero case
    return 0;
}

bool equals_u64_ext(uint64_t a, uint64_t b) {
    return a == b;
}

char* fetch_ptr(char* container, uint32_t index) {
    return &container[index];
}

char* ptr_add(char* ptr, uint32_t offset) {
    return ptr + offset;
}

int greater_number(int a, int b) {
    return a > b;
}

int less_number(int a, int b) {
    return a < b;
}

int mod_number(int a, int b) {
    if (b != 0) {
        return a % b;
    }
    // Handle division by zero case
    return 0;
}

bool greater_u64_ext(uint64_t a, uint64_t b) {
    return a > b;
}

bool less_u64_ext(uint64_t a, uint64_t b) {
    return a < b;
}

uint64_t mod_u64_ext(uint64_t a, uint64_t b) {
    if (b != 0) {
        return a % b;
    }
    // Handle division by zero case
    return 0;
}

bool char_eq(char a, char b) {
    return a == b;
}


char* concat_strings(char* a, char* b) {
    size_t length = strlen(a) + strlen(b);
    char* result = (char*)GC_MALLOC(length + 1);
    if (result) {
        strcpy(result, a);
        strcat(result, b);
    }
    return result;
}

void* malloc_ext(uint64_t size) {
    return malloc(size);
}

int free_ext(void* ptr) {
    free(ptr);
    return 0;
}

void* realloc_ext(void* ptr, uint64_t size) {
    return GC_REALLOC(ptr, size);
}

void panic_ext(char* message) {
    fprintf(stderr, "%s\n", message);
    exit(1); 
}

bool is_whitespace(char c) {
    return isspace((unsigned char)c);
}

bool is_alpha(char c) {
    return isalpha((unsigned char)c);
}

bool is_alphanumeric(char c) {
    return isalnum((unsigned char)c);
}

bool is_digit(char c) {
    return isdigit((unsigned char)c);
}

void* BDWGC_malloc(size_t size) {
    return GC_MALLOC(size);
}

void* BDWGC_realloc(void* ptr, size_t size) {
    return GC_REALLOC(ptr, size);
}

void BDWGC_free(void* ptr) {
    GC_FREE(ptr);
}

uint64_t i32_to_u64(int32_t value) {
    return (uint64_t)(uint32_t)value;
}
