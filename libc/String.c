#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include "../dependencies/gc/src/gc.h"

char* malloc_string(const char* content) {
    size_t length = strlen(content);
    char* copy = (char*)gc_malloc(&gc, length + 1);
    if (copy) {
        strcpy(copy, content);
    }
    return copy;
}

char* number_to_string(int number) {
    size_t length = snprintf(NULL, 0, "%d", number);

    char* result = (char*)gc_malloc(&gc, length + 1);
    if (result) {
        snprintf(result, length + 1, "%d", number);
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
