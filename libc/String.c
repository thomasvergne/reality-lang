#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

char* malloc_string(const char* content) {
    size_t length = strlen(content);
    char* copy = (char*)malloc(length + 1);
    if (copy) {
        strcpy(copy, content);
    }
    return copy;
}

char* number_to_string(int number) {
    size_t length = snprintf(NULL, 0, "%d", number);

    char* result = (char*)malloc(length + 1);
    if (result) {
        snprintf(result, length + 1, "%d", number);
    }
    return result;
}

int add_number(int a, int b) {
    return a + b;
}
