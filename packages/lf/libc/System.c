#include <unistd.h>
#include <gc.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

char* get_current_working_directory() {
    size_t size = 1024;
    char* buffer = (char*)GC_MALLOC(size * sizeof(char));
    if (getcwd(buffer, size) != NULL) {
        return buffer;
    } else {
        GC_FREE(buffer);
        return NULL;
    }
}

bool file_exists(const char* path) {    
    return access(path, F_OK) != -1;
}

int execute_command(const char* command) {
    return system(command);
}

bool folder_exists(const char* path) {
    return access(path, F_OK) != -1;
}
