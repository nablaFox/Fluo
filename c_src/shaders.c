#include "shaders.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

uint8_t* read_shader(const char* file, size_t* size) {
    if (!file || !size) return NULL;

    const char* prefix = SHADERS_DIR;
    const char* suffix = ".spv";
    size_t plen = strlen(prefix);
    size_t flen = strlen(file);
    size_t slen = strlen(suffix);
    char* full = (char*)malloc(plen + flen + slen + 1);

    if (!full) {
        fprintf(stderr, "Failed to load %s%s\n", SHADERS_DIR, file);
        return NULL;
    }

    memcpy(full, prefix, plen);
    memcpy(full + plen, file, flen);
    memcpy(full + plen + flen, suffix, slen + 1);
    FILE* f = fopen(full, "rb");

    if (!f) {
        fprintf(stderr, "Failed to load %s%s\n", SHADERS_DIR, file);
        free(full);
        return NULL;
    }

    if (fseek(f, 0, SEEK_END) != 0) {
        fprintf(stderr, "Failed to load %s%s\n", SHADERS_DIR, file);
        fclose(f);
        free(full);
        return NULL;
    }

    long end = ftell(f);
    if (end < 0) {
        fprintf(stderr, "Failed to load %s%s\n", SHADERS_DIR, file);
        fclose(f);
        free(full);
        return NULL;
    }

    *size = (size_t)end;
    if (fseek(f, 0, SEEK_SET) != 0) {
        fprintf(stderr, "Failed to load %s%s\n", SHADERS_DIR, file);
        fclose(f);
        free(full);
        return NULL;
    }

    uint8_t* data = (uint8_t*)malloc(*size);
    if (!data) {
        fprintf(stderr, "Failed to load %s%s\n", SHADERS_DIR, file);
        fclose(f);
        free(full);
        return NULL;
    }

    size_t n = fread(data, 1, *size, f);
    fclose(f);
    if (n != *size) {
        fprintf(stderr, "Failed to load %s%s\n", SHADERS_DIR, file);
        free(full);
        free(data);
        return NULL;
    }

    free(full);
    return data;
}
