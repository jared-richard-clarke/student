# Reading from a File

An example function that reads bytes into a manually-allocated array.

```c
static char* read_file(const char* path) {
    FILE* file = fopen(path, "rb");
    if (file == NULL) {
        fprintf(stderr, "Unable to open \"%s\".\n", path);
        exit(1);
    }

    fseek(file, 0L, SEEK_END);
    size_t file_size = ftell(file);
    rewind(file);

    char* buffer = (char*)malloc(file_size + 1);
    if (buffer == NULL) {
        fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
        exit(1);
    }
    size_t bytes_read = fread(buffer, sizeof(char), file_size, file);
    if (bytes_read < file_size) {
        fprintf(stderr, "Incomplete read of \"%s\".\n", path);
        exit(1);
    }

    buffer[bytes_read] = '\0';

    fclose(file);
    return buffer;
}
```
