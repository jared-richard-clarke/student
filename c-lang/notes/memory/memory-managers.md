# C Functions for Memory Management

C implements a number of functions in `stdlib.h` and `string.h` that are
used to manipulate memory. With the exception of `memcpy`, `memmove`, and
`memset` — which are located in `string.h` — all the functions mentioned are
located in `stdlib.h`.

> "As a rule of thumb, dynamic allocation should mostly be used for pieces
> of data where the byte size of the data is unknown or for data whose
> lifespan needs to be preserved outside of the scope of the function it
> is contained inside."
>
> — University of Illinois Urbana-Champaign, **Systems Encyclopedia**

## `void *malloc(size_t size);`

Given some unsigned integer `size`, `malloc` will allocate a contiguous block
of `size` bytes and return a pointer to that allocated block. This block will
exist in memory until it is explicitly deallocated with `free`.

## `void *calloc(size_t nmemb, size_t size);`

`calloc` allocates `size` × `nmemb` bytes of memory. Unlike `malloc`, `calloc`
sets each byte of the allocated memory to `0`, meaning that the allocated
memory can be used without being initialized.

## `void *realloc(void *ptr, size_t size);`

Given a `ptr` to some piece of allocated memory and an unsigned integer
`size`, `realloc` will `free` the memory that `ptr` points to and reallocate
a new block of `size` bytes. `realloc` will attempt to preserve date inside
the block that `ptr` points to during reallocation. If `size` is greater than
the original block size, then `realloc` will simply copy that data and leave
the rest of the memory unset. If `size` is smaller than the original size,
`realloc` will copy as much as it can and truncate the rest. In the event that
`realloc` fails, it will return `NULL` and the pointer to the original data
will be lost.

## `sizeof`

The `sizeof` operator returns the size in bytes of a particular type
signature.

## `void free(void *ptr)`

Given a pointer to a piece of allocated memory, `free` deallocates that
memory.

## `void *memcpy(void *restrict dest, const void *restrict src, size_t n);`

Given two pointers, `dest` and `src`, `memcpy` will copy `n` bytes from
`src` into `dest` and return a pointer to `dest`.

## `void *memmove(void *dest, const void *src, size_t n);`

`memcpy` performs the same function as `memcpy` but has well-defined behavior
for when the memory regions that `src` and `dest` point to overlap.

## `void *memset(void *s, int c, size_t n);`

`memset` defines a singular value to copy into a block of memory rather than
an entire buffer. Given some pointer `s` and a byte value `c`, `memset` will
set `n` bytes of memory in the block pointed to by `s` to take the value `c`.
