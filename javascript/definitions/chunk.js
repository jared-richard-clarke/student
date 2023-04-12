// chunk([any], number) -> [[any]]
// Splits the given array into an array of arrays of the given size.
// chunk([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 4); -> [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10]]

function chunk(xs, size) {
    if (size <= 0 || !Number.isInteger(size) || xs.length === 0) {
        return xs.slice();
    }

    const chunks = [];
    let read = 0;
    let write = 0;

    while (read < xs.length) {
        chunks[write] = xs.slice(read, read + size);
        write += 1;
        read += size;
    }

    return chunks;
}
