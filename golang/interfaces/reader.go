// strings.NewReader implements the io.Reader interface, which represents the read end of a stream of data.
// io.Reader wraps the Read method. Read fills a given byte slice with data.
// Read returns the number of bytes pushed to the slice and an error value. Returns io.EOF when the stream ends.
// Example pulled from https://go.dev/tour/methods/21

package main

import (
	"fmt"
	"io"
	"strings"
)

func main() {
	r := strings.NewReader("Hello, Reader!") // &{s:Hello, Reader! i:0 prevRune:-1}

	b := make([]byte, 8) // [0 0 0 0 0 0 0 0]
	for {
		n, err := r.Read(b)
		fmt.Printf("n = %v err = %v b = %v\n", n, err, b)
		fmt.Printf("b[:n] = %q\n", b[:n])
		if err == io.EOF {
			break
		}
	}
}

// === Output ===
// n = 8 err = <nil> b = [72 101 108 108 111 44 32 82]
// b[:n] = "Hello, R"
// n = 6 err = <nil> b = [101 97 100 101 114 33 32 82]
// b[:n] = "eader!"
// n = 0 err = EOF b = [101 97 100 101 114 33 32 82]
// b[:n] = ""
