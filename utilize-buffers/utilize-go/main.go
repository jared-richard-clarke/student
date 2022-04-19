// utilize replaces variations of "utilize" with variations of "use",
// writing the content of one plain text file into the body of another.
// Call program with commands -input "old.txt" -output "new.txt". Files must be plain text.

package main

import (
	"bufio"
	"errors"
	"flag"
	"io"
	"log"
	"os"
	"path"
	"regexp"
)

const delimiter = '.'

// command flags and errors
var (
	// flags
	input  = flag.String("input", "", "Input plain text file: -input [old].txt")
	output = flag.String("output", "", "Output plain text file: -output [new].txt]")
	// errors
	errMissedFlag = errors.New("utilize: must call program with input and output flags")
	errPlainText  = errors.New("utilize: input and output files must be plain text")
)

// replacers for "utilize" and variants
var (
	dictionary = map[string]string{
		"utilize":   "use",
		"utilise":   "use",
		"Utilize":   "Use",
		"Utilise":   "Use",
		"utilizes":  "uses",
		"utilises":  "uses",
		"utilized":  "used",
		"utilised":  "used",
		"utilizing": "using",
		"utilising": "using",
		"Utilizing": "Using",
		"Utilising": "Using",
	}
	re      = regexp.MustCompile(`[uU]tili(?:[zs]ed|[zs]e|[zs]ing)`)
	matcher = func(s string) string { return dictionary[s] }
)

func main() {
	// 1. collect user input
	flag.Parse()
	var input, output = *input, *output
	if input == "" || output == "" {
		log.Fatal(errMissedFlag)
	} else if path.Ext(input) != ".txt" || path.Ext(output) != ".txt" {
		log.Fatal(errPlainText)
	}
	// 2. open files
	file, err := os.Open(input)
	if err != nil {
		log.Fatal(err)
	}
	newfile, err := os.Create(output)
	if err != nil {
		log.Fatal(err)
	}
	// 3. initialize writer and reader
	writer := bufio.NewWriter(newfile)
	reader := bufio.NewReader(file)
	// 4. scan file, periodically flushing writer to disk when buffer surpasses limit
	for {
		line, err := reader.ReadString(delimiter)
		if err == io.EOF {
			break
		}
		if err != nil {
			log.Println(err)
		}
		_, err = writer.WriteString(re.ReplaceAllStringFunc(line, matcher))
		if err != nil {
			log.Println(err)
		}
	}
	writer.Flush()
	// 5. Close files, checking for errors.
	if err = file.Close(); err != nil {
		log.Fatal(err)
	}
	if err = newfile.Close(); err != nil {
		log.Fatal(err)
	}
}
