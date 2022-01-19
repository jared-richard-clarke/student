#!/usr/bin/env node

const path = require("path");
const { createReadStream, createWriteStream } = require("fs");
const { Transform } = require("stream");

const infile = "test.txt";
const outfile = "outtest.txt"

// 1. Check user input. TODO: throw error or use process.exit?
if (path.extname(infile) !== ".txt" || path.extname(outfile) !== ".txt") {
    console.error("input and output must be plain text files -> [file].txt");
    process.exit(1);
}
const readStream = createReadStream(infile, "utf-8");
readStream.on("error", (err) => {
    console.error(err);
    process.exit(1);
});
const writeStream = createWriteStream(outfile);

// replace(string) -> string
// replaces 'utilize' and its variants with 'use' and its variants.
const replace = (function () {
    const dictionary = {
        utilize: "use",
        utilise: "use",
        Utilize: "Use",
        Utilise: "Use",
        utilizes: "uses",
        utilises: "uses",
        utilized: "used",
        utilised: "used",
        utilizing: "using",
        utilising: "using",
        Utilizing: "Using",
        Utilising: "Using",
    };
    return function (text) {
        if (typeof text !== "string") {
            throw "argument must be type string";
        }
        return text.replace(
            /[uU]tili([zs]e|[zs]ed|[zs]ing)/g,
            function (match) {
                return dictionary[match];
            }
        );
    };
})();

// Simplified constructor approach. Alternative: ES6-style constructor
// First argument to callback function is ignored for buffers.
// Second argument to callback function will be forwarded to the transform.push() method.
const transform_text = new Transform({
    transform: function (chunk, encoding, callback) {
        callback(null, replace(String(chunk)));
    },
});
// 4. Pipe read stream through transformer, then write to file.
readStream
    .pipe(transform_text)
    .pipe(writeStream)
    .on("error", (err) => console.error(err));
