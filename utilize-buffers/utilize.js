#!/usr/bin/env node
const path = require("path");
const { createReadStream, createWriteStream } = require("fs");
const { Transform } = require("stream");

const infile = "test.txt";
const outfile = "re-" + infile;

if (path.extname(infile) !== ".txt") {
    console.error("input must be a plain text file -> [file].txt");
    process.exit(1);
}

const readStream = createReadStream(infile, "utf-8");
readStream.on("error", function (err) {
    console.error(err);
});
readStream.on("ready", function () {
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
            callback(null, replace(chunk));
        },
        decodeStrings: false,
    });
    const writeStream = createWriteStream(outfile);
    writeStream.on("ready", function () {
        readStream
            .pipe(transform_text)
            .pipe(writeStream)
            .on("error", (err) => console.error(err));
    });
});
