#!/usr/bin/env node

const path = require("path");
const fs = require("fs");
const stream = require("stream");

const infile = "test.txt";
const outfile = "re-" + infile;

if (path.extname(infile) !== ".txt") {
    throw new Error("input must be a plain text file -> [file].txt");
}

const readStream = fs.createReadStream(infile, "utf-8");

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
                throw new TypeError("argument must be of type string");
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
    const transform_text = new stream.Transform({
        decodeStrings: false,
        transform: function (chunk, encoding, callback) {
            callback(null, replace(chunk));
        },
    });

    const writeStream = fs.createWriteStream(outfile);
    
    writeStream.on("ready", function () {
        readStream
            .on("error", (err) => console.error(err))
            .pipe(transform_text)
            .on("error", (err) => console.error(err))
            .pipe(writeStream)
            .on("error", (err) => console.error(err))
    });
});
