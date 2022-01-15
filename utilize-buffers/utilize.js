// TODO: add shebang line?
const { createReadStream, createWriteStream } = require("fs");
const { Transform } = require("stream");

const readStream = createReadStream("test.txt", "utf-8");
const writeStream = createWriteStream("outfile.txt");

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
// Second argument to callback function will be forwarded to the transform.push() method.
const transform_text = new Transform({
    // TODO: setup error handler
    transform: function (chunk, encoding, callback) {
        callback(null, replace(String(chunk)));
    },
});

readStream.pipe(transform_text).pipe(writeStream).on("error", console.error);
