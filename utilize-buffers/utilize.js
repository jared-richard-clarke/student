const replace_utilize = (function () {
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
        Utilising: "Using"
    };
    return function (text) {
        if (typeof text !== "string") {
            raise "argument must be type string";
        }
        return text.replace(/[uU]tili([zs]e|[zs]ed|[zs]ing)/g, function (match) {
            return dictionary[match];
        });
    };
}());
