function type_check(type, action) {
    return function (value) {
        return typeof value !== type ? value : action(value);
    };
}
