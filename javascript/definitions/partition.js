// partition([...any], (x) -> boolean) -> [[...any], [...any]]
// Returns an array of two arrays, where the first
// contains all the elements that satisfy the predicate,
// and the second contains all the elements that don't.
// partition([1, 2, 3, 4, 5], (x) => x % 2 === 0) -> [[2, 4], [1, 3, 5]]

function partition(xs, test) {
    const yes = [];
    const no = [];
    xs.forEach((x) => {
        if (test(x)) {
            yes.push(x);
        } else {
            no.push(x);
        }
    });
    return [yes, no];
}
