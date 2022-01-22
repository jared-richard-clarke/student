function pipe( ...actions ) {
    return function ( input ) {
        return actions.reduce(
            ( accum, action ) => action( accum ), 
            input
        );
    };
}
