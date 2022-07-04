function identity()
    [1 0 0;
     0 1 0;
     0 0 1]
end

function translate(x, y)
    [1 0 x;
     0 1 y;
     0 0 1]
end

function scale(x, y)
    [x 0 0;
     0 y 0;
     0 0 1]
end

function rotate(angle)
    c = cos(angle)
    s = sin(angle)
    [c -s 0;
     s c 0;
     0 0 1]
end

function shear(x, y)
    [1 x 0;
     y 1 0;
     0 0 1]
end

function transform(matrices...)
    reduce(*, matrices)
end
