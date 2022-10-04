using VectorGraph
using Test

@testset "vectors" begin
    Vec2 = Vectors.Vec2
    Vec3 = Vectors.Vec3
    invert = Vectors.invert
    mag = Vectors.mag
    scale = Vectors.scale
    dot = Vectors.dot
    distance = Vectors.distance
    lerp = Vectors.lerp
    normalize = Vectors.normalize

    # addition
    @test Vec2(1, 2) + Vec2(3, 4) == Vec2(4, 6)
    @test Vec3(1, 2, 1) + Vec3(3, 4, 1) == Vec3(4, 6, 2)
    # subtraction
    @test Vec2(3, 4) - Vec2(1, 2) == Vec2(2, 2)
    @test Vec3(3, 4, 5) - Vec3(1, 2, 3) == Vec3(2, 2, 2)
    # negation
    @test -Vec2(3, 4) == Vec2(-3, -4)
    @test -Vec3(3, 4, 5) == Vec3(-3, -4, -5)
    # inversion
    @test invert(Vec2(2, 2)) == Vec2(0.5, 0.5)
    @test invert(Vec3(2, 2, 2)) == Vec3(0.5, 0.5, 0.5)
    # summation
    @test +(Vec2(1, 2), Vec2(3, 4), Vec2(1, 1)) == Vec2(5, 7)
    @test +(Vec2(3, 4)) == Vec2(3, 4)
    v3 = Vec3(1, 2, 3)
    @test +(v3, v3, v3, v3) == Vec3(4, 8, 12)
    # magnitude
    @test mag(Vec2(3, 4)) == 5
    @test mag(Vec3(1, 2, 3)) == 3.741657386773941
    # scale
    @test scale(Vec2(3, 4), 11) == Vec2(33, 44)
    @test scale(Vec3(3, 4, 1), 11) == Vec3(33, 44, 11)
    # dot product
    @test dot(Vec2(3, 4), Vec2(1, 2)) == 11
    @test dot(Vec3(3, 4, 5), Vec3(1, 2, 3)) == 26
    # distance
    @test distance(Vec2(8, 0), Vec2(1, 0)) == 7
    @test distance(Vec3(10, 0, 0), Vec3(3, 0, 0)) == 7
    # interpolation
    @test lerp(Vec2(1, 1), Vec2(3, 2), 0.5) == Vec2(2, 1.5)
    @test lerp(Vec3(3, 4, 5), Vec3(1, 2, 3), 0.5) == Vec3(2, 3, 4)
    # normalize
    @test normalize(Vec2(3, 4)) == Vec2(0.6, 0.8)
    @test normalize(Vec3(3, 4, 1)) == Vec3(0.5883484054145521, 0.7844645405527362, 0.19611613513818404)
end

@testset "matrices" begin
    Mat3 = Matrices.Mat3
    ID = Matrices.identity()
    translate = Matrices.translate
    scale = Matrices.scale
    rotate = Matrices.rotate
    shear = Matrices.shear
    transform = Matrices.transform

    @test Mat3(1, 0, 0, 1, 0, 0) == ID
    @test translate(3, 4) * Mat3(1, 2, 3, 1, 0, 0) == Mat3(1, 2, 3, 1, 15, 10)
    @test scale(2, 2) * Mat3(1, 0, 3, 2, 0, 0) == Mat3(2, 0, 6, 4, 0, 0)
    
    rotation = rotate(deg2rad(90)) * ID
    @test ID == rotate(deg2rad(-90)) * rotation
    
    @test shear(2, 2) * ID == Mat3(1, 2, 2, 1, 0, 0)
    @test transform(rotate(deg2rad(90)), rotate(deg2rad(-90))) == ID
end
