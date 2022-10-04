using VectorGraph
using Test

@testset "vectors" begin
    # addition
    @test Vectors.Vec2(1, 2) + Vectors.Vec2(3, 4) == Vectors.Vec2(4, 6)
    @test Vectors.Vec3(1, 2, 1) + Vectors.Vec3(3, 4, 1) == Vectors.Vec3(4, 6, 2)
    # subtraction
    @test Vectors.Vec2(3, 4) - Vectors.Vec2(1, 2) == Vectors.Vec2(2, 2)
    @test Vectors.Vec3(3, 4, 5) - Vectors.Vec3(1, 2, 3) == Vectors.Vec3(2, 2, 2)
    # negation
    @test -Vectors.Vec2(3, 4) == Vectors.Vec2(-3, -4)
    @test -Vectors.Vec3(3, 4, 5) == Vectors.Vec3(-3, -4, -5)
    # inversion
    @test Vectors.invert(Vectors.Vec2(2, 2)) == Vectors.Vec2(0.5, 0.5)
    @test Vectors.invert(Vectors.Vec3(2, 2, 2)) == Vectors.Vec3(0.5, 0.5, 0.5)
    # summation
    @test +(Vectors.Vec2(1, 2), Vectors.Vec2(3, 4), Vectors.Vec2(1, 1)) == Vectors.Vec2(5, 7)
    @test +(Vectors.Vec2(3, 4)) == Vectors.Vec2(3, 4)
    v3 = Vectors.Vec3(1, 2, 3)
    @test +(v3, v3, v3, v3) == Vectors.Vec3(4, 8, 12)
    # magnitude
    @test Vectors.mag(Vectors.Vec2(3, 4)) == 5
    @test Vectors.mag(Vectors.Vec3(1, 2, 3)) == 3.741657386773941
    # scale
    @test Vectors.scale(Vectors.Vec2(3, 4), 11) == Vectors.Vec2(33, 44)
    @test Vectors.scale(Vectors.Vec3(3, 4, 1), 11) == Vectors.Vec3(33, 44, 11)
    # dot product
    @test Vectors.dot(Vectors.Vec2(3, 4), Vectors.Vec2(1, 2)) == 11
    @test Vectors.dot(Vectors.Vec3(3, 4, 5), Vectors.Vec3(1, 2, 3)) == 26
    # distance
    @test Vectors.distance(Vectors.Vec2(8, 0), Vectors.Vec2(1, 0)) == 7
    @test Vectors.distance(Vectors.Vec3(10, 0, 0), Vectors.Vec3(3, 0, 0)) == 7
    # interpolation
    @test Vectors.lerp(Vectors.Vec2(1, 1), Vectors.Vec2(3, 2), 0.5) == Vectors.Vec2(2, 1.5)
    @test Vectors.lerp(Vectors.Vec3(3, 4, 5), Vectors.Vec3(1, 2, 3), 0.5) == Vectors.Vec3(2, 3, 4)
    # normalize
    @test Vectors.normalize(Vectors.Vec2(3, 4)) == Vectors.Vec2(0.6, 0.8)
    @test Vectors.normalize(Vectors.Vec3(3, 4, 1)) == Vectors.Vec3(0.5883484054145521, 0.7844645405527362, 0.19611613513818404)
end

@testset "matrices" begin
    @test Matrices.Mat3(1, 0, 0, 1, 0, 0) == Matrices.identity()
end
