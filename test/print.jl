@testset "Printing" begin
    m = PolyModel{PolyVar{true}}()
    @variable m x[1:3]
    @constraint m  x .>= 0
    sumcon = @constraint m sum(x) == 1
    @objective m Min (x[1] - x[2])^2 + (x[2] - x[3])^2

    @test function_string(JuMP.REPLMode, jump_function(constraint_object(sumcon))) == "x₁ + x₂ + x₃ - 1.0"
    @test sprint(show, m) isa String
    @test sprint(print, m) isa String
end

