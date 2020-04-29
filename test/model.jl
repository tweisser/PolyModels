@testset "Basic Model" begin
    @test PolyModel{Float64, PolyVar{true}}() isa PolyModel
    @test PolyModel{Float64, PolyVar{false}}() isa PolyModel
     
end
