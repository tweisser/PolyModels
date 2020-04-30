@testset "Basic Model" begin
    @test PolyModel{PolyVar{true}}() isa PolyModel
    @test PolyModel{PolyVar{false}}() isa PolyModel
     
end
