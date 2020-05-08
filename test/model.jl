function test_model_basic(ModelType)
    @test ModelType() isa PolyModel
end

@testset "Basic Model" begin
    test_model_basic(PolyModel{PolyVar{true}})
    test_model_basic(PolyModel{PolyVar{false}})
    @test JuMP.num_nl_constraints(PolyModel{PolyVar{true}}()) == 0
end
