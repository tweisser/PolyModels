function test_model_basic(ModelType)
    m = ModelType() 
    @test m isa PolyModel
    @test termination_status(m) == MOI.TerminationStatusCode(24)
    @test backend(m) == m
    @test JuMP.num_nl_constraints(m) == 0

end

@testset "Basic Model" begin
    test_model_basic(PolyModel{PolyVar{true}})
    test_model_basic(PolyModel{PolyVar{false}})

    m = PolyModel{PolyVar{true}}()
    @test PolyModels.polyvariable_type(m) == PolyVar{true}
end
