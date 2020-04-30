
@testset "Unsupported objective_function" begin
    model = PolyModel{PolyVar{true}}()
    func = MOI.SingleVariable(MOI.VariableIndex(1))
    @test_throws MethodError JuMP.set_objective_function(model, func)
end

@testset "Unsupported function in macro" begin
    model = PolyModel{PolyVar{true}}()
    @variable(model, x[1:2])
    @test_throws MethodError @objective(model, Min, x)
end

function objectives_test(ModelType::Type{PolyModel{VT}}) where {VT}
                      
    @testset "objective_sense set and get" begin
        model = ModelType()
        JuMP.set_objective_sense(model, MOI.FEASIBILITY_SENSE)
        @test MOI.FEASIBILITY_SENSE == @inferred JuMP.objective_sense(model)
    end

    @testset "SingleVariable objectives" begin
        m = ModelType()
        @variable(m, x)

        @objective(m, Min, x)
        @test MOI.MIN_SENSE == @inferred JuMP.objective_sense(m)
        @test JuMP.objective_function_type(m) == VT
        @test JuMP.objective_function(m) == x

        @objective(m, Max, x)
        @test MOI.MAX_SENSE == @inferred JuMP.objective_sense(m)
        @test JuMP.objective_function_type(m) == VT
        @test JuMP.objective_function(m) == x
    end

    @testset "Linear objectives" begin
        m = ModelType()
        @variable(m, x)

        @objective(m, Min, 2x)
        @test MOI.MIN_SENSE == @inferred JuMP.objective_sense(m)
        @test JuMP.objective_function_type(m) == termtype(VT, Int)
        @test JuMP.isequal_canonical(JuMP.objective_function(m), 2x)
        @objective(m, Max, x + 3x + 1)
        @test MOI.MAX_SENSE == @inferred JuMP.objective_sense(m)
        @test JuMP.objective_function_type(m) == polynomialtype(VT, Int)
        @test JuMP.isequal_canonical(JuMP.objective_function(m), 4x + 1)
    end

    @testset "Quadratic objectives" begin
        m = ModelType()
        @variable(m, x)

        @objective(m, Min, x^2 + 2x)
        @test MOI.MIN_SENSE == @inferred JuMP.objective_sense(m)
        @test JuMP.objective_function_type(m) == polynomialtype(VT, Int)

        @test JuMP.isequal_canonical(JuMP.objective_function(m), x^2 + 2x)
    end

    @testset "Sense as symbol" begin
        m = ModelType()
        @variable(m, x)
        @test_throws ErrorException @objective(m, :Min, 2x)
    end

    @testset "Sense in variable" begin
        m = ModelType()
        @variable(m, x)

        sense = MOI.MIN_SENSE
        @objective(m, sense, 2x)
        @test MOI.MIN_SENSE == @inferred JuMP.objective_sense(m)

        sense = :Min
        @test_throws ErrorException @objective(m, sense, 2x)
    end

    @testset "Constant objective" begin
        model = ModelType()
        @objective(model, Min, 3)
        @test JuMP.objective_sense(model) == MOI.MIN_SENSE
        @test JuMP.isequal_canonical(zero(polynomialtype(Float64, VT)),
            JuMP.objective_function(model))
    end
end
