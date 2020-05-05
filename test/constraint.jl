function test_constraint_name(constraint, name, F::Type, S::Type)
    @test name == @inferred JuMP.name(constraint)
end

function constraints_test(ModelType::Type{PolyModel{VT}}) where { VT}

    @testset "SingleVariable constraints" begin
        m = ModelType()
        @variable(m, x)

        @constraint(m, cref, x in MOI.LessThan(10.0))
        test_constraint_name(cref, "cref", VT,
                             MOI.LessThan{Float64})
        c = JuMP.constraint_object(cref)
        @test c.func == PolyModels.object(x)
        @test c.set == MOI.LessThan(10.0)

        @variable(m, y[1:2])
        @constraint(m, cref2[i=1:2], y[i] in MOI.LessThan(float(i)))
        test_constraint_name(cref2[1], "cref2[1]", VT,
                             MOI.LessThan{Float64})
        c = JuMP.constraint_object(cref2[1])
        @test c.func == PolyModels.object(y[1])
        @test c.set == MOI.LessThan(1.0)
    end

    @testset "VectorOfVariables constraints" begin
        m = ModelType()
        @variable(m, x[1:2])

        cref = @constraint(m, x .== 0)
        c = JuMP.constraint_object.(cref)
        @test c[1].func == PolyModels.object(x[1])
        @test c[2].set == MOI.EqualTo(0.0)

    end

    @testset "Linear constraints" begin
        @testset "Scalar" begin
            model = ModelType()
            @variable(model, x)

            cref = @constraint(model, 2x <= 10)
            @test "" == @inferred JuMP.name(cref)
            JuMP.set_name(cref, "c")
            test_constraint_name(cref, "c", polynomialtype(Float64, VT), MOI.GreaterThan{Float64})

            c = JuMP.constraint_object(cref)
            @test JuMP.isequal_canonical(c.func, -2*x + 10)
            @test c.set == MOI.GreaterThan(0.0)

            cref = @constraint(model, 3x + 1 ≥ 10)
            c = JuMP.constraint_object(cref)
            @test JuMP.isequal_canonical(c.func, 3x - 9)
            @test c.set == MOI.GreaterThan(0.0)

            cref = @constraint(model, 1 == -x)
            c = JuMP.constraint_object(cref)
            @test JuMP.isequal_canonical(c.func, x + 1)
            @test c.set == MOI.EqualTo(0.0)

            @test_throws MethodError @constraint(model, 2 == 1)
        end

        @testset "Vectorized" begin
            model = ModelType()
            @variable(model, x)

            err = ErrorException("In `@constraint(model, [x, 2x] == [1 - x, 3])`: Unexpected vector in scalar constraint. Did you mean to use the dot comparison operators like .==, .<=, and .>= instead?")
            @test_throws err @constraint(model, [x, 2x] == [1-x, 3])

            cref = @constraint(model, [x, 2x] .== [1-x, 3])
            c = JuMP.constraint_object.(cref)
            @test JuMP.isequal_canonical(c[1].func, 2.0x-1)
            @test c[1].set == MOI.EqualTo(0.0)
            @test JuMP.isequal_canonical(c[2].func, 2.0x-3)
            @test c[2].set == MOI.EqualTo(0.0)
        end

        @testset "Vector" begin
            model = ModelType()
            @test_throws MethodError  @constraint(model, [1, 2] in MOI.Zeros(2))
        end
    end
    @testset "delete / is_valid constraints" begin
        model = ModelType()
        @variable(model, x)
        constraint_ref = @constraint(model, 2x <= 1)
        @test JuMP.is_valid(model, constraint_ref)
        JuMP.delete(model, constraint_ref)
        @test !JuMP.is_valid(model, constraint_ref)
        second_model = ModelType()
        @test_throws AssertionError JuMP.delete(second_model, constraint_ref)
    end

    @testset "batch delete / is_valid constraints" begin
        model = ModelType()
        @variable(model, x[1:9])
        cons = [@constraint(model, sum(x[1:2:9]) <= 3)]
        push!(cons, @constraint(model, sum(x[2:2:8]) <= 2))
        push!(cons, @constraint(model, sum(x[1:3:9]) <= 1))
        @test all(JuMP.is_valid.(model, cons))
        JuMP.delete(model, cons[[1, 3]])
        @test all((!JuMP.is_valid).(model, cons[[1, 3]]))
        @test JuMP.is_valid(model, cons[2])
        second_model = ModelType()
        @test_throws AssertionError JuMP.delete(second_model, cons[[1, 3]])
        @test_throws AssertionError JuMP.delete(second_model, [cons[2]])
    end

    @testset "Broadcasted constraint (.==)" begin
        m = ModelType()
        @variable(m, x[1:2])

        A = [1.0 2.0; 3.0 4.0]
        b = [4.0, 5.0]

        cref = @constraint(m, A*x .== b)
        @test (2,) == @inferred size(cref)

        c1 = JuMP.constraint_object(cref[1])
        @test JuMP.isequal_canonical(c1.func, x[1] + 2x[2] - 4)
        @test c1.set == MOI.EqualTo(0.0)
        c2 = JuMP.constraint_object(cref[2])
        @test JuMP.isequal_canonical(c2.func, 3x[1] + 4x[2] - 5)
        @test c2.set == MOI.EqualTo(0.0)
    end

    @testset "Broadcasted constraint (.<=)" begin
        m = ModelType()
        @variable(m, x[1:2,1:2])

        UB = [1.0 2.0; 3.0 4.0]

        cref = @constraint(m, x .+ 1 .<= UB)
        @test (2,2) == @inferred size(cref)
        for i in 1:2
            for j in 1:2
                c = JuMP.constraint_object(cref[i,j])
                @test JuMP.isequal_canonical(c.func, -x[i,j] + UB[i,j] - 1)
                @test c.set == MOI.GreaterThan(0.0)
            end
        end
    end

    @testset "Broadcasted constraint with indices" begin
        m = ModelType()
        @variable m x[1:2]
        @constraint m cref1[i=2:4] x .== [i, i+1]
        ConstraintRefType = eltype(cref1[2])
        @test cref1 isa JuMP.Containers.DenseAxisArray{Vector{ConstraintRefType}}
        @constraint m cref2[i=1:3, j=1:4] x .≤ [i+j, i-j]
        ConstraintRefType = eltype(cref2[1])
        @test cref2 isa Matrix{Vector{ConstraintRefType}}
        @variable m y[1:2, 1:2]
        @constraint m cref3[i=1:2] y[i,:] .== 1
        ConstraintRefType = eltype(cref3[1])
        @test cref3 isa Vector{Vector{ConstraintRefType}}
    end

    @testset "Quadratic constraints" begin
        model = ModelType()
        @variable(model, x)
        @variable(model, y)

        cref = @constraint(model, x^2 + x <= 1)
        c = JuMP.constraint_object(cref)
        @test JuMP.isequal_canonical(c.func, -x^2 - x  + 1)
        @test c.set == MOI.GreaterThan(0.0)

        cref = @constraint(model, y*x - 1.0 == 0.0)
        c = JuMP.constraint_object(cref)
        @test JuMP.isequal_canonical(c.func, x*y-1)
        @test c.set == MOI.EqualTo(0.0)

    end


    @testset "Constraint name" begin
        model = ModelType()
        @variable(model, x)
        @constraint(model, con, x^2 == 1)
        test_constraint_name(con, "con", polynomialtype(Float64, VT), MOI.EqualTo{Float64})
        JuMP.set_name(con, "kon")
        @test JuMP.constraint_by_name(model, "con") isa Nothing
        test_constraint_name(con, "kon", polynomialtype(Float64, VT), MOI.EqualTo{Float64})
        @test_throws ErrorException  @constraint(model, kon, [x^2, x] .== 0 )
    end


   end

@testset "Constraints for PolyModel" begin
    constraints_test(PolyModel{PolyVar{true}})
    @testset "all_constraints (scalar)" begin
        model = PolyModel{PolyVar{true}}()
        @variable(model, x >= 0)
        @test 1 == num_constraints(model, polynomialtype(Float64, PolyVar{true}),
                                             MOI.GreaterThan{Float64})
        @test 0 ==  num_constraints(model, polynomialtype(Float64, PolyVar{true}) ,
                                             MOI.EqualTo{Float64})
     end

    @testset "list_of_constraint_types" begin
        model = PolyModel{PolyVar{true}}()
        @variable(model, x >= 0, Bin)
        @constraint(model, 2x <= 1)
        constraint_types = list_of_constraint_types(model)
        @test Set(constraint_types) == Set([
            (Polynomial{true, Float64}, MathOptInterface.EqualTo{Float64}),
            (Polynomial{true, Float64}, MathOptInterface.GreaterThan{Float64})
                                           ])
    end
end

