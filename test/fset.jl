function test_fset_problem(ModelType)
    m = ModelType()
    @variable m x[1:3]
    @objective m Min sum(x[i]^i for i in 1:3)
    @constraint m 1 - x'*x >= 0
    @constraint m x'*x <= 1
    @constraint m x[2] == 1
    @constraint m x[1]*x[2] + 1== x[3]
    fix(x[3], 0)
    return m
end


function test_csp_fset_problem(ModelType)
    m = ModelType()
    @variable m x[1:4]
    @objective m Min sum(x[i]*x[i+1] for i in 1:3)
    @constraint m sum(x[2:4])  <= 1
    @constraint m x[2] == 1
    @constraint m x[1]*x[2] + 1 == x[3]
    return m
end


@testset "Feasible Sets" begin
    @testset "Feasible Set" begin
        m = test_fset_problem(PolyModel{PolyVar{true}})
        F = feasible_set(m)
        @test variables(F) == sort!(PolyModels.object.(all_variables(m)), rev = true)
        @test cliques(F) == [variables(F)]
        @test set(F) isa PolyModels.SemialgebraicSets.BasicSemialgebraicSet
        @test sets(F) == [set(F)]
        @test length(PolyModels.SemialgebraicSets.equalities(set(F))) == 3
        @test length(PolyModels.SemialgebraicSets.inequalities(set(F))) == 2
    end
    @testset "Feasible Set with Fix" begin
        m = test_fset_problem(PolyModel{PolyVar{true}})
        F = feasible_set(m; fix = true)
        @test length(variables(F)) == 1
        @test length(fixed_variables(F)) == 2
        @test Set(values(fixed_variables(F))) == Set([1.0, 0.0])
        @test length(PolyModels.SemialgebraicSets.equalities(set(F))) == 1
        @test length(PolyModels.SemialgebraicSets.inequalities(set(F))) == 2
    end

    @testset "CSP Feasible Set" begin
        m = test_fset_problem(PolyModel{PolyVar{true}})
        F = feasible_set(m; csp = true, fix = false)
        @test isempty(fixed_variables(F))
        @test length(cliques(F)) == 1

        F = feasible_set(m; csp = true, fix = true)
        @test length(fixed_variables(F)) == 2

        m = test_csp_fset_problem(PolyModel{PolyVar{true}})
        F = feasible_set(m; csp = true)
        @test length(cliques(F)) == 2
        @test isempty(fixed_variables(F))
        @test length(cliques(F)[1]) == 3
        @test length(cliques(F)[2]) == 3

        F = feasible_set(m; csp = true, fix = true)
        @test length(cliques(F)) == 2
        @test length(fixed_variables(F)) == 1
        @test length(cliques(F)[1]) == 2
        @test length(cliques(F)[2]) == 2


    end
end
