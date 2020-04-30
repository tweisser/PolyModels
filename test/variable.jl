#tests adapted from JuMP/test/variable.jl 

function test_variable_no_bound(ModelType::Type{PolyModel{CT, VT}}) where {CT, VT}
    model = ModelType()
    @variable(model, nobounds)
    @test nobounds isa VT
    @test zero(nobounds) isa polynomialtype(nobounds)
    @test one(nobounds) isa monomialtype(nobounds)
    @test_throws ErrorException @variable(model, nobounds)
end

function test_variable_binary_plus_x(ModelType)
    model = ModelType()
    @variable(model, lbonly >= 0, Bin)
    
    # Has the lower bound be ignored because Bin includes this information?
    @test length(model.constraints) == 1
    # Has binary constraint be encoded as constraint? 
    @test jump_function(model.constraints[1]) == -lbonly^2 + lbonly
    @test moi_set(model.constraints[1]) == MOI.EqualTo{Float64}(0.0)

    @test_throws ErrorException @variable(model, lbonly)

end
function test_variable_lower_bound_rhs(ModelType)
    model = ModelType()
    @variable(model, lblhs >= 0)

    # Has lower bound be encoded as constraint? 
    @test jump_function(model.constraints[1]) == lblhs
    @test moi_set(model.constraints[1]) == MOI.GreaterThan{Float64}(0.0)
end

function test_variable_lower_bound_lhs(ModelType)
    model = ModelType()
    @variable(model, 0 <= lblhs)

    # Has lower bound be encoded as constraint? 
    @test jump_function(model.constraints[1]) == lblhs
    @test moi_set(model.constraints[1]) == MOI.GreaterThan{Float64}(0.0) 
end

function test_variable_upper_bound_rhs(ModelType)
    model = ModelType()
    @variable(model, ubrhs <= 1)
    
    # Has upper bound be encoded as constraint? 
    @test jump_function(model.constraints[1]) == -ubrhs + 1.0
    @test moi_set(model.constraints[1]) == MOI.GreaterThan{Float64}(0.0) 

end

function test_variable_upper_bound_lhs(ModelType)
    model = ModelType()
    @variable(model, 1 >= ublhs)
    
    # Has upper bound be encoded as constraint? 
    @test jump_function(model.constraints[1]) == -ublhs + 1.0
    @test moi_set(model.constraints[1]) == MOI.GreaterThan{Float64}(0.0) 
end

function test_variable_interval(ModelType)
    function has_bounds(model, idx, var, lb, ub)
        con = model.constraints[idx] 
        @test jump_function(con) == (var - lb)*(ub - var)
        @test moi_set(con) == MOI.GreaterThan{Float64}(0.0) 
    end
    model = ModelType()
    ct = 0
    @variable(model, 0 <= bothb1 <= 1)
    ct += 1
    has_bounds(model, ct, bothb1, 0.0, 1.0)
    @variable(model, 0 ≤  bothb2 ≤  1)
    ct += 1
    has_bounds(model, ct, bothb2, 0.0, 1.0)    
    @variable(model, 1 >= bothb3 >= 0)
    ct += 1
    has_bounds(model, ct, bothb3, 0.0, 1.0)
    @variable(model, 1 ≥  bothb4 ≥  0)
    ct += 1
    has_bounds(model, ct, bothb4, 0.0, 1.0)
end

function test_variable_fix(ModelType)
    model = ModelType()
    @variable(model, fixed == 1.0)

    # Has fixed be encoded as constraint? 
    @test jump_function(model.constraints[1]) == fixed - 1.0
    @test moi_set(model.constraints[1]) == MOI.EqualTo{Float64}(0.0) 

end

function test_variable_int(ModelType)
    model = ModelType()
    @variable model int1 lower_bound = 1 upper_bound = 3 Int
    @test jump_function(model.constraints[1]) == (int1-1)*(int1-2)*(int1-3)
    @test moi_set(model.constraints[1]) == MOI.EqualTo{Float64}(0.0) 
    
    @variable model int2 lower_bound = 0.9 upper_bound = 3.1 Int
    @test jump_function(model.constraints[2]) == (int2-1)*(int2-2)*(int2-3)

    @test_throws AssertionError @variable(model, int3, Int)
    @test_throws AssertionError @variable(model, int3, Int, lower_bound = 0)
    @test_throws AssertionError @variable(model, int3, Int, upper_bound = 7)

    @test length(model.variables) == 2
end

function test_variable_start(ModelType)
    model = ModelType()
    info = VariableInfo(false, NaN, false, NaN, false, NaN, true, 1.0, false, false)
    @test_logs (:warn, "Start value not supported. Information has been lost.") JuMP.add_variable(model, JuMP.build_variable(x -> "", info), "x")
end

function test_variable_custom_index_sets(ModelType)
    model = ModelType()
    @variable(model, 0 <= onerangeub[-7:1] <= 10, Int)
    @variable(model, manyrangelb[0:1, 10:20, 1:1] >= 2)
    @test jump_function(model.constraints[20]) == manyrangelb[0, 15, 1] - 2
    @test moi_set(model.constraints[20]) == MOI.GreaterThan{Float64}(0.0)
    s = ["Green","Blue"]
    @variable(model, x[i=-10:10, s] <= 5.5)
    @test isequal(model[:onerangeub][-7], onerangeub[-7])
    @test_throws KeyError model[:foo]
end

function test_variable_anonymous(ModelType)
    model = ModelType()
    @test_throws ErrorException @variable(model, [(0, 0)])  # #922
    x = @variable(model, [(0, 2)])
    @test "noname" == @inferred MultivariatePolynomials.name(x[0])
    @test "noname" == @inferred MultivariatePolynomials.name(x[2])
end

function test_variable_is_valid_delete(ModelType)
    model = ModelType()
    @variable(model, x)
    @test JuMP.is_valid(model, x)
    JuMP.delete(model, x)
    @test !JuMP.is_valid(model, x)
    second_model = ModelType()
    @test_throws Exception JuMP.delete(second_model, x)
end

function test_variable_oneto_index_set(ModelType::Type{PolyModel{CT, VT}}) where {CT, VT}
    # Tests that Base.OneTo can be used in index set (JuMP issue #933).
    model = ModelType()
    auto_var = @variable(model, [Base.OneTo(3), 1:2], container=Auto)
    @test auto_var isa Matrix{VT}
    @test (3, 2) == @inferred size(auto_var)
    array_var = @variable(model, [Base.OneTo(3), 1:2], container=Array)
    @test array_var isa Matrix{VT}
    @test (3, 2) == @inferred size(array_var)
    denseaxisarray_var = @variable(model, [Base.OneTo(3), 1:2], container=DenseAxisArray)
    @test denseaxisarray_var isa JuMP.Containers.DenseAxisArray{VT}
    @test length.(axes(denseaxisarray_var)) == (3, 2)
end

function test_variable_base_name_in_macro(ModelType)
    model = ModelType()
    @variable(model, normal_var)
    @test MultivariatePolynomials.name(normal_var) == "normal_var"
    no_indices = @variable(model, base_name="foo")
    @test MultivariatePolynomials.name(no_indices) == "foo"
    # Note that `z` will be ignored in name.
    indices = @variable(model, z[i=2:3], base_name="t")
    @test MultivariatePolynomials.name(indices[2]) == "t[2]"
    @test MultivariatePolynomials.name(indices[3]) == "t[3]"
end

function test_variable_name(ModelType)
    model = ModelType()
    @variable(model, x)
    @test_throws MethodError set_name(x, "y")
    @test JuMP.variable_by_name(model, "x") == x
    y = @variable(model, base_name="x")
    err(name) = ErrorException("Multiple variables have the name $name.")
    @test JuMP.variable_by_name(model, "y") isa Nothing
    @test_throws err("x") JuMP.variable_by_name(model, "x")
end

function test_variable_condition_in_indexing(ModelType)
    function test_one_dim(x)
        @test 5 == @inferred length(x)
        for i in 1:10
            if iseven(i)
                @test haskey(x, i)
            else
                @test !haskey(x, i)
            end
        end
    end

    function test_two_dim(y)
        @test 15 == @inferred length(y)
        for j in 1:10, k in 3:2:9
            if isodd(j+k) && k <= 8
                @test haskey(y, (j,k))
            else
                @test !haskey(y, (j,k))
            end
        end
    end

    model = ModelType()
    # Parses as ref on 0.7.
    @variable(model, named_one_dim[i=1:10; iseven(i)])
    test_one_dim(named_one_dim)
    # Parses as vcat on 0.7.
    anon_one_dim = @variable(model, [i=1:10; iseven(i)])
    test_one_dim(anon_one_dim)
    # Parses as typed_vcat on 0.7.
    @variable(model, named_two_dim[j=1:10, k=3:2:9; isodd(j + k) && k <= 8])
    test_two_dim(named_two_dim)
    # Parses as vect on 0.7.
    anon_two_dim = @variable(model, [j=1:10, k=3:2:9; isodd(j + k) && k <= 8])
    test_two_dim(anon_two_dim)
end

function test_variable_macro_return_type(ModelType::Type{PolyModel{CT, VT}}) where {CT, VT}
    model = ModelType()
    @variable(model, x[1:3, 1:4, 1:2])
    @test typeof(x) == Array{VT,3}
end


function test_variable_end_indexing(ModelType)
    model = ModelType()
    @variable(model, x[0:2, 1:4])
    @variable(model, z[0:2])
    @test x[end,1] == x[2, 1]
    @test x[0, end-1] == x[0, 3]
    @test z[end] == z[2]
    @test_throws KeyError x[end-1]
end

function test_variable_unsigned_index(ModelType)
    # Tests unsigned int can be used to construct index set (JuMP issue #857).
    model = ModelType()
    t = UInt(4)
    @variable(model, x[1:t])
    @test 4 == @inferred num_variables(model)
end

function test_batch_delete_variables(ModelType)
    model = ModelType()
    @variable(model, x[1:3] >= 1)
    @objective(model, Min, sum([1, 2, 3] .* x))
    @test all(is_valid.(model, x))
    delete(model, x[[1, 3]])
    @test all((!is_valid).(model, x[[1, 3]]))
    @test is_valid(model, x[2])
    second_model = ModelType()
    @test_throws Exception JuMP.delete(second_model, x[2])
    @test_throws Exception JuMP.delete(second_model, x[[1, 3]])
end

function variables_test(ModelType)

    @testset "Constructors" begin
        test_variable_no_bound(ModelType)
        test_variable_binary_plus_x(ModelType)
        test_variable_lower_bound_rhs(ModelType)
        test_variable_lower_bound_lhs(ModelType)
        test_variable_upper_bound_rhs(ModelType)
        test_variable_upper_bound_lhs(ModelType)
        test_variable_interval(ModelType)
        test_variable_fix(ModelType)
        test_variable_int(ModelType)
        test_variable_start(ModelType)
        test_variable_custom_index_sets(ModelType)
        test_variable_anonymous(ModelType)
    end

    @testset "isvalid and delete variable" begin
        test_variable_is_valid_delete(ModelType)
    end

    @testset "Base.OneTo as index set (#933)" begin
        test_variable_oneto_index_set(ModelType)
    end

    @testset "base_name= in @variable" begin
        test_variable_base_name_in_macro(ModelType)
    end

    @testset "condition in indexing" begin
        test_variable_condition_in_indexing(ModelType)
    end

    @testset "@variable returning Array{VariableRef}" begin
        test_variable_macro_return_type(ModelType)
    end

    @testset "end for indexing a DenseAxisArray" begin
        test_variable_end_indexing(ModelType)
    end

    @testset "Unsigned dimension lengths (#857)" begin
        test_variable_unsigned_index(ModelType)
    end

    @testset "Batch deletion of variables" begin
        test_batch_delete_variables(ModelType)
    end
end

@testset "Variables for PolyModel" begin
    variables_test(PolyModel{Float64, PolyVar{true}})
    @testset "all_variables" begin
        model = PolyModel{Float64, PolyVar{true}}()
        @variable(model, x)
        @variable(model, y)
        @test [x, y] == @inferred JuMP.all_variables(model)
    end
    @testset "@variables" begin
        model = PolyModel{Float64, PolyVar{true}}()
        @variables model begin
            0 ≤ x[i=1:2] ≤ i, Int
            y ≥ 2, Bin
            z ≤ 3
        end
        @test length(model.constraints) == 4
    end
end
