#tests adapted from JuMP/test/variable.jl 

function test_variable_name(variable, name)
    @test name == JuMP.name(variable)
    @test variable == JuMP.variable_by_name(JuMP.owner_model(variable), name)
end

function test_variable_no_bound(ModelType::Type{PolyModel{VT}}) where {VT}
    model = ModelType()
    @variable(model, nobounds)
    @test nobounds isa PolyVariableRef
    @test !JuMP.has_lower_bound(nobounds)
    @test !JuMP.has_upper_bound(nobounds)
    @test !JuMP.is_fixed(nobounds)
    @test !JuMP.is_binary(nobounds)
    @test !JuMP.is_integer(nobounds)
    test_variable_name(nobounds, "nobounds")
    @test zero(nobounds) isa polynomialtype(VT, Int)
    @test one(nobounds) isa monomialtype(VT)
    @test_throws ErrorException @variable(model, nobounds)
    @test nobounds[1] == nobounds
    @test !iszero(nobounds)
    x = nobounds
    @test JuMP.isequal_canonical(x, x)
    @test x + x - x == PolyModels.object(x)
    @test x - x + x == PolyModels.object(x)
    @test 2*x - x == PolyModels.object(x)
    @test x*2 - x == PolyModels.object(x)
end

function test_variable_name(ModelType)
    model = ModelType()
    @variable(model, x)
    test_variable_name(x, "x")
    JuMP.set_name(x, "y")
    @test JuMP.variable_by_name(model, "x") isa Nothing
    test_variable_name(x, "y")
    y = @variable(model, base_name="y")
    err(name) = ErrorException("Multiple variables have the name $name.")
    @test_throws err("y") JuMP.variable_by_name(model, "y")
    JuMP.set_name(y, "x")
    test_variable_name(x, "y")
    test_variable_name(y, "x")
    JuMP.set_name(x, "x")
    @test_throws err("x") JuMP.variable_by_name(model, "x")
    @test JuMP.variable_by_name(model, "y") isa Nothing
    JuMP.set_name(y, "y")
    test_variable_name(x, "x")
    test_variable_name(y, "y")   
end

function test_constraint(cref, jumpfunction, moiset)
    @test jump_function(constraint_object(cref)) == jumpfunction
    @test moi_set(constraint_object(cref)) == moiset  
end

function test_variable_lower_bound_rhs(ModelType)
    model = ModelType()
    @variable(model, lbrhs >= 0)

    @test JuMP.has_lower_bound(lbrhs)
    @test JuMP.lower_bound(lbrhs) == 0.0
    test_constraint(JuMP.LowerBoundRef(lbrhs), PolyModels.object(lbrhs), MOI.GreaterThan(0.0))

    @test !JuMP.has_upper_bound(lbrhs)
    @test !JuMP.is_fixed(lbrhs)
    @test !JuMP.is_binary(lbrhs)
    @test !JuMP.is_integer(lbrhs)
    
    @test isequal(model[:lbrhs], lbrhs)
    JuMP.delete_lower_bound(lbrhs)
    @test !JuMP.has_lower_bound(lbrhs)
    @test isempty(all_constraints(model))
    @test length(all_variables(model)) == 1
end

function test_variable_lower_bound_lhs(ModelType)
    model = ModelType()
    @variable(model, 0 <= lblhs)
    
    @test JuMP.has_lower_bound(lblhs)
    @test 0.0 == JuMP.lower_bound(lblhs)
    test_constraint(JuMP.LowerBoundRef(lblhs), PolyModels.object(lblhs), MOI.GreaterThan(0.0))
   
    @test !JuMP.has_upper_bound(lblhs)
    @test !JuMP.is_fixed(lblhs)
    @test !JuMP.is_binary(lblhs)
    @test !JuMP.is_integer(lblhs)
end

function test_variable_upper_bound_rhs(ModelType)
    model = ModelType()
    @variable(model, ubrhs <= 1)

    @test JuMP.has_upper_bound(ubrhs)
    @test 1.0 == JuMP.upper_bound(ubrhs)
    test_constraint(JuMP.UpperBoundRef(ubrhs), -ubrhs + 1.0, MOI.GreaterThan(0.0))
    
    @test !JuMP.has_lower_bound(ubrhs)
    @test !JuMP.is_fixed(ubrhs)
    @test !JuMP.is_binary(ubrhs)
    @test !JuMP.is_integer(ubrhs)

    @test isequal(model[:ubrhs], ubrhs)
    JuMP.delete_upper_bound(ubrhs)
    @test !JuMP.has_upper_bound(ubrhs)
    @test isempty(all_constraints(model))
    @test length(all_variables(model)) == 1
end

function test_variable_upper_bound_lhs(ModelType)
    model = ModelType()
    @variable(model, 1 >= ublhs)
    
    @test JuMP.has_upper_bound(ublhs)
    @test 1.0 == JuMP.upper_bound(ublhs)
    test_constraint(JuMP.UpperBoundRef(ublhs), -ublhs + 1.0, MOI.GreaterThan(0.0))

    @test !JuMP.has_lower_bound(ublhs)
    @test !JuMP.is_fixed(ublhs)
    @test !JuMP.is_binary(ublhs)
    @test !JuMP.is_integer(ublhs) 
end

function test_variable_interval(ModelType)
    function has_bounds(var, lb, ub)
        @test PolyModels.in_interval(var)
        @test JuMP.has_lower_bound(var)
        @test lb == JuMP.lower_bound(var)
        @test JuMP.has_upper_bound(var)
        @test ub == JuMP.upper_bound(var)
        @test interval(var) == [lb, ub]
        
        test_constraint(IntervalRef(var), (var - lb)*(ub - var), MOI.GreaterThan(0.0))

        @test !JuMP.is_fixed(var)
        @test !JuMP.is_binary(var)
        @test !JuMP.is_integer(var) 
    end

    model = ModelType()
    @variable(model, 0 <= bothb1 <= 1)
    has_bounds(bothb1, 0.0, 1.0)
    @variable(model, 0 ≤  bothb2 ≤  1)
    has_bounds(bothb2, 0.0, 1.0)    
    @variable(model, 1 >= bothb3 >= 0)
    has_bounds(bothb3, 0.0, 1.0)
    @variable(model, 1 ≥  bothb4 ≥  0)
    has_bounds(bothb4, 0.0, 1.0)

    delete_in_interval(bothb4)
    @test !PolyModels.in_interval(bothb4)
    @test length(all_constraints(model)) == 3
    @test length(all_variables(model)) == 4

end

function test_variable_interval_delete_upper(ModelType)
    model = ModelType()
    @variable(model, 0 <= both <= 1)

    @test length(all_constraints(model)) == 1

    JuMP.delete_upper_bound(both)
    @test !has_upper_bound(both)
    @test has_lower_bound(both)
    @test !PolyModels.in_interval(both)

    test_constraint(JuMP.LowerBoundRef(both), PolyModels.object(both), MOI.GreaterThan(0.0))

    set_in_interval(both, 0, 1)
    @test in_interval(both)
    
    set_in_interval(both, -1, 2)
    @test in_interval(both)
end

function test_variable_interval_delete_lower(ModelType)
    model = ModelType()
    @variable(model, 0 <= both <= 1)

    @test length(all_constraints(model)) == 1

    JuMP.delete_lower_bound(both)
    @test has_upper_bound(both)
    @test !has_lower_bound(both)
    @test !PolyModels.in_interval(both)

    test_constraint(JuMP.UpperBoundRef(both), -PolyModels.object(both) + 1, MOI.GreaterThan(0.0))

    set_in_interval(both, 0, 1)
    @test in_interval(both)
    
    set_in_interval(both, 0, 2)
    @test in_interval(both)

end

function test_variable_fix(ModelType)
    model = ModelType()
    @variable(model, fixed == 1.0)
    @test is_fixed(fixed)
    @test JuMP.fix_value(fixed) == 1.0
    test_constraint(JuMP.FixRef(fixed), fixed - 1.0, MOI.EqualTo(0.0))
    
    @test !JuMP.has_lower_bound(fixed)
    @test !JuMP.has_upper_bound(fixed)
    @test !JuMP.is_binary(fixed)
    @test !JuMP.is_integer(fixed)

    unfix(fixed)
    @test !is_fixed(fixed)
    @test isempty(all_constraints(model))
    @test length(all_variables(model)) == 1

    JuMP.set_lower_bound(fixed, 0.0)
    @test_throws Exception JuMP.fix(fixed, 1.0)
    JuMP.fix(fixed, 1.0; force = true)
    @test !JuMP.has_lower_bound(fixed)
    @test !JuMP.has_upper_bound(fixed)
    @test JuMP.is_fixed(fixed)
    @test 1.0 ==  JuMP.fix_value(fixed)
end

function test_variable_binary(ModelType)
    model = ModelType()
    @variable(model, bin, Bin)
    @test is_binary(bin)
    test_constraint(JuMP.BinaryRef(bin), bin*(1-bin), MOI.EqualTo(0.0))
    
    @test !JuMP.has_lower_bound(bin)
    @test !JuMP.has_upper_bound(bin)
    @test !JuMP.is_fixed(bin)
    @test !JuMP.is_integer(bin)

    unset_binary(bin)
    @test !is_binary(bin)
    @test isempty(all_constraints(model))
    @test length(all_variables(model)) == 1
end

function test_variable_integer(ModelType)
    model = ModelType()
    @variable model int1 lower_bound = 1 upper_bound = 3 Int

    @test is_integer(int1)
    test_constraint(JuMP.IntegerRef(int1), (int1-1)*(int1-2)*(int1-3), MOI.EqualTo(0.0))

    @variable model int2 lower_bound = 0.9 upper_bound = 3.1 Int
    @test is_integer(int2)
    test_constraint(JuMP.IntegerRef(int2), (int2-1)*(int2-2)*(int2-3), MOI.EqualTo(0.0))

    @test_throws AssertionError @variable(model, int3, Int)
    @test_throws AssertionError @variable(model, int3, Int, lower_bound = 0)
    @test_throws AssertionError @variable(model, int3, Int, upper_bound = 7)

    @test length(model.variables) == 2

    unset_integer(int1)
    @test !is_integer(int1)
    @test in_interval(int1)
    test_constraint(IntervalRef(int1), (int1 - 1)*(3 - int1), MOI.GreaterThan(0.0))
    
    unset_integer(int2)
    @test !is_integer(int2)
    @test in_interval(int2)
    test_constraint(IntervalRef(int2), (int2 - 0.9)*(3.1 - int2), MOI.GreaterThan(0.0))
end


function test_variable_starts_set_get(ModelType)
    model = ModelType()
    @variable(model, y, start = 1.0)
    @test start_value(y) == 1.0
    JuMP.set_start_value(y, 0.0)
    @test start_value(y) == 0.0

    @variable(model, x[1:3])
    x0 = collect(1:3)
    JuMP.set_start_value.(x, x0)
    @test start_value.(x) == x0
    @test JuMP.start_value.([x[1],x[2],x[3]]) == x0
end

function test_variable_bounds_set_get(ModelType)
    model = ModelType()
    @variable(model, 0 <= x <= 2)
    @test 0 == JuMP.lower_bound(x)
    @test 2 == JuMP.upper_bound(x)
    @test_throws Exception LowerBoundRef(x)
    @test_throws Exception UpperBoundRef(x)
    @test length(all_constraints(model)) == 1

    set_lower_bound(x, 1)
    @test 1 == JuMP.lower_bound(x)
    @test_throws Exception LowerBoundRef(x)
    @test_throws Exception UpperBoundRef(x)
    test_constraint(IntervalRef(x), (x-1)*(2-x), MOI.GreaterThan(0.0))
    @test length(all_constraints(model)) == 1

    set_lower_bound(x, 2)
    @test 2 == JuMP.lower_bound(x)
    
    set_upper_bound(x, 3)
    @test 3 == JuMP.upper_bound(x)
    test_constraint(IntervalRef(x), (x-2)*(3-x), MOI.GreaterThan(0.0))
    @test length(all_constraints(model)) == 1
    
    delete_lower_bound(x)
    @test !has_lower_bound(x)
    set_upper_bound(x, 2)
    @test upper_bound(x) == 2
end

function test_variable_fixed_set_get(ModelType)
    model = ModelType()
    @variable(model, fixedvar == 2)
    @test 2.0 == JuMP.fix_value(fixedvar)
    test_constraint(FixRef(fixedvar), fixedvar - 2, MOI.EqualTo(0.0))
    
    JuMP.fix(fixedvar, 5)
    @test 5 == JuMP.fix_value(fixedvar)
    test_constraint(FixRef(fixedvar), fixedvar - 5, MOI.EqualTo(0.0))
    
    @test_throws Exception JuMP.lower_bound(fixedvar)
    @test_throws Exception JuMP.upper_bound(fixedvar)

end

function test_variable_binary_set_get(ModelType)
    model = ModelType()
    @variable(model, q, Bin)
    @test !JuMP.has_lower_bound(q)
    @test !JuMP.has_upper_bound(q)
    @test !in_interval(q)

    test_constraint(BinaryRef(q), q*(1-q), MOI.EqualTo(0.0))
    
    set_lower_bound(q, 1)
    @test length(all_constraints(model)) == 2
    @test JuMP.has_lower_bound(q)

    set_upper_bound(q, 1)
    @test length(all_constraints(model)) == 2
    @test in_interval(q)

    @variable(model, 0 <= y <= 2, Bin)
    @test 0 == JuMP.lower_bound(y)
    @test 2 == JuMP.upper_bound(y)
    @test is_binary(y)





end


function test_variable_integrality_set_get(ModelType)
    model = ModelType()
    @variable(model, x[1:3])

    # Unlike than in JuMP.Model for PolyModel integrality constraints can only be formulated in combination with lower and upper bounds. 
    @test_throws AssertionError JuMP.set_integer(x[2])
    @test !JuMP.is_integer(x[2])
    
    set_lower_bound(x[2], 0)
    set_upper_bound(x[2], 2)

    set_integer(x[2])
    @test JuMP.is_integer(x[2])
    @test has_lower_bound(x[2])
    @test has_upper_bound(x[2])
    @test lower_bound(x[2]) == 0
    @test upper_bound(x[2]) == 2
    @test length(all_constraints(model)) == 1


    set_integer(x[2]) # second call does not add new constraints
    @test length(all_constraints(model)) == 1

    set_upper_bound(x[2], 3) # should change integrality constraint and instead of introducing a new constraint.
    @test length(all_constraints(model)) == 1    
    @test JuMP.is_integer(x[2])    
    test_constraint(IntegerRef(x[2]), x[2]*(x[2]-1)*(x[2]-2)*(x[2]-3), MOI.EqualTo(0.0))
   
    set_lower_bound(x[2], 1) # should change integrality constraint and instead of introducing a new constraint.
    @test length(all_constraints(model)) == 1
    @test JuMP.is_integer(x[2])    
    test_constraint(IntegerRef(x[2]), (x[2]-1)*(x[2]-2)*(x[2]-3), MOI.EqualTo(0.0))

    set_in_interval(x[2], 0, 2)
    @test length(all_constraints(model)) == 1
    @test JuMP.is_integer(x[2])
    test_constraint(IntegerRef(x[2]), x[2]*(x[2]-1)*(x[2]-2), MOI.EqualTo(0.0))

    JuMP.unset_integer(x[2])
    @test !JuMP.is_integer(x[2])

    JuMP.set_binary(x[1])
    JuMP.set_binary(x[1])  # test duplicated call
    @test JuMP.is_binary(x[1])
    @test_throws Exception JuMP.set_integer(x[1])
    JuMP.unset_binary(x[1])
    @test !JuMP.is_binary(x[1])

    @variable(model, y, binary = true)
    @test JuMP.is_binary(y)
    @test_throws Exception JuMP.set_integer(y)
    JuMP.unset_binary(y)
    @test !JuMP.is_binary(y)

    @test_throws ErrorException set_binary(x[2])
    set_binary(x[2]; force = true)
    @test is_binary(x[2])
    @test !is_integer(x[2])
end

function test_variable_custom_index_sets(ModelType)
    model = ModelType()
    @variable(model, 0 <= onerangeub[-7:1] <= 10, Int)
    @variable(model, manyrangelb[0:1, 10:20, 1:1] >= 2)
    @test has_lower_bound(manyrangelb[0, 15, 1]) 
    test_constraint(LowerBoundRef(manyrangelb[0, 15, 1]), manyrangelb[0, 15, 1] - 2.0, MOI.GreaterThan(0.0))
    s = ["Green","Blue"]
    @variable(model, x[i=-10:10, s] <= 5.5, start=i+1)
    @test 5.5 ==  JuMP.upper_bound(x[-4, "Green"])
    test_variable_name(x[-10, "Green"], "x[-10,Green]")
    @test JuMP.start_value(x[-3, "Blue"]) == -2
    @test isequal(model[:onerangeub][-7], onerangeub[-7])
    @test_throws KeyError model[:foo]
end

function test_variable_anonymous(ModelType)
    model = ModelType()
    @test_throws ErrorException @variable(model, [(0, 0)])  # #922
    x = @variable(model, [(0, 2)])
    @test "noname" == JuMP.name(x[0])
    @test "noname" == JuMP.name(x[2])
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

function test_variable_oneto_index_set(ModelType::Type{PolyModel{VT}}) where {VT}
    # Tests that Base.OneTo can be used in index set (JuMP issue #933).
    model = ModelType()
    auto_var = @variable(model, [Base.OneTo(3), 1:2], container=Auto)
    @test auto_var isa Matrix{PolyVariableRef{VT}}
    @test (3, 2) == @inferred size(auto_var)
    array_var = @variable(model, [Base.OneTo(3), 1:2], container=Array)
    @test array_var isa Matrix{PolyVariableRef{VT}}
    @test (3, 2) == @inferred size(array_var)
    denseaxisarray_var = @variable(model, [Base.OneTo(3), 1:2], container=DenseAxisArray)
    @test denseaxisarray_var isa JuMP.Containers.DenseAxisArray{PolyVariableRef{VT}}
    @test length.(axes(denseaxisarray_var)) == (3, 2)
end

function test_variable_base_name_in_macro(ModelType)
    model = ModelType()
    @variable(model, normal_var)
    @test JuMP.name(normal_var) == "normal_var"
    no_indices = @variable(model, base_name="foo")
    @test JuMP.name(no_indices) == "foo"
    # Note that `z` will be ignored in name.
    indices = @variable(model, z[i=2:3], base_name="t")
    @test JuMP.name(indices[2]) == "t[2]"
    @test JuMP.name(indices[3]) == "t[3]"
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

function test_variable_macro_return_type(ModelType::Type{PolyModel{VT}}) where {VT}
    model = ModelType()
    @variable(model, x[1:3, 1:4, 1:2])
    @test typeof(x) == Array{PolyVariableRef{VT},3}
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
        test_variable_lower_bound_rhs(ModelType)
        test_variable_lower_bound_lhs(ModelType)
        test_variable_upper_bound_rhs(ModelType)
        test_variable_upper_bound_lhs(ModelType)
        test_variable_interval(ModelType)
        test_variable_interval_delete_upper(ModelType)
        test_variable_interval_delete_lower(ModelType)
        test_variable_fix(ModelType)
        test_variable_binary(ModelType)
        test_variable_integer(ModelType)
        test_variable_starts_set_get(ModelType)
        test_variable_bounds_set_get(ModelType)
        test_variable_fixed_set_get(ModelType)
        test_variable_binary_set_get(ModelType)
        test_variable_integrality_set_get(ModelType)
        test_variable_name(ModelType)
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
    variables_test(PolyModel{PolyVar{true}})
    @testset "all_variables" begin
        model = PolyModel{PolyVar{true}}()
        @variable(model, x)
        @variable(model, y)
        @test [x, y] == JuMP.all_variables(model)
    end
    @testset "@variables" begin
        model = PolyModel{PolyVar{true}}()
        @variables model begin
            0 ≤ x[i=1:2] ≤ i, Int
            y ≥ 2, Bin
            z ≤ 3
        end
        @test length(all_constraints(model)) == 4
    end
end
