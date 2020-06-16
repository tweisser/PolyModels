module PolyModels

using Reexport
@reexport using JuMP
using PolyJuMP


using MultivariatePolynomials
const MP = MultivariatePolynomials
using MutableArithmetics

using LinearAlgebra

abstract type AbstractPolyModel <: JuMP.AbstractModel end


export PolyModel
"""
    PolyModel
A model for a polynomial optimization problem.
"""
mutable struct PolyModel{VT <: MP.AbstractVariable} <: AbstractPolyModel
    vct::Int
    variable_names::Vector{AbstractString}
    variables::Dict{Int, VT}
    variable_info::Dict{Int, Any} # PolyVariableInfo is not defined yet. 
    
    cct::Int
    constraint_names::Vector{AbstractString}
    constraints::Dict{Int, JuMP.AbstractConstraint}

    objective_sense::MOI.OptimizationSense
    objective_function::MP.AbstractPolynomialLike
    
    obj_dict::Dict{Symbol, Any}
end

Base.broadcastable(pm::PolyModel) = Ref(pm)

function PolyModel{VT}() where {VT <: MP.AbstractVariable}
    return PolyModel{VT}(
                             0, AbstractString[], Dict{VT, Int}(), Dict{Int, Any}(),
                             0, AbstractString[], Dict{Int, JuMP.AbstractConstraint}(),
                             MOI.FEASIBILITY_SENSE, zero(polynomialtype(VT, Float64)),
                             Dict{Symbol, Any}())
end

# define basic functions needed for JuMP models
JuMP.num_variables(model::PolyModel)::Int64 = length(model.variables)
JuMP.object_dictionary(model::PolyModel) = model.obj_dict
JuMP.termination_status(::PolyModel) = MOI.TerminationStatusCode(24)
JuMP.backend(model::PolyModel) = model
polyvariable_type(model::PolyModel{VT}) where VT = VT

# additional JuMP function to use AbstractPolynomiallike
JuMP.isequal_canonical(p1::AbstractPolynomialLike, p2::AbstractPolynomialLike) = p1 == p2

include("constraints.jl")
include("variables.jl")
include("objective.jl")
include("print.jl")

using SemialgebraicSets
import SumOfSquares.Certificate.chordal_csp_graph

export FeasibleSet, FeasibleSetWithFix, CSPFeasibleSet
export cliques, sets, set, fixed_variables

abstract type AbstractFeasibleSet end

struct FeasibleSet{VT, ST <:SemialgebraicSets.AbstractSemialgebraicSet} <: AbstractFeasibleSet
    variables::Vector{VT}
    set::ST
end

MP.variables(fset::AbstractFeasibleSet) = fset.variables
cliques(fset::AbstractFeasibleSet) = [variables(fset)]
set(fset::AbstractFeasibleSet) = fset.set
sets(fset::AbstractFeasibleSet) = [set(fset)]
fixed_variables(fset::FeasibleSet{VT}) where VT = Dict{VT, Int}()


struct FeasibleSetWithFix{VT, ST <: SemialgebraicSets.AbstractSemialgebraicSet, T <: Number} <:AbstractFeasibleSet
    variables::Vector{VT}
    set::ST
    fixed_variables::Dict{VT, T}
end

struct CSPFeasibleSet{VT, ST <: SemialgebraicSets.AbstractSemialgebraicSet, T <: Number} <: AbstractFeasibleSet
    cliques::Vector{Vector{VT}}
    sets::Vector{ST}
    fixed_variables::Dict{VT, T}
end

fixed_variables(fset::Union{FeasibleSetWithFix, CSPFeasibleSet}) = fset.fixed_variables

cliques(fset::CSPFeasibleSet) = fset.cliques
MP.variables(fset::CSPFeasibleSet) = sort!(collect(set(vars for vars in cliques(fset))), rev = true)
sets(fset::CSPFeasibleSet) = fset.sets
Base.length(fset::CSPFeasibleSet) = length(cliques(fset))
Base.iterate(fset::CSPFeasibleSet) = (FeasibleSet(first(cliques(fset)), first(sets(fset))), 1)
Base.iterate(fset::CSPFeasibleSet, i) = (FeasibleSet(cliques(fset)[i+1], sets(fset)[i+1]), i+1)

export feasible_set

"""
    feasible_set(model::PolyModel; csp = false, fix = false)

Returns the feasible set of model.
If `csp = true` returns a vector of feasible sets .
If `fix = true` constraints `variable == value` are eliminated and a `FeaeibleSetWithFix` is returned. 
"""
function feasible_set(model::PolyModel; csp = false, fix = false)
    if csp
        return csp_feasible_set(model, fix = fix)
    elseif fix
        return feasible_set_with_fix(model)
    else
        return dense_feasible_set(model)
    end
end

function dense_feasible_set(model::PolyModel{VT}) where {VT}
    eqs = polynomialtype(Float64, VT)[]
    ineqs = polynomialtype(Float64, VT)[]

    for con in all_constraints(model)
        if con.set isa MOI.EqualTo
            push!(eqs, con.func)
        else
            @assert con.set isa MOI.GreaterThan
            push!(ineqs, con.func)
        end
    end
    set = basicsemialgebraicset(FullSpace(), ineqs)
    for eq in eqs
        set = intersect(set, @set eq == 0)
    end
    return FeasibleSet(sort!(object.(all_variables(model)), rev = true), set)
end

function _single_variable(p::PT) where {PT <: MP. AbstractPolynomialLike}
    if maxdegree(p) == 1 && length(effective_variables(p)) == 1
        success = true
        constant = 1
        if length(coefficients(p)) == 2
            constant = -coefficients(p)[2]
        else
            constant = 0 
        end
        constant = constant/first(coefficients(p))
        pol = first(effective_variables(p))
    else
        success = false
        constant = nothing
        pol = p
    end
    return success, pol, constant
end

function feasible_set_with_fix(model::PolyModel{VT}) where {VT}
    eqs = polynomialtype(Float64, VT)[]
    ineqs = polynomialtype(Float64, VT)[]
    fixed = Dict{VT, Float64}()

    for con in all_constraints(model)
        if con.set isa MOI.EqualTo
            s, p, c = _single_variable(jump_function(con)-MOI.constant(moi_set(con)))
            if s
                fixed[p] = c
            else
                push!(eqs, con.func - MOI.constant(moi_set(con)))
            end
        elseif con.set isa MOI.GreaterThan
            push!(ineqs, con.func - MOI.constant(moi_set(con)))
        else
            push!(ineqs, MOI.constant(moi_set(con)) - con.func)
        end
    end
    for i in 1:length(ineqs)
        tmp = subs(ineqs[i],  [k => v for (k,v) in fixed]...)
        if maxdegree(tmp) == 0
            @assert convert(Float64, tmp) >= 0 "A negative number is requested to be non-negative. Set is empty."
        else
            ineqs[i] = subs(ineqs[i],  [k => v for (k,v) in fixed]...)
        end
    end
    set = basicsemialgebraicset(FullSpace(), ineqs)
    
    for eq in eqs
        tmp = subs(eq, [k => v for (k,v) in fixed]...)
        if maxdegree(tmp) == 0
            @assert convert(Float64, tmp) == 0 "A non-zero number is requested to be zero. Set is empty."
        else
            set = intersect(set, @set subs(eq, [k => v for (k,v) in fixed]...)  == 0)
        end
    end
    vars = setdiff(object.(all_variables(model)), collect(keys(fixed)))
    return FeasibleSetWithFix(sort!(vars, rev = true), set, fixed)
end



function csp_feasible_set(model::PolyModel; fix = true)
    fset = feasible_set(model; fix = fix)
    if fix
        obj = subs(objective_function(model), fixed_variables(fset)...)
    else
        obj = objective_function(model)
    end

    _, cliques = chordal_csp_graph(obj, set(fset))

    PT = polynomialtype(polyvariable_type(model), Float64)
    eqs = [PT[] for i = 1: length(cliques)]
    ineqs = [PT[] for i = 1: length(cliques)]

    for (i, vars) in enumerate(cliques)

        for p in equalities(set(fset))
            if variables(p) ⊆ vars
                push!(eqs[i], p)
            end
        end

        for p in inequalities(set(fset))
            if variables(p) ⊆ vars
                push!(ineqs[i], p)            
            end
        end

    end

    return CSPFeasibleSet(cliques, [basicsemialgebraicset(algebraicset(eqs[i]), ineqs[i]) for i = 1:length(cliques)], fixed_variables(fset))
end


function MP.maxdegree(model::PolyModel)
    deg = maxdegree(objective_function(model))
    cdeg = maxdegree.([jump_function(con) for con in all_constraints(model)])
    return maximum([deg, cdeg...])
end
#=
function moment_relaxation(m::PolyModel, factory; degree = maxdegree(model), scheme = PutinarScheme())
    F = feasible_set(m)
    gmp = GMPModel(factory)
    @variable gmp mu Meas(variables(F), support = set(F), scheme = scheme)
    @objective gmp objective_sense(m) Mom(objective_function(m), mu)
    @constraint gmp Mom(1, mu) == 1
    set_approximation_degree(gmp, degree)
    optimize!(gmp)
    return gmp, mu
end
=#
end # module
