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
    variables::Dict{Int, VT}
    variable_info::Dict{Int, Any}
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
                             0, Dict{VT, Int}(), Dict{Int, Any}(),
                             0, AbstractString[], Dict{Int, JuMP.AbstractConstraint}(),
                             MOI.FEASIBILITY_SENSE, zero(polynomialtype(VT, Float64)),
                             Dict{Symbol, Any}())
end

# define basic functions needed for JuMP models
JuMP.num_variables(model::PolyModel)::Int64 = length(model.variables)
JuMP.object_dictionary(model::PolyModel) = model.obj_dict
JuMP.termination_status(::PolyModel) = MOI.TerminationStatusCode(24)
JuMP.backend(model::PolyModel) = model

# additional JuMP function to use AbstractPolynomiallike
JuMP.isequal_canonical(p1::AbstractPolynomialLike, p2::AbstractPolynomialLike) = p1 == p2

include("constraints.jl")
include("variables.jl")
include("objective.jl")
include("print.jl")

using SemialgebraicSets
using SumOfSquares.Certificate.CEG

export FeasibleSet, FeasibleSetWithFix, CSPFeasibleSet
export cliques, sets, set

abstract type AbstractFeasibleSet end

struct FeasibleSet{VT, ST <:SemialgebraicSets.AbstractSemialgebraicSet} <: AbstractFeasibleSet
    variables::Vector{VT}
    set::ST
end

MP.variables(fset::AbstractFeasibleSet) = fset.variables
cliques(fset::AbstractFeasibleSet) = [variables(fset)]
set(fset::AbstractFeasibleSet) = fset.set
sets(fset::AbstractFeasibleSet) = [set(fset)]

struct FeasibleSetWithFix{VT, ST <: SemialgebraicSets.AbstractSemialgebraicSet, T <: Number} <:AbstractFeasibleSet
    variables::Vector{VT}
    set::ST
    fixed_variables::Dict{VT, T}
end

fixed_variables(fset::FeasibleSetWithFix) = fset.fixed_variables

struct CSPFeasibleSet{VT} <: AbstractFeasibleSet
    cliques::Vector{Vector{VT}}
    sets::Vector{Any}
end

cliques(fset::CSPFeasibleSet) = fset.cliques
MP.variables(fset::CSPFeasibleSet) = sort!(collect(set(vars for vars in cliques(fset))), rev = true)
sets(fset::CSPFeasibleSet) = fset.set
Base.length(fset::CSPFeasibleSet) = length(cliques(fset))
Base.iterate(fset::CSPFeasibleSet) = (FeasibleSet(first(cliques(fset)), first(sets(fset))), 1)
Base.iterate(fset::CSPFeasibleSet, i) = (FeasibleSet(cliques(fset)[i+1], sets(fset)[i+1]), i+1)

export feasible_set

"""
    feasible_set(model::PolyModel; csp = false)

Returns the feasible set of model.
If `csp = true` returns a vector of feasible sets .
"""
function feasible_set(model::PolyModel; csp = false)
    csp ? csp_feasible_set : dense_feasible_set(model) 
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
        constant = first(coefficients(p))
        pol = first(effective_variables(p))
    else
        success = false
        constant = nothing
        pol = p
    end
    return success, pol, constant
end

export feasible_set_with_fix
function feasible_set_with_fix(model::PolyModel{VT}) where {VT}
    eqs = polynomialtype(Float64, VT)[]
    ineqs = polynomialtype(Float64, VT)[]
    fixed = Dict{VT, Float64}()

    for con in all_constraints(model)
        if con.set isa MOI.EqualTo
            s, p, c = _single_variable(jump_function(con))
            if s
                fixed[p] = MOI.constant(moi_set(con))/c
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
        ineqs[i] = subs(ineqs[i],  [k => v for (k,v) in fixed]...)
    end
    set = basicsemialgebraicset(FullSpace(), ineqs)
    
    for eq in eqs
        set = intersect(set, @set subs(eq, [k => v for (k,v) in fixed]...)  == 0)
    end
    vars = setdiff(object.(all_variables(model)), collect(keys(fixed)))
    @info object.(all_variables(model))
    @info collect(keys(fixed))
    return FeasibleSetWithFix(sort!(vars, rev = true), set, fixed)
end



function csp_feasible_set(model::PolyModel)

end


end # module
