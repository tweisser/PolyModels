module PolyModels

using Reexport
@reexport using JuMP

using MultivariatePolynomials
const MP = MultivariatePolynomials
using SemialgebraicSets

abstract type AbstractPolyModel <: JuMP.AbstractModel end


export PolyModel
"""
    PolyModel
A model for a polynomial optimization problem.
"""
mutable struct PolyModel{CT <: Number, VT <: MP.AbstractVariable} <: AbstractPolyModel
    vct::Int
    variables::Dict{VT, Int}
    cct::Int
    constraint_names::Vector{AbstractString}
    constraints::Dict{Int, JuMP.AbstractConstraint}

    objective_sense::MOI.OptimizationSense
    objective_function::MP.AbstractPolynomialLike
    
    obj_dict::Dict{Symbol, Any}
end

Base.broadcastable(pm::PolyModel) = Ref(pm)

function PolyModel{CT, VT}() where {CT <: Number, VT <: MP.AbstractVariable}
    return PolyModel{CT, VT}(
                             0, Dict{VT, Int}(), 
                             0, AbstractString[], Dict{Int, Any}(),
                             MOI.FEASIBILITY_SENSE, zero(polynomialtype(VT, CT)),
                             Dict{Symbol, Any}())
end

# define basic functions needed for JuMP models
JuMP.num_variables(model::PolyModel)::Int64 = length(model.variables)
JuMP.object_dictionary(model::PolyModel) = model.obj_dict
JuMP.termination_status(::PolyModel) = MOI.TerminationStatusCode(24)

# additional JuMP function to use AbstractPolynomiallike
JuMP.isequal_canonical(p1::AbstractPolynomialLike, p2::AbstractPolynomialLike) = p1 == p2

include("constraints.jl")
include("variables.jl")
include("objective.jl")
include("print.jl")
end # module
