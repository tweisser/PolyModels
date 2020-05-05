index(cref::ConstraintRef{PolyModel{VT}, MOI.ConstraintIndex{F, S}, <: JuMP.AbstractShape}) where {VT, F, S} = cref.index.value

function JuMP.name(con_ref::ConstraintRef{PolyModel{VT}, MOI.ConstraintIndex{F, S}, <: JuMP.AbstractShape}) where{VT, F, S}
    return owner_model(con_ref).constraint_names[index(con_ref)]::String
end

function JuMP.set_name(con_ref::ConstraintRef{PolyModel{VT}, MOI.ConstraintIndex{F, S}, <: JuMP.AbstractShape}, s::String) where{VT, F, S}
    idx = findall(x -> x == JuMP.name(con_ref), owner_model(con_ref).constraint_names)
    if length(idx) == 1
        delete!(object_dictionary(owner_model(con_ref)), Symbol(JuMP.name(con_ref)))
    end
    owner_model(con_ref).constraint_names[index(con_ref)] = s
    object_dictionary(owner_model(con_ref))[Symbol(s)] = con_ref
    return nothing
end

function JuMP.constraint_by_name(model::PolyModel, name::String)
    idx = findall(x -> x == name, model.constraint_names)
    if isempty(idx)
        return nothing
    elseif length(idx) > 1
        throw(ErrorException("Multiple constraints have the name $name."))
    else
        return object_dictionary(model)[Symbol(name)] 
    end
end

function JuMP.is_valid(model::PolyModel{VT}, con_ref::ConstraintRef{PolyModel{VT}, MOI.ConstraintIndex{F, S}, <: JuMP.AbstractShape}) where{VT, F, S} 
    return owner_model(con_ref) == model && haskey(model.constraints, index(con_ref))
end

function JuMP.delete(model::PolyModel{VT}, con_ref::ConstraintRef{PolyModel{VT}, MOI.ConstraintIndex{F, S}, <: JuMP.AbstractShape}) where{VT, F, S} 
    @assert is_valid(model, con_ref)
    idx = findall(x -> x == JuMP.name(con_ref), owner_model(con_ref).constraint_names)
    if length(idx) == 1
        delete!(object_dictionary(owner_model(con_ref)), Symbol(JuMP.name(con_ref)))
    end
    model.constraint_names[index(con_ref)] = ""
    delete!(model.constraints, index(con_ref))
    return nothing
end
function JuMP.delete(model::PolyModel{VT}, con_ref::Vector{<:ConstraintRef{PolyModel{VT}, MOI.ConstraintIndex{F, S}, <: JuMP.AbstractShape}}) where{VT, F, S} 
    delete.(model, con_ref)
    return nothing
end

function JuMP.delete(::PolyModel, ::Nothing) end

abstract type AbstractPolynomialConstraint <: JuMP.AbstractConstraint end

struct PolynomialConstraint{FT <: AbstractPolynomialLike, ST <: MOI.AbstractScalarSet} <: AbstractPolynomialConstraint
    func::FT
    set::ST
    function PolynomialConstraint(func, set)
        PT = polynomialtype(Float64, MP.variable_union_type(func))
        new{PT, typeof(set)}(convert(PT, func), set)
    end
end

struct PolynomialVectorConstraint{F <: AbstractPolynomialLike,
                        S <: MOI.AbstractVectorSet} <: AbstractPolynomialConstraint
    func::Vector{F}
    set::S
    function PolynomialConstraint(func, set)
        PT = polynomialtype(Float64, MP.variable_union_type(func))
        new{PT, typeof(set)}(convert.(PT, func), set)
    end
end

JuMP.jump_function(con::AbstractPolynomialConstraint) = con.func
JuMP.jump_function_type(con::PolynomialConstraint{FT, ST}) where {FT, ST} = FT
JuMP.jump_function_type(con::PolynomialVectorConstraint{FT, ST}) where {FT, ST} = Vector{FT}
JuMP.moi_set(con::AbstractPolynomialConstraint) = con.set
moi_set_type(con::PolynomialConstraint{FT, ST}) where {FT, ST} = ST
moi_set_type(con::PolynomialVectorConstraint{FT, ST}) where {FT, ST} = ST
JuMP.shape(con::PolynomialConstraint) = JuMP.ScalarShape()
JuMP.shape(con::PolynomialVectorConstraint) = JuMP.VectorShape()


function JuMP.constraint_object(cref::ConstraintRef{PolyModel{VT}, MOI.ConstraintIndex{F, S}, <:Union{JuMP.ScalarShape, JuMP.VectorShape}}) where {VT, F, S} 
    return owner_model(cref).constraints[index(cref)]
end

function JuMP.function_string(mode, mc::AbstractPolynomialConstraint)
    return string(mc.func)
end

function Base.show(io::IO, con::AbstractPolynomialConstraint)
    print(io, JuMP.constraint_string(JuMP.REPLMode, con))
end

function JuMP.add_constraint(model::PolyModel, bridgeable::BridgeableConstraint{<:VectorConstraint{<:Any, <:PolyJuMP.ZeroPolynomialSet, <:PolyJuMP.PolynomialShape }, <:Any}, name::String="") 
    @assert all([isempty(expr.terms) for expr in bridgeable.constraint.func])
    poly = polynomial(constant.(bridgeable.constraint.func), bridgeable.constraint.shape.monomials)
    add_constraint(model, PolynomialConstraint(poly, MOI.EqualTo(0.0)), name) 
end

function JuMP.add_constraint(model::PolyModel, pjcon::PolyJuMP.Constraint{<:Any, <:Any, PolyJuMP.NonNegPoly}, name::String = "")
    add_constraint(model, PolynomialConstraint(pjcon.polynomial_or_matrix, MOI.GreaterThan(0.0)), name) 
end

function JuMP.add_constraint(model::PolyModel, con::AbstractPolynomialConstraint, name::String="")

    @assert all(is_valid.(model, variables(jump_function(con)))) "At least one polynomial variable is not registered in the model."
    model.cct += 1
    cindex = MOI.ConstraintIndex{typeof(jump_function(con)), typeof(moi_set(con))}(model.cct)
    cref = ConstraintRef(model, cindex, shape(con))
    model.constraints[cref.index.value] = con
    push!(model.constraint_names, name)
    return cref
end

function JuMP.all_constraints(model::PolyModel)
    return [model.constraints[i] for i in sort!(collect(keys(model.constraints)))]
end

function JuMP.list_of_constraint_types(model::PolyModel)
    list = all_constraints(model)
    return unique(Tuple{DataType, DataType}[(jump_function_type(con), moi_set_type(con)) for con in list])
end

function JuMP.num_constraints(
    model::PolyModel,
    function_type::Type{<:Union{AbstractPolynomialLike,
                                Vector{<:AbstractPolynomialLike}}},
    set_type::Type{<:MOI.AbstractSet})::Int64
    JuMP._error_if_not_concrete_type(function_type)
    JuMP._error_if_not_concrete_type(set_type)
    # TODO: Support JuMP's set helpers like SecondOrderCone().
    return length([con for con in values(model.constraints) if (jump_function_type(con), moi_set_type(con)) == (function_type, set_type)])
end

JuMP.num_nl_constraints(::PolyModel) = 0
