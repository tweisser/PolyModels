mutable struct PolyVariableInfo
    has_lb::Bool
    lower::Union{Nothing, Float64}
    lb_constraint::Union{Nothing, ConstraintRef}
    has_ub::Bool
    upper::Union{Nothing, Float64}
    ub_constraint::Union{Nothing, ConstraintRef}
    in_interval::Bool
    in_constraint::Union{Nothing, ConstraintRef}
    has_fix::Bool
    fix_value::Union{Nothing, Float64}
    fix_constraint::Union{Nothing, ConstraintRef}
    binary::Bool
    bin_constraint::Union{Nothing, ConstraintRef}
    integer::Bool
    int_constraint::Union{Nothing, ConstraintRef}
    has_start::Bool
    start_value::Union{Nothing, Float64}
end

PolyVariableInfo() = PolyVariableInfo(
           false, nothing, nothing,     # lower
           false, nothing, nothing,     # upper
           false, nothing,              # interval
           false, nothing, nothing,     # fix
           false, nothing,              # binary
           false, nothing,              # integer
           false, nothing)              # start


export PolyVariableRef
struct PolyVariableRef{VT <: MP.AbstractVariable}
    model::PolyModel{VT}
    index::Int
end

Base.broadcastable(vref::PolyVariableRef) = Ref(vref)
Base.iszero(::PolyVariableRef) = false
index(vref::PolyVariableRef) = vref.index
JuMP.owner_model(vref::PolyVariableRef) = vref.model
Base.copy(v::PolyVariableRef) = PolyVariableRef(owner_model(v), index(v))
JuMP.isequal_canonical(v::PolyVariableRef, other::PolyVariableRef) = isequal(v, other)
object(vref::PolyVariableRef) = owner_model(vref).variables[index(vref)]
Base.isequal(vref1::PolyVariableRef, vref2::PolyVariableRef) = isequal(object(vref1), object(vref2))
info(vref::PolyVariableRef) = owner_model(vref).variable_info[index(vref)]
JuMP.name(vref::PolyVariableRef) = owner_model(vref).variable_names[index(vref)]
Base.show(io::IO, vref::PolyVariableRef) = show(io, JuMP.name(vref))
Base.zero(::Type{PolyVariableRef{VT}}) where {VT} = zero(VT)
Base.one(::Type{PolyVariableRef{VT}}) where {VT} = one(VT)
Base.zero(vref::PolyVariableRef) = zero(object(vref))
Base.one(vref::PolyVariableRef) = one(object(vref))


Base.ndims(::Type{<:PolyVariableRef}) = 0
LinearAlgebra.symmetric_type(::Type{T}) where T <: PolyVariableRef = T
LinearAlgebra.symmetric(scalar::PolyVariableRef, ::Symbol) = scalar
LinearAlgebra.adjoint(scalar::PolyVariableRef) = scalar
Base.iterate(x::PolyVariableRef) = (x, true)
Base.iterate(::PolyVariableRef, state) = nothing
Base.isempty(::PolyVariableRef) = false
Base.length(::PolyVariableRef) = 1
LinearAlgebra.dot(v1::PolyVariableRef{VT}, v2::PolyVariableRef{VT}) where {VT} = v1[1]*v2[1]
function Base.getindex(vref::PolyVariableRef, i)
    @assert i == 1 "PolyVariableRef $vref refers to a single variable only."
    return vref
end


Base.promote_rule(::Type{T}, ::Type{PolyVariableRef{VT}}) where {T <: Union{Number, JuMP.AbstractJuMPScalar}, VT <:MP.AbstractVariable} = promote_type(T, VT)
Base.promote_rule(::Type{PolyVariableRef{VT}}, ::Type{T}) where {T <: Union{Number, JuMP.AbstractJuMPScalar}, VT <:MP.AbstractVariable} = promote_type(T, VT)

Base.promote_rule(::Type{PT}, ::Type{PolyVariableRef{VT}}) where {PT<:(MP.AbstractPolynomialLike{T} where T), VT <:MP.AbstractVariable} = promote_type(PT, VT)
Base.promote_rule(::Type{PolyVariableRef{VT}}, ::Type{PT}) where {PT<:(MP.AbstractPolynomialLike{T} where T), VT <:MP.AbstractVariable} = promote_type(PT, VT)

Base.convert(::Type{PT}, vref::PolyVariableRef) where {PT <: (MP.AbstractPolynomialLike{T} where T) }  = convert(PT, object(vref))

# arithmetic operations with PolyVariableRefs return an MP.AbstractPolynomialLike
Base.sum(vrefs::Vector{<:PolyVariableRef}) = sum(object.(vrefs))
Base.prod(vrefs::Vector{<:PolyVariableRef}) = prod(object.(vrefs))
Base.:+(vref1::PolyVariableRef, vref2::PolyVariableRef) = sum([vref1, vref2])
Base.:-(vref::PolyVariableRef) = -object(vref)
Base.:-(vref1::PolyVariableRef, vref2::PolyVariableRef) =  sum([vref1, -vref2])
Base.:*(vref1::PolyVariableRef, vref2::PolyVariableRef) =  prod([vref1, vref2])
Base.:^(vref::PolyVariableRef, e::Int) = object(vref)^e

Base.:+(p::Number, vref::PolyVariableRef) = sum(Base.promote_typeof(p, vref)[p, vref])
Base.:+(p::MP.AbstractPolynomialLike, vref::PolyVariableRef) = sum(Base.promote_typeof(p, vref)[p, vref])
Base.:+(vref::PolyVariableRef, p::Number) = sum(Base.promote_typeof(p, vref)[vref, p])
Base.:+(vref::PolyVariableRef, p::MP.AbstractPolynomialLike) = sum(Base.promote_typeof(p, vref)[vref, p])
Base.:-(p::Number, vref::PolyVariableRef) = sum(Base.promote_typeof(p, vref)[p, -vref])
Base.:-(p::MP.AbstractPolynomialLike, vref::PolyVariableRef) = sum(Base.promote_typeof(p, vref)[p, -vref])
Base.:-(vref::PolyVariableRef, p::Number) = sum(Base.promote_typeof(p, vref)[vref, -p])
Base.:-(vref::PolyVariableRef, p::MP.AbstractPolynomialLike) = sum(Base.promote_typeof(p, vref)[vref, -p])
Base.:*(p::Number, vref::PolyVariableRef) = prod(Base.promote_typeof(p, vref)[p, vref])
Base.:*(p::MP.AbstractPolynomialLike, vref::PolyVariableRef) = prod(Base.promote_typeof(p, vref)[p, vref])
Base.:*(vref::PolyVariableRef, p::Number) = prod(Base.promote_typeof(p, vref)[vref, p])
Base.:*(vref::PolyVariableRef, p::MP.AbstractPolynomialLike) = prod(Base.promote_typeof(p, vref)[vref, p])

MutableArithmetics.mutable_operate_to!(p::PT, ::typeof(*), a::Float64, v::PolyVariableRef{VT}) where {PT<:MP.AbstractPolynomialLike, VT<:MP.AbstractVariable} = MutableArithmetics.mutable_operate_to!(p, *, a, object(v))
MutableArithmetics.mutable_operate_to!(p::PT, ::typeof(*), a::PolyVariableRef{VT}, v::PolyVariableRef{VT}) where {PT<:MP.AbstractPolynomialLike, VT<:MP.AbstractVariable} = MutableArithmetics.mutable_operate_to!(p, *, object(a), object(v))

function JuMP.is_valid(model::PolyModel, vref::PolyVariableRef)
    return owner_model(vref) == model && haskey(owner_model(vref).variables, index(vref)) 
end

function JuMP.is_valid(model::PolyModel{VT}, x::VT) where {VT <: MP.AbstractVariable}
    return x ∈ Set(values(model.variables))
end

function JuMP.set_name(v::PolyVariableRef, s::AbstractString)
    delete!(object_dictionary(owner_model(v)), Symbol(JuMP.name(v)))
    owner_model(v).variable_names[index(v)] = s
    object_dictionary(owner_model(v))[Symbol(s)] = v
end

function JuMP.variable_by_name(m::PolyModel, sname::String)
    idx = findall(isequal(sname), m.variable_names)
    if length(idx)>  1 
        error("Multiple variables have the name $sname.")
    end
    if isempty(idx)
        return nothing
    else
        return PolyVariableRef(m, first(idx))
    end
end

function JuMP.delete(m::PolyModel, vref::PolyVariableRef)
    @assert JuMP.is_valid(m, vref)
    delete!(object_dictionary(m), Symbol(JuMP.name(vref)))
    m.variable_names[index(vref)] = ""
    delete!(m.variables, index(vref))
    delete!(m.variable_info, index(vref))
    return nothing
end

function JuMP.delete(m::PolyModel{VT}, vref::Vector{PolyVariableRef{VT}}) where {VT}
    for v in vref
        delete(m, v)
    end
    return nothing
end

function JuMP.build_constraint(error::Function, x::PolyVariableRef, set::MOI.AbstractScalarSet)
    return PolynomialConstraint(object(x), set)
end

# lower bound for variables
function JuMP.has_lower_bound(v::PolyVariableRef)
    return info(v).has_lb
end

function JuMP.lower_bound(v::PolyVariableRef)
    @assert has_lower_bound(v) "Variable $(v) does not have a lower bound."
    return info(v).lower
end

function _set_lower_bound(model::PolyModel, idx::Int, lower::Number)
    model.variable_info[idx].has_lb = true
    model.variable_info[idx].lower = lower
    var = model.variables[idx]
    model.variable_info[idx].lb_constraint = @constraint model var >= lower
    return 
end

function _delete_lower_bound(model::PolyModel, idx::Int)
    model.variable_info[idx].has_lb = false
    model.variable_info[idx].lower = nothing
    delete(model, model.variable_info[idx].lb_constraint)
    model.variable_info[idx].lb_constraint = nothing
    return
end

function JuMP.set_lower_bound(v::PolyVariableRef, lower::Number)
    if has_lower_bound(v)  
        if !(lower_bound(v) == lower)
            if in_interval(v)
                upper = upper_bound(v)
                if is_integer(v)
                    _unset_integer(owner_model(v), index(v))
                    _set_integer(owner_model(v), index(v), lower, upper)
                else
                    _delete_in_interval(owner_model(v), index(v))
                    _set_in_interval(owner_model(v), index(v), lower, upper)
                end
            else
                _delete_lower_bound(owner_model(v), index(v))
                _set_lower_bound(owner_model(v), index(v), lower)
            end
        end
    else
        if has_upper_bound(v)
            upper = upper_bound(v)
            _delete_upper_bound(owner_model(v), index(v))
            _set_in_interval(owner_model(v), index(v), lower, upper)
        else
            _set_lower_bound(owner_model(v), index(v), lower)
        end
    end
    return
end

function JuMP.LowerBoundRef(v::PolyVariableRef)
    @assert has_lower_bound(v) "Variable $v is not bounded from below."
    @assert !in_interval(v) "$v is constrained to be in an interval. Try calling IntervalRef($v)."
    return info(v).lb_constraint
end

function JuMP.delete_lower_bound(v::PolyVariableRef)
    @assert has_lower_bound(v) "Variable $v is not bounded from below."
    @assert !is_integer(v) "Cannot delete lower bound. Try unset_integer($v) first."
    _delete_lower_bound(owner_model(v), index(v))
     if in_interval(v)
        upper = upper_bound(v)
        _delete_in_interval(owner_model(v), index(v))
        _set_upper_bound(owner_model(v), index(v), upper)
    else
        _delete_lower_bound(owner_model(v), index(v))
    end
    return
end


# upper bound for variables
function JuMP.has_upper_bound(v::PolyVariableRef)
    return info(v).has_ub
end

function JuMP.upper_bound(v::PolyVariableRef)
    @assert has_upper_bound(v) "Variable $(v) does not have a upper bound."
    return info(v).upper
end

function _set_upper_bound(model::PolyModel, idx::Int, upper::Number)
    model.variable_info[idx].has_ub = true
    model.variable_info[idx].upper = upper
    var = model.variables[idx]
    model.variable_info[idx].ub_constraint = @constraint model var <= upper
    return 
end

function _delete_upper_bound(model::PolyModel, idx::Int)
    model.variable_info[idx].has_ub = false
    model.variable_info[idx].upper = nothing
    delete(model, model.variable_info[idx].ub_constraint)
    model.variable_info[idx].ub_constraint = nothing
    return
end

function JuMP.set_upper_bound(v::PolyVariableRef, upper::Number)
    if has_upper_bound(v)  
        if !(upper_bound(v) == upper)
            if in_interval(v)
                lower = lower_bound(v)
                if is_integer(v)
                    _unset_integer(owner_model(v), index(v))
                    _set_integer(owner_model(v), index(v), lower, upper)
                else
                    _delete_in_interval(owner_model(v), index(v))
                    _set_in_interval(owner_model(v), index(v), lower, upper)
                end
            else
                _delete_upper_bound(owner_model(v), index(v))
                _set_upper_bound(owner_model(v), index(v), upper)
            end
        end
    else
        if has_lower_bound(v)
            lower = lower_bound(v)
            _delete_lower_bound(owner_model(v), index(v))
            _set_in_interval(owner_model(v), index(v), lower, upper)
        else
            _set_upper_bound(owner_model(v), index(v), upper)
        end
    end
    return
end

function JuMP.UpperBoundRef(v::PolyVariableRef)
    @assert has_upper_bound(v) "$v is not bounded from above."
    @assert !in_interval(v) "$v is constrained to be in an interval. Try calling IntervalRef($v)."
    return info(v).ub_constraint
end

function JuMP.delete_upper_bound(v::PolyVariableRef)
    @assert has_upper_bound(v) "$v is not bounded from above."
    @assert !is_integer(v) "Cannot delete upper bound. Try unset_integer($v) first."
    if in_interval(v)
        lower = lower_bound(v)
        _delete_in_interval(owner_model(v), index(v))
        _set_lower_bound(owner_model(v), index(v), lower)
    else
        _delete_upper_bound(owner_model(v), index(v))
    end
    return
end


# variable in interval
export in_interval
function in_interval(v::PolyVariableRef)
    return info(v).in_interval
end

export interval
function interval(v::PolyVariableRef)
    @assert in_interval(v) "Variable $v is not constrained to an interval."
    return [info(v).lower, info(v).upper]
end

function _set_in_interval(model::PolyModel, idx::Int, lower::Number, upper::Number)
    model.variable_info[idx].in_interval = true
    model.variable_info[idx].has_lb = true 
    model.variable_info[idx].has_ub = true 
    model.variable_info[idx].lower = lower 
    model.variable_info[idx].upper = upper
    var = model.variables[idx]
    model.variable_info[idx].in_constraint = @constraint model (var - lower)*(upper - var) >= 0
    return
end

function _delete_in_interval(model::PolyModel, idx::Int)
    delete(model, model.variable_info[idx].in_constraint)
    model.variable_info[idx].has_lb = false
    model.variable_info[idx].has_ub = false
    model.variable_info[idx].lower = nothing
    model.variable_info[idx].upper = nothing
    model.variable_info[idx].in_interval = false
     model.variable_info[idx].in_constraint = nothing
    return
end

export set_in_interval
function set_in_interval(v::PolyVariableRef, lower::Number, upper::Number)
    if is_integer(v)
        _unset_integer(owner_model(v), index(v))
        _set_integer(owner_model(v), index(v), lower, upper)

    else
        if has_lower_bound(v)
            _delete_lower_bound(owner_model(v), index(v))
        end
        if has_upper_bound(v)
            _delete_upper_bound(owner_model(v), index(v))        
        end
        if in_interval(v)
            _delete_in_interval(owner_model(v), index(v)) 
        end
        _set_in_interval(owner_model(v), index(v), lower, upper)
    end
    return
end

export IntervalRef
function IntervalRef(v::PolyVariableRef)
    return info(v).in_constraint
end

export delete_in_interval
function delete_in_interval(v::PolyVariableRef)
    @assert in_interval(v) "Variable $v is not constrained to some interval.."
    @assert !is_integer(v) "Cannot delete interval constraint. Try unset_integer($v) first."
    _delete_in_interval(owner_model(v), index(v))
    return
end


# fixed value
function JuMP.is_fixed(v::PolyVariableRef)
    return info(v).has_fix
end

function JuMP.fix_value(v::PolyVariableRef)
    @assert is_fixed(v) "Variable $v is not fixed to any value."
    return info(v).fix_value
end

function _fix(model::PolyModel, idx::Int, value::Number)
    model.variable_info[idx].has_fix = true
    model.variable_info[idx].fix_value = value
    var = model.variables[idx]
    model.variable_info[idx].fix_constraint = @constraint model var == value
    _set_start_value(model, idx, value)
    return 
end

function _unfix(model::PolyModel, idx::Int)
    model.variable_info[idx].has_fix = false
    model.variable_info[idx].fix_value = nothing
    delete(model, model.variable_info[idx].fix_constraint)
    model.variable_info[idx].fix_constraint = nothing
    return
end

function JuMP.fix(v::PolyVariableRef, value::Number; force::Bool = false)
    if is_fixed(v)
        if !(fix_value(v) == value)
            _unfix(owner_model(v), index(v))        
            _fix(owner_model(v), index(v), value)
        end
    else  # Add a new fixing constraint.
        if has_upper_bound(v) || has_lower_bound(v)
            if !force
                error("Unable to fix $(v) to $(value) because it has " *
                      "existing variable bounds. Consider calling " *
                      "`JuMP.fix(variable, value; force=true)` which will " *
                      "delete existing bounds before fixing the variable.")
            end

            in_interval(v) ? delete_in_interval(v) : nothing
            has_upper_bound(v) ? delete_upper_bound(v) : nothing
            has_lower_bound(v) ? delete_lower_bound(v) : nothing
            is_integer(v) ? unset_integer(v) : nothing
            is_binary(v) ? unset_binary(v) : nothing
        end
        _fix(owner_model(v), index(v), value)           
    end
    return
end

function JuMP.FixRef(v::PolyVariableRef)
    return info(v).fix_constraint    
end
function JuMP.unfix(v::PolyVariableRef)
    @assert is_fixed(v) "Variable $v is not fixed."
    _unfix(owner_model(v), index(v))
    return
end


function JuMP.is_binary(v::PolyVariableRef)
    return info(v).binary
end

function _set_binary(model::PolyModel, idx::Int)
    model.variable_info[idx].binary = true
    var = model.variables[idx]
    model.variable_info[idx].bin_constraint = @constraint model var*(1-var) == 0
end

function _unset_binary(model::PolyModel, idx::Int)
    model.variable_info[idx].binary = false
    delete(model, model.variable_info[idx].bin_constraint)
    model.variable_info[idx].bin_constraint = nothing
end


function JuMP.set_binary(v::PolyVariableRef; force = false)
    if !is_binary(v)
        if has_upper_bound(v) || has_lower_bound(v) || is_fixed(v) || is_integer(v) 
            if !force
                error("Unable to set $(v) as binary because it has " *
                      "other properties set. Consider calling " *
                      "`JuMP.set_binary(variable; force=true)` which will " *
                      "delete existing properties before seting the variable binary.")
            end
            has_lower_bound(v) ? _delete_lower_bound(owner_model(v), index(v)) : nothing 
            has_upper_bound(v) ? _delete_upper_bound(owner_model(v), index(v)) : nothing
            in_interval(v) ? _delete_in_interval(owner_model(v), index(v)) : nothing
            is_fixed(v) ? _unfix(owner_model(v), index(v)) : nothing
            is_integer(v) ? _unset_integer(owner_model(v), index(v)) : nothing
         end
        _set_binary(owner_model(v), index(v))
    end
    return 
end

function JuMP.BinaryRef(v::PolyVariableRef)
    return info(v).bin_constraint
end

function JuMP.unset_binary(v::PolyVariableRef)
    @assert is_binary(v) "$v is not constrained to be binary."
    _unset_binary(owner_model(v), index(v))
    return
end


# integer variables
function JuMP.is_integer(v::PolyVariableRef)
    return info(v).integer
end

function _set_integer(model::PolyModel, idx::Int, lower::Number, upper::Number)
    model.variable_info[idx].integer = true
    model.variable_info[idx].has_lb = true
    model.variable_info[idx].has_ub = true
    model.variable_info[idx].in_interval = true
    model.variable_info[idx].lower = lower 
    model.variable_info[idx].upper = upper

    var = model.variables[idx]
    poly =  prod((var-i) for i = ceil(lower):floor(upper)) 
    model.variable_info[idx].int_constraint = @constraint model poly == 0
    return
end

function _unset_integer(model::PolyModel, idx::Int)
    model.variable_info[idx].integer = false
    delete(model, model.variable_info[idx].int_constraint)
    model.variable_info[idx].int_constraint  = nothing
end

function JuMP.set_integer(v::PolyVariableRef)
    @assert !is_binary(v) "Variable $v is already constrained to be binary."
    @assert in_interval(v) "Variable $v needs to be contained in some interval to impose an integer constraint'"
    if !(is_integer(v))
        lower = lower_bound(v)
        upper = upper_bound(v)
        _delete_in_interval(owner_model(v), index(v))
        _set_integer(owner_model(v), index(v), lower, upper)
    end
    return 
end

function JuMP.IntegerRef(v::PolyVariableRef)
    return info(v).int_constraint    
end


function JuMP.unset_integer(v::PolyVariableRef)
    @assert is_integer(v) "$v is not constrained to be integer."
    lower = lower_bound(v)
    upper = upper_bound(v)
    _unset_integer(owner_model(v), index(v))
    _set_in_interval(owner_model(v), index(v), lower, upper)
    return
end


# start value
function JuMP.start_value(v::PolyVariableRef)::Union{Nothing, Float64}
    return info(v).start_value
end
function _set_start_value(model::PolyModel, idx::Int, value::Number)
    model.variable_info[idx].has_start = true
    model.variable_info[idx].start_value = value
    return
end

function JuMP.set_start_value(v::PolyVariableRef, value::Number)
    _set_start_value(owner_model(v),  index(v), value)
end

function _poly_info(info::JuMP.VariableInfo)
    pinfo = PolyVariableInfo()
    if info.has_lb
         pinfo.has_lb = true
         pinfo.lower = info.lower_bound
    end
    if info.has_ub
         pinfo.has_ub = true
         pinfo.upper = info.upper_bound
    end
    
    pinfo.in_interval = info.has_lb && info.has_ub
    
    if info.has_fix
        pinfo.has_fix = true
        pinfo.fix_value = info.fixed_value
    end

    pinfo.binary = info.binary
    
    if info.integer
        @assert pinfo.in_interval "PolyModels only supports integer constraints in combination with upper and lower bound."
        pinfo.integer = true
    end
    
    if info.has_start
        pinfo.has_start = true
        pinfo.start_value = info.start
    end
    return pinfo
end


# add variable
function JuMP.add_variable(model::PolyModel{VT}, v::ScalarVariable, name::String="") where {VT}
    pinfo = _poly_info(v.info)

    model.vct += 1
    if isempty(name)
        name = "noname"
    end

    var = model.variables[model.vct] = VT(name)
    model.variable_info[model.vct] = pinfo
    push!(model.variable_names, name)

    if pinfo.has_fix
        pinfo.fix_constraint = @constraint model var in MOI.EqualTo(pinfo.fix_value)
    elseif pinfo.integer
        poly =  prod((var-i) for i = ceil(pinfo.lower):floor(pinfo.upper)) 
        pinfo.int_constraint = @constraint model poly == 0.0
    elseif pinfo.binary 
        pinfo.bin_constraint = @constraint model var*(1.0-var) == 0.0

    elseif pinfo.in_interval
        pinfo.in_constraint = @constraint model (var - pinfo.lower)*(pinfo.upper - var) >= 0
    elseif pinfo.has_lb  
        pinfo.lb_constraint = @constraint model var in MOI.GreaterThan(pinfo.lower)
    elseif pinfo.has_ub
        pinfo.ub_constraint =  @constraint model var in MOI.LessThan(pinfo.upper)
    end

    vref = PolyVariableRef{variable_union_type(var)}(model, model.vct)

    if !isempty(name)
        object_dictionary(model)[Symbol(name)] = vref
    end

    return vref
end

function JuMP.all_variables(model::PolyModel)
    indices = sort!(collect(keys(model.variables)))
    return PolyVariableRef.(model, indices)
end
