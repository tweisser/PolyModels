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
Base.show(io::IO, vref::PolyVariableRef) = show(io, object(vref))
info(vref::PolyVariableRef) = owner_model(vref).variable_info[index(vref)]
JuMP.name(vref::PolyVariableRef) = MP.name(object(vref))
Base.zero(::Type{PolyVariableRef{VT}}) where {VT <: MP.AbstractVariable} = zero(VT)
Base.one(::Type{PolyVariableRef{VT}}) where {VT <: MP.AbstractVariable} = one(VT)

function JuMP.VariableRef(model::PolyModel{VT}, x::VT) where {VT<:MP.AbstractVariable}
    lookup = Dict(val => key for (key, val) in model.variables)
    @assert haskey(lookup, x) "Polynomial variable $x is not registered in requested model."
    return PolyVariableRef(model, lookup[x])
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

function JuMP.is_valid(model::PolyModel, vref::PolyVariableRef)
    return owner_model(vref) == model && haskey(owner_model(vref).variables, index(vref)) 
end

function JuMP.is_valid(model::PolyModel{VT}, x::VT) where {VT <: MP.AbstractVariable}
    return x ∈ Set(values(model.variables))
end

function JuMP.delete(m::PolyModel, vref::PolyVariableRef)
    @assert JuMP.is_valid(m, vref)
    delete!(m.variables, index(vref))
    delete!(m.variable_info, index(vref))
    #TODO remove name
    return nothing
end

function JuMP.delete(m::PolyModel{VT}, vref::Vector{PolyVariableRef{VT}}) where {VT}
    for v in vref
        delete(m, v)
    end
    return nothing
end

function JuMP.variable_by_name(m::PolyModel, sname::String)
    lookup = Dict(val => key for (key, val) in model.variables)
    lookup_keys = collect(keys(lookup))
    idx = findall(x -> JuMP.name(x) == sname, lookup_keys)
    if isempty(idx)
        return nothing
    elseif length(idx) > 1
        throw(ErrorException("Multiple variables have the name $sname."))
    else
        return PolyVariableRef(m, lookup[first(idx)])
    end
end

function JuMP.build_constraint(error::Function, x::PolyVariableRef, set::MOI.AbstractScalarSet)
    return PolynomialConstraint(object(x), set)
end

# lower bound for variables
function JuMP.has_lower_bound(v::PolyVariableRef)
    return info(v).has_lb
end

function _lower_bound_index(v::PolyVariableRef)
    return info(v).lb_constraint    
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
        lower = maximum(lower, lower_bound(v))
    end
    if has_upper_bound(v)
        upper = upper_bound(v)
        if in_interval(v)
            _delete_interval(owner_model(v), index(v))
        else 
            _delete_upper_bound(owner_model(v), index(v))
        end
        _set_interval(owner_model(v), index(v), lower, upper)
    else
        _delete_lower_bound(owner_model(v), index(v))
        _set_lower_bound(owner_model(v), index(v), lower)
    end
    return
end

function JuMP.LowerBoundRef(v::PolyVariableRef)
    @assert has_lowerbound(v) "Variable $v is not bounded from below."
    @assert !in_interval(v) "$v is constrained to be in an interval. Try calling IntervalRef($v)."
    return info(v).lb_constraint
end

function JuMP.delete_lower_bound(v::PolyVariableRef)
    @assert has_lowerbound(v) "Variable $v is not bounded from below."
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

function _upper_bound_index(v::PolyVariableRef)
    return info(v).ub_constraint    
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
        lower = minimum(upper, upper_bound(v))
    end
    if has_lower_bound(v)
        lower = lower_bound(v)
        if in_interval(v)
            _delete_interval(owner_model(v), index(v))
        else 
            _delete_lower_bound(owner_model(v), index(v))
        end
        _set_interval(owner_model(v), index(v), lower, upper)
    else
        _delete_upper_bound(owner_model(v), index(v))
        _set_upper_bound(owner_model(v), index(v), upper)
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

function _interval_index(v::PolyVariableRef)
    return info(v).in_constraint    
end

export interval
function interval(v::PolyVariableRef)
    @assert in_interval(v) "Variable $v is not constrained to an interval."
    return [info(v).lower, info(v).upper]
end

function _set_in_interval(model::PolyModel, idx::Int, lower::Number, upper::Number)
    model.variable_info[idx].in_interval = true
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
    if has_lower_bound(v)
        lower = maximum(lower, lower_bound(v))
        _delete_lower_bound(owner_model(v), index(v))
    end
    if has_upper_bound(v)
        upper = maximum(upper, upper_bound(v))
        _delete_upper_bound(owner_model(v), index(v))        
    end
    if in_interval(v)
        _delete_in_interval(owner_model(v), index(v)) 
    end
    _set_in_interval(owner_model(v), index(v), lower, upper)
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

function JuMP._fix_index(v::PolyVariableRef)
    return info(v).fix_constraint
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
    model.variable_info[idx].is_fixed = false
    model.variable_info[idx].fix_value = nothing
    delete(model, model.variable_info[idx].fix_constraint)
    model.variable_info[idx].fix_constraint = nothing
    return
end

function JuMP.fix(v::PolyVariableRef, value::Number; force::Bool = false)
    if _is_fixed(v) 
        _unfix(owner_model(v), index(v))        
        _fix(owner_model(v), index(v), value)
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
    @assert has_fix(v) "Variable $v is not fixed."
    _unfix(owner_model(v), index(v))
    return
end



function JuMP.is_binary(v::PolyVariableRef)
    return info(v).is_binary
end

function _binary_index(v::PolyVariableRef)
    return info(v).bin_constraint
end

function _set_binary(model::PolyModel, idx::Int)
    model.variable_info[idx].binary = true
    var = model.variables[idx]
    variable_info[idx].bin_constraint = @constraint model var*(1-var) == 0
end

function _unset_binary(model::PolyModel, idx::Int)
    model.variable_info[idx].binary = false
    delete(model, variable_info[idx].bin_constraint)
    variable_info[idx].bin_constraint = nothing
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

function JuMP.unset_binary(variable_ref::PolyVariableRef)
    @assert is_binary(v) "$v is not constrained to be binary."
    _unset_binary(owner_model(v), index(v))
    return
end


# integer variables
function JuMP.is_integer(v::PolyVariableRef)
    return info(v).is_integer
end

function _integer_index(v::PolyVariableRef)
    return info(v).int_constraint
end

function _set_integer(model::PolyModel, idx::Int, lower::Number, upper::Number)
    model.variable_info[idx].integer = true
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
    lower = model.variable_info[idx].lower
    upper = model.variable_info[idx].upper
    _delete_in_interval(owner_model(v), index(v))
    _set_integer(owner_model(v), index(v), lower, upper)
    return 
end

function JuMP.IntegerRef(v::PolyVariableRef)
    return info(v).int_constraint    
end


function JuMP.unset_integer(v::PolyVariableRef)
    @assert is_integer(v) "$v is not constrained to be integer."
    lower = model.variable_info[idx].lower
    upper = model.variable_info[idx].upper
    _unset_integer(owner_model(v), index(v))
    _set_interval(owner_model(v), index(v), lower, upper)
    return
end


# start value
function JuMP.start_value(v::PolyVariableRef)::Union{Nothing, Float64}
    return info(v).start_value
end

function JuMP.set_start_value(v::PolyVariableRef, value::Number)
    info(v).has_start = true
    info(v).start_value = value
    return
end


# add variable
function JuMP.add_variable(model::PolyModel{VT}, v::ScalarVariable, name::String="") where {VT}
    info = v.info
    
     @assert (!info.integer||(info.has_lb && info.has_ub)) "Only support integer constraints in combination with upper and lower bound."

    model.vct += 1
    if isempty(name)
        name = "noname"
    else 
        #TODO register name
    end

    var = model.variables[model.vct] = VT(name)
    pinfo = model.variable_info[model.vct] = PolyVariableInfo()
    if info.has_fix
        pinfo.has_fix = true
        pinfo.fix_value = info.fixed_value
        pinfo.fix_constraint = @constraint model var in MOI.EqualTo(info.fixed_value)
    elseif info.binary 
        pinfo.binary = true
        pinfo.bin_constraint = @constraint model var*(1.0-var) == 0.0
    elseif info.integer
        pinfo.integer = true 
        poly =  prod((var-i) for i = ceil(info.lower_bound):floor(info.upper_bound)) 
        pinfo.int_constraint = @constraint model poly == 0.0
    elseif info.has_lb && info.has_ub
        pinfo.in_interval = true
        pinfo.has_lb = true 
        pinfo.has_ub = true 
        pinfo.lower = info.lower_bound
        pinfo.upper = info.upper_bound
        pinfo.in_constraint = @constraint model (var - info.lower_bound)*(info.upper_bound - var) >= 0
    else
        if info.has_lb  
            pinfo.has_lb = true 
            pinfo.lower = info.lower_bound
            pinfo.lb_constraint = @constraint model  var in MOI.GreaterThan(info.lower_bound)
        end
        if info.has_ub
            pinfo.has_ub = true 
            pinfo.upper = info.upper_bound
            pinfo.ub_constraint =  @constraint model var in MOI.LessThan(info.upper_bound)
        end
    end

    if info.has_start
        pinfo.has_start = true
        pinfo.start_value = info.start
    end

    return PolyVariableRef{variable_union_type(var)}(model, model.vct)
end


function JuMP.all_variables(model::PolyModel)
    indices = sort!(collect(keys(model.variables)))
    return PolyVariableRef.(model, indices)
end
