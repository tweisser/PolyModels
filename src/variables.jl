JuMP.isequal_canonical(v::MP.AbstractVariable, other::MP.AbstractVariable) = v == other

function JuMP.is_valid(model::PolyModel, vref::MP.AbstractVariable)
    return haskey(model.variables, vref)
end

function JuMP.delete(m::PolyModel, vref::MP.AbstractVariable)
    @assert JuMP.is_valid(m, vref)
    delete!(m.variables, vref)
    return nothing
end

function JuMP.delete(m::PolyModel, vref::Vector{<:MP.AbstractVariable})
    for v in vref
        delete(m, v)
    end
    return nothing
end


function JuMP.variable_by_name(m::PolyModel, sname::String)
    idx = findall(x -> MP.name(x) == sname, collect(keys(m.variables)))
    if isempty(idx)
        return nothing
    elseif length(idx) > 1
        throw(ErrorException("Multiple variables have the name $sname."))
    else
        return collect(keys(m.variables))[first(idx)]
    end
end

function JuMP.add_variable(model::PolyModel{VT}, v::ScalarVariable, name::String="") where {VT}
    info = v.info

     @assert (!info.integer||(info.has_lb && info.has_ub)) "Only support integer constraints in combination with upper and lower bound."

    model.vct += 1
    if isempty(name)
        name = "noname"
    end

    var = VT(name)

    model.variables[var] = model.vct

    if info.has_fix
        @constraint model var == info.fixed_value
    elseif info.binary 
        @constraint model var*(1-var) == 0
    elseif info.integer
        poly =  prod((var-i) for i = ceil(info.lower_bound):floor(info.upper_bound)) 
        @constraint model  poly == 0
    elseif info.has_lb && info.has_ub
        @constraint model (var - info.lower_bound)*(info.upper_bound - var) >= 0
    else
        if info.has_lb
            @constraint model  var >= info.lower_bound
        end
        if info.has_ub
            @constraint model var <= info.upper_bound
        end
    end
    if info.has_start
        @warn "Start value not supported. Information has been lost."   
    end

    return var
end

function JuMP.all_variables(model::PolyModel)
    return sort!(collect(keys(model.variables)), rev = true)
end
