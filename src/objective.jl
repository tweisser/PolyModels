function JuMP.objective_sense(model::PolyModel)
    return model.objective_sense
end

function JuMP.set_objective_sense(model::PolyModel, sense::MOI.OptimizationSense)
    model.objective_sense = sense
end


function JuMP.set_objective_function(model::PolyModel, func::AbstractPolynomialLike)
    @assert all([haskey(model.variables, var) for var in variables(func)]) "At least one polynomial variable is not registered in the model."
    model.objective_function = func
end

function JuMP.set_objective_function(model::PolyModel{VT}, func::Real) where {VT}
    set_objective_function(model, func*zero(polynomialtype(Float64, VT)))
end

function JuMP.set_objective(
    model::PolyModel,
    sense::MOI.OptimizationSense,
    func::Union{AbstractPolynomialLike, Real}
)
    set_objective_sense(model, sense)
    set_objective_function(model, func)
end

function JuMP.objective_function_type(model::PolyModel)
    return typeof(model.objective_function)
end

function JuMP.objective_function(model::PolyModel)
    return model.objective_function
end
