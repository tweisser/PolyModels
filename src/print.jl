function JuMP.show_objective_function_summary(io::IO, model::PolyModel)
    println(io, "Objective function type: ", objective_function_type(model))
end

function JuMP.objective_function_string(print_mode, model::PolyModel)
    return function_string(print_mode, objective_function(model))
end

function JuMP.function_string(print_mode, v::AbstractPolynomialLike)
    return sprint(show, v)
end

function JuMP.show_backend_summary(io::IO, model::PolyModel)
    print(io, "This is an abstract model without attached Optimizer.")
end

function JuMP.constraints_string(print_mode, model::PolyModel)
    strings = String[]
    for (cref, con) in model.constraints
        push!(strings, constraint_string(print_mode, cref, in_math_mode = true))
    end
    return strings
end

function JuMP.show_constraints_summary(io::IO, model::PolyModel)
    for (F, S) in list_of_constraint_types(model)
        n_constraints = num_constraints(model, F, S)
        println(io, "`$F`-in-`$S`: $n_constraints constraint",
                JuMP._plural(n_constraints))
    end
end
