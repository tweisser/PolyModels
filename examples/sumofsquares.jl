using DynamicPolynomials
using PolyModels

# generate a model with PolyVar{true} as variable type
m = PolyModel{PolyVar{true}}()

# add new variables to the PolyModel using JuMP syntax
@variables m begin 
    w
    x
    y
    z
end

# define some data
f = w^2*x^2 + x*y + y*z 
g1 = 1 - w^2 - x^2
g2 = 1 - x^2 - y^2
g3 = 1 - y^2 - z^2
h = x + y 

# use JuMP syntax to add constraints and objective

@objective m Min f
@constraint m [g1, g2, g3] .>= 0
@constraint m h == 1 

# this is how the model looks like
print(m)


# now we can start using the PolyModel, for example to approximate its solution with an SOS approach
using SumOfSquares

sosm = SOSModel()
@variable sosm t
@objective sosm Max t
con = @constraint sosm objective_function(m) - t in SOSCone() domain = set(feasible_set(m))

using MosekTools
set_optimizer(sosm, Mosek.Optimizer)
optimize!(sosm)

extractatoms(moment_matrix(con), 1e-3)
