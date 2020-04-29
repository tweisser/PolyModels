using DynamicPolynomials
using PolyModels

m = PolyModel{Float64, PolyVar{true}}()
@variables m begin 
    w
    x
    y
    z
end

f = w^2*x^2 + x*y + y*z 
g1 = 1 - w^2 - x^2
g2 = 1 - x^2 - y^2
g3 = 1 - y^2 - z^2

@objective m Min f
@constraint m [g1, g2, g3] .>= 0

print(m)

using PolyJuMP
@constraint m g1 == 0
