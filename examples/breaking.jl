using DynamicPolynomials
using PolyModels
m = PolyModel{PolyVar{true}}()
@variable m x

[x, 2x]
#=
ERROR: MethodError: no method matching Term{true,Int64}(::Int64)
Closest candidates are:
  Term{true,Int64}(::Any, ::Any) where {C, T} at /home/tweisser/.julia/packages/DynamicPolynomials/ZA8sG/src/term.jl:4
Stacktrace:
 [1] convert(::Type{Term{true,Term{true,Int64}}}, ::Term{true,Int64}) at /home/tweisser/.julia/packages/DynamicPolynomials/ZA8sG/src/term.jl:12
 [2] setindex!(::Array{Term{true,Term{true,Int64}},1}, ::Term{true,Int64}, ::Int64) at ./array.jl:826
 [3] copyto!(::Array{Term{true,Term{true,Int64}},1}, ::Tuple{PolyVariableRef{PolyVar{true}},Term{true,Int64}}) at ./abstractarray.jl:724
 [4] vect(::PolyVariableRef{PolyVar{true}}, ::Vararg{Any,N} where N) at ./array.jl:151
 [5] top-level scope at REPL[8]:1
=#

Base.promote_typeof(x, 2x) == Term{true,Term{true,Int64}}
promote_type(typeof(x), typeof(2x)) == Term{true,Term{true,Int64}}
typeof(x) == PolyVariableRef{PolyVar{true}}
typeof(2x) == Term{true,Int64}
