# PolyModels
Dev: [![Build Status](https://travis-ci.org/tweisser/PolyModels.svg?branch=master)](https://travis-ci.org/tweisser/PolyModels)[![codecov](https://codecov.io/gh/tweisser/PolyModels/branch/master/graph/badge.svg)](https://codecov.io/gh/tweisser/PolyModels)


PolyModels provides an JuMP.AbstractModel that represents a polynomial optimization problem of the form 
```
Min/Max f(x)
    st. g_i(x) >=/<=/== 0
        x in R^n
```

The intention of this package is to be able to use code that generates JuMP models (such as PowerModels) to generate `x`, `f` and the `g_i` as polynomials (represented by MultivariatePolynomials). The data generated this way then can be used with a package that is intended to approximate polynomial optimization problems such as SumOfSquares. 


