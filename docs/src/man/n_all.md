# [All Overloaded Functions](@id all)
The following functions from Base have been overloaded for operations with `TPS`:
```
+, -, *, /, ^, ∘, inv, atan, hypot, abs, sqrt, exp, log, 
sin, cos, tan, csc, sec, cot, sinc, sinh, cosh, tanh, csch, 
sech, coth, asin, acos, atan, acsc, asec, acot, asinh, acosh, 
atanh, acsch, asech, acoth, zero, zeros, one, ones, real, imag, 
conj, angle, complex, promote_rule, getindex, setindex!, ==, <, 
>, <=, >=, !=, isequal, show, copy!, lastindex, firstindex,
rand
```

`zeros` and `ones` are overloaded from Base so that allocated `TPS`s are placed in each element. If we didn't explicity overload these functions, every element would correspond to the exact same heap-allocated TPS, which is problematic when setting individual monomial coefficients of the same TPS.

`GTPSA.jl` overloads (and exports) the following functions from the corresponding packages:
 **`LinearAlgebra`**: `norm`, `mul!` 
**`SpecialFunctions`**: `erf`, `erfc`

`GTPSA.jl` also provides the following functions NOT included in Base or any of the above packages (and not already documented in [TPS Methods](@ref tpsmethods)):
```
add!, sub!, div!, unit, sinhc, asinc, asinhc, polar, rect
```

If there is a mathematical function in Base which you'd like and is not included in the above list, feel free to submit an [issue](https://github.com/bmad-sim/GTPSA.jl/issues).