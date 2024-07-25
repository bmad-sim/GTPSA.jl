# [All Overloaded Functions](@id all)
The following functions from Base have been overloaded for operations with `TPS`:
```
+, -, *, /, ^, ∘, inv, atan, hypot, abs, sqrt, exp, log, sin, cos, 
tan, csc, sec, cot, sinc, sinh, cosh, tanh, csch, sech, coth, asin, 
acos, atan, acsc, asec, acot, asinh, acosh, atanh, acsch, asech, 
acoth, zero, zeros, one, ones, real, imag, conj, angle, complex, 
promote_rule, getindex, setindex!, ==, <, >, <=, >=, !=, isequal, 
isless, isinf, isnan, show, copy!, lastindex, firstindex, rand, 
unsafe_convert, eltype, eps, floatmin, floatmax
```

`zeros` and `ones` are overloaded from Base so that allocated `TPS`s are placed in each element. Because of the mutability of `TPS`, if we didn't explicity overload these functions every element would correspond to the exact same heap-allocated TPS.

`GTPSA.jl` overloads (and exports) the following functions from the corresponding packages:
 **`LinearAlgebra`**: `norm`, `mul!` 
**`SpecialFunctions`**: `erf`, `erfc`

`GTPSA.jl` also provides the following math functions NOT included in Base or any of the above packages (and not already documented in [TPS Methods](@ref tpsmethods)):
```
unit, sinhc, asinc, asinhc, polar, rect
```

If there is a mathematical function in Base which you'd like and is not included in the above list, feel free to submit an [issue](https://github.com/bmad-sim/GTPSA.jl/issues).

## Mutable Mathematics Interface
For every math function, `GTPSA.jl` provides mutating functions to set an allocated `TPS` in-place. For non-arithmetic operators, this is denoted with a bang (!) symbol. E.g. for `sin(a)`, there also exists `GTPSA.sin!(b, a)` where `b` is set equal to `sin(a)`. Aliasing (`b === a`) for all operators is allowed in GTPSA.

For the arithmetic operators `+`, `-`, `*`, `/`, `^`, there are the corresponding in-place functions `GTPSA.add!(c, a, b)`, `GTPSA.sub!(c, a, b)`, `GTPSA.mul!(c, a, b)`, `GTPSA.div!(c, a, b)`, and `GTPSA.pow!(c, a, b)`. While `c` must be an allocated `TPS`, `a` and `b` could be a `TPS` or any `Number` type, so long as they can be appropriately converted to the destination `TPS` parametric number type. Aliasing for all arithmetic operators (`a === b === c`) is allowed in GTPSA.

