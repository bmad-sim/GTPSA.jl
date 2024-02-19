# [All Overloaded Functions](@id all)
The following functions from Base have been overloaded for operations with `TPS`/`ComplexTPS`, and also maps:
```
+, -, *, /, ^, ∘, inv, atan, hypot, abs, sqrt, exp, log, 
sin, cos, tan, csc, sec, cot, sinc, sinh, cosh, tanh, csch, 
sech, coth, asin, acos, atan, acsc, asec, acot, asinh, acosh, 
atanh, acsch, asech, acoth, zero, one, real, imag, conj, angle, 
complex, promote_rule, getindex, setindex!, ==, <, >, <=, >=, 
!=, isequal, show
```

`GTPSA.jl` overloads one function from `LinearAlgebra`: `norm`

`GTPSA.jl` also provides the following functions NOT included in Base:
```
unit, sinhc, asinc, asinhc, erf, erfc, polar, rect 
```

If there is a mathematical function in Base which you'd like and is not included in the above list, feel free to submit an [issue](https://github.com/bmad-sim/GTPSA.jl/issues).