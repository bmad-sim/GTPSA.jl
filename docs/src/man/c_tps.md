# [`TPS`](@id tps)
*Truncated Power Series struct*
```TPS{T<:Union{Float64,ComplexF64},D} <: Number```

`T` is the number type of the monomial coefficients in the `TPS`. *Currently, GTPSA only supports `TPS`s with `Float64` or `ComplexF64` monomial coefficients.*

`D` is either a `Descriptor` (for static `Descriptor` resolution) or `GTPSA.Dynamic` (for dynamic `Descriptor` resolution). 

## Aliases
```julia
const TPS64 = TPS{Float64}
const ComplexTPS64 = TPS{ComplexF64}
```
Note that `TPS{Float64}{D} == TPS{Float64,D} == TPS64{D}`. [See the Julia documentation for `UnionAll` types for more details.](https://docs.julialang.org/en/v1/devdocs/types/#UnionAll-types)

## Syntax
```
t = TPS[{descriptor|numtype}]([number] [, use=(descriptor|tps)])

# Static Descriptor resolution:
t = TPS{descriptor}([number])
t = TPS{Float64|ComplexF64, descriptor}([number])

# Dynamic Descriptor resolution:
t = TPS([number] [, use=(descriptor|tps)]) 
t = TPS{Float64|ComplexF64 [, GTPSA.Dynamic]}([number] [, use=(descriptor|tps)]) 
```

## Static `Descriptor` Resolution
`t = TPS{descriptor}([number])` constructs a new `TPS` using `descriptor`, equal to `number` if provided. If `number` is a `TPS` and has the same `Descriptor`, then this is a copy constructor. If `number` is a `TPS` and has a different `Descriptor`, then this will change the `Descriptor` granted the number of variables + parameters are equivalent. The monomial coefficient number type is inferred from `number`, defaulting to `Float64`.

`t = TPS{Float64|ComplexF64, descriptor}([number])` is the same as `t = TPS{descriptor}([number])` but with the monomial coefficient number type explicitly specified.

### Examples
```@repl desc
using GTPSA; GTPSA.show_sparse = false; #hide
d1 = Descriptor(1, 1); # 1 variable to order 1
t1_1 = TPS{d1}()
t2_1 = TPS{d1}(5im)
t3_1 = TPS{d1}(t2_1)
d10 = Descriptor(1, 10); # New Descriptor to order 10
t1_2 = TPS64{d10}() # Uses d10
t2_2 = ComplexTPS64{d10}(6)
t3_2 = ComplexTPS64{d10}(t3_1) # Promotes and changes Descriptor
```

## Dynamic `Descriptor` Resolution
`t = TPS([number] [, use=(descriptor|tps)])` construct a new `TPS`, equal to `number` if provided, and inferring the `Descriptor` from the input arguments:

| Ctor Call                                | Descriptor                                                      |
| :-------------------                     | :--------------------------------------------                   |
| `TPS()`                                  | `GTPSA.desc_current`                                            |
| `TPS(use=descriptor)`                    | `descriptor`                                                    |
| `TPS(use=tps1)`                          | That of `tps1`                                                  |
| `TPS(tps)`                               | That of `tps`                                                   |
| `TPS(number)`                            | `GTPSA.desc_current`                                            |
| `TPS(number, use=(descriptor or tps1) )` | `descriptor` or that of `tps1`                                  |
| `TPS(tps, use=(descriptor or tps1) )`    | `descriptor` or that of `tps1` (copies + changes `Descriptor`!) |

`t = TPS{Float64|ComplexF64 [, GTPSA.Dynamic]}([number] [, use=(descriptor|tps)]) ` is the same as `TPS([number] [, use=(descriptor|tps)])` but with the monomial coefficient number type explicitly specified, and optionally the `GTPSA.Dynamic` dynamic mode identifier explicitly specified.

### Optional Keyword Argument

`use=(descriptor|tps)` creates a new `TPS` having a `Descriptor` equal to that passed. If changing the `Descriptor` of a `TPS`, the number of variable + number of parameters must be equivalent, and invalid monomials in the new `Descriptor` will be removed.

### Examples
```@repl desc
using GTPSA; GTPSA.show_sparse = false; #hide
d1 = Descriptor(1, 1); # 1 variable to order 1
t1_1 = TPS()
t2_1 = TPS(5im)
t3_1 = TPS(t2_1)
d10 = Descriptor(1, 10); # New Descriptor to order 10
t1_2 = TPS64() # Uses d10
t2_2 = ComplexTPS64(6)
t3_2 = ComplexTPS64(t3_1, use=d10) # Promotes and changes Descriptor
```

## Documentation
```@docs
TPS
```
