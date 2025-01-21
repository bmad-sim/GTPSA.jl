# [`TPS`](@id tps)
*Truncated Power Series struct*
```TPS{T<:Union{Float64,ComplexF64},D} <: Number```
## Syntax
```
# Static Descriptor resolution:
t = TPS{descriptor}([number])
t = TPS64{descriptor}([number])
t = ComplexTPS64{descriptor}([number])

# Dynamic Descriptor resolution:
t = TPS([number] [, use=(descriptor|tps)])
t = TPS64([number] [, use=(descriptor|tps)]) 
t = ComplexTPS64([number] [, use=(descriptor|tps)]) 
t = TPS64{GTPSA.Dynamic}([number] [, use=(descriptor|tps)])
t = ComplexTPS64{GTPSA.Dynamic}([number] [, use=(descriptor|tps)])
```
*Currently, GTPSA only supports `TPS`s with `Float64` or `ComplexF64` monomial coefficients.*

## Static `Descriptor` Resolution
`t = TPS{descriptor}()` creates a new `TPS{Float64,descriptor}` with all coefficients equal to zero

`t = TPS{descriptor}(number)` creates a new `TPS` equal to `number`, inferring the monomial coefficent type from the type of `number`. If `number` is a `TPS` and has the same `Descriptor`, then this is a copy constructor. If `number` is a `TPS` and has a different `Descriptor`, then this will change the `Descriptor` granted the number of variables + parameters are equivalent.

`t = TPS64{descriptor}()` creates a new `TPS{Float64,descriptor}` with all coefficients equal to zero

`t = TPS64{descriptor}(number)` creates a new `TPS{Float64,descriptor}` equal to `number`. If `number` is a `TPS` and has the same `Descriptor`, then this is a copy constructor. If `number` is a `TPS` and has a different `Descriptor`, then this will change the `Descriptor` granted the number of variables + parameters are equivalent.

`t = ComplexTPS64{descriptor}()` creates a new `TPS{ComplexF64,descriptor}` with all coefficients equal to zero

`t = ComplexTPS64{descriptor}(number)` creates a new `TPS{ComplexF64,descriptor}` equal to `number`. If `number` is a `TPS` and has the same `Descriptor`, then this is a copy constructor. If `number` is a `TPS` and has a different `Descriptor`, then this will change the `Descriptor` granted the number of variables + parameters are equivalent.

### Examples
```@repl desc
using GTPSA; GTPSA.show_sparse = false; #hide
d1 = Descriptor(1, 1); # 1 variable to order 1
t1_1 = TPS{d1}()
t2_1 = TPS{d1}(5)
t3_1 = TPS{d1}(t2_1)
d2 = Descriptor(1, 10); # New Descriptor to order 10
t1_2 = ComplexTPS64{d2}() # Uses d2
t2_2 = ComplexTPS64{d2}(6)
t3_2 = ComplexTPS64{d2}(t3_1) # Promotes and changes Descriptor
```

## Dynamic `Descriptor` Resolution

`t = TPS()` creates a new `TPS{Float64}` (=`TPS64`) with all coefficients equal to zero using the `Descriptor` in `GTPSA.desc_current`

`t = TPS(number)` creates a new `TPS` equal to `number`. If `number` is a `TPS`, then this acts as a copy constructor, using that `TPS`'s `Descriptor` (nesting `TPS`s is not permitted). Else, a `TPS` with monomial coefficient type `promote_type(Float64,typeof(number))` is constructed using the `Descriptor` in `GTPSA.desc_current`

`t = TPS64()` creates a new `TPS{Float64}` with all coefficients equal to zero using the `Descriptor` in `GTPSA.desc_current`

`t = TPS64(real)` creates a new `TPS{Float64}` equal to `real`. If `real` is a `TPS`, then this acts as a copy constructor, using that `TPS`'s `Descriptor` (nesting `TPS`s is not permitted). Else, a `TPS` with monomial coefficient type `Float64` is constructed using the `Descriptor` in `GTPSA.desc_current`

`t = ComplexTPS64()` creates a new `TPS{ComplexF64}` with all coefficients equal to zero using the `Descriptor` in `GTPSA.desc_current`

`t = ComplexTPS64(number)` creates a new `TPS{ComplexF64}` equal to `number`. If `number` is a `TPS`, then this acts as a copy constructor, using that `TPS`'s `Descriptor` (nesting `TPS`s is not permitted). Else, a `TPS` with monomial coefficient type `ComplexF64` is constructed using the `Descriptor` in `GTPSA.desc_current`

### Optional Keyword Argument

`use=descriptor` creates a new `TPS` having a `Descriptor` equal to that passed. If changing the `Descriptor` of a `TPS`, the number of variable + number of parameters must be equivalent, and invalid monomials in the new `Descriptor` will be removed.

`use=tps` creates a new `TPS` having a `Descriptor` equal to that used by the passed `TPS`. If changing the `Descriptor` of a `TPS`, the number of variable + number of parameters must be equivalent, and invalid monomials in the new `Descriptor` will be removed.

### Examples
```@repl desc
using GTPSA; GTPSA.show_sparse = false; #hide
d1 = Descriptor(1, 1); # 1 variable to order 1
t1_1 = TPS()
t2_1 = TPS(5)
t3_1 = TPS(t2_1)
d2 = Descriptor(1, 10); # New Descriptor to order 10
t1_2 = ComplexTPS64() # Uses d2
t2_2 = ComplexTPS64(6)
t3_2 = ComplexTPS64(t3_1, use=d2) # Promotes and changes Descriptor
```

## Documentation
```@docs
TPS
```