# [`TPS`](@id tps)
*Truncated Power Series struct*
```TPS{T<:Union{Float64,ComplexF64}} <: Number```
## Syntax
```
t = TPS([number] [, use=(descriptor|tps)])

t = TPS64([real] [, use=(descriptor|tps)])

t = ComplexTPS64([number] [, use=(descriptor|tps)])
```

## Description
*Currently, GTPSA only supports `TPS`s with `Float64` or `ComplexF64` monomial coefficients.*

`t = TPS()` creates a new `TPS{Float64}` (=`TPS64`) with all coefficients equal to zero using the `Descriptor` in `GTPSA.desc_current`

`t = TPS(number)` creates a new `TPS` equal to `number`. If `number` is a `TPS`, then this acts as a copy constructor, using that `TPS`'s `Descriptor` (nesting `TPS`s is not permitted). Else, a `TPS` with monomial coefficient type `promote_type(Float64,typeof(number))` is constructed using the `Descriptor` in `GTPSA.desc_current`

`t = TPS64()` creates a new `TPS{Float64}` with all coefficients equal to zero using the `Descriptor` in `GTPSA.desc_current`

`t = TPS64(real)` creates a new `TPS{Float64}` equal to `real`. If `real` is a `TPS`, then this acts as a copy constructor, using that `TPS`'s `Descriptor` (nesting `TPS`s is not permitted). Else, a `TPS` with monomial coefficient type `Float64` is constructed using the `Descriptor` in `GTPSA.desc_current`

`t = ComplexTPS64()` creates a new `TPS{ComplexF64}` with all coefficients equal to zero using the `Descriptor` in `GTPSA.desc_current`

`t = ComplexTPS64(number)` creates a new `TPS{ComplexF64}` equal to `number`. If `number` is a `TPS`, then this acts as a copy constructor, using that `TPS`'s `Descriptor` (nesting `TPS`s is not permitted). Else, a `TPS` with monomial coefficient type `ComplexF64` is constructed using the `Descriptor` in `GTPSA.desc_current`

### Optional Keyword Argument

`use=descriptor` creates a new `TPS` having a `Descriptor` equal to that passed. If changing the `Descriptor` of a `TPS`, the number of variable + number of parameters must be equivalent, and invalid monomials in the new `Descriptor` will be removed.

`use=tps` creates a new `TPS` having a `Descriptor` equal to that used by the passed `TPS`. If changing the `Descriptor` of a `TPS`, the number of variable + number of parameters must be equivalent, and invalid monomials in the new `Descriptor` will be removed.

## Examples
```@repl desc
using GTPSA; GTPSA.show_sparse = false; #hide
GTPSA.show_header = true
d1 = Descriptor(1, 1); # 1 variable to order 1
t1_1 = TPS()
t2_1 = TPS(5)
t3_1 = TPS(t2_1)
d2 = Descriptor(1, 10); # New Descriptor to order 10
t1_2 = ComplexTPS64() # Uses d2
t2_2 = ComplexTPS64(6)
t3_2 = ComplexTPS64(t3_1, use=d2) # Promotes and changes Descriptor
GTPSA.show_header = false #hide
```

## Documentation
```@docs
TPS
```