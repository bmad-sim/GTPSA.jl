# [`ComplexTPS`](@id complextps)
*Complex Truncated Power Series struct*
```ComplexTPS <: Number```
## Syntax
```
ct = ComplexTPS([number] [, use=(descriptor|tps|complextps)])

ct = ComplexTPS([real1, real2] [, use=(descriptor|tps|complextps)])
```

## Description

`ct = ComplexTPS()` creates a new `ComplexTPS` with all coefficients equal to zero using the `Descriptor` in `GTPSA.desc_current`

`ct = ComplexTPS(number)` creates a new `ComplexTPS` equal to `number` using the `Descriptor` in `GTPSA.desc_current`

`ct = ComplexTPS(real1, real2)` creates a new `ComplexTPS` equal to `complex(real1, real2)` using the `Descriptor` in `GTPSA.desc_current`

### Optional Keyword Argument

`use=descriptor` creates a new `ComplexTPS` having a `Descriptor` equal to that passed. Invalid monomials will be removed if necessary

`use=(tps|complextps)` creates a new `ComplexTPS` having a `Descriptor` equal to that used by the passed `TPS` or `ComplexTPS`



## Examples
```@repl desc
using GTPSA; GTPSA.show_sparse = false; #hide
GTPSA.show_header = true
d1 = Descriptor(1, 1); # 1 variable to order 1
t1_1 = ComplexTPS()
t2_1 = ComplexTPS(5)
t3_1 = ComplexTPS(6, TPS(5))
t3_1 = ComplexTPS(TPS(5))
d2 = Descriptor(1, 10); # New Descriptor to order 10
t3_2 = ComplexTPS(t3_1, use=d2) # Copies and changes Descriptor
GTPSA.show_header = false #hide
```

## Documentation
```@docs
ComplexTPS
```