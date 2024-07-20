# [`ComplexTPS64`](@id ComplexTPS64)
*Complex Truncated Power Series struct*
```ComplexTPS64 <: Number```
## Syntax
```
ct = ComplexTPS64([number] [, use=(descriptor|tps|ComplexTPS64)])

ct = ComplexTPS64([real1, real2] [, use=(descriptor|tps|ComplexTPS64)])
```

## Description

`ct = ComplexTPS64()` creates a new `ComplexTPS64` with all coefficients equal to zero using the `Descriptor` in `GTPSA.desc_current`

`ct = ComplexTPS64(number)` creates a new `ComplexTPS64` equal to `number`. If `number` is a `TPS`/`ComplexTPS64`, then its `Descriptor` is used (equivalent to a copy constructor). Else, the `Descriptor` in `GTPSA.desc_current` is used

`ct = ComplexTPS64(real1, real2)` creates a new `ComplexTPS64` equal to `complex(real1, real2)`. If `real1` or `real2` is a `TPS` then its `Descriptor` is used. Else, the `Descriptor` in `GTPSA.desc_current` is used

### Optional Keyword Argument

`use=descriptor` creates a new `ComplexTPS64` having a `Descriptor` equal to that passed

`use=(tps|ComplexTPS64)` creates a new `ComplexTPS64` having a `Descriptor` equal to that used by the passed `TPS` or `ComplexTPS64`

## Examples
```@repl desc
using GTPSA; GTPSA.show_sparse = false; #hide
GTPSA.show_header = true
d1 = Descriptor(1, 1); # 1 variable to order 1
t1_1 = ComplexTPS64()
t2_1 = ComplexTPS64(5)
t3_1 = ComplexTPS64(6, TPS(5))
t3_1 = ComplexTPS64(TPS(5))
d2 = Descriptor(1, 10); # New Descriptor to order 10
t3_2 = ComplexTPS64(t3_1, use=d2) # Copies and changes Descriptor
GTPSA.show_header = false #hide
```

## Documentation
```@docs
ComplexTPS64
```