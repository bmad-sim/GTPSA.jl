# [`TPS`](@id tps)
*Truncated Power Series struct*
```TPS <: Real```
## Syntax
```
t = TPS([real] [, use=(descriptor|tps|complextps)])
```

## Description
`t = TPS()` creates a new `TPS` with all coefficients equal to zero using the `Descriptor` in `GTPSA.desc_current`

`t = TPS(real)` creates a new `TPS` equal to `real`. If `real` is a `TPS`, then its `Descriptor` is used (equivalent to a copy constructor). Else, the `Descriptor` in `GTPSA.desc_current` is used

### Optional Keyword Argument

`use=descriptor` creates a new `TPS` having a `Descriptor` equal to that passed

`use=(tps|complextps)` creates a new `TPS` having a `Descriptor` equal to that used by the passed `TPS` or `ComplexTPS`

## Examples
```@repl desc
using GTPSA; GTPSA.show_sparse = false; #hide
GTPSA.show_header = true
d1 = Descriptor(1, 1); # 1 variable to order 1
t1_1 = TPS()
t2_1 = TPS(5)
t3_1 = TPS(t2_1)
d2 = Descriptor(1, 10); # New Descriptor to order 10
t1_2 = TPS() # Uses d2
t2_2 = TPS(6)
t3_2 = TPS(t3_1, use=d2) # Copies and changes Descriptor
GTPSA.show_header = false #hide
```

## Documentation
```@docs
TPS
```