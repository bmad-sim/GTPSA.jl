# [`TPS`](@id tps)
*Truncated Power Series struct*
```TPS <: Real```
## Syntax
```
t = TPS()
t = TPS(real)

t = TPS(..., use=descriptor)
t = TPS(..., use=tps)
t = TPS(..., use=complextps)
```

## Description
`t = TPS()` creates a new `TPS` with all coefficients equal to zero using the `Descriptor` in `GTPSA.desc_current`

`t = TPS(real)` creates a new `TPS` equal to `real` using the `Descriptor` in `GTPSA.desc_current`

### Optional Argument

`t = TPS(..., use=descriptor)` creates a new `TPS` using any of the above methods, but having a `Descriptor` equal to that specified in `use`

`t = TPS(..., use=tps)` creates a new `TPS` using any of the above methods, but having a `Descriptor` equal to that used by the passed `TPS`

`t = TPS(..., use=complextps)` creates a new `TPS` using any of the above methods, but having a `Descriptor` equal to that used by the passed  `ComplexTPS`


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