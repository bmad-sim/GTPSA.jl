# [`TPS`/`ComplexTPS`](@id tpscomplextps)
*Truncated Power Series (TPS) struct*
## Syntax
```
t = TPS(descriptor)
t = TPS(tps)
t = TPS(real)
t = TPS(real, use=tps)
t = TPS() 

ct = ComplexTPS(descriptor)
ct = ComplexTPS(tps)
ct = ComplexTPS(complextps)
ct = ComplexTPS(number)
ct = ComplexTPS(number, use=tps)
ct = ComplexTPS() 
```

## Description
`t = TPS(descriptor)` creates a new `TPS` with all coefficients equal to zero, in the GTPSA defined by the passed `Descriptor`

`t = TPS(tps)` creates a new copy of the passed `TPS`

`t = TPS(real)` creates a new `TPS` with scalar value equal to `real` using the `Descriptor` in `GTPSA.desc_current`

`t = TPS(real, use=tps)` creates a new `TPS` with scalar value equal to `real` using the same `Descriptor` as that in the passed `tps`

`t = TPS()` creates a new `TPS` with all coefficients equal to zero using the `Descriptor` in `GTPSA.desc_current`

-----

`ct = ComplexTPS(descriptor)` creates a new `ComplexTPS` with all coefficients equal to zero, in the GTPSA defined by the passed `Descriptor`

`ct = ComplexTPS(tps)` creates a new copy of the passed `TPS` promoted to a `ComplexTPS`

`ct = ComplexTPS(complextps)` creates a new copy of the passed `ComplexTPS`

`ct = ComplexTPS(number)` creates a new `ComplexTPS` with scalar value equal to `number` using the `Descriptor` in `GTPSA.desc_current`

`ct = ComplexTPS(number, use=tps)` creates a new `ComplexTPS` with scalar value equal to `number` using the same `Descriptor` as that in the passed `tps`

`ct = ComplexTPS()` creates a new `ComplexTPS` with all coefficients equal to zero using the `Descriptor` in `GTPSA.desc_current`

## Examples
```@repl desc
using GTPSA; GTPSA.show_sparse = false; #hide
d1 = Descriptor(2, 10);
t1_1 = TPS()
t1_2 = TPS(5)
t1_3 = TPS(t1_2)
ct1 = ComplexTPS(t1_3)
d2 = Descriptor(5, 10);
t2_1 = TPS()
t2_2 = TPS(6)
t1_4 = TPS(d1)
t1_5 = TPS(6, use=t1_4)
```

## Documentation
```@docs
TPS
ComplexTPS
```