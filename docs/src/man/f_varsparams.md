# [`vars`, `params`](@id varsparams)
*Creates a vector of TPSs corresponding to each variable/parameter in the GTPSA*
## Syntax
```
x  = vars([(descriptor|tps|ComplexTPS64)])
xc = complexvars([(descriptor|tps|ComplexTPS64)])

k  = params([(descriptor|tps|ComplexTPS64)])
kc = complexparams([(descriptor|tps|ComplexTPS64)])
```

## Description
`x = vars()` creates a vector of `TPS`s corresponding to each of the variables in the GTPSA defined by the `Descriptor` in `GTPSA.desc_current`

`x = complexvars()` creates a vector of `ComplexTPS64`s corresponding to each of the variables in the GTPSA defined by the `Descriptor` in `GTPSA.desc_current`

------

`k = params()` creates a vector of `TPS`s corresponding to each of the parameters in the GTPSA defined by the `Descriptor` in `GTPSA.desc_current`

`k = complexparams()` creates a vector of `ComplexTPS64`s corresponding to each of the parameters in the GTPSA defined by the `Descriptor` in `GTPSA.desc_current`

### Optional Argument

`descriptor` creates a vector of `TPS`/`ComplexTPS64`s corresponding to each of the variables/parameters in the GTPSA defined by the passed `Descriptor`

`(tps|ComplexTPS64)` creates a vector of `TPS`/`ComplexTPS64`s corresponding to each of the variables/parameters in the GTPSA defined by the `Descriptor` of the passed `TPS` or `ComplexTPS64`


## Examples
```@repl desc
using GTPSA; GTPSA.show_sparse = false; #hide
GTPSA.show_header=true
d1 = Descriptor(3, 5, 2, 5); # 3 vars, 2 params, all to order 5
x1 = vars()
k1 = params()
d2 = Descriptor(2, 5, 1, 5); # 2 vars, 1 param, all to order 5
x2 = vars()
k2 = params()
k1 = params(d1)
```

## Documentation
```@docs
vars
complexvars
params
complexparams
```