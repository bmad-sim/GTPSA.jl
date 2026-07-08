# [`vars`, `params`](@id varsparams)
*Creates a vector of TPSs corresponding to each variable/parameter in the GTPSA*
## Syntax
```
# Dynamic Descriptor Resolution:
Δx  = vars([descriptor|tps])
Δk  = params([descriptor|tps])
Δx  = complexvars([descriptor|tps])
Δk  = complexparams([descriptor|tps])

# Static Descriptor Resolution:
Δx  = @vars(descriptor [,complex=bool])
Δk  = @params(descriptor [,complex=bool])
```

## Dynamic `Descriptor` Resolution
`Δx = vars([descriptor|tps])` creates a vector of `TPS`s corresponding to each of the variables in the GTPSA given by `descriptor|tps`, which defaults to `GTPSA.desc_current` if not provided.

------

`Δk = params([descriptor|tps])` creates a vector of `TPS`s corresponding to each of the parameters in the GTPSA given by `descriptor|tps`, which defaults to `GTPSA.desc_current` if not provided

------ 
`Δx = complexvars([descriptor|tps])` creates a vector of complex `TPS`s corresponding to each of the variables in the GTPSA given by `descriptor|tps`, which defaults to `GTPSA.desc_current` if not provided.

------

`Δk = complexparams([descriptor|tps])` creates a vector of complex `TPS`s corresponding to each of the parameters in the GTPSA given by `descriptor|tps`, which defaults to `GTPSA.desc_current` if not provided


### Examples
```@repl desc4
using GTPSA;  #hide
d5 = Descriptor(3, 5, 2, 5); # 3 vars, 2 params, all to order 5
Δx = vars(d5)
Δxc = complexvars(d5)
Δkc = complexparams(d5)
```

## Static `Descriptor` Resolution
`Δx = @vars(descriptor)` creates a vector of `TPS`s corresponding to each of the variables in the GTPSA `descriptor`

------

`Δk = @params(descriptor)` creates a vector of `TPS`s corresponding to each of the parameters in the GTPSA `descriptor`

### Optional Argument

`complex` if `true`, will return the corresponding `TPS`s as `ComplexTPS64`. Default is `false`.

### Examples
```@repl desc5
using GTPSA;  #hide
d5 = Descriptor(3, 5, 2, 5); # 3 vars, 2 params, all to order 5
Δx = @vars(d5)
Δxc = @vars(d5, complex=true)
Δk = @params(d5, complex=true)
```

## Documentation
```@docs
vars
params
complexvars
complexparams
@vars
@params
```