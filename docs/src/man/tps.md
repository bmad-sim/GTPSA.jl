# `TPS`/`ComplexTPS`
## Syntax
```
t = TPS(desc)
t = TPS(tps)
t = TPS(num [, use=tps]) 
t = TPS() 
```

## Description
`d = Descriptor(num_vars, var_ord)` defines a GTPSA `Descriptor` with `num_vars` variables all having the same truncation order `var_ord`

`d = Descriptor([var1_ord, var2_ord, ..., varN_ord])` defines a GTPSA `Descriptor` with `N` variables each having individual truncation orders specified in the vector

`d = Descriptor(num_vars, var_ord, num_params, param_ord)` defines a GTPSA `Descriptor` with `num_vars` variables all having the same truncation order `var_ord`, and `num_params` parameters all having the same truncation order `param_ord`

`d = Descriptor([var1_ord, ..., varN_ord], [param1_ord, ..., paramM_ord])` defines a GTPSA `Descriptor` with `N` variables each having individual truncation orders specified in the first vector, and `M` parameters each having individual truncation orders specified in the second vector

-----


## Examples
```@repl desc
using GTPSA #hide
d1 = Descriptor(2, 10)         
d2 = Descriptor([1, 2, 3])     
d3 = Descriptor(3, 4, 1, 2)    
d4 = Descriptor([6, 5], [4])   
GTPSA.desc_current = d1
```

## Constructors
```@docs
```