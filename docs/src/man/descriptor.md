# `Descriptor`
## Syntax
```
d = Descriptor(num_vars, var_ord)     
d = Descriptor(var_ords)
d = Descriptor(num_vars, var_ord, num_params, param_ord)   
d = Descriptor(var_ords, param_ords)  
```

## Description
`d = Descriptor(num_vars, var_ord)` defines a GTPSA `Descriptor` with `num_vars` variables all having the same truncation order `var_ord`

`d = Descriptor(var_ords)` defines a GTPSA `Descriptor` with `length(var_ords)` variables each having individual truncation orders specified in the `var_ords` vector

`d = Descriptor(num_vars, var_ord, num_params, param_ord)` defines a GTPSA `Descriptor` with `num_vars` variables all having the same truncation order `var_ord`, and `num_params` parameters all having the same truncation order `param_ord`

`d = Descriptor(var_ords, param_ords)` defines a GTPSA `Descriptor` with `length(var_ords)` variables each having individual truncation orders specified in the `var_ords` vector, and `length(param_ords)` parameters each having individual truncation orders specified in the `param_ords` vector

-----

A `Descriptor` defines all information about the GTPSA, including the number of variables and parameters, and the truncation orders for each variable and parameter. Before any operations using TPSs, a `Descriptor` must be defined. Each time a new `Descriptor` is created, the non-constant global variable `GTPSA.desc_current` is set. If a `Descriptor` is not explicitly (or implicitly in a TPS copy constructor) passed to the TPS Constructors, `GTPSA.desc_current` will be used by default. This non-constant global can be set by the user throughout a program using multiple `Descriptor`s.

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
Descriptor
```