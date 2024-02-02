# `Descriptor`
*Defines the number of variables, number of parameters, and order(s) for each in the GTPSA*
## Syntax
```
d = Descriptor(num_vars, var_order)     
d = Descriptor(var_orders)
d = Descriptor(num_vars, var_order, num_params, param_order)   
d = Descriptor(var_orders, param_orders)  
```

## Description
`d = Descriptor(num_vars, var_order)` defines a GTPSA `Descriptor` with `num_vars` variables all having the same truncation order `var_order`

`d = Descriptor(var_orders)` defines a GTPSA `Descriptor` with `length(var_orders)` variables each having individual truncation orders specified in the `var_orders` vector

`d = Descriptor(num_vars, var_order, num_params, param_order)` defines a GTPSA `Descriptor` with `num_vars` variables all having the same truncation order `var_order`, and `num_params` parameters all having the same truncation order `param_order`

`d = Descriptor(var_orders, param_orders)` defines a GTPSA `Descriptor` with `length(var_orders)` variables each having individual truncation orders specified in the `var_orders` vector, and `length(param_orders)` parameters each having individual truncation orders specified in the `param_orders` vector

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

## Documentation
```@docs
Descriptor
```