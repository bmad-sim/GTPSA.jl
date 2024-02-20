# [`Descriptor`](@id descriptor)
*Defines the number of variables, number of parameters, and order(s) for each in the GTPSA*
## Syntax
```
d = Descriptor(num_vars, var_order)     
d = Descriptor(var_orders)
d = Descriptor(num_vars, var_order, num_params, param_order)   
d = Descriptor(var_orders, param_orders)  

GTPSA.desc_current = d
```

## Description
`d = Descriptor(num_vars, var_order)` defines a GTPSA `Descriptor` with `num_vars` variables all having the same truncation order `var_order`

`d = Descriptor(var_orders)` defines a GTPSA `Descriptor` with `length(var_orders)` variables each having individual truncation orders specified in the `var_orders` vector

`d = Descriptor(num_vars, var_order, num_params, param_order)` defines a GTPSA `Descriptor` with `num_vars` variables all having the same truncation order `var_order`, and `num_params` parameters all having the same truncation order `param_order`

`d = Descriptor(var_orders, param_orders)` defines a GTPSA `Descriptor` with `length(var_orders)` variables each having individual truncation orders specified in the `var_orders` vector, and `length(param_orders)` parameters each having individual truncation orders specified in the `param_orders` vector

-----

`GTPSA.desc_current` is a global variable that is set each time a user creates a new `Descriptor`, and can also be set manually by a user. `GTPSA.desc_current` defines the `Descriptor` to use when that information is not explicitly (or implicitly in a TPS copy constructor) available, e.g. when calling `TPS(a)` where `a` is not a `TPS`. This also allows one to use general `Number` commands like `convert(TPS, a)` and `zeros(TPS, 6)`.


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