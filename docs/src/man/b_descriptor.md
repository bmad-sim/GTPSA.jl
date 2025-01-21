# [`Descriptor`](@id descriptor)
*Defines the number of variables, number of parameters, and order(s) for each in the GTPSA*
## Syntax
```
d = Descriptor(num_vars, max_order)     
d = Descriptor(vars_orders, max_order)
d = Descriptor(num_vars, max_order, num_params, param_order)   
d = Descriptor(vars_orders, max_order, params_orders, param_order)

GTPSA.desc_current = d
```

## Description
`d = Descriptor(num_vars, max_order)` defines a GTPSA `Descriptor` with `num_vars` variables and a maximum truncation order `max_order`

`d = Descriptor(vars_orders, max_order)` defines a GTPSA `Descriptor` with `length(var_orders)` variables each having individual truncation orders specified in the `var_orders` vector, and a maximum truncation order `max_order` for the entire monomial

`d = Descriptor(num_vars, max_order, num_params, param_order)` defines a GTPSA `Descriptor` with `num_vars` variables and `num_params` parameters. The parameters part of a monomial is truncated at `param_order`, and the entire monomial is truncated at `max_order` (so `param_order <= max_order`)

`d = Descriptor(vars_orders, max_order, params_orders, param_order)` defines a GTPSA `Descriptor` with `length(var_orders)` variables each having individual truncation orders specified in the `vars_orders` vector, and `length(param_orders)` parameters each having individual truncation orders specified in the `params_orders` vector. The parameters part of the monomial is truncated at `param_order`,  and the entire monomial is truncated at `max_order` (so `param_order <= max_order`)

-----

`GTPSA.desc_current` is a global variable that is set each time a user creates a new `Descriptor`, and can also be set manually by a user. `GTPSA.desc_current` defines the `Descriptor` to use when [dynamic `Descriptor` resolution](@ref descmodes) is used and a `Descriptor` cannot be inferred, e.g. when calling `TPS(a)` where `a` is not a `TPS`. 


## Examples
```@repl desc
using GTPSA #hide
d1 = Descriptor(2, 10)         
d2 = Descriptor([1, 2, 3], 5)     
d3 = Descriptor(3, 4, 1, 2)    
d4 = Descriptor([6, 5], 8, [4, 3], 7)   
GTPSA.desc_current = d1
```

## Documentation
```@docs
Descriptor
```