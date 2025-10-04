# [`@vars`, `@params`](@id varsparams)
*Creates a vector of TPSs corresponding to each variable/parameter in the GTPSA*
## Syntax
```
Δx  = @vars(descriptor [,complex=bool] [, dynamic=bool])

Δk  = @params(descriptor [,complex=bool] [, dynamic=bool])
```

## Description
`Δx = @vars(descriptor)` creates a vector of `TPS`s corresponding to each of the variables in the GTPSA `descriptor`

------

`Δk = @params(descriptor)` creates a vector of `TPS`s corresponding to each of the parameters in the GTPSA `descriptor``

### Optional Argument

`complex` if `true`, will return the corresponding `TPS`s as `ComplexTPS64`. Default is `false`.

`dynamic` if `true`, will use dynamic `Descriptor` resolution for the returned `TPS`s

## Examples
```@repl desc
using GTPSA; GTPSA.show_sparse = false; #hide
d5 = Descriptor(3, 5, 2, 5); # 3 vars, 2 params, all to order 5
Δx = @vars(d5)
Δxc = @vars(d5, complex=true)
Δxd = @vars(d5, dynamic=true)
Δk = @params(d5, complex=true, dynamic=true)
```

## Documentation
```@docs
@vars
@params
```