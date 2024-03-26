# [`mono`/`complexmono`](@id mono)
*Creates a TPS corresponding to a specific monomial*
## Syntax
```
m = mono(var_idx [, use=(descriptor|tps|complextps)])
m = mono(param=param_idx [, use=(descriptor|tps|complextps)])

m = mono(orders [, use=(descriptor|tps|complextps)])

m = mono([vars_sparse_mono] [, params=params_sparse_mono] [, use=(descriptor|tps|complextps)])

m = complexmono(...)
```

## Description
#### Index by Variable/Parameter

`m = mono(var_idx)` creates a `TPS` equal to the variable specified by `var_idx` and the `Descriptor` in `GTPSA.desc_current`

`m = mono(param=param_idx)` creates a `TPS` equal to the parameter specified by `param_idx` and the `Descriptor` in `GTPSA.desc_current`

------

#### Indexing by Order

`m = mono(orders)` creates a `TPS` equal to the monomial specified by the indexing-by-order vector `orders` and the `Descriptor` in `GTPSA.desc_current`

------

#### Indexing by Sparse Monomial

`m = mono(vars_sparse_mono, params=params_sparse_mono)` creates a `TPS` equal to the monomial specified by the indexing-by-sparse monomial vector `vars_sparse_mono` and `params_sparse_mono` and the `Descriptor` in `GTPSA.desc_current`

------

### Optional Keyword Argument

`use=(descriptor|tps|complextps)` creates a mono using any of the above methods but using the `Descriptor` specified by `use`

------

### Complex Monomial

`complexmono` will create a `ComplexTPS` using any of the above methods without the overhead of creating a `TPS` and converting it to a `ComplexTPS`

## Examples
```@repl desc
using GTPSA; GTPSA.show_sparse = false;  GTPSA.show_header=false;#hide
d1 = Descriptor(3, 15, 2, 15); # 3 vars, 2 params, all to order 15
x1 = mono(1)
k1 = mono(param=1)
m312 = mono([3,1,2])
m31221 = mono([3,1,2,2,1])
m312 = mono([1=>3, 2=>1, 3=>3])
m31221 = mono([1=>3, 2=>1, 3=>2], params=[1=>2, 2=>1])
```

## Documentation
```@docs
mono
```