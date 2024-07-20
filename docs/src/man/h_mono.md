# [`mono`/`complexmono`](@id mono)
*Creates a TPS corresponding to a specific monomial*
## Syntax
```
m = mono(orders [, use=(descriptor|tps|ComplexTPS64)])

m = mono([vars_sparse_mono] [, params=params_sparse_mono] [, use=(descriptor|tps|ComplexTPS64)])

m = mono(idx [, use=(descriptor|tps|ComplexTPS64)])
m = mono(param=param_idx [, use=(descriptor|tps|ComplexTPS64)])

m = complexmono(...)
```

## Description
#### Indexing by Order

`m = mono(orders)` creates a `TPS` equal to the monomial specified by the indexing-by-order vector/tuple `orders` using the `Descriptor` in `GTPSA.desc_current`

------

#### Indexing by Sparse Monomial

`m = mono(vars_sparse_mono, params=params_sparse_mono)` creates a `TPS` equal to the monomial specified by the indexing-by-sparse monomial vector/tuple `vars_sparse_mono` and `params_sparse_mono` using the `Descriptor` in `GTPSA.desc_current`

------

#### Indexing by Monomial Index

`m = mono(idx)` creates a `TPS` equal to the monomial specified by `idx` and the `Descriptor` in `GTPSA.desc_current`

`m = mono(param=param_idx)` creates a `TPS` equal to the monomial specified by `param_idx + nv` where `nv` is the number of variables in the GTPSA, using the `Descriptor` in `GTPSA.desc_current`

------

### Optional Keyword Argument

`use=(descriptor|tps|ComplexTPS64)` creates a mono using any of the above methods but using the `Descriptor` specified by `use`

------

### Complex Monomial

`complexmono` will create a `ComplexTPS64` using any of the above methods without the overhead of creating a `TPS` and converting it to a `ComplexTPS64`

## Examples
```@repl desc
using GTPSA; GTPSA.show_sparse = false;  GTPSA.show_header=false;#hide
d1 = Descriptor(3, 15, 2, 15); # 3 vars, 2 params, all to order 15
x1 = mono(1)
k1 = mono(param=1)
m312 = mono([3,1,2])
m31221 = mono((3,1,2,2,1)) # Tuples allowed for indexing
m312 = mono([1=>3, 2=>1, 3=>3])
m31221 = mono((1=>3, 2=>1, 3=>2), params=(1=>2, 2=>1))
```

## Documentation
```@docs
mono
```