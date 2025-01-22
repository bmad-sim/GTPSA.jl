# [`mono`/`complexmono`](@id mono)
*Creates a TPS corresponding to a specific monomial*
## Syntax
```
m = mono([tpstype, ] monomialindex [, use=(descriptor|tps)])

# Static Descriptor resolution:
m = mono(TPS{Float64|ComplexF64, descriptor}, monomialindex)

# Dynamic Descriptor
m = mono(monomialindex [, use=(descriptor|tps)])
m = mono(TPS{Float64|ComplexF64 [, GTPSA.Dynamic]} monomialindex [, use=(descriptor|tps)])
```

## Description
`monomialindex` can be any of kind monomial indexing: by index, by order, and by sparse monomial. See the [monomial indexing](@ref monoindex) for more details on each.

## Examples
```@repl desc
using GTPSA; GTPSA.show_sparse = false; # hide
d1 = Descriptor(3, 15, 2, 15); # 3 vars, 2 params, all to order 15
mono(1)
mono(TPS64{d}, 1)
mono(ComplexTPS64{d}, param=1)
mono(TPS64, [3,1,2], use=d1)
mono(TPS64{d}, (3,1,2,2,1))
mono([1=>3, 2=>1, 3=>3])
mono((1=>3, 2=>1, 3=>2), params=(1=>2, 2=>1), use=d1)
```

## Documentation
```@docs
mono
```