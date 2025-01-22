# [Slicing and `par`](@id slice)
*Indexing a specific polynomial within the TPS*

## Slicing a TPS
A polynomial within the TPS with certain variable orders can be extracted by slicing the TPS. When indexing by order, a colon (`:`) can be used in place for a variable order to include all orders of that variable. If the last specified index is a colon, then the rest of the variable indices are assumed to be colons (else, they are assumed to be zero, following the convention of monomial coefficient indexing).

```@repl slice
using GTPSA; GTPSA.show_sparse = false; GTPSA.show_header = false; #hide
d = Descriptor(5, 10, 2, 10);
Δx = @vars(d);
Δk = @params(d);
f = 2*Δx[1]^2*Δx[3] + 3*Δx[1]^2*Δx[2]*Δx[3]*Δx[4]^2*Δx[5]*Δk[1] + 6*Δx[3] + 5
g = f[[2,:,1]]
h = f[[2,:,1,:]]
```

A TPS can also be sliced with indexing by sparse monomial. In this case, if a colon is included anywhere in the sparse monomial variable index, then all orders of all variables and parameters not explicity specified will be included (colon position does not matter in sparse monomial indexing):

```@repl slice
g = f[[1=>2, :, 3=>1, 4=>0, 5=>0], params=[1=>0, 2=>0]]
h = f[(1=>2, 3=>1, :)]  # Colon position is irrelevant in slicing with sparse monomial indexing
```

When indexing by monomial index, a colon simply needs to be included after the variable index, or just a colon if a parameter is specified:

```@repl slice
fΔx3 = f[3,:]
fΔk1 = f[:,param=1]
```

## `par`

`par` is very similar to slicing a TPS, with two differences:

1. The specified variables and parameters are removed from the resulting slice
2. When indexing by order, a colon is always presumed for unincluded variables/parameters

### Syntax
```
f = par(tps, monomialindex)
```

### Description
## Description
`monomialindex` can be any of kind monomial indexing: by index, by order, and by sparse monomial. See the [monomial indexing](@ref monoindex) for more details on each.

### Examples

```@repl par
using GTPSA; GTPSA.show_sparse = false; GTPSA.show_header=false; #hide
d = Descriptor(5, 10, 2, 10);
Δx = @vars(d);
Δk = @params(d);
f = 2*Δx[1]^2*Δx[3] + 3*Δx[1]^2*Δx[2]*Δx[3]*Δx[4]^2*Δx[5]*Δk[1] + 6*Δx[3] + 5
par(f, 3)
par(f, param=1)
par(f, [2,:,1])
par(f, [2,0,1])
par(f, [1=>2, 3=>1])
par(f, params=[1=>1])
```

### Documentation
```@docs
par
```




