# Slicing and `par`
*Indexing a specific polynomial within the TPS*

## Slicing a TPS
A polynomial within the TPS with certain variable orders can be extracted by slicing the TPS. When indexing by order, a colon (`:`) can be used in place for a variable order to include all orders of that variable. If the last specified index is a colon, then the rest of the variable indices are assumed to be colons (else, they are assumed to be zero, following the convention of monomial coefficient indexing).

```@repl slice
using GTPSA;  #hide
d = Descriptor(5, 10, 2, 10);
x = vars(d);
k = params(d);
 f = 2*x[1]^2*x[3] + 3*x[1]^2*x[2]*x[3]*x[4]^2*x[5]*k[1] + 6*x[3] + 5
g = f[2,:,1]
h = f[2,:,1,:]
```

A TPS can also be sliced with indexing by sparse monomial. In this case, if a colon is included anywhere in the sparse monomial variable index, then all orders of all variables and parameters not explicity specified will be included (colon position does not matter in sparse monomial indexing):

```@repl slice
g = f[1=>2, :, 3=>1, 4=>0, 5=>0, params=[1=>0, 2=>0]]
h = f[1=>2, 3=>1, :]  # Colon position is irrelevant in slicing with sparse monomial indexing
```

## `par`

`par` is very similar to slicing a TPS, with two differences:

1. The specified variables and parameters are removed from the resulting slice
2. When indexing by order, a colon is always presumed for unincluded variables/parameters

### Syntax
```
f = par(tps, var_idx)
f = par(tps, param=param_idx)

f = par(tps, orders)

f = par(tps, vars_sparse_mono)
f = par(tps, vars_sparse_mono, params=params_sparse_mono)
```

### Description
#### Index by Variable/Parameter
`f = par(tps, var_idx)` extracts the polynomial from the TPS with a first-order dependence on the specified variable, and removes the variable from the polynomial

`f = par(tps, param=param_idx)` extracts the polynomial from the TPS with a first-order dependence on the specified parameter, and removes the parameter from the polynomial

------

#### Indexing by Order
`f = par(tps, orders)` extracts the polynomial from the TPS with the monomial indexed-by-order in `orders`, and removes the variables/parameters included in the indexing from the polynomial

------

#### Indexing by Sparse Monomial
`f = par(tps, vars_sparse_mono)` extracts the polynomial from the TPS with the monomial indexed-by-sparse monomial in `vars_sparse_mono`, and removes the variables included in the indexing from the polynomial

`f = par(tps, params=params_sparse_mono)` extracts the polynomial from the TPS with the monomial indexed-by-sparse monomial in `params_sparse_mono`, and removes the parameters included in the indexing from the polynomial

`f = par(tps, vars_sparse_mono, params=params_sparse_mono)` extracts the polynomial from the TPS with the monomial indexed-by-sparse monomial in `vars_sparse_mono` and `params_sparse_mono`, and removes the variables and/or parameters included in the indexing from the polynomial

### Examples

```@repl par
using GTPSA;  #hide
d = Descriptor(5, 10, 2, 10);
x = vars(d);
k = params(d);
f = 2*x[1]^2*x[3] + 3*x[1]^2*x[2]*x[3]*x[4]^2*x[5]*k[1] + 6*x[3] + 5
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




