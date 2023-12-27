# Usage
In some use cases, the TPS may define the evolution of some state $x$ of a dynamical system, e.g. $x_f = \mathcal{M}(x_i,k_1)$, where $k_1$ is some defined external parameter. When $\mathcal{M}$ is expanded in powers of $x_i$, this is referred to as a *Taylor map*. $x$ is referred to as a map variable or *variable*, as its evolution is defined by the TPS. $k_1$ which does not evolve with $\mathcal{M}$, is referred to as a *parameter*. GTPSA allows distinction between map variables and parameters, as well as the individual orders for each, to speed up computations.

## Custom Variable/Parameter Orders
The `Descriptor` defines all information about the TPSA, including the number of variables, order for each variable, number of parameters, and order for each parameter. The constructors for a `Descriptor` are:

```
# Descriptor for 2 variables with max order 10 for each
d1 = Descriptor(2, 10)    # Max order 10 for all variables
d1 = Descriptor([10, 10]) # Equivalent definition specifying order for each

# Descriptor for 3 variables with max order 4 for each, and 1 parameter with max order 2
d2 = Descriptor(3, 4, 1, 2)     # Max order 4 for all variables, 2 for all parameters
d2 = Descriptor([4, 4, 4], [2]) # Equivalent definition specifying order for each

# Descriptor for 2 variables with max orders 6, 5 respectively and 1 parameter with max order 4
d3 = Descriptor([6, 5], [4])
```
The constructor for `Descriptor` definitions are:
```@docs
Descriptor
```

## TPS Constructors
After defining a `Descriptor` for the TPSA, generally one would want the variables or parameters in the descriptor, defined as `TPS`s or `ComplexTPS`s. The convenience functions `vars` and `params`, and their complex counterparts `complexvars` and `complexparams` are provided which return a Vector of all variables in the TPSA as `TPS`s (or `ComplexTPS`s). For example, suppose we wish to evaluate the Taylor series for $f(x_1,x_2,k_1) = \cos{(x_1)} + \sin{(k_1)}\sqrt{1+x_2}$ to 10th order in $x_1$ and $x_2$ but only 5th order in the parameter $k_1$:

```
d = Descriptor(2, 10)

x = vars(d)     # Returns a Vector of each variable as a TPS
k = params(d)   # Returns a Vector of each parameter as a TPS

# f is a TPS containing the result
f = cos(x[1]) + sin(k[1]) * sqrt(1 + x[2])
```

A blank `TPS` or `ComplexTPS` with all coefficients equal to zero can be created using `TPS(d)` or `ComplexTPS(d)` respectively.

### TPS
```@docs
vars
params
TPS
```
### ComplexTPS 
```@docs
complexvars
complexparams
ComplexTPS
```

## Monomial Indexing
A TPS contains the Taylor coefficients for all of the monomials in the Taylor series up to the specified truncation order. The monomial coefficients in a TPS can be get/set with two methods of indexing:

1. **By Order:** A particular monomial in a TPS `t` can be indexed by `t[<x_1 order>, ..., <x_nv order>, <k_1 order>, ..., <k_np order>]`. For example, for a TPS with variables $x_1$, $x_2$ and parameter $k_1$, the $x_1^3x_2^1k_1^2$ monomial is accessed with `t[3,1,2]`. The 0th order part (the *scalar* part) of the TPS is set with `t[0,0,0]` or equivalently `t[0]` as leaving out indices for variables/parameters is allowed if they are not included.
```
# Example of indexing by order

# Descriptor for 2 variables with max order 15
d = Descriptor(2, 15)
x = vars(d)
x1 = x[1]
x2 = x[2]

y1 = TPS(d)         # Create blank TPS with zero for all coefficients
y1[1,0] = 1         # Set first-order part for first variable equal to 1

y1[1,0] == x1[1,0]  # Is true
y1 == x1            # Is true

y2 = TPS(d)         # Create blank TPS with zero for all coefficients
y2[0,1] = 1         # Set first-order part for second variable equal to 1

y2[0,1] == x2[0,1]  # Is true
y2 == x2            # Is true
```

2. **By Var => Order, Param => Order:** A particular monomial in a TPS `t` can be indexed by `t[<ix_var> => <order>, ..., params=(<ix_param> => <order>, ...)]`. This method of indexing is convenient when a TPS contains many variables and parameters. For example, for a TPS with variables $x_1$, $x_2$ and parameter $k_1$, the $x_1^3x_2^1k_1^2$ monomial is accessed with `t[1=>3, 2=>1, params=(1=>2,)]`. The 0th order part (the *scalar* part) of the TPS cannot be set with this method.
```
# Example of indexing by var => order, param => order

# Descriptor for 3 variables with order 5, and 2 parameters with orders 3, 4 respectively
d = Descriptor([5, 5, 5], [3, 4])
x = vars(d)
x1 = x[1]
x2 = x[2]
x3 = x[3]
k = params(d)
k1 = k[1]
k2 = k[2]

y1 = TPS(d)             # Create blank TPS with zero for all coefficients
y1[1=>1] = 1            # Set first-order part for first variable equal to 1     

y1 == x1                # Is true          

z1 = TPS(d)             # Create blank TPS with zero for all coefficients
z1[params=(1=>1,)] = 1  # Set first-order part for first parameter equal to 1

z1 == k1                # Is true
```

## Using Complex TPSs

Mixing `ComplexTPS`s with `TPS`s is currently not allowed, however this may be added in the future. The usage for `ComplexTPS`s is the same as with regular `TPS`s, with the addition of some extra functions including `real` and `imag`.


