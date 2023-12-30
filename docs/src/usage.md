# Usage
## Definitions
In some use cases, the TPS may define the evolution of some state $x$ of a dynamical system, e.g. $x_f = \mathcal{M}(x_i,k)$, where $k$ is some defined external parameter. When $\mathcal{M}$ is expanded in powers of $\Delta x_i$ and $\Delta k$, this is referred to as a *Taylor map*. $x$ is referred to as a map variable or *variable*, as its evolution is defined by the TPS. $k$, which does not evolve with $\mathcal{M}$, is referred to as a *parameter*. GTPSA allows distinction between map variables and parameters, as well as the individual orders for each, to speed up Taylor map analyses.

## Defining the TPSA
The `Descriptor` defines all information about the TPSA, including the number of variables, order for each variable, number of parameters, and order for each parameter. The constructors for a `Descriptor` are:

```
# Descriptor for 2 variables with max order 10 for each (equivalent definitions)
d1 = Descriptor(2, 10) 
d1 = Descriptor([10, 10])

# Descriptor for 3 variables with max order 4 for each, and 1 parameter with max order 2 (equivalent definitions)
d2 = Descriptor(3, 4, 1, 2)     
d2 = Descriptor([4, 4, 4], [2]) 

# Descriptor for 2 variables with max orders 6, 5 respectively and 1 parameter with max order 4
d3 = Descriptor([6, 5], [4])
```

See [Constructors](@ref) for the documentation of each `Descriptor` constructor.

## Calculating a TPS
After defining a `Descriptor` for the TPSA, generally one would want the variables or parameters in the descriptor, defined as `TPS`s or `ComplexTPS`s. The convenience functions `vars` and `params`, and their complex counterparts `complexvars` and `complexparams`, are provided which return a Vector of all variables in the TPSA as `TPS`s (or `ComplexTPS`s) themselves. For example, suppose we wish to calculate the Taylor series for $f(x_1,x_2,k_1) = \cos{(x_1)} + \sin{(k_1)}\sqrt{1+x_2}$ to 10th order in $x_1$ and $x_2$ but only 5th order in the parameter $k_1$:

```
d = Descriptor(2, 10, 1, 5)  # or equivalently Descriptor([10, 10], [5])

x = vars(d)     # Returns a Vector of each variable as a TPS
k = params(d)   # Returns a Vector of each parameter as a TPS

# f is a TPS containing the result
f = cos(x[1]) + sin(k[1]) * sqrt(1 + x[2])
```

A blank `TPS` or `ComplexTPS`, with all coefficients equal to zero, can be created using `TPS(d)` or `ComplexTPS(d)` respectively. 

```@docs
vars
complexvars
params
complexparams
```

## Monomial Indexing
The monomial coefficients in a TPS `t` can be get/set with two methods of indexing:

1. **By Order:** `t[<x_1 order>, ..., <x_nv order>, <k_1 order>, ..., <k_np order>]`. For example, for a TPS with variables $x_1$, $x_2$ and parameter $k_1$, the $x_1^3x_2^1k_1^2$ monomial coefficient is accessed with `t[3,1,2]`. The 0th order part (the *scalar* part) of the TPS is indexed with `t[0,0,0]` or equivalently `t[0]`, as leaving out trailing zeros for unincluded variables/parameters is allowed.
2. **By Var => Order, Param => Order:** `t[<ix_var> => <order>, ..., params=(<ix_param> => <order>, ...)]`. This method of indexing is convenient when a TPS contains many variables and parameters. For example, for a TPS with variables $x_1$, $x_2$ and parameter $k_1$, the $x_1^3x_2^1k_1^2$ monomial is accessed with `t[1=>3, 2=>1, params=(1=>2,)]`. The scalar part of the TPS cannot be set with this method.

```
# Descriptor for 2 variables with order 5, and 2 parameters with orders 5
d = Descriptor(2, 5, 2, 5)
x = vars(d)
x1 = x[1]
x2 = x[2]
k = params(d)
k1 = k[1]
k2 = k[2]

# Example of indexing by order -------
y1 = TPS(d)             # Create blank TPS with zero for all coefficients
y1[1,0,0,0] = 1         # Set first-order part for first variable equal to 1
y1 == x1                # Is true

z2 = TPS(d)             # Create blank TPS with zero for all coefficients
z2[0,0,0,1] = 1         # Set first-order part for second parameter equal to 1
z2 == k2                # Is true

# Example of indexing by var => order, param => order -------
y1 = TPS(d)             # Create blank TPS with zero for all coefficients
y1[1=>1] = 1            # Set first-order part for first variable equal to 1
y1 == x1                # Is true

z2 = TPS(d)             # Create blank TPS with zero for all coefficients
z2[params=(2=>1,)] = 1  # Set first-order part for second parameter equal to 1
z2 == k2                # Is true
```

## Promotion of TPS to ComplexTPS

`TPS`s and `ComplexTPS`s can be mixed freely without concern. Any time an operation with a `TPS` and a `ComplexTPS` or a `Complex` number occurs, the result will be a `ComplexTPS`. A `ComplexTPS` can be converted back to a `TPS` using the `real` and `imag` operators.


