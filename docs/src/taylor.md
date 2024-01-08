# Taylor Map Analysis
## Definitions
In some use cases, the TPS may define the evolution of some state $x$ of a dynamical system, e.g. $x_f = \mathcal{M}(x_i,k)$, where $k$ is some defined external parameter. When $\mathcal{M}$ is expanded in powers of $\Delta x_i$ and $\Delta k$, this is referred to as a *Taylor map*. $x$ is referred to as a map variable or *variable*, as its evolution is defined by the TPS. $k$, which does not evolve with $\mathcal{M}$, is referred to as a *parameter*. GTPSA allows distinction between map variables and parameters, as well as the individual orders for each, to speed up Taylor map analyses.

## TPSA Including Variables and Parameters

Two extra `Descriptor` constructors can be used to define a TPSA with both variables and parameters.

```
# Descriptor for 3 variables with max order 4 for each, and 1 parameter with max order 2 (equivalent definitions)
d2 = Descriptor(3, 4, 1, 2)     
d2 = Descriptor([4, 4, 4], [2]) 

# Descriptor for 2 variables with max orders 6, 5 respectively and 1 parameter with max order 4
d3 = Descriptor([6, 5], [4])
```

Just as the variables as a vector of TPSs can be obtained using `vars` or `complexvars`, the parameters as a vector of TPSs can be obtained using `params` or `complexparams`. For example, suppose we wish to calculate the Taylor series for $f(x_1,x_2,k_1) = \cos{(x_1)} + \sin{(k_1)}\sqrt{1+x_2}$ to 10th order in $x_1$ and $x_2$ but only 5th order in the parameter $k_1$:

```
d = Descriptor(2, 10, 1, 5)  # or equivalently Descriptor([10, 10], [5])

x = vars(d)     # Returns a Vector of each variable as a TPS
k = params(d)   # Returns a Vector of each parameter as a TPS

# f is a TPS containing the result
f = cos(x[1]) + sin(k[1]) * sqrt(1 + x[2])
```

The monomial coefficient indexing in a TPS `t` including is also extended:

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

The gradient, Jacobian, and Hessian convenience getters include an option kwarg `include_params`, which may be set to true to include parameters in the extracted partial derivatives.

## Taylor Map Methods

