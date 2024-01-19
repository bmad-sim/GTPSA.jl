# Usage
## Defining the TPSA
The `Descriptor` defines all information about the TPSA, including the number of variables and orders for each variable. The constructors for a `Descriptor` are:

```
# Descriptor for 2 variables with max order 10 for each
d1 = Descriptor(2, 10)      # Use this ctor when all variables have the same order

# Descriptor for 3 variables with orders 1, 2, 3 respectively
d2 = Descriptor([1, 2, 3])  # Use this ctor when variables have different truncation orders
```

## Calculating a TPS
After defining a `Descriptor` for the TPSA, the variables (which themselves are represented as TPSs) can be obtained using `vars` or `complexvars`. For example, suppose we wish to calculate the Taylor series for $f(v_1,v_2) = \cos{(v_1)} + \sqrt{1+v_2}$ to 4th order in $v_1$ and but only 1st order in $v_2$:

```
d = Descriptor([4, 1])

# Returns a Vector of each variable as a TPS
x = vars(d) 

# f is a TPS containing the result
f = cos(x[1]) + sqrt(1 + x[2])
```

A blank `TPS` or `ComplexTPS`, with all coefficients equal to zero, can be created using `TPS(d)` or `ComplexTPS(d)` respectively. 


## Partial Derivative Getting/Setting
### Individual Partial Derivatives
Individual monomial coefficients in a TPS `t` can be get/set with two methods of indexing:

1. **By Order:** `t[<v_1 order>, ..., <x_nv order>]`. For example, for a TPS with variables $v_1$, $v_2$, the $v_1^3v_2^1$ monomial coefficient is accessed with `t[3,1]`. The 0th order part (the *scalar* part) of the TPS is indexed with `t[0,0]` or equivalently `t[0]`, as leaving out trailing zeros for unincluded variables is allowed.
2. **By Var => Order:** `t[<ix_var> => <order>, ...]`. This method of indexing is convenient when a TPS contains many variables and parameters. For example, for a TPS with variables $v_1,v_2,...x_{100}$, the $x_{1}^3x_{99}^1$ monomial is accessed with `t[1=>3, 99=>1]`. The scalar part of the TPS cannot be get/set with this method.

```
# Descriptor for 2 variables with order 5
d = Descriptor(2, 5)
x = vars(d)
x1 = x[1]
x2 = x[2]

# Example of indexing by order -------
y1 = TPS(d)             # Create blank TPS with zero for all coefficients
y1[1,0] = 1             # Set first-order part for first variable equal to 1
y1 == x1                # Is true

z2 = TPS(d)             # Create blank TPS with zero for all coefficients
z2[0,1] = 1             # Set first-order part for second variable equal to 1
z2 == x2                # Is true

# Example of indexing by var => order -------
y1 = TPS(d)             # Create blank TPS with zero for all coefficients
y1[1=>1] = 1            # Set first-order part for first variable equal to 1
y1 == x1                # Is true

z2 = TPS(d)             # Create blank TPS with zero for all coefficients
z2[2=>1] = 1            # Set first-order part for second variable equal to 1
z2 == x2                # Is true
```

### Gradients, Jacobians, Hessians
The convenience getters `gradient`, `jacobian`, and `hessian` (as well as their corresponding in-place methods `gradient!`, `jacobian!`, and `hessian!`) are also provided for extracting certain monomial coefficients from a TPS/Vector of TPSs. Note that these functions are not actually calculating anything - at this point the TPS should already have been propagated through the system, and these functions are just extracting the corresponding partial derivatives.

```
# 2nd Order TPSA with 100 variables
d = Descriptor(100, 2)
x = vars(d)

out = cumsum(x)

# Convenience getters for partial derivative extracting:
grad1 = gradient(out[1])
J = jacobian(out)
h1 = hessian(out[1])

# Also in-place getters
gradient!(grad1, out[1])
jacobian!(J, out)
hessian!(h1, out[1])
```

## `@FastGTPSA` Macro

The macro `@FastGTPSA` can be used to speed up evaluation of expressions that contain `TPS`s and/or `ComplexTPS`s. The macro is completely transparent to all other types, so it can be prepended to any existing expressions while still maintaining generic code. Any functions in the expression that are not overloaded by GTPSA will be ignored.

```
using BenchmarkTools

d = Descriptor(3, 5)
x = vars(d)

@btime $x[1]^3*sin($x[2])/log(2+$x[3])-exp($x[1]*$x[2])*im
# Output:  1.346 μs (10 allocations: 160 bytes)

 @btime @FastGTPSA $x[1]^3*sin($x[2])/log(2+$x[3])-exp($x[1]*$x[2])*im
# Output:  1.225 μs (1 allocation: 16 bytes)
```

The advantages of using the macro become especially apparent in more complicated systems, for example in `benchmark/taylormap.jl`. To read about how it works, see [For Developers](@ref).

## Promotion of TPS to ComplexTPS

`TPS`s and `ComplexTPS`s can be mixed freely without concern. Any time an operation with a `TPS` and a `ComplexTPS` or a `Complex` number occurs, the result will be a `ComplexTPS`. A `ComplexTPS` can be converted back to a `TPS` using the `real` and `imag` operators.
