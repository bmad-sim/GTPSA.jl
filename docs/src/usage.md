# Usage

GTPSA distinguishes between **variables** $x_i$ and **parameters** $k_j$ in a TPSA, such that the parameters are strictly unaffected by the variables, however the variables do depend on the parameters. Explicitly, $\partial x_i/\partial k_j \neq 0$ but $\partial k_j/\partial x_i = 0$. This generalization allows for a significant speed-up in computation time.

## Custom Variable/Parameter Orders
The `Descriptor` defines all information about the TPSAs, including the number of variables, order for each variable, number of parameters, and order for each parameter. There are three constructors for a `Descriptor`:

```
# Descriptor for 2 variables with max order 10 for each, and no parameters
d1 = Descriptor(2, 10)

# Descriptor for 3 variables with max order 4 for each, and 1 parameter with max order 2
d2 = Descriptor(3, 4, 1, 2)

# Descriptor for 2 variables and 1 parameter with max orders [6, 5, 4] respectively
d3 = Descriptor(2, 1, [6, 5, 4])
```
The constructor for `Descriptor` definitions are:
```@docs
Descriptor
```
## Monomial Indexing
A TPSA is, roughly-speaking, a structure containing the Taylor coefficients for all of the monomials in the Taylor series. The monomial coefficients can be accessed with indexing by *order* in each of the variables and parameters. A particular monomial in a TPSA `t` is indexed by `t[<x1 order>, ..., <x_nv order>, <k1 order>, ..., <k_np order>]`. For example, suppose we have a TPSA 3rd order in the variables $x_1,x_2$ and 2nd order in the parameter $k_1$. The $x_1^3x_2^1k_1^2$ term is accessed with `t[3,1,2]`. The 0th order part (the *scalar* part) of the TPSA is set with `t[0,0,0]`.

As another example, let's calculate the Taylor expansion for $g(x_1,x_2,k_1)= \cos{(x_1)}+\sin{(k_1)}\sqrt{1+x_2}$ up to 3rd order in the two variables and 2nd order in the one parameter. Explicitly, this would be:

```
# Define the Descriptor for these TPSAs with 2 variables of 3rd order and 1 parameter of 2nd order
d = Descriptor(2, 3, 1, 2)

# Create TPSAs which we will set to equal each of the variables and parameter
x1 = TPSA(d)
x2 = TPSA(d)
k1 = TPSA(d)

# Set the first TPSA to correspond to the first variable
x1[1, 0, 0] = 1

# Set the second TPSA to correspond to the second variable
x2[0, 1, 0] = 1

# Set the third TPSA to correspond to the first parameter
k1[0, 0, 1] = 1

# Calculate g
g = cos(x1) + sin(k1)*sqrt(1+x2)

print(g)

#= Output:
         :  R, NV =   2, MO =  3, NP =   1, PO =  2
 *******************************************************
     I   COEFFICIENT             ORDER   EXPONENTS
     1   1.0000000000000000E+00    0     0 0
     2   0.0000000000000000E+00    1     1 0
     3   0.0000000000000000E+00    1     0 1
     4   1.0000000000000000E+00    1     0 0  3^1
     5  -5.0000000000000000E-01    2     2 0
     6   0.0000000000000000E+00    2     1 1
     7   0.0000000000000000E+00    2     0 2
     8   0.0000000000000000E+00    2     1 0  3^1
     9   5.0000000000000000E-01    2     0 1  3^1
    10   0.0000000000000000E+00    2     0 0  3^2
    11   0.0000000000000000E+00    3     3 0
    12   0.0000000000000000E+00    3     2 1
    13   0.0000000000000000E+00    3     1 2
    14   0.0000000000000000E+00    3     0 3
    15   0.0000000000000000E+00    3     2 0  3^1
    16   0.0000000000000000E+00    3     1 1  3^1
    17  -1.2500000000000000E-01    3     0 2  3^1
    18   0.0000000000000000E+00    3     1 0  3^2
    19   0.0000000000000000E+00    3     0 1  3^2
=#
```

## Using Complex TPSAs

The usage for `ComplexTPSA`s is the same as with regular `TPSA`s, other than some extra functions including `real` and `imag`.



## TPSA Constructors
The general user will want to use the `TPSA(::Descriptor)` or `ComplexTPSA(::Descriptor)` to construct a TPSA. However, other methods, which are used by the operators for TPSAs, are shown below for completeness.
### TPSA
```@docs
TPSA
```
### ComplexTPSA 
```@docs
ComplexTPSA
```