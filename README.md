# GTPSA.jl

*A Julia interface to the Generalised Truncated Power Series Algebra (GTPSA) library in MAD.*

This package provides a full-featured Julia interface to the [Generalised Truncated Power Series Algebra (GTPSA) library](https://github.com/MethodicalAcceleratorDesign/MAD-NG) included in MAD, which computes Taylor expansions of real and complex multivariable functions to arbitrary orders in each of the variables and function parameters individually, chosen by the user. GTPSA also allows distinction between variables $x_i$ and parameters $k_j$ in the function such that $\partial x_i/\partial k_j \neq 0$ but $\partial k_j/\partial x_i = 0$. We refer advanced users to [this paper](https://inspirehep.net/files/286f2ab60e1e7c372cec485337ab5eb6) written by the developers of the GTPSA library for more details.

These generalizations, paired with an efficient monomial indexing function, make GTPSA very fast. See the `benchmark/fodo.jl` example for comparison of `GTPSA.jl` with `ForwardDiff.jl` and `TaylorSeries.jl` in computing all coefficients of a Taylor map 2nd order in 4 variables and 2 parameters. `TaylorDiff.jl` does not allow for trivial computation of all the individual coefficients in a multivariable Taylor series, and so a comparison with this package is not included.

## Setup
To use `GTPSA.jl`, in the Julia REPL run

```
] add https://github.com/bmad-sim/GTPSA.jl.git
```

For developers,

```
] dev https://github.com/bmad-sim/GTPSA.jl.git
```

## Basic Usage
First, a `Descriptor` must be created specifying the number of variables, number of parameters, the orders of each variable, and the orders of each parameter for the TPSA(s). The `Descriptor` stores all of the monomial indexing/lookup information for TPSAs, based on these values. A `TPSA` or `ComplexTPSA` can then be created based on the `Descriptor`. TPSAs can be manipulated using all of the elementary math operators (`+`,`-`,`*`,`/`,`^`) and basic math functions (e.g. `abs`, `sqrt`, `sin`, `exp`, `log`, `coth`, etc.).

TPSAs can be viewed as structures containing the coefficients for all of the monomials of a multivariable Taylor expansion up to the orders specified in the `Descriptor`. Therefore, for a TPSA to represent some variable in the function, the first-order coefficient for that variable in the Taylor expansiion must be set to 1. For example, to compute the power series of a function $f(x_1) = x_1^2\frac{\sin{(2+x_1)}}{\exp{[(1+x_1)^{-1}]}}$ up to 15th order:

```
using GTPSA

# Define the Descriptor for the TPSAs
d = Descriptor(1, 15)

# Create a TPSA based on the Descriptor
x1 = TPSA(d)

# Set the first-order coefficient of the TPSA (index by order) so it equals 1*x1
x1[1] = 1

# Manipulate the TPSAs as you would any other mathematical variable in Julia
f = x1^2*sin(2+x1)/exp((1+x1)^-1)
```

`f` itself is a TPSA. Note that scalars do not need to be defined as TPSAs when writing expressions. Running `print(f)` then gives the output

```
         :  R, NV =   1, MO = 15
 *******************************************************
     I   COEFFICIENT             ORDER   EXPONENTS
     1   3.3451182923926226E-01    2     2
     2   1.8141996356503595E-01    3     3
     3  -4.8760369491348854E-01    4     4
     4  -9.4426992969365992E-03    5     5
     5   1.1150394307975423E-01    6     6
     6  -8.7314614604415114E-02    7     7
     7   8.2968303215296232E-02    8     8
     8  -7.4445976247838025E-02    9     9
     9   5.9713679541442431E-02   10     10
    10  -4.2660311388393345E-02   11     11
    11   2.5430250837118938E-02   12     12
    12  -9.3821808135966887E-03   13     13
    13  -4.6081926391356139E-03   14     14
    14   1.6049422765485353E-02   15     15
```
This print function will be rewritten.

For multivariable TPSAs including variables/parameters with different orders, and complex TPSAs, see [the documentation](https://bmad-sim.github.io/GTPSA.jl/).

## Acknowledgements
We thank Laurent Deniau, the creator of GTPSA, for very detailed and lengthy discussions on using his C library. 
