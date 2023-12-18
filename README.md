# GTPSA.jl
[![Build Status](https://github.com/bmad-sim/GTPSA.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/bmad-sim/GTPSA.jl/actions/workflows/CI.yml?query=branch%3Amain)

This package provides a full-featured Julia interface to the [Generalised Truncated Power Series Algebra (GTPSA) library](https://github.com/MethodicalAcceleratorDesign/MAD-NG), which computes Taylor expansions of real and complex multivariable functions to arbitrary orders in each of the variables and function parameters individually, chosen by the user. GTPSA also allows distinction between variables $x_i$ and parameters $k_j$ in the function such that $\partial x_i/\partial k_j \neq 0$ but $\partial k_j/\partial x_i = 0$. We refer advanced users to [this paper](https://inspirehep.net/files/286f2ab60e1e7c372cec485337ab5eb6) written by the developers of the GTPSA library for more details.

These generalizations, paired with an efficient monomial indexing function, make GTPSA very fast and memory efficient. See the `benchmark/fodo.jl` example for comparison of `GTPSA.jl` with `ForwardDiff.jl` and `TaylorSeries.jl` in computing all coefficients of a Taylor map 2nd order in 4 variables and 2 parameters. `TaylorDiff.jl` does not allow for trivial computation of all the individual coefficients in a multivariable Taylor series, and so a comparison with this package is not included.

## Installation
To use GTPSA.jl, in the Julia REPL simply run

```
] add https://github.com/bmad-sim/GTPSA.jl.git
```

For developers,

```
] dev https://github.com/bmad-sim/GTPSA.jl.git
```

## Basic Usage
First, a `Descriptor` must be created specifying the number of variables, number of parameters, the orders of each variable, and the orders of each parameter for the TPSA(s). A `TPSA` or `ComplexTPSA` can then be created based on the descriptor. TPSAs can be manipulated using all of the elementary math operators (`+`,`-`,`*`,`/`,`^`) and basic math functions (e.g. `abs`, `sqrt`, `sin`, `coth`, etc.). For example, to compute the power series of a function $f$ to 12th order in 2 variables,

```
using GTPSA

# Define the Descriptor for the TPSAs
d = Descriptor(2, 12)

# Create new TPSAs from the Descriptor, with all monomials set to 0 initially
x1 = TPSA(d)
x2 = TPSA(d)

# Set the TPSAs so they correspond to the variables x1 and x2
# Indexes are orders of corresponding variable
x1[1,0] = 1
x2[0,1] = 1


# Manipulate the TPSAs as you would any other variables in Julia
f = sin(5+x1)*cos(x2)
```

`f` itself is a TPSA. Note that scalars do not need to be defined as TPSAs when writing expressions. Running `print(f)` then gives the output

```
         :  R, NV =   2, MO = 12
 *******************************************************
     I   COEFFICIENT             ORDER   EXPONENTS
     1  -9.5892427466313845E-01    0     0 0
     2   2.8366218546322625E-01    1     1 0
     3   0.0000000000000000E+00    1     0 1
     4   4.7946213733156923E-01    2     2 0
     5   0.0000000000000000E+00    2     1 1
     6   4.7946213733156923E-01    2     0 2
     7  -4.7277030910537705E-02    3     3 0
     8   0.0000000000000000E+00    3     2 1
     9  -1.4183109273161312E-01    3     1 2

                  ...
```
This print function will be rewritten.

For creating more detailed TPSAs, see [the documentation](https://bmad-sim.github.io/GTPSA.jl/).

## Acknowledgements
We'd like to thank Laurent Deniau, the creator of GTPSA, for very detailed and lengthy discussions on using his C library. 
