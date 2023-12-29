# GTPSA.jl
[![Build Status](https://github.com/bmad-sim/GTPSA.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/bmad-sim/GTPSA.jl/actions/workflows/CI.yml?query=branch%3Amain)

This package provides a full-featured Julia interface to the [Generalised Truncated Power Series Algebra (GTPSA) library](https://github.com/MethodicalAcceleratorDesign/MAD-NG), which computes Taylor expansions, or Truncated Power Series (TPSs) of real and complex multivariable functions to arbitrary orders in the variables. 

GTPSA, which uses the Truncated Power Series Algebra method for performing automatic differentation (AD), has several advantages over current Julia AD packages:

1. **Speed and Accuracy**: Because the TPSA method does not take any symbolic derivatives, nor use finite differencing, derivatives are calculated with high efficiency and accuracy
2. **Arbitrary Orders in Individual Variables**: For example, computing the Taylor expansion of $f(x_1,x_2)$ to 5th order in $x_1$ and 1st order in $x_2$ is done trivially in GTPSA
3. **All Taylor Coefficients Stored**: GTPSA implements an efficient monomial coefficient indexing function for high speed even with TPSs having large number of variables to high orders
4. **Distinction Between "Variables" and "Parameters"**: When the TPS represents a *Taylor map* of a dynamical system, which defines the evolution of map *variables* given some variations in map *parameters*, distinguishing between the two allows for substantial computational benefits


**GTPSA is fast!** See the `benchmark/taylormap.jl` example for a speed comparison of `GTPSA.jl` with `ForwardDiff.jl` and `TaylorSeries.jl` in calculating a multivariable Taylor map to 2nd order.

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
First, a `Descriptor` must be created specifying the number of variables, number of parameters, the orders of each variable, and the orders of each parameter for the TPSA. The `Descriptor` stores all of the monomial indexing/lookup information for TPSs in the TPSA, based on these values. A `TPS` or `ComplexTPS` can then be created based on the `Descriptor`. TPSs can be manipulated using all of the elementary math operators (`+`,`-`,`*`,`/`,`^`) and basic math functions (e.g. `abs`, `sqrt`, `sin`, `exp`, `log`, `coth`, etc.).

TPSs can be viewed as structures containing the coefficients for all of the monomials of a multivariable Taylor expansion up to the orders specified in the `Descriptor`. Therefore, for a TPS to represent some variable in the function, the first-order coefficient for that variable in the Taylor expansion must be set to 1. For example, to compute the power series of a function $f(x_1) = x_1^2\frac{\sin{(2+x_1)}}{\exp{[(1+x_1)^{-1}]}}$ up to 15th order:

```
using GTPSA

# Define the Descriptor for the TPSA
d = Descriptor(1, 15)

# Create a TPS based on the Descriptor
x1 = TPS(d)

# Set the first-order coefficient of the TPS (index by order) so it equals 1*x1
x1[1] = 1

# Manipulate the TPSs as you would any other mathematical variable in Julia
f = x1^2*sin(2+x1)/exp((1+x1)^-1)
```

`f` itself is a TPS. Note that scalars do not need to be defined as TPSs when writing expressions. Running `print(f)` then gives the output

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

For multivariable TPSs including variables/parameters with different orders, and complex TPSs, see [Usage](@ref).

Advanced users are referred to [this paper](https://inspirehep.net/files/286f2ab60e1e7c372cec485337ab5eb6) written by the developers of the GTPSA library for more details.

## Acknowledgements
We thank Laurent Deniau, the creator of GTPSA, for very detailed and lengthy discussions on using his C library. 
