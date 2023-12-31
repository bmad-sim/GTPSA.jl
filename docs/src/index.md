# GTPSA.jl

*A Julia interface to the Generalised Truncated Power Series Algebra (GTPSA) library in MAD.*

This package provides a full-featured Julia interface to the [Generalised Truncated Power Series Algebra (GTPSA) library](https://github.com/MethodicalAcceleratorDesign/MAD-NG), which computes Taylor expansions, or Truncated Power Series (TPSs) of real and complex multivariable functions to arbitrary orders in the variables. 

GTPSA, which uses the Truncated Power Series Algebra method for performing automatic differentation (AD), has several advantages over current Julia AD packages:

1. **Speed and Accuracy**: Because the TPSA method does not take any symbolic derivatives, nor use finite differencing, derivatives are calculated with high efficiency and accuracy
2. **Arbitrary Orders in Individual Variables**: For example, computing the Taylor expansion of $f(x_1,x_2)$ to 5th order in $x_1$ and 1st order in $x_2$ is done trivially in GTPSA
3. **All Taylor Coefficients Stored**: GTPSA implements an efficient monomial coefficient indexing function for high speed even with TPSs having large number of variables to high orders
4. **Distinction Between "Variables" and "Parameters"**: When the TPS represents a *Taylor map* of a dynamical system, which defines the evolution of map *variables* given some variations in map *parameters*, distinguishing between the two proves advantageous for analyses of Taylor maps, such as normal form


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
First, a `Descriptor` must be created specifying the number of variables, number of parameters, the orders of each variable, and the orders of each parameter for the TPSA. The `Descriptor` stores all of the monomial indexing/lookup information for TPSs in the TPSA, based on these values. A `TPS` or `ComplexTPS` can then be created based on the `Descriptor`. TPSs can be manipulated using all of the elementary math operators (`+`,`-`,`*`,`/`,`^`) and math functions (e.g. `abs`, `sqrt`, `sin`, `exp`, `log`, `coth`, etc.).

TPSs can be viewed as structures containing the coefficients for all of the monomials of a multivariable Taylor expansion up to the orders specified in the `Descriptor`. As an example, to compute the truncated power series of a function $f(x_1,x_2) = x_1^2\frac{\sin{(2+x_1x_2)}}{\exp{(x_1+x_2)}}$ to 15th order in $x_1$ and $x_2$:
```
using GTPSA

# Define the Descriptor for the TPSA
d = Descriptor(2, 15)

# Get the TPSs corresponding to each variable based on the Descriptor
x = vars(d)

# Manipulate the TPSs as you would any other mathematical variable in Julia
f = x[1]^2*sin(2+x[1]*x[2])/exp(x[1]+x[2])
```

`f` itself is a TPS. Note that scalars do not need to be defined as TPSs when writing expressions. Running `print(f)` then gives the output

```
         :  R, NV =   2, MO = 15
 *******************************************************
     I   COEFFICIENT             ORDER   EXPONENTS
     1   9.0929742682568171E-01    2     2 0
     2   0.0000000000000000E+00    2     1 1
     3   0.0000000000000000E+00    2     0 2
     4  -9.0929742682568171E-01    3     3 0
     5  -9.0929742682568171E-01    3     2 1
     6   0.0000000000000000E+00    3     1 2
     7   0.0000000000000000E+00    3     0 3
     8   4.5464871341284085E-01    4     4 0
     9   4.9315059027853930E-01    4     3 1
    10   4.5464871341284085E-01    4     2 2
    11   0.0000000000000000E+00    4     1 3
    12   0.0000000000000000E+00    4     0 4
    13  -1.5154957113761358E-01    5     5 0
    14  -3.8501876865698448E-02    5     4 1
                    ...
```
This print function will be rewritten.

For multivariable TPSs including variables/parameters with different individual orders, and complex TPSs, see [Usage](@ref).

Advanced users are referred to [this paper](https://inspirehep.net/files/286f2ab60e1e7c372cec485337ab5eb6) written by the developers of the GTPSA library for more details.

## Acknowledgements
We thank Laurent Deniau, the creator of GTPSA, for very detailed and lengthy discussions on using his C library. 
