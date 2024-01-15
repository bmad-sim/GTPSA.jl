# GTPSA.jl
[![Build Status](https://github.com/bmad-sim/GTPSA.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/bmad-sim/GTPSA.jl/actions/workflows/CI.yml?query=branch%3Amain)

This package provides a full-featured Julia interface to the [Generalised Truncated Power Series Algebra (GTPSA) library](https://github.com/MethodicalAcceleratorDesign/MAD-NG), which computes Taylor expansions, or Truncated Power Series (TPSs) of real and complex multivariable functions to arbitrary orders in the variables. 

Truncated Power Series Algebra (TPSA) performs forward-mode automatic differentation (AD) similar to the dual-number implementation of `ForwardDiff.jl`. However, instead of nesting derivatives for higher orders, TPSA naturally extends to arbitary orders by directly using the power series expansions. This, paired with a fast monomial indexing function for propagating the partial derivatives, gives `GTPSA.jl` nearly the same performance as `ForwardDiff.jl` to 1st-order, but significantly faster performance for 2nd-order calculations and above.

GTPSA provides several advantages over current Julia AD packages:

1. **Speed**: By directly using the power series expansions, `GTPSA.jl` is significantly faster than `ForwardDiff.jl` for 2nd-order and above, and has the nearly the same performance as `ForwardDiff.jl` for 1st-order
2. **Custom Orders in Individual Variables**: For example, computing the Taylor expansion of $f(x_1,x_2)$ to 2nd order in $x_1$ and 6th order in $x_2$ is done trivially in GTPSA
3. **Complex Numbers**: GTPSA natively supports complex numbers, and allows for mixing of complex and real truncated power series
4. **All Taylor Coefficients Stored**: GTPSA implements an efficient partial derivative indexing function for high speed even with TPSs having a large number of variables to high orders
5. **Distinction Between "Variables" and "Parameters"**: When the TPS represents a *Taylor map* of a dynamical system, which defines the evolution of map *variables* given some variations in map *parameters*, distinguishing between the two can be very advantageous for analyses of Taylor maps, such as normal form analysis
6. **Fast JIT Compilation**: Though powerful, GTPSA is a lightweight package with a fast JIT compilation, easing iterative REPL development 

See the `benchmark/taylormap.jl` example for a speed comparison of `GTPSA.jl` with `ForwardDiff.jl` in calculating the partial derivatives for a system with 56 inputs and 4 outputs. **We observed GTPSA to be nearly x4 faster than ForwardDiff to 2nd order, and x19 faster to 3rd order in our example.**

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
First, a `Descriptor` must be created specifying the number of variables and truncation order for each variable in the TPSA. A `TPS` or `ComplexTPS` can then be created based on the `Descriptor`. TPSs can be manipulated using all of the arithmetic operators (`+`,`-`,`*`,`/`,`^`) and math functions (e.g. `abs`, `sqrt`, `sin`, `exp`, `log`, `tanh`, etc.).

TPSs can be viewed as structures containing the coefficients for all of the monomials of a multivariable Taylor expansion up to the orders specified in the `Descriptor`. As an example, to compute the truncated power series of a function $f(x_1, x_2) = \cos{(x_1)}+i\sin{(x_2)}$ to 6th order in $x_1$ and $x_2$:
```
using GTPSA

# Descriptor for TPSA with 2 variables to 6th order
d = Descriptor(2, 6)

# Get the TPSs corresponding to each variable based on the Descriptor
x = vars(d)

# Manipulate the TPSs as you would any other mathematical variable in Julia
f = cos(x[1]) + im*sin(x[2])
```

`f` itself is a TPS. Note that scalars do not need to be defined as TPSs when writing expressions. Running `print(f)` then gives the output

```
ComplexTPS:
  REAL                      IMAG                      ORDER    EXPONENTS
   1.0000000000000000e+00    0.0000000000000000e+00    0        0    0
   0.0000000000000000e+00    1.0000000000000000e+00    1        0    1
  -5.0000000000000000e-01    0.0000000000000000e+00    2        2    0
   0.0000000000000000e+00   -1.6666666666666666e-01    3        0    3
   4.1666666666666664e-02    0.0000000000000000e+00    4        4    0
   0.0000000000000000e+00    8.3333333333333332e-03    5        0    5
  -1.3888888888888887e-03    0.0000000000000000e+00    6        6    0
```

For more details, including TPSs with custom orders in individual variables, see [the documentation](https://bmad-sim.github.io/GTPSA.jl/).

Advanced users are referred to [this paper](https://inspirehep.net/files/286f2ab60e1e7c372cec485337ab5eb6) written by the developers of the GTPSA library for more details.

## Acknowledgements
We thank Laurent Deniau, the creator of GTPSA, for very detailed and lengthy discussions on using his C library. 
