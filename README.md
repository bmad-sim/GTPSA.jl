# GTPSA.jl
[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://bmad-sim.github.io/GTPSA.jl/stable/)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://bmad-sim.github.io/GTPSA.jl/dev/)
[![Build Status](https://github.com/bmad-sim/GTPSA.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/bmad-sim/GTPSA.jl/actions/workflows/CI.yml?query=branch%3Amain)

## Overview

This package provides a full-featured Julia interface to the [Generalised Truncated Power Series Algebra (GTPSA) library](https://mad.web.cern.ch/mad/releases/madng/html/mad_mod_diffalg.html), which computes Taylor expansions, or Truncated Power Series (TPSs) of real and complex multivariable functions to arbitrary orders. 

Truncated Power Series Algebra (TPSA) performs forward-mode automatic differentation (AD) similar to the dual-number implementation of `ForwardDiff.jl`. However, instead of nesting derivatives for higher orders, TPSA naturally extends to arbitary orders by directly using the power series expansions. This, paired with a highly optimized monomial indexing function/storage for propagating the partial derivatives, makes `GTPSA.jl` significantly faster for 2nd-order calculations and above (for 1st-order
calculations the performance is similar to `ForwardDiff.jl`).
See the [`benchmark/track.jl`](https://github.com/bmad-sim/GTPSA.jl/blob/main/benchmark/track.jl) example for a speed comparison of `GTPSA.jl` with `ForwardDiff.jl` in calculating the partial derivatives for a system with 58 inputs and 6 outputs. **In this example, GTPSA was nearly x3 faster than ForwardDiff to 2nd order, and x15 faster to 3rd order.**

GTPSA provides several advantages over current Julia AD packages:

1. **Speed**: `GTPSA.jl` is significantly faster than `ForwardDiff.jl` for 2nd-order calculations and above, and has similar performance at 1st-order
2. **Custom Orders in Individual Variables**: Other packages use a single maximum order for all variables. With GTPSA, the
maximum order can be set differently for different variables. For example, computing the Taylor expansion of $f(x_1,x_2)$ to 2nd order in $x_1$ and 6th order in $x_2$ is possible
3. **Complex Numbers**: GTPSA natively supports complex numbers and allows for mixing of complex and real truncated power series
4. **Distinction Between State Variables and Parameters**: Distinguishing between dependent variables and parameters in the solution of a differential equation expressed as a power series in the dependent variables/parameters can be advantageous in analysis

## Setup
To use `GTPSA.jl`, in the Julia REPL run

```julia
import Pkg; Pkg.add("GTPSA")
```

## Basic Usage
First, a `Descriptor` must be created specifying the number of variables, number of parameters, and truncation order for each variable/parameter in the TPSA. A `TPS` or `ComplexTPS` can then be created based on the `Descriptor`. TPSs can be manipulated using all of the arithmetic operators (`+`,`-`,`*`,`/`,`^`) and math functions (e.g. `abs`, `sqrt`, `sin`, `exp`, `log`, `tanh`, etc.).

TPSs can be viewed as structures containing the coefficients for all of the monomials of a multivariable Taylor expansion up to the orders specified in the `Descriptor`. As an example, to compute the truncated power series of a function $f(x_1, x_2) = \cos{(x_1)}+i\sin{(x_2)}$ to 6th order in $x_1$ and $x_2$:
```julia
using GTPSA

# Descriptor for TPSA with 2 variables to 6th order
d = Descriptor(2, 6)

# Get the TPSs corresponding to each variable based on the Descriptor
x = vars()
# x[1] corresponds to the first variable and x[2] corresponds to the second variable

# Manipulate the TPSs as you would any other mathematical variable in Julia
f = cos(x[1]) + im*sin(x[2])
# This creates a new TPS called f
```

Note that scalars do not need to be defined as TPSs when writing expressions. Running `print(f)` gives the output

```
ComplexTPS:
 Real                     Imag                       Order   Exponent
  1.0000000000000000e+00   0.0000000000000000e+00      0      0   0
  0.0000000000000000e+00   1.0000000000000000e+00      1      0   1
 -5.0000000000000000e-01   0.0000000000000000e+00      2      2   0
  0.0000000000000000e+00  -1.6666666666666666e-01      3      0   3
  4.1666666666666664e-02   0.0000000000000000e+00      4      4   0
  0.0000000000000000e+00   8.3333333333333332e-03      5      0   5
 -1.3888888888888887e-03   0.0000000000000000e+00      6      6   0
```

For more details, including using TPSs with differing truncation orders for each variable, see the [GTPSA documentation](https://bmad-sim.github.io/GTPSA.jl/).

## Acknowledgements
Much thanks must be given to Laurent Deniau, the creator of the C GTPSA library, for his time and great patience in explaining his code. 

Advanced users are referred to [this paper](https://inspirehep.net/files/286f2ab60e1e7c372cec485337ab5eb6) discussing
the inner workings of the C GTPSA library.
