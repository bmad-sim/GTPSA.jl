# GTPSA.jl
[![Build Status](https://github.com/bmad-sim/GTPSA.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/bmad-sim/GTPSA.jl/actions/workflows/CI.yml?query=branch%3Amain)

This package provides a full-featured Julia interface to the [Generalised Truncated Power Series Algebra (GTPSA) library](https://github.com/MethodicalAcceleratorDesign/MAD-NG), which computes Taylor expansions of real and complex multivariable functions to arbitrary orders in each of the variables and function parameters individually, chosen by the user. GTPSA also allows distinction between variables $x_i$ and parameters $k_j$ in the function such that $\partial x_i/\partial k_j \neq 0$ but $\partial k_j/\partial x_i = 0$. We refer advanced users to [this paper](https://inspirehep.net/files/286f2ab60e1e7c372cec485337ab5eb6) written by the developers of the GTPSA library for more details.

These generalizations, paired with an efficient monomial indexing function, make GTPSA very fast and memory efficient. See the `benchmark/fodo.jl` example for comparison of GTPSA.jl with ForwardDiff.jl in computing a Taylor map 2nd order in 4 variables and 2 parameters.

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
# Define the TPSAs
d = Descriptor(2, 12)
x1 = TPSA(d)
x2 = TPSA(d)

# Set the TPSAs so they correspond to the variables x1 and x2 
x1[1] = 1
x2[2] = 1

f = sin(x1)*cos(x2)
```

Running `print(f)` then gives the output

```
         :  R, NV =   2, MO = 12
 *******************************************************
     I   COEFFICIENT             ORDER   EXPONENTS
     1   1.0000000000000000E+00    1     1 0
     2   0.0000000000000000E+00    1     0 1
     3  -1.6666666666666666E-01    3     3 0
     4   0.0000000000000000E+00    3     2 1
     5  -5.0000000000000000E-01    3     1 2
     6   0.0000000000000000E+00    3     0 3
     7   8.3333333333333332E-03    5     5 0
     8   0.0000000000000000E+00    5     4 1
     9   8.3333333333333329E-02    5     3 2
    10   0.0000000000000000E+00    5     2 3
    11   4.1666666666666664E-02    5     1 4
    12   0.0000000000000000E+00    5     0 5
    13  -1.9841269841269841E-04    7     7 0
    14   0.0000000000000000E+00    7     6 1
    15  -4.1666666666666666E-03    7     5 2
    16   0.0000000000000000E+00    7     4 3
    17  -6.9444444444444441E-03    7     3 4
    18   0.0000000000000000E+00    7     2 5
    19  -1.3888888888888887E-03    7     1 6
    20   0.0000000000000000E+00    7     0 7
    21   2.7557319223985893E-06    9     9 0
    22   0.0000000000000000E+00    9     8 1
    23   9.9206349206349206E-05    9     7 2
    24   0.0000000000000000E+00    9     6 3
    25   3.4722222222222218E-04    9     5 4
    26   0.0000000000000000E+00    9     4 5
    27   2.3148148148148144E-04    9     3 6
    28   0.0000000000000000E+00    9     2 7
    29   2.4801587301587298E-05    9     1 8
    30   0.0000000000000000E+00    9     0 9
    31  -2.5052108385441720E-08   11     11 0
    32   0.0000000000000000E+00   11     10 1
    33  -1.3778659611992946E-06   11     9 2
    34   0.0000000000000000E+00   11     8 3
    35  -8.2671957671957661E-06   11     7 4
    36   0.0000000000000000E+00   11     6 5
    37  -1.1574074074074072E-05   11     5 6
    38   0.0000000000000000E+00   11     4 7
    39  -4.1335978835978830E-06   11     3 8
    40   0.0000000000000000E+00   11     2 9
    41  -2.7557319223985888E-07   11     1 10
    42   0.0000000000000000E+00   11     0 11
```


