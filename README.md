# GTPSA.jl
[![Build Status](https://github.com/bmad-sim/GTPSA.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/bmad-sim/GTPSA.jl/actions/workflows/CI.yml?query=branch%3Amain)

This package provides a full-featured Julia interface to the [Generalised Truncated Power Series Algebra (GTPSA) library](https://github.com/MethodicalAcceleratorDesign/MAD-NG), which computes Taylor series expansions of multivariable functions to arbitrary orders in each of the variables, chosen by the user. We refer advanced users/developers to [this paper](https://inspirehep.net/files/286f2ab60e1e7c372cec485337ab5eb6) written by the developers of the GTPSA library for more details.

GTPSA is extremely fast, and memory efficient. See the `benchmark/fodo.jl` example for comparison of GTPSA.jl with ForwardDiff.jl in computing a Taylor map 2nd order in 4 map variables and 2 knob variables. 

## Installation
To use GTPSA.jl, in the Julia REPL simply run

```
] add https://github.com/bmad-sim/GTPSA.jl.git
```

For developers,

```
] dev https://github.com/bmad-sim/GTPSA.jl.git
```



