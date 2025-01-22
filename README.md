# GTPSA.jl
[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://bmad-sim.github.io/GTPSA.jl/stable/)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://bmad-sim.github.io/GTPSA.jl/dev/)
[![Build Status](https://github.com/bmad-sim/GTPSA.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/bmad-sim/GTPSA.jl/actions/workflows/CI.yml?query=branch%3Amain)

*A full-featured Julia interface to the Generalised Truncated Power Series Algebra library.*

`GTPSA.jl` is a full-featured Julia interface to the [Generalised Truncated Power Series Algebra (GTPSA) library](https://madx.web.cern.ch/releases/madng/html/mad_mod_diffalg.html), which computes Taylor expansions, or Truncated Power Series (TPSs), of real and complex multivariable functions to high orders.

Truncated Power Series Algebra (TPSA) performs forward-mode automatic differentation (AD) similar to the typical dual-number implementation, as in `ForwardDiff.jl`. However, instead of nesting derivatives for higher orders, TPSA naturally extends to arbitary orders by directly using the power series expansions. Furthermore, because TPSA is designed for high order AD, the storage, indexing, and propagation of all nonzero partial derivatives has been highly optimized.

`GTPSA.jl` provides several unique features and functionalities:

1. **High Order AD Speed:** `GTPSA.jl` is significantly faster than `ForwardDiff.jl` for 2nd-order calculations and above, and has very similar performance at 1st-order. See [our example](https://github.com/bmad-sim/GTPSA.jl/blob/main/benchmark/track.jl) where **GTPSA was x3.5 faster than ForwardDiff to 2nd order, and x19.8 faster to 3rd order.**

2. **Easy Monomial Indexing:** Beyond 2nd order, accessing/manipulating the partial derivatives in an organized way can be a significant challenge. GTPSA provides three simple indexing schemes for getting/setting monomial coefficients in a truncated power series, as well as a `cycle!` function for cycling through all nonzero monomials.

3. **Easy `TPS` slicing:** Suppose you would like to get all terms in a multivariable truncated power series proportional to a specific variable; `GTPSA.jl` provides a simple syntax similar to array slicing to extract parts of a `TPS`.

4. **Complex Numbers:** `GTPSA.jl` natively supports complex numbers and allows for mixing of complex and real truncated power series.

5. **Distinction Between State Variables and Parameters:** Distinguishing between dependent variables and parameters in the solution of a differential equation expressed as a power series in the dependent variables/parameters can be advantageous in analysis.

6. **Different Orders in Variables/Parameters:** The truncation order can be set differently for the variables and parameters. For example, computing the expansion of a function $f(x_1,k_1)$ to 6th order in $\Delta x_1$, but ignoring terms $O(\Delta k_1^2)$, is possible in GTPSA.


`GTPSA.jl` also includes useful truncated power series operations such as evaluation, composition, translation, and inversion.

## Setup
To use `GTPSA.jl`, in the Julia REPL run

```julia
import Pkg; Pkg.add("GTPSA")
```

## Basic Usage
```julia-repl
julia> using GTPSA

julia> d = Descriptor(2, 6); # 2 variables to 6th order

julia> Δx = @vars(d)  # Get truncated power series (TPSs) corresponding to the variables

julia> f = cos(Δx[1]) + im*sin(Δx[2]) # Manipulate TPSs as you would any other number
ComplexTPS64{Descriptor(NV=2, MO=6)}:
 Real                     Imag                       Order   Exponent
  1.0000000000000000e+00   0.0000000000000000e+00      0      0   0
  0.0000000000000000e+00   1.0000000000000000e+00      1      0   1
 -5.0000000000000000e-01   0.0000000000000000e+00      2      2   0
  0.0000000000000000e+00  -1.6666666666666666e-01      3      0   3
  4.1666666666666664e-02   0.0000000000000000e+00      4      4   0
  0.0000000000000000e+00   8.3333333333333332e-03      5      0   5
 -1.3888888888888887e-03   0.0000000000000000e+00      6      6   0
```

The GTPSA library currently only supports truncated power series representing `Float64` and `ComplexF64` number types.

## Acknowledgements
Much thanks must be given to Laurent Deniau, the creator of the C GTPSA library, for his time and great patience in explaining his code. 

Advanced users are referred to [this paper](https://inspirehep.net/files/286f2ab60e1e7c372cec485337ab5eb6) discussing the inner workings of the C GTPSA library.
