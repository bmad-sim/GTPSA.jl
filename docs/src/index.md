# GTPSA.jl

*A full-featured Julia interface to the Generalised Truncated Power Series Algebra library.*

This package provides a full-featured Julia interface to the [Generalised Truncated Power Series Algebra (GTPSA) library](https://mad.web.cern.ch/mad/releases/madng/html/mad_mod_diffalg.html), which computes Taylor expansions, or Truncated Power Series (TPSs) of real and complex multivariable functions to arbitrary orders in the variables. 

Truncated Power Series Algebra (TPSA) performs forward-mode automatic differentation (AD) similar to the dual-number implementation of `ForwardDiff.jl`. However, instead of nesting derivatives for higher orders, TPSA naturally extends to arbitary orders by directly using the power series expansions. This, paired with a fast monomial indexing function for propagating the partial derivatives, gives `GTPSA.jl` similar performance as `ForwardDiff.jl` to 1st-order, but significantly faster performance for 2nd-order calculations and above.

GTPSA provides several advantages over current Julia AD packages:

1. **Speed**: By directly using the power series expansions, `GTPSA.jl` is significantly faster than `ForwardDiff.jl` for 2nd-order calculations and above, and has similar performance at 1st-order
2. **Custom Orders in Individual Variables**: For example, computing the Taylor expansion of $f(x_1,x_2)$ to 2nd order in $x_1$ and 6th order in $x_2$ is done trivially in GTPSA
3. **Complex Numbers**: GTPSA natively supports complex numbers, and allows for mixing of complex and real truncated power series
4. **All Taylor Coefficients Stored**: GTPSA implements an efficient monomial coefficient indexing function for high speed even with TPSs having a large number of variables to high orders
5. **Distinction Between State Variables and Parameters**: Distinguishing between dependent variables and parameters in the solution of a differential equation expressed as a power series in the dependent variables/parameters is very advantageous in analysis
6. **Fast JIT Compilation**: Though powerful, GTPSA is a lightweight package with a fast JIT compilation, easing iterative REPL development 
7. **Long-Term Maintenance**: The underlying GTPSA C library will be used in the accelerator physics software [MAD-NG](https://github.com/MethodicalAcceleratorDesign/MAD-NG) being developed at CERN and this Julia interface will be used in [Bmad-Julia](https://github.com/bmad-sim) being developed at Cornell University

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
First, a `Descriptor` must be created specifying the number of variables, number of parameters, and truncation order for each variable/parameter in the TPSA. A `TPS` or `ComplexTPS` can then be created based on the `Descriptor`. TPSs can be manipulated using all of the arithmetic operators (`+`,`-`,`*`,`/`,`^`) and math functions (e.g. `abs`, `sqrt`, `sin`, `exp`, `log`, `tanh`, etc.).

TPSs can be viewed as structures containing the coefficients for all of the monomials of a multivariable Taylor expansion up to the orders specified in the `Descriptor`. As an example, to compute the truncated power series of a function $f(x_1, x_2) = \cos{(x_1)}+i\sin{(x_2)}$ to 6th order in $x_1$ and $x_2$:
```@example
using GTPSA

# Descriptor for TPSA with 2 variables to 6th order
d = Descriptor(2, 6);

# Get the TPSs corresponding to each variable based on the Descriptor
x = vars(d);

# Manipulate the TPSs as you would any other mathematical variable in Julia
f = cos(x[1]) + 1im*sin(x[2])
```

Advanced users are referred to [this paper](https://inspirehep.net/files/286f2ab60e1e7c372cec485337ab5eb6) written by the developers of the GTPSA library for more details.

## Acknowledgements
We thank Laurent Deniau, the creator of GTPSA, for very detailed and lengthy discussions on using his C library. 
