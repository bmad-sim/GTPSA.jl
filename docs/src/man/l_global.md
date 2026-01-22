# [Global Variables](@id global)
The following non-constant global variables can be set:

```julia
desc_current::Descriptor     # Descriptor to use when dynamic Descriptor resolution cannot infer
show_eps::Float64 =  0.0     # Print epsilon
```

**`desc_current`** defines the `Descriptor` to use when that information is not explicitly (or implicitly in a TPS copy constructor) available with dynamic `Descriptor` resolution, e.g. when calling `TPS(a)` where `a` is not a `TPS`

**`show_eps`** defines the value below which the absolute value of a monomial coefficient is NOT printed. See the [I/O page of the manual](@ref io)

-----

## Examples

```@repl
using GTPSA;  #hide
GTPSA.desc_current
d6 = Descriptor(1, 6);
GTPSA.desc_current
```
