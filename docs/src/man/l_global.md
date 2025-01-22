# [Global Variables](@id global)
The following non-constant global variables can be set:

```julia
desc_current::Descriptor     # Descriptor to use when dynamic Descriptor resolution cannot infer
show_eps::Float64 =  0.0     # Print epsilon
show_sparse::Bool = false    # Use sparse monomial print
```

**`desc_current`** defines the `Descriptor` to use when that information is not explicitly (or implicitly in a TPS copy constructor) available with dynamic `Descriptor` resolution, e.g. when calling `TPS(a)` where `a` is not a `TPS`

**`show_eps`** defines the value below which the absolute value of a monomial coefficient is NOT printed

**`show_sparse`** specifies whether the sparse monomial format is used for printing. This is useful for GTPSAs containing a large number of variables and parameters

-----

## Examples

```@repl
using GTPSA; GTPSA.show_sparse = false; #hide
GTPSA.desc_current
d1 = Descriptor(1, 6);
GTPSA.desc_current
x = @vars()
GTPSA.show_sparse = true;
x
GTPSA.show_sparse = false;
GTPSA.show_sparse
x
```
