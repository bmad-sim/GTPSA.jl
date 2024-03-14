# [Global Variables] (@id global)
The following non-constant global variables can be set:

```julia
desc_current::Descriptor     # Current Descriptor to use
show_eps::Float64 =  0.0     # Print epsilon
show_sparse::Bool = false    # Use sparse monomial print
show_header::Bool = false    # Print a header above each TPS
```

**`desc_current`** defines the `Descriptor` to use when that information is not explicitly (or implicitly in a TPS copy constructor) available, e.g. when calling `TPS(a)` where `a` is not a `TPS`. This also allows one to use general `Number` commands like `convert(TPS, a)` and `zeros(TPS, 6)` 

**`show_eps`** defines the value below which the absolute value of a monomial coefficient is NOT printed

**`show_sparse`** specifies whether the sparse monomial format is used for printing. This is useful for GTPSAs containing a large number of variables and parameters

**`show_header`** specifies whether or not to print the GTPSA `Descriptor` information above each TPS output

-----

Global variables can be get/set using either the standard syntax (e.g. `GTPSA.show_header = true`), or using the helper functions `getGTPSA` and `setGTPSA!`.

## Examples

```@repl
using GTPSA; GTPSA.show_sparse = false; GTPSA.show_header=false; #hide
d1 = Descriptor(1, 6);
x = vars()
GTPSA.show_sparse = true;
x
setGTPSA!("show_sparse", false);
getGTPSA("show_sparse")
x
```
