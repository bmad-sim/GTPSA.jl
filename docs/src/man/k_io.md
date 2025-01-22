# [I/O](@id io)
## Global Variables
There are two non-constant global variables which can be set to customize the printed output of TPSs:

```julia
show_eps::Float64 =  0.0     # Print epsilon
show_sparse::Bool = false    # Use sparse monomial print
```

**`show_eps`** defines the value below which the absolute value of a monomial coefficient is NOT printed

**`show_sparse`** specifies whether the sparse monomial format is used for printing. This is useful for GTPSAs containing a large number of variables and parameters

## Examples

```@repl
using GTPSA; GTPSA.show_sparse = false; #hide
d = Descriptor(100, 1, 10, 1)
x = @vars(d) # Doesn't fit on screen
GTPSA.show_sparse = true;
x
```
