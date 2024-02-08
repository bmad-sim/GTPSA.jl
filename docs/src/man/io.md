# [I/O](@id io)
## Global Variables
There are three non-constant global variables which can be set to customize the printed output of TPSs:

```julia
show_eps::Float64 =  0.0                               # Print epsilon
show_sparse::Bool = false                              # Use sparse monomial print
show_header::Bool = false                              # Print a header above each TPS
```

**`show_eps`** defines the value below which the absolute value of a monomial coefficient is NOT printed

**`show_sparse`** specifies whether the sparse monomial format is used for printing. This is useful for GTPSAs containing a large number of variables and parameters

**`show_header`** specifies whether or not to print the GTPSA `Descriptor` information above each TPS output

## Examples

```@repl
using GTPSA; GTPSA.show_sparse=false #hide
d = Descriptor(100, 1, 10, 1)
x = vars() # Doesn't fit on screen
GTPSA.show_sparse = true;
x
GTPSA.show_header = true
x[1]
```
