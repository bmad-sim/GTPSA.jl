# [I/O](@id io)
## Global Variables
One non-constant global variable can be set to customize the printed output of TPSs:

```julia
show_eps::Float64 =  eps(Float64)     # Print epsilon
```

**`show_eps`** defines the precision below which a monomial coefficient is NOT printed

## Examples

```@repl
using GTPSA;  #hide
d = Descriptor(1, 20)
x = vars(d)
sin(x[1])
GTPSA.show_eps=1e-3
sin(x[1])
```


