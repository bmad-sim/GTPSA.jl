# [`@FastGTPSA`](@id fastgtpsa)
*Speed up evaluation of expressions containing TPSs, transparent to other types*

The `@FastGTPSA` macro can be preprended to any mathematical expressions that may contain operations using `TPS`/`ComplexTPS`s. **The macro is completely transparent to non-TPS types, and so can be prepended in all functions while still maintaining type-generic code.**

Here's an example of `@FastGTPSA` in action:

```@repl
using GTPSA, BenchmarkTools
GTPSA.show_sparse = false; GTPSA.show_header=false; # hide

d = Descriptor(3, 5);
x = vars(use=d);

@btime $x[1]^3*sin($x[2])/log(2+$x[3])-exp($x[1]*$x[2])*im;

@btime @FastGTPSA $x[1]^3*sin($x[2])/log(2+$x[3])-exp($x[1]*$x[2])*im;
```

Without using the macro, each time an operation is performed using a TPS, a new TPS is dynamically-allocated containing the result. For example in the above expression, the calculation of `sin(x[2])` creates a new TPS, and the calculation of `x[1]^3` also creates a new TPS. The multiplication of these two resulting TPSs creates a new TPS, and so on until a TPS containing the full result of the evaluated expression is obtained. The intermediate TPSs that must be created to evaluate the expression are referred to as *temporaries*, because they only exist temporarily. In the above example, we have 9 temporaries being created, with the last allocation being the result of the entire expression. Julia's garbage collector notices when the dynamically-allocated temporaries are no longer in scope, and cleans up that memory. This process can cause slowdowns in performance critical code however, especially in more complicated expressions where a lot of temporaries are created.

The macro `@FastGTPSA` basically tells the code to instead use a permanent, pre-allocated buffer of TPSs to contain the temporaries during evaluation of the expression, so there is no dynamic memory allocation until the result is obtained; the number of allocations is reduced to 1. Furthermore, these temporaries are accessed and deleted in a stack-like manner from the buffer, so that temporaries involved in operations are right next to each other in memory. This ensures minimal cache misses throughout the evaluation of the expression.

The speedup of using the macro can be quite significant. See our [example](https://github.com/bmad-sim/GTPSA.jl/blob/main/benchmark/taylormap.jl), where we observe a roughly x2.5 speedup.