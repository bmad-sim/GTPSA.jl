# [`@FastGTPSA`/`@FastGTPSA!` Macros](@id fastgtpsa)
*Speed up evaluation of expressions containing TPSs, transparent to other types*

The thread-safe macros [`@FastGTPSA`/`@FastGTPSA!`](@ref fastgtpsa) can be used to speed up evaluation of expressions that may contain `TPS`s. **Both macros are completely transparent to all other types, so they can be prepended to any existing expressions while still maintaining type-generic code.** Any functions in the expression that are not overloaded by GTPSA will be ignored. Both macros do **not** use any `-ffast-math` business (so still IEEE compliant), but instead will use a thread-safe pre-allocated buffer in the `Descriptor` for any temporaries that may be generated during evaluation of the expression.

The first macro, `@FastGTPSA` can be prepended to an expression following assignment (`=`, `+=`, etc) to only construct one `TPS` (which requires two allocations), instead of a `TPS` for every temporary:

```@repl
using GTPSA, BenchmarkTools

d = Descriptor(3, 7);  x = vars(d);

@btime $x[1]^3*sin($x[2])/log(2+$x[3])-exp($x[1]*$x[2])*im;

@btime @FastGTPSA $x[1]^3*sin($x[2])/log(2+$x[3])-exp($x[1]*$x[2])*im;

y = rand(3); # transparent to non-TPS types

@btime $y[1]^3*sin($y[2])/log(2+$y[3])-exp($y[1]*$y[2])*im;

@btime @FastGTPSA $y[1]^3*sin($y[2])/log(2+$y[3])-exp($y[1]*$y[2])*im;
```

The second macro, `@FastGTPSA!` can be prepended to the LHS of an assignment, and will fill a preallocated `TPS` with the result of an expression. `@FastGTPSA!` will calculate a `TPS` expression with _zero_ allocations, and will still have no impact if a non-TPS type is used. The only requirement is that all symbols in the expression are defined:

```@repl
using GTPSA, BenchmarkTools # hide
d = Descriptor(3, 7); x = vars(d); # hide

t = ComplexTPS64(); # pre-allocate

@btime @FastGTPSA! $t = $x[1]^3*sin($x[2])/log(2+$x[3])-exp($x[1]*$x[2])*im; 

y = rand(3); @gensym z; # transparent to non-TPS types

@btime @FastGTPSA! $z = $y[1]^3*sin($y[2])/log(2+$y[3])-exp($y[1]*$y[2])*im;
```

Both `@FastGTPSA` and `@FastGTPSA!` can also be prepended to a block of code, in which case they are applied to each assignment in the block:

```@repl
using GTPSA, BenchmarkTools # hide
d = Descriptor(3, 7); x = vars(d);

y = rand(3);

@btime @FastGTPSA begin
        t1 = $x[1]^3*sin($x[2])/log(2+$x[3])-exp($x[1]*$x[2])*im;
        t2 = $x[1]^3*sin($x[2])/log(2+$x[3])-exp($x[1]*$x[2])*im;
        z  = $y[1]^3*sin($y[2])/log(2+$y[3])-exp($y[1]*$y[2])*im;
       end;

t3 = ComplexTPS64(); t4 = ComplexTPS64(); @gensym w;

@btime @FastGTPSA! begin
        $t3 = $x[1]^3*sin($x[2])/log(2+$x[3])-exp($x[1]*$x[2])*im;
        $t4 = $x[1]^3*sin($x[2])/log(2+$x[3])-exp($x[1]*$x[2])*im;
        $w  = $y[1]^3*sin($y[2])/log(2+$y[3])-exp($y[1]*$y[2])*im;
       end;

```

Without using the macro, each time an operation is performed using a TPS, a new TPS is dynamically-allocated containing the result. For example in the above expression, the calculation of `sin(x[2])` creates a new TPS, and the calculation of `x[1]^3` also creates a new TPS. The multiplication of these two resulting TPSs creates a new TPS, and so on until a TPS containing the full result of the evaluated expression is obtained. The intermediate TPSs that must be created to evaluate the expression are referred to as *temporaries*, because they only exist temporarily. Julia's garbage collector notices when the dynamically-allocated temporaries are no longer in scope, and cleans up that memory when it decides it's a good time to. This process can cause slowdowns in performance critical code however, especially in more complicated expressions where a lot of temporaries are created.

Both `@FastGTPSA` and `@FastGTPSA!` basically tell the code to instead use a permanent, pre-allocated thread-safe buffer of TPSs for the temporaries during evaluation of the expression, so there is no dynamic memory allocation; for `@FastGTPSA`, the number of allocations is reduced to two (which is one single `TPS`), and for `@FastGTPSA!` the number of allocations is zero. Furthermore, these temporaries are accessed and deleted in a stack-like manner from the buffer by each thread, so that temporaries involved in operations are right next to each other in memory. This ensures minimal cache misses throughout the evaluation of the expression.

The speedup of using the macro can be quite significant. See our [example](https://github.com/bmad-sim/GTPSA.jl/blob/main/benchmark/track.jl), where we observe a x2 speedup at 2nd order. 


## Documentation
```@docs
@FastGTPSA
@FastGTPSA!
```