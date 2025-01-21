# Quickstart Guide
## Defining the GTPSA
We first must define a `Descriptor` which includes all information about a GTPSA, including the number of variables and truncation order. The constructors for a `Descriptor` including only variables are:

```@example desc
using GTPSA #hide
# 2 variables with max truncation order 10
d1 = Descriptor(2, 10)     
```

## Calculating a Truncated Power Series
`GTPSA.jl` centers around the `TPS` (truncated power series) type, which represents a multivariable Taylor series truncated at the chosen order in the `Descriptor`. Using [Einstein notation](https://en.wikipedia.org/wiki/Einstein_notation) for the variable indices, and letting ``n`` specify the order up to a maximum truncation order ``MO``, we can express a function ``f`` expanded around ``\vec{a}`` as a `TPS`:

```math
f(\vec{x}) = f(\vec{a}) + \sum_{n=1}^{MO} \left.\frac{\partial f}{\partial x_{i_1} \partial x_{i_2}\ldots \partial x_{i_n}}\right\rvert_{\vec{a}} \Delta x_{i_1} \Delta x_{i_2} \ldots\Delta x_{i_n}
```

The `TPS` type stores all of the monomial coefficients in this Taylor series up to the chosen order (e.g. ``f(\vec{a})``, ``\partial f /\partial x_i |_{\vec{a}}``, etc.). You can manipulate and propagate a `TPS` through some functions like any other number type, and the result will be a `TPS` containing the Taylor expansion representing all preceding operations. If you have some familiarity with automatic differentiation, then you can view a `TPS` as basically a Dual number which has been highly optimized for high order automatic differentiation with high numbers of variables/parameters.

**In the context of `GTPSA.jl`, we refer to "variables" as each ``\Delta x_i`` in the `TPS`.** The reason for this is that one can always choose a coordinate system where ``\vec{a}=\vec{0}``, and such a choice greatly simplifies the terminology and analysis. This brings up an important point: the `TPS` type itself does NOT explicitly store any expansion point. It just stores the monomial coefficients of each term in the Taylor series, truncated at the chosen order of the (tiny) variables. The setup of the problem, done by you our dear user, will decide the expansion point. In the parlance of Dual numbers, the quantity ``\Delta x_i`` is equivalent to ``0+1\epsilon_i``.

Let's dive into some examples, which will make these above points clearer. After defining the `Descriptor`, we can obtain the variables, which themselves are represented as `TPS`s (with each variable's first order monomial coefficient set to 1) using `@vars`:

```@example 1
using GTPSA; GTPSA.show_sparse=false;#hide
d6 = Descriptor(2, 6); # 2 variables to 6th order

# Returns a Vector of each variable as a TPS
Δx = @vars(d6) 
```

The result is a `TPS` vector function ``\begin{bmatrix} \Delta x_1 \\ \Delta x_2 \end{bmatrix}`` corresponding directly to each variable. `TPS64` is an alias for `TPS{Float64}`, which are `TPS`s that represent 64-bit floats. Likewise, `ComplexTPS64` is an alias for `TPS{ComplexF64}`. Currently, GTPSA only supports `TPS`s which represent `Float64` and `ComplexF64` numbers. 

 Now let's use this to compute the Maclaurin series of the function ``f(\vec{x}) = \cos{(x_1)} + \textrm{i}\sin{(x_2)}``:

```@example 1
f(x) = cos(x[1]) + im*sin(x[2]); # Define the function

f(Δx) # Maclaurin series
```

Another way to view "variables" in the context of `GTPSA.jl` are as tiny "wiggles". Specifically, if you wiggle the input, how does the output depend on those input wiggles?. The vector of variables `Δx` are thus just unit wiggles.

To instead compute the Taylor series of `f` expanded around ``(-\pi/2,\pi/2)``, we simply "wiggle" around ``(-\pi/2,\pi/2)``:

```@example 1
x0 = [-pi/2, pi/2];

ft = f(x0 + Δx)
```

`ft` is a `TPS` containing the expansion of `f` around ``(-\pi/2,\pi/2)`` up to 6th order, with the variables specifying deviations from ``(-\pi/2,\pi/2)``. Note how some of the monomial coefficients are small but nonzero due to floating point roundoff error. To hide the display of small monomial coefficients below a certain absolute value, we can set the global variable `GTPSA.show_eps`:


```@example 1
GTPSA.show_eps = 1e-15
println(ft)
```
!!! note
    Setting `GTPSA.show_eps` does not actually reset any monomial coefficients in the `TPS`s equal to zero, rather just suppresses their output when `show`ing any `TPS`.

## TPS Evaluation, Composition, Translation, and Inversion

We can evaluate `TPS`s just like any other function. Continuing with our example above, let's evaluate `ft`, which is an expansion around ``(-\pi/2,\pi/2)`` at, say, ``(\Delta x_1,\Delta x_2)=(\pi,-\pi)``, and see how well it agrees with the exact calculation using `f`:

```@example 1
abs(ft([-pi, pi]) - f(x0 + [-pi, pi]))
```
The disagreement is quite large! This is due to the truncation error of the `TPS`. If we increase the maximum order of the GTPSA, we can obtain a better approximation of the function `f` for such large variables. Let's test that now, setting the truncation order to 20 for both variables:

```@example 1
d20 = Descriptor(2, 20); # two variables to 20th order
Δx = @vars(d20);
ft = f(x0 + Δx);
abs(ft([-pi, pi]) - f(x0 + [-pi, pi]))
```

`ft` now approximates `f` much better for such large variables!

We also can _compose_ `TPS`s. For this example, let's define a new GTPSA with only 1 variable to 1st order, and define the functions ``f(x) = x^2+2x`` and ``g(x)=3+4x``:

```@example 2
using GTPSA; GTPSA.show_sparse=false;#hide
d = Descriptor(1, 1);
Δx = first(@vars(d));
f(x) = x^2 + 2*x;
g(x) = 3 + 4*x;
```
 
Of course, we can compute a `TPS` representing the expansion of ``f\circ g`` around ``x=0`` by:

```@example 2
fg_exact = f(g(Δx))
```

However, we can also separately compute `TPS`s for ``f`` and ``g`` around ``x=0``, and then _compose_ them:

```@example 2
ft = f(Δx);
gt = g(Δx);
fg_composed = ft ∘ gt # Or equivalently ft(gt)
```

Wait a minute, what happened here? `fg_composed` certainly is not equal to `fg_exact`.

This example presents the important concept of `TPS` _feed-down error_. The explanation is simple; for `fg_composed`, we separately obtained `TPS`s expanded around ``x=0``, and then tried to compose them. However, after passing through ``g``, a nonzero 0th order term (also called the _scalar_ part of the `TPS`) was picked up. Thus, we should have expanded ``f`` around the scalar part of `gt`, instead of ``x=0``. Then, to keep the same coordinates as initially inputted into `gt`, we'll also need to `translate` this `TPS` expansion point back to the original expansion point:

```@example 2
# First find the expansion of `f` around the 0th order (scalar part) of `g`
ft1 = f(scalar(gt) + Δx); # We can use `scalar` to get the scalar part of a `TPS`

# Then `translate` the expansion point back to the original expansion point
ft = translate(ft1, -scalar(gt));

fg_composed = ft ∘ gt
```

The two now fully agree. 

This example shows the care that must be taken when composing two separate truncated power series. Another, perhaps simpler, way of dealing with this problem is to always chose a coordinate system such that the scalar part of a `TPS` is equal to zero.

GTPSA also includes a routine to invert a `TPS` map. The inversion routine ignores any scalar part of the `TPS`, so it is the responsibility of the user to ensure the coordinate system is consistent by either translating the `TPS`s or using a coordinate system where the scalar part is zero. 

As an example, let's invert the following `TPS` map, which has no scalar part:

```@example 3
using GTPSA; GTPSA.show_sparse=false;#hide
d = Descriptor(2, 2);
Δx = @vars(d);
M = [  Δx[1] + 2*Δx[2] + 3*Δx[1]*Δx[2], 
     3*Δx[1] + 4*Δx[2] + Δx[1]^2 + Δx[2]^2]
M_inv = inv(M)

M_inv ∘ M 
```

## Other TPS Constructors

After defining a `Descriptor`, we can construct a `TPS` using any of the following:

```@repl
using GTPSA; GTPSA.show_sparse=false; # hide
d = Descriptor(3, 5); # 3 variables to 5th order
t = TPS64{d}() # Constructs a blank `TPS`, equivalent to `TPS{Float64,d}()`
t1 = TPS64{d}(1.0) # Constructs a `TPS` with scalar part set to 1.0
t1c = ComplexTPS64{d}(1.0) # Equivalent to `TPS{ComplexF64,d}(1.0)`
t1im = TPS{d}(1.0im) # If the number type is not specified, then it is inferred
```

When constructing `TPS`s in this manner, it is important to include the `Descriptor` in the type parameter of the constructor, to ensure the `Descriptor` of the `TPS` is resolved statically (at compile time). If it is not included, then the `Descriptor` of the `TPS` will be resolved dynamically instead, at runtime. `GTPSA.jl` provides both static and dynamic `Descriptor` resolution modes, each of which have certain advantages in different use cases. See the [advanced topics](@ref descmodes) section of the documentation for more details.

## Partial Derivative Getting/Setting
### Individual Monomial Coefficient
!!! note
    The value of a partial derivative is equal to the monomial coefficient in the Taylor series multiplied by a constant factor. E.g. for an expansion around zero ``f(x)\approx f(0) + \frac{\partial f}{\partial x}\rvert_0x + \frac{1}{2!}\frac{\partial^2 f}{\partial x^2}\rvert_0 x^2 + ...``, the 2nd order monomial coefficient is ``\frac{1}{2!}\frac{\partial^2 f}{\partial x^2}\rvert_0``. 

Individual monomial coefficients in a `TPS` `t` can be get/set with three methods of indexing:

1. **By Monomial Index:** `t[idx::Integer]`. Indexes the `TPS` with all monomials sorted by order. For example, for a `TPS` with two variables ``Δx_1`` and ``Δx_2``, the ``Δx_1`` monomial is indexed with `t[1]` and the ``Δx_1^2`` monomial is indexed with `t[3]`. The zeroth order part, or the *scalar* part of the TPS, can be indexed with `t[0]`.
2. **By Order:** `t[[<Δx_1 order>, ..., <Δx_NV order>]]`. For example, for a `TPS` with three variables ``Δx_1``, ``Δx_2``, and ``Δx_3``, the ``Δx_1^3Δx_2^1`` monomial coefficient is accessed with `t[[3,1,0]]` or equivalently `t[[3,1]]`, as leaving out trailing zeros for unincluded variables is allowed. A tuple is also allowed instead of a vector for the list of orders.
3. **By Sparse Monomial:** `t[[<ix_var> => <order>, ...]]`. This method of indexing is convenient when a `TPS` contains many variables and parameters. For example, for a `TPS` with variables ``Δx_1,Δx_2,...Δx_{100}``, the ``Δx_{1}^3Δx_{99}^1`` monomial coefficient is accessed with `t[[1=>3, 99=>1]]`. A tuple is also allowed instead of a vector for the list of pairs.

These three methods of indexing are best shown with an example:

```@example 
using GTPSA; GTPSA.show_sparse = false; #hide
# Example of indexing by monomial index -----------
d = Descriptor(3, 10);
t = TPS{d}(); # or equivalently TPS{Float64,d}()

t[0] = 0;
t[1] = 1;
t[2] = 2;
t[3] = 3;
t[4] = 4;
t[5] = 5; 
t[6] = 6;
t[7] = 7;
t[8] = 8;
t[9] = 9;
t[10] = 10;

t
```


```@example
using GTPSA; GTPSA.show_sparse=false;#hide
# Example of indexing by order -----------
d = Descriptor(3, 10);
t = TPS{d}();

t[[0]] = 1;
t[[1]] = 2;
t[[0,1]] = 3;
t[(0,0,1)] = 4;  # Tuples also allowed
t[(2,1,3)] = 5;

t
```

```@example 
using GTPSA; GTPSA.show_sparse=false; #hide
# Example of indexing by sparse monomial -----------
d = Descriptor(3, 10);
t = TPS{d}();

t[[1=>1]] = 2;
t[[2=>1]] = 3;
t[[3=>1]] = 4;
t[(1=>2,2=>1,3=>3)] = 5;  # Tuples also allowed

t
```

The [`GTPSA.cycle!`](@ref) function can also be used to cycle through all nonzero monomials in a `TPS`.

### Gradients, Jacobians, Hessians
The convenience getters `gradient`, `jacobian`, and `hessian` (as well as their corresponding in-place methods `gradient!`, `jacobian!`, and `hessian!`) are also provided for extracting partial derivatives from a `TPS`/array of `TPS`s. Note that these functions are not actually calculating anything - at this point the `TPS` should already have been propagated through, and these functions are just extracting the corresponding partial derivatives.

```julia
using GTPSA; GTPSA.show_sparse=false; #hide
# 2nd Order TPSA with 100 variables
d = Descriptor(100, 2);
Δx = @vars(d);

out = cumsum(Δx);

# Convenience getters for partial derivative extracting:
grad1 = GTPSA.gradient(out[1]);
J = GTPSA.jacobian(out);
h1 = GTPSA.hessian(out[1]);

# Also in-place getters
GTPSA.gradient!(grad1, out[1]);
GTPSA.jacobian!(J, out);
GTPSA.hessian!(h1, out[1]);
```

## Slicing a `TPS`
Parts of a `TPS` with certain variable orders can be extracted by slicing the `TPS`. When indexing by order, a colon (`:`) can be used in place for a variable order to include all orders of that variable. If the last specified index is a colon, then the rest of the variable indices are assumed to be colons:

```@example slice
using GTPSA; GTPSA.show_sparse=false; #hide
d = Descriptor(5, 10);
Δx = @vars(d);

f = 2*Δx[1]^2*Δx[3] + 3*Δx[1]^2*Δx[2]*Δx[3]*Δx[4]^2*Δx[5] + 6*Δx[3] + 5;
g = f[[2,:,1]];
h = f[[2,:,1,:]];

print(f)
print(g)
print(h)
```

A `TPS` can also be sliced with indexing by sparse monomial. In this case, if a colon is included anywhere in the sparse monomial index, then all orders of all variables not explicity specified will be included:

```@example slice
 # Colon position does not matter in sparse-monomial indexing
g = f[[1=>2, :, 3=>1, 4=>0, 5=>0]];
h = f[[1=>2, 3=>1, :]];

print(g)
print(h)
```

## `@FastGTPSA`/`@FastGTPSA!` Macros

The macros [`@FastGTPSA`/`@FastGTPSA!`](@ref fastgtpsa) can be used to speed up evaluation of expressions that may contain `TPS`s. **Both macros are completely transparent to all other types, so they can be prepended to any existing expressions while still maintaining type-generic code.** Any functions in the expression that are not overloaded by GTPSA will be ignored. Both macros do **NOT** use any `-ffast-math` business (so still IEEE compliant), but instead will use a thread-safe pre-allocated buffer in the `Descriptor` for any temporaries that may be generated during evaluation of the expression.

The first macro, `@FastGTPSA` can be prepended to an expression following assignment (`=`, `+=`, etc) to only construct one `TPS` (which requires two allocations), instead of a `TPS` for every temporary:

```@repl
using GTPSA, BenchmarkTools

d = Descriptor(3, 7);  Δx = @vars(d);

@btime $Δx[1]^3*sin($Δx[2])/log(2+$Δx[3])-exp($Δx[1]*$Δx[2])*im;

@btime @FastGTPSA $Δx[1]^3*sin($Δx[2])/log(2+$Δx[3])-exp($Δx[1]*$Δx[2])*im;

y = rand(3); # transparent to non-TPS types

@btime $y[1]^3*sin($y[2])/log(2+$y[3])-exp($y[1]*$y[2])*im;

@btime @FastGTPSA $y[1]^3*sin($y[2])/log(2+$y[3])-exp($y[1]*$y[2])*im;
```

The second macro, `@FastGTPSA!` can be prepended to the left-hand side of an assignment, and will fill a preallocated `TPS` with the result of an expression. `@FastGTPSA!` will calculate a `TPS` expression with _zero_ allocations, and will still have no impact if a non-TPS type (even if immutable) is used. The only requirement is that all symbols in the expression are defined:

```@repl
using GTPSA, BenchmarkTools # hide
d = Descriptor(3, 7); Δx = @vars(d); # hide

t = ComplexTPS64(); # pre-allocate

@btime @FastGTPSA! $t = $Δx[1]^3*sin($Δx[2])/log(2+$Δx[3])-exp($Δx[1]*$Δx[2])*im; 

y = rand(3); @gensym z; # transparent to non-TPS types

@btime @FastGTPSA! $z = $y[1]^3*sin($y[2])/log(2+$y[3])-exp($y[1]*$y[2])*im;
```

Both `@FastGTPSA` and `@FastGTPSA!` can also be prepended to a block of code, in which case they are applied to each assignment in the block:

```@repl
using GTPSA, BenchmarkTools # hide
d = Descriptor(3, 7); Δx = @vars(d);

y = rand(3);

@btime @FastGTPSA begin
        t1 = $Δx[1]^3*sin($Δx[2])/log(2+$Δx[3])-exp($Δx[1]*$Δx[2])*im;
        t2 = $Δx[1]^3*sin($Δx[2])/log(2+$Δx[3])-exp($Δx[1]*$Δx[2])*im;
        z  = $y[1]^3*sin($y[2])/log(2+$y[3])-exp($y[1]*$y[2])*im;
       end;

t3 = ComplexTPS64(); t4 = ComplexTPS64(); @gensym w;

@btime @FastGTPSA! begin
        $t3 = $Δx[1]^3*sin($Δx[2])/log(2+$Δx[3])-exp($Δx[1]*$Δx[2])*im;
        $t4 = $Δx[1]^3*sin($Δx[2])/log(2+$Δx[3])-exp($Δx[1]*$Δx[2])*im;
        $w  = $y[1]^3*sin($y[2])/log(2+$y[3])-exp($y[1]*$y[2])*im;
       end;

```

Both macros are also compatible with broadcasted, vectorized operators:

```@repl
using GTPSA, BenchmarkTools # hide
d = Descriptor(3, 7); Δx = @vars(d); y = rand(3);
@btime @FastGTPSA begin
        out = @. $Δx^3*sin($y)/log(2+$Δx)-exp($Δx*$y)*im;
       end;
out = zeros(ComplexTPS64, 3); # pre-allocate
@btime @FastGTPSA! begin
        @. $out = $Δx^3*sin($y)/log(2+$Δx)-exp($Δx*$y)*im;
       end;
```

The advantages of using the macros become especially apparent in more complicated systems. See our [example](https://github.com/bmad-sim/GTPSA.jl/blob/main/benchmark/track.jl) where we observed a significant speedup.

## Automatic Differentiation using `GTPSA.jl`

At the time of writing this documentation, a pull request is ready for merge with Julia's [`DifferentiationInterface.jl`](https://github.com/JuliaDiff/DifferentiationInterface.jl) package, which provides a generic interface for computing pushforwards (Jacobian vector products), derivatives, gradients, Jacobians, Hessians, and Hessian vector products using any automatic differentiation (AD) backend. If you are only interested in using GTPSA for these computations, then we strongly recommend using `DifferentiationInterface.jl` with the `AutoGTPSA` AD type.

!!! note
    GTPSA has been highly optimized for high order AD with large numbers of variables and parameters. If you are only interested in first order AD, then other AD backends will likely be faster. GTPSA, however, may likely
    be faster in cases where the entire Hessian must be materialized.

## Compatibility with `DifferentialEquations.jl`

`GTPSA.jl` bindings have been added to `DiffEqBase.jl` so that `TPS`s can be integrated using any of the solvers provided by [SciML's extensive Differential Equations ecosystem](https://docs.sciml.ai/DiffEqDocs/stable/). Simply specify your initial conditions as `TPS` types. This allows, for example, for computation of high-order Hamiltonian maps that can then be analyzed using canonical perturbation theory.