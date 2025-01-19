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

## TPS Evaluation, Composition, and Translation

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

We also can _compose_ `TPS`s. For this example, let's define a new GTPSA with only 1 variable to 1st order, and define the functions ``f(x) = x^2+3x`` and ``g(x)=2+x``:

```@example 2
using GTPSA; GTPSA.show_sparse=false;#hide
d = Descriptor(1, 1);
Δx = first(@vars(d));
f(x) = 7+8*x+ 9*x^2;
g(x) = 3 + 4*x + 5*x^2;
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

Wait a minute, what happened here? `fg_composed` certainly is not equal to `fg_exact`. Is there a bug in GTPSA? Can GTPSA be trusted for your most critical analyses?

This example presents the important concept of `TPS` _feed-down error_. The explanation is simple; for `fg_composed`, we separately obtained `TPS`s expanded around ``x=0``, and then tried to compose them. However, after passing through ``g``, a nonzero 0th order term (also called the _scalar_ part of the `TPS`) was picked up. Thus, we should have expanded ``f`` around the scalar part of `gt`, instead of ``x=0``. Then, to keep the same coordinates as initially inputted into `gt`, we'll also need to `translate` this `TPS` expansion point back to the original expansion point:

```@example 2
# First find the expansion of `f` around the 0th order (scalar part) of `g`
ft1 = f(scalar(gt) + Δx); # We can use `scalar` to get the scalar part of a `TPS`

# Then `translate` the expansion point back to the original expansion point
ft = translate(ft1, -scalar(gt));

fg_composed = ft ∘ gt;
fg_composed - fg_exact
```

The two now agree well. 

This example shows the care that must be taken when composing two separate truncated power series. Another, perhaps simpler, way of dealing with this problem is to always chose a coordinate system such that the scalar part of a `TPS` is equal to zero.


## Partial Derivative Getting/Setting

## Autodifferentiation using `GTPSA.jl`

## `@FastGTPSA`/`@FastGTPSA!` Macros


Another global variable we can set to modify the `show` of a `TPS` is `GTPSA.show_sparse`, which defaults to `false`. Sparse `TPS` output may be convenient for GTPSAs GTPSAs with large numbers of variables/parameters:

```@repl
using GTPSA; GTPSA.show_sparse = false; #hide
d = Descriptor(100, 1); # 100 variables to 1st order
x = @vars(d) # Doesn't fit on the screen
GTPSA.show_sparse = true;
println(x)
```



After 


**"Variables" in the context of `GTPSA.jl` are small deviations from some expansion point.** 

The best way to understand this is with an example: let's compute the Maclaurin series of the function ``f(\vec{x}) = \cos{(x_1)} + \textrm{i}\sin{(x_2)}``:
```@example 1
f(x) = cos(x[1]) + im*sin(x[2]); # Define the function

f(Δx) # Maclaurin series
```






Basically, you wi


The `TPS` type, however, does store the expansion anywhere. Rather, it just stores 

This `TPS` vector is the _identity map_: ``[x_1,x_2] \mapsto [x_1,x_2]``. What if we would like a different expansion point of the `TPS` though, say around ``(\tilde{x}_1,\tilde{x}_2)=(5.0,6.0)``? _Because this is an identity map_, we can translate the expansion point exactly by simply adding the new expansion point. This is fully equivalent to shifting our coordinate system ``(x_1,x_2) \mapto (so that 


The result is a new `TPS` which now describes deviations from ``(5.0,6.0)`` (*NOT ). Explicitly:

An important point is that the `TPS` itself does not 
The `TPS` itself doesn't actually store the expansion point, only the 


 It is important to note that GTPSA


We can now plug this

or example, to calculate the Maclaurin series of ``f(x_1) = \cos{(x_1)}`` to 20th order:




 For example, to calculate the Taylor series for ``f(x_1,x_2) = \cos{(x_1)} + \sqrt{1+x_2}`` to 4th order in ``x_1`` and but only 1st order in ``x_2`` (up to maximally 1 + 4 = 5th order):





A `TPS` can be plugged into any mathematical function















First, we need to define a `Descriptor`
## Calculating a TPS





 After defining a `Descriptor`, the variables (which themselves are represented as `TPS`s) can be obtained using `vars` or `complexvars`. For example, to calculate the Taylor series for ``f(x_1,x_2) = \cos{(x_1)} + \sqrt{1+x_2}`` to 4th order in ``x_1`` and but only 1st order in ``x_2`` (up to maximally 1 + 4 = 5th order):


A blank TPS with all coefficients equal to zero can be created using `TPS(use=d)`. This will construct a new `TPS` where each monomial coefficient is a `Float64`. If `use` is not explicitly passed in the constructor, then the global `GTPSA.desc_current`, which is set each time a new `Descriptor` is defined, will be used. Equivalently, `TPS64(use=d)` or `TPS{Float64}(use=d)` could have been used; a `TPS` is a [parametric type](https://docs.julialang.org/en/v1/manual/types/#Parametric-Types) where the type parameter specifies the number type that the `TPS` represents, and therefore the number type for each monomial coefficient in the TPS. `TPS64` is an alias for `TPS{Float64}`, and if no type parameter is specified, then the default is `TPS64`. Likewise, a blank complex TPS can be created used `ComplexTPS64(use=d)` or `TPS{ComplexF64}(use=d)`, with `ComplexTPS64` being an alias for `TPS{ComplexF64}`. Currently, the GTPSA library only supports `TPS`s representing `Float64` and `ComplexF64` number types.

A regular scalar number `a` can be promoted to a `TPS` using `TPS(a)` (note by excluding the `use` keyword argument, `GTPSA.desc_current` is used for the `Descriptor`). In this case, the type parameter of the `TPS` is inferred from the type of `a`.

When a TPS contains a lot of variables, the default output showing each variable exponent can be larger than the screen can show. A global variable `GTPSA.show_sparse`, which is by default set to `false`, can be set to `true` to instead show each specific monomial instead of the exponents for each variable:

```@example
using GTPSA; GTPSA.show_sparse=false;#hide
d = Descriptor(10, 10);
x = vars(d);

GTPSA.show_sparse = true;
g = sin(x[1]*x[3]^2) + cos(x[2]*x[7]);

print(g)
```

Another global variable `GTPSA.show_eps` can be set to exclude showing monomials with coefficients having an absolute value less than `GTPSA.show_eps`.

## Partial Derivative Getting/Setting
### Individual Monomial Coefficient
!!! note
    The value of a partial derivative is equal to the monomial coefficient in the Taylor series multiplied by a constant factor. E.g. for an expansion around zero ``f(x)\approx f(0) + \frac{\partial f}{\partial x}\rvert_0x + \frac{1}{2!}\frac{\partial^2 f}{\partial x^2}\rvert_0 x^2 + ...``, the 2nd order monomial coefficient is ``\frac{1}{2!}\frac{\partial^2 f}{\partial x^2}\rvert_0``. 

Individual monomial coefficients in a TPS `t` can be get/set with three methods of indexing:

1. **By Order:** `t[[<x_1 order>, ..., <x_nv order>]]`. For example, for a TPS with three variables ``x_1``, ``x_2``, and ``x_3``, the ``x_1^3x_2^1`` monomial coefficient is accessed with `t[[3,1,0]]` or equivalently `t[[3,1]]`, as leaving out trailing zeros for unincluded variables is allowed. A tuple is also allowed instead of a vector for the list of orders.
2. **By Sparse Monomial** `t[[<ix_var> => <order>, ...]]`. This method of indexing is convenient when a TPS contains many variables and parameters. For example, for a TPS with variables ``x_1,x_2,...x_{100}``, the ``x_{1}^3x_{99}^1`` monomial coefficient is accessed with `t[[1=>3, 99=>1]]`. A tuple is also allowed instead of a vector for the list of pairs.
3. **By Monomial Index** `t[idx]`. *This method is generally not recommended for indexing above first order.* Indexes the TPS with all monomials sorted by order. For example, for a TPS with two variables ``x_1`` and ``x_2``, the ``x_1`` monomial is indexed with `t[1]` and the ``x_1^2`` monomial is indexed with `t[3]`. The zeroth order part, or the *scalar* part of the TPS, can be set with `t[0]`. This method requires zero allocations for indexing, unlike the other two.

These three methods of indexing are best shown with an example:

```@example
using GTPSA; GTPSA.show_sparse=false;#hide
# Example of indexing by order -----------
d = Descriptor(3, 10);
t = TPS(use=d); # Create zero TPS based on d

t[[0]] = 1;
t[[1]] = 2;
t[[0,1]] = 3;
t[(0,0,1)] = 4;  # Tuples also allowed
t[(2,1,3)] = 5;

print(t)
```

```@example 
using GTPSA; GTPSA.show_sparse=false; #hide
# Example of indexing by sparse monomial -----------
d = Descriptor(3, 10);
t = TPS(use=d); # Create zero TPS based on d

t[[1=>1]] = 2;
t[[2=>1]] = 3;
t[[3=>1]] = 4;
t[(1=>2,2=>1,3=>3)] = 5;  # Tuples also allowed

print(t)
```

```@example 
using GTPSA; GTPSA.show_sparse = false; #hide
# Example of indexing by monomial index -----------
d = Descriptor(3, 10);
t = TPS(use=d); # Create zero TPS based on d

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
print(t)
```

### Gradients, Jacobians, Hessians
The convenience getters `gradient`, `jacobian`, and `hessian` (as well as their corresponding in-place methods `gradient!`, `jacobian!`, and `hessian!`) are also provided for extracting partial derivatives from a TPS/Vector of TPSs. Note that these functions are not actually calculating anything - at this point the TPS should already have been propagated through the system, and these functions are just extracting the corresponding partial derivatives. Note that these function names are not exported, because they are commonly used by other automatic differentiation packages.

```julia
using GTPSA; GTPSA.show_sparse=false; #hide
# 2nd Order TPSA with 100 variables
d = Descriptor(100, 2);
x = vars(d);

out = cumsum(x);

# Convenience getters for partial derivative extracting:
grad1 = GTPSA.gradient(out[1]);
J = GTPSA.jacobian(out);
h1 = GTPSA.hessian(out[1]);

# Also in-place getters
GTPSA.gradient!(grad1, out[1]);
GTPSA.jacobian!(J, out);
GTPSA.hessian!(h1, out[1]);
```

## Slicing a TPS
Parts of a TPS with certain variable orders can be extracted by slicing the TPS. When indexing by order, a colon (`:`) can be used in place for a variable order to include all orders of that variable. If the last specified index is a colon, then the rest of the variable indices are assumed to be colons:

```@example slice
using GTPSA; GTPSA.show_sparse=false; #hide
d = Descriptor(5, 10);
x = vars(d);

f = 2*x[1]^2*x[3] + 3*x[1]^2*x[2]*x[3]*x[4]^2*x[5] + 6*x[3] + 5;
g = f[[2,:,1]];
h = f[[2,:,1,:]];

print(f)
print(g)
print(h)
```

A TPS can also be sliced with indexing by sparse monomial. In this case, if a colon is included anywhere in the sparse monomial index, then all orders of all variables not explicity specified will be included:

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

Both macros are also compatible with broadcasted, vectorized operators:

```@repl
using GTPSA, BenchmarkTools # hide
d = Descriptor(3, 7); x = vars(d); y = rand(3);
@btime @FastGTPSA begin
        out = @. $x^3*sin($y)/log(2+$x)-exp($x*$y)*im;
       end;
out = zeros(ComplexTPS64, 3); # pre-allocate
@btime @FastGTPSA! begin
        @. $out = $x^3*sin($y)/log(2+$x)-exp($x*$y)*im;
       end;
```

The advantages of using the macros become especially apparent in more complicated systems, for example in [`benchmark/track.jl`](https://github.com/bmad-sim/GTPSA.jl/blob/main/benchmark/track.jl). 

## Promotion of `TPS64` to `ComplexTPS64`

`TPS64`s and `ComplexTPS64`s can be mixed freely without concern. Any time an operation with a `TPS64` and a `ComplexTPS64` or a `Complex` number occurs, the result will be a `ComplexTPS64`. A `ComplexTPS64` can be converted back to a `TPS64` using the `real` and `imag` operators.
