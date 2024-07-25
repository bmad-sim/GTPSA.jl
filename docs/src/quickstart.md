# Quickstart Guide
## Defining the GTPSA
A `Descriptor` defines all information about the GTPSA, including the number of variables and truncation orders for each variable. The constructors for a `Descriptor` including only variables are:

```@example desc
using GTPSA #hide
# 2 variables with max truncation order 10
d1 = Descriptor(2, 10)     
```

```@example desc
# 3 variables with individual truncation orders 1, 2, 3 and max truncation order 6
d2 = Descriptor([1, 2, 3], 6)
```

## Calculating a TPS
After defining a `Descriptor` for the TPSA, the variables (which themselves are represented as `TPS`s) can be obtained using `vars` or `complexvars`. For example, to calculate the Taylor series for ``f(x_1,x_2) = \cos{(x_1)} + \sqrt{1+x_2}`` to 4th order in ``x_1`` and but only 1st order in ``x_2`` (up to maximally 1 + 4 = 5th order):

```@example 1
using GTPSA; GTPSA.show_header=false; GTPSA.show_sparse=false;#hide
d = Descriptor([4, 1], 5);

# Returns a Vector of each variable as a TPS
x = vars(d) 
```

These `TPS`s can then be manipulated just like any other mathematical quantity in Julia:

```@example 1
f = cos(x[1]) + sqrt(1 + x[2])
```

A blank TPS with all coefficients equal to zero can be created using `TPS(use=d)`. This will construct a new `TPS` where each monomial coefficient is a `Float64`. If `use` is not explicitly passed in the constructor, then the global `GTPSA.desc_current`, which is set each time a new `Descriptor` is defined, will be used. Equivalently, `TPS64(use=d)` or `TPS{Float64}(use=d)` could have been used; a `TPS` is a [parametric type](https://docs.julialang.org/en/v1/manual/types/#Parametric-Types) where the type parameter specifies the number type that the `TPS` represents, and therefore the number type for each monomial coefficient in the TPS. `TPS64` is an alias for `TPS{Float64}`, and if no type parameter is specified, then the default is `TPS64`. Likewise, a blank complex TPS can be created used `ComplexTPS64(use=d)` or `TPS{ComplexF64}(use=d)`, with `ComplexTPS64` being an alias for `TPS{ComplexF64}`. Currently, the GTPSA library only supports `TPS`s representing `Float64` and `ComplexF64` number types.

A regular scalar number `a` can be promoted to a `TPS` using `TPS(a)` (note by excluding the `use` keyword argument, `GTPSA.desc_current` is used for the `Descriptor`). In this case, the type parameter of the `TPS` is inferred from the type of `a`.

When a TPS contains a lot of variables, the default output showing each variable exponent can be larger than the screen can show. A global variable `GTPSA.show_sparse`, which is by default set to `false`, can be set to `true` to instead show each specific monomial instead of the exponents for each variable:

```@example
using GTPSA; GTPSA.show_header=false; GTPSA.show_sparse=false;#hide
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
using GTPSA; GTPSA.show_header=false; GTPSA.show_sparse=false;#hide
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
using GTPSA; GTPSA.show_header=false; GTPSA.show_sparse=false; #hide
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
using GTPSA; GTPSA.show_sparse = false;  GTPSA.show_header=false; #hide
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

```@example
using GTPSA; GTPSA.show_header=false; GTPSA.show_sparse=false; #hide
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
using GTPSA; GTPSA.show_header=false; GTPSA.show_sparse=false; #hide
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

The macros [`@FastGTPSA`/`@FastGTPSA!`](@ref fastgtpsa) can be used to speed up evaluation of expressions that may contain `TPS`s. **Both macros are completely transparent to all other types, so they can be prepended to any existing expressions while still maintaining type-generic code.** Any functions in the expression that are not overloaded by GTPSA will be ignored. Both macros do **not** use any `-ffast-math` business (so still IEEE compliant), but instead will use a pre-allocated buffer in the `Descriptor` for any temporaries that may be generated during evaluation of the expression.

The first macro, `@FastGTPSA` can be prepended to an expression following assignment (`=`, `+=`, etc) to only construct one `TPS` (which requires two allocations), instead of a `TPS` for every temporary:

```@repl
using GTPSA, BenchmarkTools

d = Descriptor(3, 7);  x = vars(d);

@btime $x[1]^3*sin($x[2])/log(2+$x[3])-exp($x[1]*$x[2])*im;

@btime @FastGTPSA $x[1]^3*sin($x[2])/log(2+$x[3])-exp($x[1]*$x[2])*im;

y = rand(3); # transparent to non-TPS types

@btime $y[1]^3*sin($y[2])/log(2+$y[3])-eyp($y[1]*$y[2])*im;

@btime @FastGTPSA $y[1]^3*sin($y[2])/log(2+$y[3])-eyp($y[1]*$y[2])*im;
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

Both `@FastGTPSA` and `@FastGTPSA!` can be prepended to a block of code, in which case they are applied to each assignment in the block:

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

The advantages of using the macro become especially apparent in more complicated systems, for example in [`benchmark/track.jl`](https://github.com/bmad-sim/GTPSA.jl/blob/main/benchmark/track.jl). 

## Promotion of `TPS64` to `ComplexTPS64`

`TPS64`s and `ComplexTPS64`s can be mixed freely without concern. Any time an operation with a `TPS64` and a `ComplexTPS64` or a `Complex` number occurs, the result will be a `ComplexTPS64`. A `ComplexTPS64` can be converted back to a `TPS64` using the `real` and `imag` operators.
