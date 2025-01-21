# Advanced Usage
## [Static and Dynamic `Descriptor` Resolution](@id descmodes)

A constructed `TPS` must correspond to some previously defined `Descriptor`. As such, two "modes" of the `TPS` type may be constructed to determine how the `Descriptor` to use is resolved:

1. Static `Descriptor` resolution -- The `Descriptor` is stored explicitly in the `TPS` type:

| Ctor Call                                | Descriptor                                                     |
| :-------------------                     | :--------------------------------------------                  |
| `TPS{descriptor}([number])`              | `descriptor`                                                   |
| `TPS{descriptor2}(::TPS{T,descriptor1})` | `descriptor2` (copies + changes `Descriptor!`)                 |  
| `TPS(::TPS{T,descriptor})`               | `descriptor`                                                   |

The same applies for all of the above constructor calls with the constructors `TPS64{...}(...)` and `ComplexTPS64{...}(...)`. The created type will be a `TPS{T,descriptor} where {T<:Union{Float64,ComplexF64}}`. Care must be taken with static `Descriptor` resolution to ensure type-stability, and in some cases it may not be possible. However, static resolution has the benefit that the `Descriptor` is stored explicitly in the type. Calls such as `zeros(TPS64{descriptor}, N)` can be made ensuring the `Descriptor` of the output is correct.

2. Dynamic `Descriptor` resolution -- The `Descriptor` is inferred at runtime, based on the passed arguments. A non-constant global variable `GTPSA.desc_current` stores the most recently-defined `Descriptor` to use if no `Descriptor` is inferrable. `GTPSA.desc_current` can also be set manually at any time by the user.

| Ctor Call                                | Descriptor                                                      |
| :-------------------                     | :--------------------------------------------                   |
| `TPS()`                                  | `GTPSA.desc_current`                                            |
| `TPS(use=descriptor)`                    | `descriptor`                                                    |
| `TPS(use=tps1)`                          | That of `tps1`                                                  |
| `TPS(tps)`                               | That of `tps`                                                   |
| `TPS(number)`                            | `GTPSA.desc_current`                                            |
| `TPS(number, use=(descriptor or tps1) )` | `descriptor` or that of `tps1`                                  |
| `TPS(tps, use=(descriptor or tps1) )`    | `descriptor` or that of `tps1` (copies + changes `Descriptor!`) |

The same applies for all of the above constructor calls with the constructors `TPS64(...)` and `ComplexTPS64(...)`. The created type will be a `TPS{T,GTPSA.Dynamic} where {T<:Union{Float64,ComplexF64}}`. Dynamic `Descriptor` resolution will always be type stable, as the `Descriptor` for each `TPS` is not stored in the type definition. E.g., one can have an array with elements of the concrete type `TPS{T,GTPSA.Dynamic}` even though the individual `TPS`s in the array may have differing `Descriptor`s. Another example is typing the field of a struct as `TPS{T,GTPSA.Dynamic}`, so that field can contain `TPS`s of different `Descriptor`s in a type-stable fashion. However, with dynamic `Descriptor` resolution the `use` kwarg must be specified if the `Descriptor` is both not inferrable nor `GTPSA.desc_current` is the desired `Descriptor`. For calls such as `zeros(TPS64, N)` or `TPS64(5.0)`, only `GTPSA.desc_current` can be inferred. Note 

Note that static `Descriptor` resolution has no knowledge of `GTPSA.desc_current`, nor is the `use` kwarg allowed.


## [Custom Truncation Orders](@id cto)
GTPSA allows for significant customization in the truncation of specific variables within a monomial of the truncated power series (TPS). One can specify individually the truncation orders for each variable in a truncated power series, as well as the maximum truncation order for an entire monomial in the TPS. This is best shown with an example:

Suppose we'd like to express a function ``f(x_1,x_2)`` as a truncated power series, and keep only terms that are up to 1st-order in ``x_1`` but up to 2nd order in ``x_2``; basically, we should not have any monomials where ``x_1`` has an exponent > 1, nor any monomials where ``x_2`` has an exponent > 2. GTPSA allows one to select the **individual truncation orders** for variables in a monomial in this manner. The next question to consider is the **maximum truncation order** for the entire monomial; in the above example, note that the 3rd-order term ``x_1x_2^2`` follows the rules we layed out so far. But what if we'd also like to truncate all monomials with order 3 and above, and not allow this monomial? This can be achieved by setting the maximum truncation order equal to 2. When defining a GTPSA, the user must always specify the maximum truncation order, which when specifying individual truncation orders must lie within the range ``[\textrm{max}{(\textrm{individual truncation orders})}, \textrm{sum}{(\textrm{individual truncation orders})}]``. If individual truncation orders are not specified, then they are automatically set to the maximum truncation order.

**Example: allowed monomials for ``f(x_1,x_2)`` with individual variable truncation orders [1,2] and different maximum truncation orders:**

| Exponents | Max Order = 2 | Max Order = 3 |
| :-------: | :-----------: | :-----------: |
|  ``1\quad 0``  |       ✅      |       ✅      |
|  ``0\quad 1``  |       ✅      |       ✅      |
|  ``2\quad 0``  |       ❌      |       ❌      |
|  ``1\quad 1``  |       ✅      |       ✅      |
|  ``0\quad 2``  |       ✅      |       ✅      |
|  ``3\quad 0``  |       ❌      |       ❌      |
|  ``2\quad 1``  |       ❌      |       ❌      |
|  ``1\quad 2``  |       ❌      |       ✅      |
|  ``0\quad 3``  |       ❌      |       ❌      |

We can define these two GTPSA `Descriptor`s respectively with:

```@repl
using GTPSA; GTPSA.show_sparse=false; #hide
d2 = Descriptor([1, 2], 2) # Variable truncation orders [1, 2] and MO=2
d3 = Descriptor([1, 2], 3) # Variable truncation orders [1, 2] and MO=2
```

## Parameters
GTPSA allows one to explicitly distinguish between *variables* and *parameters*. Generally, a variable would be a dependent variable in a differential equation, and a parameter would be a variation in something defining or influencing the system (for example, in a harmonic oscillator the restoring constant ``k`` would be a parameter). Individual truncation orders can be specified for the parameters in the same way as [described for the variables](@ref cto), however there is a special extra truncation order the can be specified for solely the parameters part of the monomial, referred to as the **parameter order**. The parameter order defines the truncation order for only the parameters part of a monomial. 

**Example: allowed monomials for ``f(x_1,k_1,k_2)`` (one variable, two parameters) with individual variable truncation order [1], individual parameter truncation orders [1,1], maximum order = 3, and different parameter orders:**

| Exponents | Parameter Order = 1 | Parameter Order = 2 |
| :-------: | :-----------: | :-----------: |
|  ``0\quad \| \quad 1 \quad 0``  |       ✅      |       ✅      |
|  ``0\quad \| \quad 0 \quad 1``  |       ✅      |       ✅      |
|  ``0\quad \| \quad 1 \quad 1``  |       ❌      |       ✅      |
|  ``1\quad \| \quad 1 \quad 0``  |       ✅      |       ✅      |
|  ``1\quad \| \quad 0 \quad 1``  |       ✅      |       ✅      |
|  ``1\quad \| \quad 1 \quad 1``  |       ❌      |       ✅      |
(Note: many monomials are excluded for brevity in the above table)

We can define these two GTPSA `Descriptor`s respectively with:

```julia
dp1 = Descriptor([1], 3, [1, 1], 1)
dp2 = Descriptor([1], 3, [1, 1], 2)
```
