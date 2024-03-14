# [Definitions](@id definitions)
*This section is incomplete, and will be expanded in the near future*

## [Custom Truncation Orders](@id cto)
GTPSA allows for significant customization in the truncation of specific variables within  a monomial of the truncated power series (TPS). One can specify individually the truncation orders for each variable in a truncated power series, as well as the maximum truncation order for an entire monomial in the TPS. This is best shown with an example:

Suppose we'd like to express a function ``f(x_1,x_2)`` as a truncated power series, and keep only terms that are up to 1st-order in ``x_1`` but up to 2nd order in ``x_2``; basically, we should not have any monomials where ``x_1`` has an exponent > 1, nor any monomials where ``x_2`` has an exponent > 2. GTPSA allows one to select the **individual truncation orders** for variables in a monomial in this manner. The next question to consider is the **maximum truncation order** for the entire monomial; in the above example, note that the 3rd-order term ``x_1x_2^2`` follows the rules we layed out so far. But what if we'd also like to truncate all monomials with order 3 and above, and not allow this monomial? This can be achieved by setting the maximum truncation order equal to 2. When defining a GTPSA, the user must always specify the maximum truncation order, which when specifying individual truncation orders must lie within the range ``[\max{(\textrm{individual truncation orders})}, \sum{(\textrm{individual truncation orders})}]``. If individual truncation orders are not specified, then they are automatically set to the maximum truncation order.

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
