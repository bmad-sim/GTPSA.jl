# Partial Derivative Getting/Setting
## Individual Monomial Coefficient
!!! note
    The value of a partial derivative is equal to the monomial coefficient in the Taylor series multiplied by a constant factor. E.g. for an expansion around zero $f(x)\approx f(0) + \frac{\partial f}{\partial x}\rvert_0x + \frac{1}{2!}\frac{\partial^2 f}{\partial x^2}\rvert_0 x^2 + ...$, the 2nd order monomial coefficient is $\frac{1}{2!}\frac{\partial^2 f}{\partial x^2}\rvert_0$. 

Individual monomial coefficients in a TPS can be get/set with two methods of indexing: **by order**, and **by sparse monomial**. 

### By Order
`t[<x_1 order>, ..., <x_nv order>]`. For example, for a TPS with variables $x_1$, $x_2$, the $x_1^3x_2^1$ monomial coefficient is accessed with `t[3,1]`. The 0th order part (the *scalar* part) of the TPS is indexed with `t[0,0]` or equivalently `t[0]`, as leaving out trailing zeros for unincluded variables is allowed.

### By Sparse Monomial
`t[<ix_var> => <order>, ...]`. This method of indexing is convenient when a TPS contains many variables and parameters. For example, for a TPS with variables $x_1,x_2,...x_{100}$, the $x_{1}^3x_{99}^1$ monomial coefficient is accessed with `t[1=>3, 99=>1]`. The scalar part of the TPS cannot be get/set with this method.

## `gradient`, `jacobian`, `hessian`

## Documentation
```@docs
gradient
gradient!
jacobian
jacobian!
hessian
hessian!
```