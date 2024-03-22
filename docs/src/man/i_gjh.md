# [`gradient`, `jacobian`, `hessian`](@id gjh)
*Extracts specific partial derivatives from a TPS*
## Syntax
```
grad = gradient(f [, include_params=bool])
gradient!(grad, f [, include_params=bool])

J = jacobian(F [, include_params=bool])
jacobian!(J, F [, include_params=bool])

Jt = jacobiant(F [, include_params=bool])
jacobiant!(Jt, F [, include_params=bool])

H = hessian(f [, include_params=bool])
hessian!(H, f [, include_params=bool])
```

## Description

`grad = gradient(f)` extracts the gradient from the TPS `f`, defined as ``\nabla f``

`gradient!(grad, f)` fills `grad` vector in-place with the gradient extracted from the TPS `f`

------

`J = jacobian(F)` extracts the Jacobian matrix from the vector of TPSs `F`, defined as ``J_{ij} = \frac{\partial F_i}{\partial x_j}``

`jacobian!(J, F)` fills the `J` matrix in-place with the Jacobian extracted from the vector of TPSs `F`

------

`Jt = jacobiant(F)` extracts the transpose of the Jacobian matrix from the vector of TPSs `F`, with the Jacobian defined as ``J_{ij} = \frac{\partial F_i}{\partial x_j}``

`jacobiant!(Jt, F)` fills the `Jt` matrix in-place with the transpose of the Jacobian extracted from the vector of TPSs `F`

------

`H = hessian(f)` extracts the Hessian matrix from the TPS `f`, defined as ``H_{ij} = \frac{\partial^2 f}{\partial x_i\partial x_j}``

`hessian!(H, f)` fills the `H` matrix in-place with the Hessian extracted from the TPS `f`

------
#### Optional Argument

`include_params` can be set to `true` (default is `false`) to include the partial derivatives with respect to the parameters in any of the extraction functions above.

## Examples

```@repl
using GTPSA; #hide
d = Descriptor(2,10);
x = vars(d);
f = x[1] + 2*x[2] + 3*x[1]^2 + 4*x[1]*x[2] + 5*x[2]^2;
g = 5*x[1] + 4*x[2] + 3*x[1]^2 + 2*x[1]*x[2] + x[2]^2;
grad = gradient(f)
J = jacobian([f, g])
H = hessian(f)
```

## Documentation
```@docs
gradient
gradient!
jacobian
jacobian!
jacobiant
jacobiant!
hessian
hessian!
```