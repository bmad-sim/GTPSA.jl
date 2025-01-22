# [`gradient`, `jacobian`, `hessian`](@id gjh)
*Extracts specific partial derivatives from a TPS*
## Syntax
```
grad = GTPSA.gradient(f [, include_params=bool])
GTPSA.gradient!(grad, f [, include_params=bool])

J = GTPSA.jacobian(F [, include_params=bool])
GTPSA.jacobian!(J, F [, include_params=bool])

Jt = GTPSA.jacobiant(F [, include_params=bool])
jacobiant!(Jt, F [, include_params=bool])

H = GTPSA.hessian(f [, include_params=bool])
GTPSA.hessian!(H, f [, include_params=bool])
```

## Description

`grad = GTPSA.gradient(f)` extracts the gradient from the TPS `f`, defined as ``\nabla f``

`GTPSA.gradient!(grad, f)` fills `grad` vector in-place with the gradient extracted from the TPS `f`

------

`J = GTPSA.jacobian(F)` extracts the Jacobian matrix from the array of TPSs `F`, defined as ``J_{ij} = \frac{\partial F_i}{\partial x_j}``

`GTPSA.jacobian!(J, F)` fills the `J` matrix in-place with the Jacobian extracted from the array of TPSs `F`

------

`Jt = GTPSA.jacobiant(F)` extracts the transpose of the Jacobian matrix from the vector of TPSs `F`, with the Jacobian defined as ``J_{ij} = \frac{\partial F_i}{\partial x_j}``

`GTPSA.jacobiant!(Jt, F)` fills the `Jt` matrix in-place with the transpose of the Jacobian extracted from the vector of TPSs `F`

------

`H = GTPSA.hessian(f)` extracts the Hessian matrix from the TPS `f`, defined as ``H_{ij} = \frac{\partial^2 f}{\partial x_i\partial x_j}``

`GTPSA.hessian!(H, f)` fills the `H` matrix in-place with the Hessian extracted from the TPS `f`

------
#### Optional Argument

`include_params` can be set to `true` (default is `false`) to include the partial derivatives with respect to the parameters in any of the extraction functions above.

## Examples

```@repl
using GTPSA; #hide
d = Descriptor(2,10);
Δx = @vars(d);
f = Δx[1] + 2*Δx[2] + 3*Δx[1]^2 + 4*Δx[1]*Δx[2] + 5*Δx[2]^2;
g = 5*Δx[1] + 4*Δx[2] + 3*Δx[1]^2 + 2*Δx[1]*Δx[2] + Δx[2]^2;
grad = GTPSA.gradient(f)
J = GTPSA.jacobian([f, g])
H = GTPSA.hessian(f)
```

## Documentation
```@docs
GTPSA.gradient
GTPSA.gradient!
GTPSA.jacobian
GTPSA.jacobian!
GTPSA.jacobiant
GTPSA.jacobiant!
GTPSA.hessian
GTPSA.hessian!
```