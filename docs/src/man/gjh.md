# `gradient`, `jacobian`, `hessian`
*Extracts specific partial derivatives from a TPS*
## Syntax
```
grad = gradient(f)
gradient!(grad, f)

J = jacobian(F)
jacobian!(J, F)

H = hessian(f)
hessian!(H, f)

(gradient/jacobian/hessian)(!)(..., include_params=true)
```

## Description

`grad = gradient(f)` extracts the gradient from the TPS `f`, defined as $\nabla f$
`gradient!(grad, f)` fills `grad` vector in-place with the gradient extracted from the TPS `f`

------

`J = jacobian(F)` extracts the Jacobian matrix from the vector of TPSs `F`, defined as $J_{i,j} = \frac{\partial F_i}{\partial x_j}$
`jacobian!(J, F)` fills the `J` matrix in-place with the Jacobian extracted from the vector of TPSs `F`

------

`H = hessian(tps)` extracts the Hessian matrix from the TPS `f`, defined as $H_{i,j} = \frac{\partial^2 f}{\partial x_i\partial x_j}$
`hessian!(H, tps)` fills the `H` matrix in-place with the Hessian extracted from the TPS `f`

------
### Optional Argument

The optional keyword argument `include_params`, which by default is set to `false`, can be set to `true` to include the partial derivatives with respect to the parameters in any of the extraction functions above.


## Documentation
```@docs
gradient
gradient!
jacobian
jacobian!
hessian
hessian!
```