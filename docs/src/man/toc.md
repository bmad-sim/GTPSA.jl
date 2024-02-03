# Table of Contents

1. **Definitions**
2. **[`Descriptor`](@ref)**: Defines the number of variables, number of parameters, and order(s) for each in the GTPSA
3. **[`TPS`/`ComplexTPS`](@ref)**: Truncated Power Series (TPS) struct
4. **[`vars`, `params`](@ref)**: Creates a vector of TPSs corresponding to each variable/parameter in the GTPSA
5. **[Monomial Indexing](@ref)**: Get/set individual monomial coefficients
6. **[`mono`](@ref)**: Creates a TPS corresponding to a specific monomial
7. **[Slicing a TPS](@ref)**: Indexing a specific polynomial within the TPS
8. **[`gradient`, `jacobian`, `hessian`](@ref)**: Extracts specific partial derivatives from a TPS
