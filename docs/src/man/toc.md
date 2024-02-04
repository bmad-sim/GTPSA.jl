# Table of Contents

1. **Definitions**
2. **[`Descriptor`](@ref)**: Defines the number of variables, number of parameters, and order(s) for each in the GTPSA
3. **[`TPS`/`ComplexTPS`](@ref)**: Truncated Power Series (TPS) struct
4. **[`vars`, `params`](@ref)**: Creates a vector of TPSs corresponding to each variable/parameter in the GTPSA
5. **[`gradient`, `jacobian`, `hessian`](@ref)**: Extracts specific partial derivatives from a TPS
6. **[Monomial Indexing](@ref)**: Get/set individual monomial coefficients
7. **[`mono`](@ref)**: Creates a TPS corresponding to a specific monomial
8. **[Slicing and `par`](@ref)**: Indexing a specific polynomial within the TPS
9. **[TPS and Map Methods](@ref)**: Integrate, differentiate, poisson bracket, map (partial) inversion, composition, etc.
10. **[`@FastGTPSA`](@ref)**: A macro to speed up evaluation of expressions containing TPSs, transparent to other types
11. **[I/O](@ref)**: Reading/writing TPSs
12. **[All Overloaded Functions](@ref)**: List of all overloaded Base functions and more 