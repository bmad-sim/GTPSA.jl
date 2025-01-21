# Table of Contents
1. **[`Descriptor`](@ref descriptor)**: Defines the number of variables and parameters, and orders for each in the GTPSA
2. **[`TPS`](@ref tps)**: Truncated Power Series struct
3. **[`@vars`, `@params`](@ref varsparams)**: Creates a vector of TPSs corresponding to each variable/parameter in the GTPSA
4. **[`gradient`, `jacobian`, `hessian`](@ref gjh)**: Extracts specific partial derivatives from a TPS
5. **[Monomial Indexing](@ref monoindex)**: Get/set individual monomial coefficients
6. **[`mono`](@ref mono)**: Creates a TPS corresponding to a specific monomial
7. **[Slicing and `par`](@ref slice)**: Indexing a specific polynomial within the TPS
8. **[TPS Methods](@ref tpsmethods)**: Integrate, differentiate, composition, evaluation, etc.
9. **[`@FastGTPSA`/`@FastGTPSA!` Macros](@ref fastgtpsa)**: Speed up evaluation of expressions containing TPSs, transparent to other types
10. **[I/O](@ref io)**: Reading/writing TPSs
11. **[All Overloaded Functions](@ref all)**: List of all overloaded Base functions and more 