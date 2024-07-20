# Table of Contents

1. **[Definitions](@ref definitions)**
2. **[`Descriptor`](@ref descriptor)**: Defines the number of variables and parameters, and orders for each in the GTPSA
3. **[`TPS`](@ref tps)**: Truncated Power Series struct
4. **[`ComplexTPS64`](@ref ComplexTPS64)**: Complex Truncated Power Series struct
5. **[`vars`, `params`](@ref varsparams)**: Creates a vector of TPSs corresponding to each variable/parameter in the GTPSA
6. **[`gradient`, `jacobian`, `hessian`](@ref gjh)**: Extracts specific partial derivatives from a TPS
7. **[Monomial Indexing](@ref monoindex)**: Get/set individual monomial coefficients
8. **[`mono`](@ref mono)**: Creates a TPS corresponding to a specific monomial
9. **[Slicing and `par`](@ref slice)**: Indexing a specific polynomial within the TPS
10. **[TPS Methods](@ref tpsmethods)**: Integrate, differentiate, composition, evaluation, etc.
11. **[`@FastGTPSA`](@ref fastgtpsa)**: Speed up evaluation of expressions containing TPSs, transparent to other types
12. **[I/O](@ref io)**: Reading/writing TPSs
13. **[All Overloaded Functions](@ref all)**: List of all overloaded Base functions and more 