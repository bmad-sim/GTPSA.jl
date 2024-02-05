# Table of Contents

1. **[Definitions](@ref definitions)**
2. **[`Descriptor`](@ref descriptor)**: Defines the number of variables and parameters, and orders for each in the GTPSA
3. **[`TPS`/`ComplexTPS`](@ref tpscomplextps)**: Truncated Power Series (TPS) struct
4. **[`vars`, `params`](@ref varsparams)**: Creates a vector of TPSs corresponding to each variable/parameter in the GTPSA
5. **[`gradient`, `jacobian`, `hessian`](@ref gjh)**: Extracts specific partial derivatives from a TPS
6. **[Monomial Indexing](@ref monoindex)**: Get/set individual monomial coefficients
7. **[`mono`](@ref mono)**: Creates a TPS corresponding to a specific monomial
8. **[Slicing and `par`](@ref slice)**: Indexing a specific polynomial within the TPS
9. **[TPS and Map Methods](@ref tpsmapmethods)**: Integrate, differentiate, poisson bracket, map (partial) inversion, etc.
10. **[`@FastGTPSA`](@ref fastgtpsa)**: Speed up evaluation of expressions containing TPSs, transparent to other types
11. **[I/O](@ref io)**: Reading/writing TPSs
12. **[All Overloaded Functions](@ref all)**: List of all overloaded Base functions and more 