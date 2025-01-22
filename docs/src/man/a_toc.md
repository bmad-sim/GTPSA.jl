# Table of Contents
1. **[`Descriptor`](@ref descriptor)**: Defines the number of variables and parameters, and orders for each in the GTPSA
2. **[`TPS`](@ref tps)**: Truncated Power Series struct
3. **[`@vars`, `@params`](@ref varsparams)**: Creates a vector of `TPS`s corresponding to each variable (``\Delta x_i``) or parameter (``\Delta k_j``) in the GTPSA
4. **[Monomial Indexing](@ref monoindex)**: Get/set individual monomial coefficients
5. **[`mono`](@ref mono)**: Creates a `TPS` corresponding to a specific monomial
6. **[`gradient`, `jacobian`, `hessian`](@ref gjh)**: Extracts specific partial derivatives from a `TPS`
7. **[Slicing and `par`](@ref slice)**: Indexing a specific polynomial within a `TPS`
8. **[`TPS`-Specific Functions](@ref tpsmethods)**: Evaluate, compose, translate, integrate, differentiate, invert, etc.
9. **[`@FastGTPSA`/`@FastGTPSA!` Macros](@ref fastgtpsa)**: Speed up evaluation of expressions containing `TPS`s, transparent to other types
10. **[I/O](@ref io)**: Reading/writing `TPS`s
11. **[Global Variables](@ref global)**: Global non-constant variables
12. **[All Overloaded Functions](@ref all)**: List of all overloaded Base functions and more 