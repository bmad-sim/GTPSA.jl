# Low-level functions/structs and constants
const NAMSZ::Int = 16 
include("low_level/mono.jl")
include("low_level/desc.jl")
include("low_level/tps.jl")
include("low_level/rtpsa.jl")
include("low_level/ctpsa.jl")

# All non-exported functions straight from GTPSA C library
# Argument orders are 1-to-1 with C library, these functions will 
# generally not be called by the user, and importantly are NOT operators.
# Argument types restricted by low_level functions defined in files included above
# These LHS functions should be defined by any generic TPSA interface

"""
    cycle!(t::TPS, i, n, m_, v_)

Given a starting index `i` (-1 if starting at 0), will optionally fill monomial `m_`
(if `m != C_NULL`, and `m_` is a `DenseVector{UInt8}`) with the monomial at index `i` 
and optionally the value at `v_` with the monomials coefficient (if `v_ != C_NULL`, 
and `v_` is a `Ref{<:Union{Float64,ComplexF64}}`), and return the next NONZERO monomial 
index in the `TPS`. This is useful for iterating through each monomial in the TPSA.

### Input
- `t`  -- TPS to scan
- `i`  -- Index to start from (-1 to start at 0)
- `n`  -- Length of monomial
- `m_` -- (Optional) Monomial to be filled if provided
- `v_` -- (Optional) Pointer to value of coefficient

### Output
- `i`  -- Index of next nonzero monomial in the TPSA, or -1 if reached the end
"""
cycle!(t::TPS{Float64},    i, n, m_, v_) = mad_tpsa_cycle!( t, Cint(i), Cint(n), m_, v_)
cycle!(t::TPS{ComplexF64}, i, n, m_, v_) = mad_ctpsa_cycle!(t, Cint(i), Cint(n), m_, v_)

# Setters
seti!( t::TPS{Float64}, i, a, b)       = mad_tpsa_seti!( t, Cint(i), Float64(a),    Float64(b))
seti!( t::TPS{ComplexF64}, i, a, b)    = mad_ctpsa_seti!(t, Cint(i), ComplexF64(a), ComplexF64(b))

setm!( t::TPS{Float64}, n, m, a, b)    = mad_tpsa_setm!( t, Cint(n), m, Float64(a),    Float64(b))
setm!( t::TPS{ComplexF64}, n, m, a, b) = mad_ctpsa_setm!(t, Cint(n), m, ComplexF64(a), ComplexF64(b))

setsm!(t::TPS{Float64}, n, m, a, b)    = mad_tpsa_setsm!( t, Cint(n), m, Float64(a),    Float64(b))
setsm!(t::TPS{ComplexF64}, n, m, a, b) = mad_ctpsa_setsm!(t, Cint(n), m, ComplexF64(a), ComplexF64(b))

setv!( t::TPS{Float64}, i, n, v)       = mad_tpsa_setv!( t, Cint(i), Cint(n), v)
setv!( t::TPS{ComplexF64}, i, n, v)    = mad_ctpsa_setv!(t, Cint(i), Cint(n), v)

# Getters
geti(t::TPS{Float64}, i)        = mad_tpsa_geti( t, Cint(i))
geti(t::TPS{ComplexF64}, i)     = mad_ctpsa_geti(t, Cint(i))

getm(t::TPS{Float64}, n, m)     = mad_tpsa_getm( t, Cint(n), m)
getm(t::TPS{ComplexF64}, n, m)  = mad_ctpsa_getm(t, Cint(n), m)

getsm(t::TPS{Float64}, n, m)    = mad_tpsa_getsm( t, Cint(n), m)
getsm(t::TPS{ComplexF64}, n, m) = mad_ctpsa_getsm(t, Cint(n), m)

getv!(t::TPS{Float64}, i, n, v)    = mad_tpsa_getv!( t, Cint(i), Cint(n), v)
getv!(t::TPS{ComplexF64}, i, n, v) = mad_ctpsa_getv!(t, Cint(i), Cint(n), v)

# Flat index from monomial
idxm(t::TPS{Float64}, n, m)    = mad_tpsa_idxm( t, Cint(n), m)
idxm(t::TPS{ComplexF64}, n, m) = mad_ctpsa_idxm(t, Cint(n), m)

# TPS number of monomials 
len(t::TPS{Float64})    = mad_tpsa_len( t, false)
len(t::TPS{ComplexF64}) = mad_ctpsa_len(t, false)

# Check if all monomial coefficients are equal
equ(a::TPS{Float64},    b::TPS{Float64},    tol_) = mad_tpsa_equ(  a, b, Float64(tol_))
equ(a::TPS{ComplexF64}, b::TPS{ComplexF64}, tol_) = mad_ctpsa_equ( a, b, Float64(tol_))
equ(a::TPS{ComplexF64}, b::TPS{Float64},    tol_) = mad_ctpsa_equt(a, b, Float64(tol_))