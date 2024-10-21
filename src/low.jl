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
cycle!(t::RealTPS,    i, n, m_, v_) = mad_tpsa_cycle!( t, Cint(i), Cint(n), m_, v_)
cycle!(t::ComplexTPS, i, n, m_, v_) = mad_ctpsa_cycle!(t, Cint(i), Cint(n), m_, v_)

# Setters
seti!( t::RealTPS, i, a, b)       = mad_tpsa_seti!( t, Cint(i), Float64(a),    Float64(b))
seti!( t::ComplexTPS, i, a, b)    = mad_ctpsa_seti!(t, Cint(i), ComplexF64(a), ComplexF64(b))

setm!( t::RealTPS, n, m, a, b)    = mad_tpsa_setm!( t, Cint(n), m, Float64(a),    Float64(b))
setm!( t::ComplexTPS, n, m, a, b) = mad_ctpsa_setm!(t, Cint(n), m, ComplexF64(a), ComplexF64(b))

setsm!(t::RealTPS, n, m, a, b)    = mad_tpsa_setsm!( t, Cint(n), m, Float64(a),    Float64(b))
setsm!(t::ComplexTPS, n, m, a, b) = mad_ctpsa_setsm!(t, Cint(n), m, ComplexF64(a), ComplexF64(b))

setv!( t::RealTPS, i, n, v)       = mad_tpsa_setv!( t, Cint(i), Cint(n), v)
setv!( t::ComplexTPS, i, n, v)    = mad_ctpsa_setv!(t, Cint(i), Cint(n), v)

# Getters
geti(t::RealTPS, i)        = mad_tpsa_geti( t, Cint(i))
geti(t::ComplexTPS, i)     = mad_ctpsa_geti(t, Cint(i))

getm(t::RealTPS, n, m)     = mad_tpsa_getm( t, Cint(n), m)
getm(t::ComplexTPS, n, m)  = mad_ctpsa_getm(t, Cint(n), m)

getsm(t::RealTPS, n, m)    = mad_tpsa_getsm( t, Cint(n), m)
getsm(t::ComplexTPS, n, m) = mad_ctpsa_getsm(t, Cint(n), m)

getv!(t::RealTPS, i, n, v)    = mad_tpsa_getv!( t, Cint(i), Cint(n), v)
getv!(t::ComplexTPS, i, n, v) = mad_ctpsa_getv!(t, Cint(i), Cint(n), v)

# Flat index from monomial
idxm(t::RealTPS, n, m)    = mad_tpsa_idxm( t, Cint(n), m)
idxm(t::ComplexTPS, n, m) = mad_ctpsa_idxm(t, Cint(n), m)

# TPS number of monomials 
len(t::RealTPS)    = mad_tpsa_len( t, false)
len(t::ComplexTPS) = mad_ctpsa_len(t, false)

# Check if all monomial coefficients are equal
equ(a::RealTPS,    b::RealTPS,    tol_) = mad_tpsa_equ(  a, b, Float64(tol_))
equ(a::ComplexTPS, b::ComplexTPS, tol_) = mad_ctpsa_equ( a, b, Float64(tol_))
equ(a::ComplexTPS, b::RealTPS,    tol_) = mad_ctpsa_equt(a, b, Float64(tol_))

# Lie bracket GTPSA only provides routines for orbital part:
liebra!(na, m1::Vector{TPS{Float64}},    m2::Vector{TPS{Float64}},    m3::Vector{TPS{Float64}})    = GTPSA.mad_tpsa_liebra!(Cint(na), m1, m2, m3)
liebra!(na, m1::Vector{TPS{ComplexF64}}, m2::Vector{TPS{ComplexF64}}, m3::Vector{TPS{ComplexF64}}) = GTPSA.mad_ctpsa_liebra!(Cint(na), m1, m2, m3)


# --- inverse ---
minv!(na, ma::Vector{<:RealTPS},    nb, mc::Vector{<:RealTPS})    = GTPSA.mad_tpsa_minv!(Cint(na), ma, Cint(nb), mc)
minv!(na, ma::Vector{<:ComplexTPS}, nb, mc::Vector{<:ComplexTPS}) = GTPSA.mad_ctpsa_minv!(Cint(na), ma, Cint(nb), mc)
  
vec2fld!(na::Cint, tpsa::TPS{Float64}, m::Vector{<:TPS{Float64}}) = (@inline; GTPSA.mad_tpsa_vec2fld!(na, tpsa, m))
vec2fld!(na::Cint, ctpsa::TPS{ComplexF64}, m::Vector{<:TPS{ComplexF64}}) = (@inline; GTPSA.mad_ctpsa_vec2fld!(na, ctpsa, m))

