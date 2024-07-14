# Low-level functions/structs and constants
# const NAMSZ::Int = 16 
#include("low_level/mono.jl")
#include("low_level/desc.jl")
include("low_level/rtpsa.jl")
include("low_level/ctpsa.jl")

# All non-exported functions straight from GTPSA C library
# Argument orders are 1-to-1 with C library, these functions will 
# generally not be called by the user, and importantly are NOT operators.
# Argument types restricted by low_level functions defined in files included above
# These LHS functions should be defined by any generic TPSA interface
cycle!(t::NewTPS{Float64},    i, n, m_, v_) = mad_tpsa_cycle!( t, Cint(i), Cint(n), m_, v_)
cycle!(t::NewTPS{ComplexF64}, i, n, m_, v_) = mad_ctpsa_cycle!(t, Cint(i), Cint(n), m_, v_)

# Setters
seti!( t::NewTPS{Float64}, i, a, b)       = mad_tpsa_seti!( t, Cint(i), Float64(a),    Float64(b))
seti!( t::NewTPS{ComplexF64}, i, a, b)    = mad_ctpsa_seti!(t, Cint(i), ComplexF64(a), ComplexF64(b))

setm!( t::NewTPS{Float64}, n, m, a, b)    = mad_tpsa_setm!( t, Cint(n), m, Float64(a),    Float64(b))
setm!( t::NewTPS{ComplexF64}, n, m, a, b) = mad_ctpsa_setm!(t, Cint(n), m, ComplexF64(a), ComplexF64(b))

setsm!(t::NewTPS{Float64}, n, m, a, b)    = mad_tpsa_setsm!( t, Cint(n), m, Float64(a),    Float64(b))
setsm!(t::NewTPS{ComplexF64}, n, m, a, b) = mad_ctpsa_setsm!(t, Cint(n), m, ComplexF64(a), ComplexF64(b))

setv!( t::NewTPS{Float64}, i, n, v)       = mad_tpsa_setv!( t, Cint(i), Cint(n), v)
setv!( t::NewTPS{ComplexF64}, i, n, v)    = mad_ctpsa_setv!(t, Cint(i), Cint(n), v)

# Getters
geti(t::NewTPS{Float64}, i)        = mad_tpsa_geti( t, Cint(i))
geti(t::NewTPS{ComplexF64}, i)     = mad_ctpsa_geti(t, Cint(i))

getm(t::NewTPS{Float64}, n, m)     = mad_tpsa_getm( t, Cint(n), m)
getm(t::NewTPS{ComplexF64}, n, m)  = mad_ctpsa_getm(t, Cint(n), m)

getsm(t::NewTPS{Float64}, n, m)    = mad_tpsa_getsm( t, Cint(n), m)
getsm(t::NewTPS{ComplexF64}, n, m) = mad_ctpsa_getsm(t, Cint(n), m)

getv!(t::NewTPS{Float64}, i, n, v)    = mad_tpsa_getv!( t, Cint(i), Cint(n), v)
getv!(t::NewTPS{ComplexF64}, i, n, v) = mad_ctpsa_getv!(t, Cint(i), Cint(n), v)

# Flat index from monomial
idxm(t::NewTPS{Float64}, n, m)    = mad_tpsa_idxm( t, Cint(n), m)
idxm(t::NewTPS{ComplexF64}, n, m) = mad_ctpsa_idxm(t, Cint(n), m)

# TPS number of monomials 
len(t::NewTPS{Float64})    = mad_tpsa_len( t, false)
len(t::NewTPS{ComplexF64}) = mad_ctpsa_len(t, false)

# Check if all monomial coefficients are equal
equ(a::NewTPS{Float64},    b::NewTPS{Float64},    tol_) = mad_tpsa_equ(  a, b, Float64(tol_))
equ(a::NewTPS{ComplexF64}, b::NewTPS{ComplexF64}, tol_) = mad_ctpsa_equ( a, b, Float64(tol_))
equ(a::NewTPS{ComplexF64}, b::NewTPS{Float64},    tol_) = mad_ctpsa_equt(a, b, Float64(tol_))