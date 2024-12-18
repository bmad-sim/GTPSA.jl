# All inplace operators (!) are defined for both 
# TPS and TempTPS using TPSType and RealTPS/ComplexTPS

# Internal constant to aid multiple dispatch
const TPSType    = Union{TempTPS, TPS}

# --- clear! ---
"""
    clear!(t::TPS) 

Clears all monomial coefficients in the `TPS` (sets to 0).
"""
clear!(t::RealTPS)    = mad_tpsa_clear!(t)
clear!(t::ComplexTPS) = mad_ctpsa_clear!(t)

# --- copy! ---
copy!(t::RealTPS, t1::RealTPS)       = (@inline; mad_tpsa_copy!(t1, t); return t)
copy!(t::ComplexTPS, t1::ComplexTPS) = (@inline; mad_ctpsa_copy!(t1, t); return t)
copy!(t::ComplexTPS, t1::RealTPS)    = (@inline; mad_ctpsa_cplx!(t1, C_NULL, t); return t)

# --- mul! ---
# TPS, TPS:
mul!(c::RealTPS,    a::RealTPS,    b::RealTPS)    = mad_tpsa_mul!(a, b, c)
mul!(c::ComplexTPS, a::ComplexTPS, b::ComplexTPS) = mad_ctpsa_mul!(a, b, c)
mul!(c::ComplexTPS, a::ComplexTPS, b::RealTPS)    = mad_ctpsa_mult!(a, b, c)
mul!(c::ComplexTPS, a::RealTPS,    b::ComplexTPS) = mad_ctpsa_mult!(b, a, c)
# TPS, scalar:
mul!(c::RealTPS,    a::RealTPS,    v) = mad_tpsa_scl!( a, Float64(v),    c)
mul!(c::ComplexTPS, a::ComplexTPS, v) = mad_ctpsa_scl!(a, ComplexF64(v), c)
mul!(c::ComplexTPS, a::RealTPS,    v) = (copy!(c, a); mul!(c, c, v))
mul!(c::TPSType, v, a::TPSType) = mul!(c, a, v)

# --- add! ---
# TPS, TPS:
add!(c::RealTPS,    a::RealTPS,    b::RealTPS)    = mad_tpsa_add!(a, b, c)
add!(c::ComplexTPS, a::ComplexTPS, b::ComplexTPS) = mad_ctpsa_add!(a, b, c)
add!(c::ComplexTPS, a::ComplexTPS, b::RealTPS)    = mad_ctpsa_addt!(a, b, c)
add!(c::ComplexTPS, a::RealTPS,    b::ComplexTPS) = mad_ctpsa_addt!(b, a, c)
# TPS, scalar:
add!(c::TPSType, a::TPSType, v) = (copy!(c, a); seti!(c, 0, 1, v))
add!(c::TPSType, v, a::TPSType) = add!(c, a, v)

# --- sub! ---
# TPS, TPS:
sub!(c::RealTPS,    a::RealTPS,    b::RealTPS)    = mad_tpsa_sub!(a, b, c)
sub!(c::ComplexTPS, a::ComplexTPS, b::ComplexTPS) = mad_ctpsa_sub!(a, b, c)
sub!(c::ComplexTPS, a::ComplexTPS, b::RealTPS)    = mad_ctpsa_subt!(a, b, c)
sub!(c::ComplexTPS, a::RealTPS,    b::ComplexTPS) = mad_ctpsa_tsub!(a, b, c)
# TPS, scalar:
sub!(c::TPSType, a::TPSType, v) = add!(c, a, -v)
sub!(c::TPSType, v, a::TPSType) = (mul!(c, -1, a); seti!(c, 0, 1, v))

# --- div!/inv! ---
# TPS, TPS:
div!(c::RealTPS,    a::RealTPS,    b::RealTPS)    = mad_tpsa_div!(a, b, c)
div!(c::ComplexTPS, a::ComplexTPS, b::ComplexTPS) = mad_ctpsa_div!(a, b, c)
div!(c::ComplexTPS, a::ComplexTPS, b::RealTPS)    = mad_ctpsa_divt!(a, b, c)
div!(c::ComplexTPS, a::RealTPS,    b::ComplexTPS) = mad_ctpsa_tdiv!(a, b, c)
# TPS, scalar:
inv!(c::RealTPS,    a::RealTPS,     v=1) = mad_tpsa_inv!(a, Float64(v), c)
inv!(c::ComplexTPS, a::ComplexTPS,  v=1) = mad_ctpsa_inv!(a, ComplexF64(v), c)
inv!(c::ComplexTPS, a::RealTPS,     v=1) = (copy!(c, a); inv!(c, c, v))
div!(c::TPSType, a::TPSType, v) = mul!(c, a, 1/v)
div!(c::TPSType, v, a::TPSType) = inv!(c, a, v)

# --- pow! ---
# TPS, TPS:
pow!(c::RealTPS,    a::RealTPS,    b::RealTPS)    = mad_tpsa_pow!(a, b, c)
pow!(c::ComplexTPS, a::ComplexTPS, b::ComplexTPS) = mad_ctpsa_pow!(a, b, c)
pow!(c::ComplexTPS, a::ComplexTPS, b::RealTPS)    = mad_ctpsa_powt!(a, b, c)
pow!(c::ComplexTPS, a::RealTPS,    b::ComplexTPS) = mad_ctpsa_tpow!(a, b, c)
# TPS, scalar:
pow!(c::RealTPS,    a::RealTPS, v)    = mad_tpsa_pown!(a, Float64(v), c)
pow!(c::ComplexTPS, a::ComplexTPS, v) = mad_ctpsa_pown!(a, ComplexF64(v), c)
pow!(c::RealTPS,    a::RealTPS,    v::Integer) = mad_tpsa_powi!(a, Cint(v), c)
pow!(c::ComplexTPS, a::ComplexTPS, v::Integer) = mad_ctpsa_powi!(a, Cint(v), c)
pow!(c::ComplexTPS, a::RealTPS, v) = (copy!(c,a); pow!(c, c, v))
pow!(c::TPSType, v, a::TPSType) = (mul!(c, log(v), a); exp!(c,c))

# --- rest of basic unary functions ---
# For unary functions we assume input type == output type unless otherwise specified
for t = (:unit, :sqrt, :exp, :log, :sin, :cos, :tan, :cot, :sinh, :cosh, :tanh, 
  :coth, :asin, :acos, :atan, :acot, :asinh, :acosh, :atanh, :acoth, :erf, :erfc)
@eval begin
$(Symbol(t,:!))(t::RealTPS,    t1::RealTPS)    = $(Symbol("mad_tpsa_",t,:!))(t1, t)
$(Symbol(t,:!))(t::ComplexTPS, t1::ComplexTPS) = $(Symbol("mad_ctpsa_",t,:!))(t1, t)
end
end

atan!(c::RealTPS, a::RealTPS, b::RealTPS) = mad_tpsa_atan2!(a, b, c)

# sinc in Julia has different definition than GTPSA
# In Julia: sinc(x) = sin(pi*x)/(pi*x)
# in C GTPSA: sinc(x) = sin(x)/x
# To make sinc agree:
sinc!(t::RealTPS,    t1::RealTPS)    = (mul!(t, t1, pi); mad_tpsa_sinc!(t, t))
sinc!(t::ComplexTPS, t1::ComplexTPS) = (mul!(t, t1, pi); mad_ctpsa_sinc!(t, t))

sinhc!(t::RealTPS,    t1::RealTPS)    = (mul!(t, t1, pi); mad_tpsa_sinhc!(t, t))
sinhc!(t::ComplexTPS, t1::ComplexTPS) = (mul!(t, t1, pi); mad_ctpsa_sinhc!(t, t))

# u = unnormalized sinc
sincu!(t::RealTPS,    t1::RealTPS)    = mad_tpsa_sinc!(t1, t)
sincu!(t::ComplexTPS, t1::ComplexTPS) = mad_ctpsa_sinc!(t1, t)

sinhcu!(t::RealTPS,    t1::RealTPS)    = mad_tpsa_sinhc!(t1, t)
sinhcu!(t::ComplexTPS, t1::ComplexTPS) = mad_ctpsa_sinhc!(t1, t)

# asinc is not in Julia, but in C is asinc(x) = asin(x)/x
# To give similiar behavior, define asinc(x) = asin(pi*x)/(pi*x)
asinc!(t::RealTPS,    t1::RealTPS)    = (mul!(t, t1, pi); mad_tpsa_asinc!(t, t))
asinc!(t::ComplexTPS, t1::ComplexTPS) = (mul!(t, t1, pi); mad_ctpsa_asinc!(t, t))

asinhc!(t::RealTPS,    t1::RealTPS)    = (mul!(t, t1, pi); mad_tpsa_asinhc!(t, t))
asinhc!(t::ComplexTPS, t1::ComplexTPS) = (mul!(t, t1, pi); mad_ctpsa_asinhc!(t, t))

asincu!(t::RealTPS,    t1::RealTPS)    = mad_tpsa_asinc!(t1, t)
asincu!(t::ComplexTPS, t1::ComplexTPS) = mad_ctpsa_asinc!(t1, t)

asinhcu!(t::RealTPS,    t1::RealTPS)    = mad_tpsa_asinhc!(t1, t)
asinhcu!(t::ComplexTPS, t1::ComplexTPS) = mad_ctpsa_asinhc!(t1, t)

# These functions are not implemented in the GTPSA C library:
csc!(t::TPSType, t1::TPSType) = (sin!(t, t1); inv!(t, t, 1))
csch!(t::TPSType, t1::TPSType) = (sinh!(t, t1); inv!(t, t, 1))
acsc!(t::TPSType, t1::TPSType) = (inv!(t, t1, 1); asin!(t, t))
acsch!(t::TPSType, t1::TPSType) = (inv!(t, t1, 1); asinh!(t, t))

sec!(t::TPSType, t1::TPSType) = (cos!(t, t1); inv!(t, t, 1))
sech!(t::TPSType, t1::TPSType) = (cosh!(t, t1); inv!(t, t, 1))
asec!(t::TPSType, t1::TPSType) = (inv!(t, t1, 1); acos!(t, t))
asech!(t::TPSType, t1::TPSType) = (inv!(t, t1, 1); acosh!(t, t))


# --- special functions for complex TPS's ---
conj!(t::ComplexTPS, t1::ComplexTPS) = mad_ctpsa_conj!(t1, t)
conj!(t::TPSType, t1::RealTPS) = copy!(t, t1)

rect!(t::ComplexTPS, t1::ComplexTPS) = mad_ctpsa_rect!(t1, t)
rect!(t::RealTPS, t1::RealTPS) = copy!(t,t1)

# --- Unary functions that return RealTPS ---
real!(t::RealTPS, t1::ComplexTPS) = mad_ctpsa_real!(t1, t)
real!(t::RealTPS, t1::RealTPS)    = copy!(t, t1) 

imag!(t::RealTPS, t1::ComplexTPS) = mad_ctpsa_imag!(t1, t)
imag!(t::RealTPS, t1::RealTPS)    = clear!(t);

abs!(t::RealTPS,    t1::RealTPS) = mad_tpsa_abs!(t1, t)
abs!(t::RealTPS, t1::ComplexTPS) = mad_ctpsa_cabs!(t1, t)

angle!(t::RealTPS, t1::ComplexTPS) = mad_ctpsa_carg!(t1, t)
@inline function angle!(t::RealTPS, t1::RealTPS)
  if geti(t1, 0) < 0 
    clear!(t)
    seti!(t, 0, 0, pi)
    return
  else
    clear!(t)
    return
  end
end      
      
# --- Unary functions that return ComplexTPS ---
polar!(t::ComplexTPS, t1::ComplexTPS) = mad_ctpsa_polar!(t1, t)
polar!(t::ComplexTPS, t1::RealTPS) = (copy!(t,t1); polar!(t, t) )

"""
    complex!(t::ComplexTPS64; tre=nothing, tim=nothing) 

Sets `t` to so that `real(t)=tre` and `imag(t)=tim` in place.
"""
complex!(t::ComplexTPS; tre=nothing, tim=nothing) = low_cplx!(t, tre, tim)

# TPS:
function low_cplx!(t::ComplexTPS, tre::Union{RealTPS,Nothing}, tim::Union{RealTPS,Nothing})
  return mad_ctpsa_cplx!(!isnothing(tre) ? tre : C_NULL, !isnothing(tim) ? tim : C_NULL, t)
end

# TPS, Number:
function low_cplx!(t::ComplexTPS, tre::Union{RealTPS,Nothing}, tim::Real)
  mad_ctpsa_cplx!(!isnothing(tre) ? tre : C_NULL, C_NULL, t)
  seti!(t, 0, 1, complex(0,tim))
end

# Number, TPS:
function low_cplx!(t::ComplexTPS, tre::Real, tim::Union{RealTPS,Nothing})
  mad_ctpsa_cplx!(C_NULL, !isnothing(tim) ? tim : t)
  seti!(t, 0, 1, tre)
end

# Number, Number
function low_cplx!(t::ComplexTPS, tre::Real, tim::Real)
  clear!(t)
  seti!(t, 0, 1, complex(tre,tim))
end

# --- hypot ---
hypot!(r::RealTPS, a::RealTPS, b::RealTPS) = mad_tpsa_hypot!(a, b, r)
hypot!(r::RealTPS, a::RealTPS, b::RealTPS, c::RealTPS) = mad_tpsa_hypot3!(a, b, c, r)

function hypot!(d::TPSType, a...)
  clear!(d)
  for n in a
    add!(d, d, abs(n)^2)
  end
  sqrt!(d,d)
end

# --- extras ---
log10!(t::RealTPS,    t1::RealTPS)    = (log!(t, t1); div!(t, t, log(10)))
log10!(t::ComplexTPS, t1::ComplexTPS) = (log!(t, t1); div!(t, t, log(10)))