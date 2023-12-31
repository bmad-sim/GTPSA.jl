# --- Unary ---
# TPS:
@inline function +(t1::TPS)::TPS
  t = TPS(t1)
  return t
end

@inline function -(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_scl!(t1.tpsa, -1., t.tpsa)
  return t
end

# ComplexTPS:
@inline function +(ct1::ComplexTPS)::ComplexTPS
  ct = ComplexTPS(ct1)
  return ct
end

@inline function -(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, -1.0+0.0*im, ct.tpsa)
  return ct
end

@inline function real(ct1::ComplexTPS)::TPS
  t = TPS(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA}, ct1.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_real!(ct1.tpsa, t.tpsa)
  return t
end

@inline function imag(ct1::ComplexTPS)::TPS
  t = TPS(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA}, ct1.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_imag!(ct1.tpsa, t.tpsa)
  return t
end

@inline function real(t1::TPS)::TPS
  return TPS(t1)
end

@inline function imag(t1::TPS)::TPS
  return zero(t1)
end


# --- Compare ---
# TPS:
@inline function ==(t1::TPS, t2::TPS)::Bool
  return convert(Bool, mad_tpsa_equ(t1.tpsa, t2.tpsa, convert(Cdouble, 0.)))
end

@inline function ==(t1::TPS, a::Real)::Bool
  t2 = TPS(a, t1)
  return t1 == t2
end

@inline function ==(a::Real, t1::TPS)::Bool
  return t1 == a
end

# ComplexTPS:
@inline function ==(ct1::ComplexTPS, ct2::ComplexTPS)::Bool
  return convert(Bool, mad_ctpsa_equ(ct1.tpsa, ct2.tpsa, convert(Cdouble, 0.)))
end

@inline function ==(ct1::ComplexTPS, a::Number)::Bool
  ct2 = ComplexTPS(a, ct1)
  return ct1 == ct2
end

@inline function ==(a::Number, ct1::ComplexTPS)::Bool
  return ct1 == a
end

# TPS/ComplexTPS and Real/Complex conversion:
@inline function ==(t1::TPS, a::Complex)::Bool
  if (imag(a) != 0)
    return false
  else
    return t1 == real(a)
  end
end

@inline function ==(a::Complex, t1::TPS)::Bool
  return t1 == a
end

@inline function ==(ct1::ComplexTPS, t1::TPS)::Bool
  ct2 = ComplexTPS(t1)
  return ct1 == ct2
end

@inline function ==(t1::TPS, ct1::ComplexTPS)::Bool
  return ct1 == t1
end


# --- add ---
# TPS:
@inline function +(t1::TPS, t2::TPS)::TPS
  t = zero(t1)
  mad_tpsa_add!(t1.tpsa, t2.tpsa, t.tpsa)
  return t
end

@inline function +(t1::TPS, a::Real)::TPS
  t = TPS(t1)
  mad_tpsa_set0!(t.tpsa, 1., convert(Float64,a))
  return t
end

@inline function +(a::Real, t1::TPS)::TPS
  return t1 + a
end

# ComplexTPS:
@inline function +(ct1::ComplexTPS, ct2::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_add!(ct1.tpsa, ct2.tpsa, ct.tpsa)
  return ct
end

@inline function +(ct1::ComplexTPS, a::Number)::ComplexTPS
  ct = ComplexTPS(ct1)
  mad_ctpsa_set0!(ct.tpsa, 1.0+0.0*im, convert(ComplexF64, a))
  return ct
end

@inline function +(a::Number, ct1::ComplexTPS)::ComplexTPS
  return ct1 + a
end

# TPS to ComplexTPS promotion, w/o creating temp ComplexTPS:
@inline function +(ct1::ComplexTPS, t1::TPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_add!(ct1.tpsa, ct.tpsa, ct.tpsa)
  return ct
end

@inline function +(t1::TPS, ct1::ComplexTPS)::ComplexTPS
  return ct1 + t1
end

@inline function +(t1::TPS, a::Complex)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_set0!(ct.tpsa, 1.0+0.0*im, convert(ComplexF64, a))
  return ct
end

@inline function +(a::Complex, t1::TPS)::ComplexTPS
  return t1 + a
end


# --- sub ---
# TPS:
@inline function -(t1::TPS, t2::TPS)::TPS
  t = zero(t1)
  mad_tpsa_sub!(t1.tpsa, t2.tpsa, t.tpsa)
  return t
end


@inline function -(t1::TPS, a::Real)::TPS
  t = TPS(t1)
  mad_tpsa_set0!(t.tpsa, 1., convert(Float64, -a))
  return t
end

@inline function -(a::Real, t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_scl!(t1.tpsa, -1., t.tpsa)
  mad_tpsa_set0!(t.tpsa, 1., convert(Float64, a))
  return t
end

# ComplexTPS:
@inline function -(ct1::ComplexTPS, ct2::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_sub!(ct1.tpsa, ct2.tpsa, ct.tpsa)
  return ct
end

@inline function -(ct1::ComplexTPS, a::Number)::ComplexTPS
  ct = ComplexTPS(ct1)
  mad_ctpsa_set0!(ct.tpsa, 1.0+0.0*im, convert(ComplexF64, -a))
  return ct
end

@inline function -(a::Number, ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, -1.0+0.0*im, ct.tpsa)
  mad_ctpsa_set0!(ct.tpsa, 1.0+0.0*im, convert(ComplexF64,a))
  return ct
end

# TPS to ComplexTPS promotion, w/o creating temp ComplexTPS:
@inline function -(ct1::ComplexTPS, t1::TPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_sub!(ct1.tpsa, ct.tpsa, ct.tpsa)
  return ct
end

@inline function -(t1::TPS, ct1::ComplexTPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_sub!(ct.tpsa, ct1.tpsa, ct.tpsa)
  return ct
end

@inline function -(t1::TPS, a::Complex)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_set0!(ct.tpsa, 1.0+0.0*im, convert(ComplexF64, -a))
  return ct
end

@inline function -(a::Complex, t1::TPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_scl!(ct.tpsa, -1.0+0.0*im, ct.tpsa)
  mad_ctpsa_set0!(ct.tpsa, 1.0+0.0*im, convert(ComplexF64,a))
  return ct
end


# --- mul ---
# TPS:
@inline function *(t1::TPS, t2::TPS)::TPS
  t = zero(t1)
  mad_tpsa_mul!(t1.tpsa, t2.tpsa, t.tpsa) 
  return t
end

@inline function *(t1::TPS, a::Real)::TPS
  t = zero(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Float64, a), t.tpsa)
  return t
end

@inline function *(a::Real, t1::TPS)::TPS
  return t1 * a
end

# ComplexTPS:
@inline function *(ct1::ComplexTPS, ct2::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_mul!(ct1.tpsa, ct2.tpsa, ct.tpsa)
  return ct
end

@inline function *(ct1::ComplexTPS, a::Number)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64,a), ct.tpsa)
  return ct
end

@inline function *(a::Number, ct1::ComplexTPS)::ComplexTPS
  return ct1 * a
end

# TPS to ComplexTPS promotion, w/o creating temp ComplexTPS:
@inline function *(ct1::ComplexTPS, t1::TPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_mul!(ct1.tpsa, ct.tpsa, ct.tpsa)
  return ct
end

@inline function *(t1::TPS, ct1::ComplexTPS)::ComplexTPS
  return ct1 * t1
end

@inline function *(t1::TPS, a::Complex)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_scl!(ct.tpsa, convert(ComplexF64,a), ct.tpsa)
  return ct
end

@inline function *(a::Complex, t1::TPS)::ComplexTPS
  return t1 * a
end


# --- div ---
# TPS:
@inline function /(t1::TPS, t2::TPS)::TPS
  t = zero(t1)
  mad_tpsa_div!(t1.tpsa, t2.tpsa, t.tpsa)
  return t
end

@inline function /(t1::TPS, a::Real)::TPS
  t = zero(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Float64, 1/a), t.tpsa)
  return t
end

@inline function /(a::Real, t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_inv!(t1.tpsa, convert(Float64,a), t.tpsa)
  return t
end

# ComplexTPS:
@inline function /(ct1::ComplexTPS, ct2::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_div!(ct1.tpsa, ct2.tpsa, ct.tpsa)
  return ct
end

@inline function /(ct1::ComplexTPS, a::Number)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, 1/a), ct.tpsa)
  return ct
end

@inline function /(a::Number, ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_inv!(ct1.tpsa, convert(ComplexF64, a), ct.tpsa)
  return ct
end

# TPS to ComplexTPS promotion, w/o creating temp ComplexTPS:
@inline function /(ct1::ComplexTPS, t1::TPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_div!(ct1.tpsa, ct.tpsa, ct.tpsa)
  return ct
end

@inline function /(t1::TPS, ct1::ComplexTPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_div!(ct.tpsa, ct1.tpsa, ct.tpsa)
  return ct
end

@inline function /(t1::TPS, a::Complex)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_scl!(ct.tpsa, convert(ComplexF64, 1/a), ct.tpsa)
  return ct
end

@inline function /(a::Complex, t1::TPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_inv!(ct.tpsa, convert(ComplexF64, a), ct.tpsa)
  return ct
end


# --- pow ---
# TPS:
@inline function ^(t1::TPS, t2::TPS)::TPS
  t = zero(t1)
  mad_tpsa_pow!(t1.tpsa, t2.tpsa, t.tpsa)
  return t
end

@inline function ^(t1::TPS, i::Integer)::TPS
  t = zero(t1)
  mad_tpsa_powi!(t1.tpsa, convert(Cint, i), t.tpsa)
  return t
end

@inline function ^(t1::TPS, a::Real)::TPS
  t = zero(t1)
  mad_tpsa_pown!(t1.tpsa, convert(Float64, a), t.tpsa)
  return t
end

@inline function ^(a::Real, t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Float64, log(a)), t.tpsa)
  mad_tpsa_exp!(t.tpsa, t.tpsa)
  return t
end

@inline function inv(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_inv!(t1.tpsa, convert(Cdouble, 1), t.tpsa)
  return t
end

# ComplexTPS:
@inline function ^(ct1::ComplexTPS, ct2::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_pow!(ct1.tpsa, ct2.tpsa, ct.tpsa)
  return ct
end

@inline function ^(ct1::ComplexTPS, i::Integer)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_powi!(ct1.tpsa, convert(Cint, i), ct.tpsa)
  return ct
end

@inline function ^(ct1::ComplexTPS, a::Number)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_pown!(ct1.tpsa, convert(ComplexF64, a), ct.tpsa)
  return ct
end

@inline function ^(a::Number, ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, log(a)), ct.tpsa)
  mad_ctpsa_exp!(ct.tpsa, ct.tpsa)
  return ct
end

@inline function inv(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_inv!(ct1.tpsa, convert(ComplexF64, 1), ct.tpsa)
  return ct
end

# TPS to ComplexTPS promotion, w/o creating temp ComplexTPS:
@inline function ^(ct1::ComplexTPS, t1::TPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_pow!(ct1.tpsa, ct.tpsa, ct.tpsa)
  return ct
end

@inline function ^(t1::TPS, ct1::ComplexTPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_pow!(ct.tpsa, ct1.tpsa, ct.tpsa)
  return ct
end

@inline function ^(t1::TPS, a::Complex)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_pown!(ct.tpsa, convert(ComplexF64, a), ct.tpsa)
  return ct
end

@inline function ^(a::Complex, t1::TPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_scl!(ct.tpsa, convert(ComplexF64, log(a)), ct.tpsa)
  mad_ctpsa_exp!(ct.tpsa, ct.tpsa)
  return ct
end


# --- atan2, hypot, norm ---
# TPS:
@inline function atan(t1::TPS, t2::TPS)::TPS
  t = zero(t1)
  mad_tpsa_atan2!(t1.tpsa, t2.tpsa, t.tpsa)
  return t
end

@inline function atan(t1::TPS, a::Real)::TPS
  t = TPS(a, t1)
  mad_tpsa_atan2!(t1.tpsa, t.tpsa, t.tpsa)
  return t
end

@inline function atan(a::Real, t1::TPS)::TPS
  t = TPS(a, t1)
  mad_tpsa_atan2!(t.tpsa, t1.tpsa, t.tpsa)
  return t
end

@inline function hypot(t1::TPS, t2::TPS)::TPS
  t = zero(t1)
  mad_tpsa_hypot!(t1.tpsa, t2.tpsa, t.tpsa)
  return t
end

@inline function hypot(t1::TPS, a::Number)::TPS
  t = TPS(abs(a), t1)
  mad_tpsa_hypot!(t1.tpsa, t.tpsa, t.tpsa)
  return t
end

@inline function hypot(a::Number, t1::TPS)::TPS
  return hypot(t1, a)
end

@inline function hypot(t1::TPS, t2::TPS, t3::TPS)::TPS
  t = zero(t1)
  mad_tpsa_hypot3!(t1.tpsa, t2.tpsa, t3.tpsa, t.tpsa)
  return t
end

@inline function hypot(t1::TPS, t2::TPS, a::Number)::TPS
  t3 = TPS(abs(a), t1)
  return hypot(t1, t2, t3)
end

@inline function hypot(t1::TPS, a::Number, t2::TPS)::TPS
  return hypot(t1, t2, a)
end

@inline function hypot(a::Number, t1::TPS, t2::TPS)::TPS
  return hypot(t1, t2, a)
end

@inline function hypot(t1::TPS, a::Number, b::Number)::TPS
  t2 = TPS(abs(a), t1)
  return hypot(t1, t2, b)
end

@inline function hypot(a::Number, t1::TPS, b::Number)::TPS
  return hypot(t1, a, b)
end

@inline function hypot(a::Number, b::Number, t1::TPS)::TPS
  return hypot(t1, a, b)
end

"""
    norm(t1::TPS)::Float64

Calculates the 1-norm of the `TPS`, which is the sum of 
the `abs` of all coefficients.
"""
@inline function norm(t1::TPS)::Float64
  return mad_tpsa_nrm(t1.tpsa)
end


# ComplexTPS:
@inline function hypot(ct1::ComplexTPS, ct2::ComplexTPS)::TPS
  t1 = TPS(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA}, ct1.tpsa), MAD_TPSA_SAME))
  t = zero(t1)
  mad_ctpsa_cabs!(ct1.tpsa, t1.tpsa)
  mad_ctpsa_cabs!(ct2.tpsa, t.tpsa)
  mad_tpsa_hypot!(t1.tpsa, t.tpsa, t.tpsa)
  return t
end

@inline function hypot(ct1::ComplexTPS, a::Number)::TPS
  t1 = TPS(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA}, ct1.tpsa), MAD_TPSA_SAME)) 
  t = zero(t1)
  t[0] = abs(a)
  mad_ctpsa_cabs!(ct1.tpsa, t1.tpsa)
  mad_tpsa_hypot!(t1.tpsa, t.tpsa, t.tpsa)
  return t
end

@inline function hypot(a::Number, ct1::ComplexTPS)::TPS
  return hypot(ct1, a)
end

@inline function hypot(ct1::ComplexTPS, ct2::ComplexTPS, ct3::ComplexTPS)::TPS
  t1 = TPS(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA}, ct1.tpsa), MAD_TPSA_SAME))
  t2 = zero(t1)
  t3 = zero(t1)
  t = zero(t1)
  mad_ctpsa_cabs!(ct1.tpsa, t1.tpsa)
  mad_ctpsa_cabs!(ct2.tpsa, t2.tpsa)
  mad_ctpsa_cabs!(ct3.tpsa, t3.tpsa)
  mad_tpsa_hypot3!(t1.tpsa, t2.tpsa, t3.tpsa, t.tpsa)
  return t
end

@inline function hypot(ct1::ComplexTPS, ct2::ComplexTPS, a::Number)::TPS
  t1 = TPS(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA}, ct1.tpsa), MAD_TPSA_SAME))
  t2 = zero(t1)
  t3 = zero(t1)
  t3[0] = abs(a)
  t = zero(t1)
  mad_ctpsa_cabs!(ct1.tpsa, t1.tpsa)
  mad_ctpsa_cabs!(ct2.tpsa, t2.tpsa)
  mad_tpsa_hypot3!(t1.tpsa, t2.tpsa, t3.tpsa, t.tpsa)
  return t
end

@inline function hypot(ct1::ComplexTPS, a::Number, ct2::ComplexTPS)::TPS
  return hypot(ct1, ct2, a)
end

@inline function hypot(a::Number, ct1::ComplexTPS, ct2::ComplexTPS)::TPS
  return hypot(ct1, ct2, a)
end

@inline function hypot(ct1::ComplexTPS, a::Number, b::Number)::TPS
  t1 = TPS(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA}, ct1.tpsa), MAD_TPSA_SAME))
  t2 = zero(t1)
  t3 = zero(t1)
  t = zero(t1)
  t2[0] = abs(a)
  t3[0] = abs(b)
  mad_ctpsa_cabs!(ct1.tpsa, t1.tpsa)
  mad_tpsa_hypot3!(t1.tpsa, t2.tpsa, t3.tpsa, t.tpsa)
  return t
end

@inline function hypot(a::Number, ct1::ComplexTPS, b::Number)::TPS
  return hypot(ct1, a, b)
end

@inline function hypot(a::Number, b::Number, ct1::ComplexTPS)::TPS
  return hypot(ct1, a, b)
end

"""
    norm(t1::ComplexTPS)::Float64

Calculates the 1-norm of the `ComplexTPS`, which is the sum of 
the `abs` of all coefficients.
"""
@inline function norm(ct1::ComplexTPS)::Float64
  return mad_ctpsa_nrm(ct1.tpsa)
end

# Hypot mixing ComplexTPS and TPS w/o unnecessary temporaries
@inline function hypot(ct1::ComplexTPS, t1::TPS)::TPS
  t = zero(t1)
  mad_ctpsa_cabs!(ct1.tpsa, t.tpsa)
  mad_ctpsa_hypot!(t.tpsa, t1.tpsa, t.tpsa)
  return t
end

@inline function hypot(t1::TPS, ct1::ComplexTPS)::TPS
  return hypot(ct1, t1)
end

@inline function hypot(ct1::ComplexTPS, ct2::ComplexTPS, t1::TPS)::TPS
  t2 = zero(t1)
  t3 = zero(t1)
  t = zero(t1)
  mad_ctpsa_cabs!(ct1.tpsa, t2.tpsa)
  mad_ctpsa_cabs!(ct2.tpsa, t3.tpsa)
  mad_tpsa_hypot3!(t1.tpsa, t2.tpsa, t3.tpsa, t.tpsa)
  return t
end

@inline function hypot(ct1::ComplexTPS, t1::TPS, ct2::ComplexTPS)::TPS
  return hypot(ct1, ct2, t1)
end

@inline function hypot(t1::TPS, ct1::ComplexTPS, ct2::ComplexTPS)::TPS
  return hypot(ct1, ct2, t1)
end

@inline function hypot(ct1::ComplexTPS, t1::TPS, t2::TPS)::TPS
  t3 = zero(t1)
  t = zero(t1)
  mad_ctpsa_cabs!(ct1.tpsa, t3.tpsa)
  mad_tpsa_hypot3!(t1.tpsa, t2.tpsa, t3.tpsa, t.tpsa)
  return t
end

@inline function hypot(t1::TPS, ct1::ComplexTPS, t2::TPS)::TPS
  return hypot(ct1, t1, t2)
end

@inline function hypot(t1::TPS, t2::TPS, ct1::ComplexTPS)::TPS
  return hypot(ct1, t1, t2)
end

@inline function hypot(ct1::ComplexTPS, t1::TPS, a::Number)::TPS
  t2 = zero(t1)
  t3 = zero(t1)
  t3[0] = abs(a)
  t = zero(t1)
  mad_ctpsa_cabs!(ct1.tpsa, t2.tpsa)
  mad_tpsa_hypot3!(t1.tpsa, t2.tpsa, t3.tpsa, t.tpsa)
  return t
end

@inline function hypot(t1::TPS, ct1::ComplexTPS, a::Number)::TPS
  return hypot(ct1, t1, a)
end

@inline function hypot(ct1::ComplexTPS, a::Number, t1::TPS)::TPS
  return hypot(ct1, t1, a)
end

@inline function hypot(t1::TPS, a::Number, ct1::ComplexTPS)::TPS
  return hypot(ct1, t1, a)
end

@inline function hypot(a::Number, ct1::ComplexTPS, t1::TPS)::TPS
  return hypot(ct1, t1, a)
end

@inline function hypot(a::Number, t1::TPS, ct1::ComplexTPS)::TPS
  return hypot(ct1, t1, a)
end

# --- rest of unary functions ---
# TPS:
macro FUN(F)
  fn = Symbol("mad_tpsa_" * F * "!")
  quote
      @inline function $(esc(Symbol(F)))(t1::TPS)::TPS
        t = zero(t1)
        $(esc(fn))(t1.tpsa, t.tpsa)
        return t
      end
  end
end

@FUN("abs"  )
@FUN("unit"  )
@FUN("sqrt"  )
@FUN("exp"  )
@FUN("log"  )
@FUN("sin"  )
@FUN("cos"  )
@FUN("tan"  )
@FUN("cot"  )
@FUN("sinh"  )
@FUN("cosh"  )
@FUN("tanh"  )
@FUN("coth"  )
@FUN("asin"  )
@FUN("acos"  )
@FUN("atan"  )
@FUN("acot"  )
@FUN("asinh")
@FUN("acosh" )
@FUN("atanh" )
@FUN("acoth" )
@FUN("erf"  )
@FUN("erfc"  )

# sinc in Julia has different definition than GTPSA
# In Julia: sinc(x) = sin(pi*x)/(pi*x)
# in C GTPSA: sinc(x) = sin(x)/x
# To make sinc agree:
@inline function sinc(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Cdouble, pi), t.tpsa)
  mad_tpsa_sinc!(t.tpsa, t.tpsa)
  return t
end

# asinc is not in Julia, but in C is asinc(x) = asin(x)/x
# To give similiar behavior, define asinc(x) = asin(pi*x)/(pi*x)
@inline function asinc(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Cdouble, pi), t.tpsa)
  mad_tpsa_asinc!(t.tpsa, t.tpsa)
  return t
end

@inline function sinhc(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Cdouble, pi), t.tpsa)
  mad_tpsa_sinhc!(t.tpsa, t.tpsa)
  return t
end

# asinc is not in Julia, but in C is asinc(x) = asin(x)/x
# To give similiar behavior, define asinc(x) = asin(pi*x)/(pi*x)
@inline function asinhc(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Cdouble, pi), t.tpsa)
  mad_tpsa_asinhc!(t.tpsa, t.tpsa)
  return t
end

# These functions are not implemented in the GTPSA C library, so they 
# are implemented below without creating unnecessary temporaries
@inline function csc(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_sin!(t1.tpsa, t.tpsa)
  mad_tpsa_inv!(t.tpsa, 1.0, t.tpsa)
  return t
end

@inline function sec(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_cos!(t1.tpsa, t.tpsa)
  mad_tpsa_inv!(t.tpsa, 1.0, t.tpsa)
  return t
end

@inline function csch(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_sinh!(t1.tpsa, t.tpsa)
  mad_tpsa_inv!(t.tpsa, 1.0, t.tpsa)
  return t
end

@inline function sech(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_cosh!(t1.tpsa, t.tpsa)
  mad_tpsa_inv!(t.tpsa, 1.0, t.tpsa)
  return t
end

@inline function acsc(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_inv!(t1.tpsa, 1.0, t.tpsa)
  mad_tpsa_asin!(t.tpsa, t.tpsa)
  return t
end

@inline function asec(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_inv!(t1.tpsa, 1.0, t.tpsa)
  mad_tpsa_acos!(t.tpsa, t.tpsa)
  return t
end

@inline function acsch(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_inv!(t1.tpsa, 1.0, t.tpsa)
  mad_tpsa_asinh!(t.tpsa, t.tpsa)
  return t
end

@inline function asech(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_inv!(t1.tpsa, 1.0, t.tpsa)
  mad_tpsa_acosh!(t.tpsa, t.tpsa)
  return t
end


# ComplexTPS:
macro FUNC(F)
  fn = Symbol("mad_ctpsa_" * F * "!")
  quote
      @inline function $(esc(Symbol(F)))(ct1::ComplexTPS)::ComplexTPS
        ct = zero(ct1)
        $(esc(fn))(ct1.tpsa, ct.tpsa)
        return ct
      end
  end
end

@inline function abs(ct1::ComplexTPS)::TPS
  t = TPS(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA}, ct1.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_cabs!(ct1.tpsa, t.tpsa)
  return t
end

@inline function conj(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_conj!(ct1.tpsa, ct.tpsa)
  return ct
end

@inline function conj(t1::TPS)::TPS
  return TPS(t1)
end

@inline function angle(ct1::ComplexTPS)::TPS
  t = TPS(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA}, ct1.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_carg!(ct1.tpsa, t.tpsa)
  return t
end

@inline function angle(t1::TPS)::TPS
  return zero(t1)
end

@inline function complex(tct1::Union{TPS,ComplexTPS})::ComplexTPS
  return ComplexTPS(tct1)
end

@inline function complex(t1::TPS, t2::TPS)::ComplexTPS
  return ComplexTPS(t1, t2)
end

@FUNC("unit"  )
@FUNC("sqrt"  )
@FUNC("exp"  )
@FUNC("log"  )
@FUNC("sin"  )
@FUNC("cos"  )
@FUNC("tan"  )
@FUNC("cot"  )
@FUNC("sinh"  )
@FUNC("cosh"  )
@FUNC("tanh"  )
@FUNC("coth"  )
@FUNC("asin"  )
@FUNC("acos"  )
@FUNC("atan"  )
@FUNC("acot"  )
@FUNC("asinh" )
@FUNC("acosh" )
@FUNC("atanh" )
@FUNC("acoth" )
@FUNC("erf"  )
@FUNC("erfc"  )

# sinc in Julia has different definition than GTPSA
# In Julia: sinc(x) = sin(pi*x)/(pi*x)
# in C GTPSA: sinc(x) = sin(x)/x
# To make sinc agree:
@inline function sinc(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, pi), ct.tpsa)
  mad_ctpsa_sinc!(ct.tpsa, ct.tpsa)
  return ct
end

# asinc is not in Julia, but in C is asinc(x) = asin(x)/x
# To give similiar behavior, define asinc(x) = asin(pi*x)/(pi*x)
@inline function asinc(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, pi), ct.tpsa)
  mad_ctpsa_asinc!(ct.tpsa, ct.tpsa)
  return t
end

@inline function sinhc(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, pi), ct.tpsa)
  mad_ctpsa_sinhc!(ct.tpsa, ct.tpsa)
  return ct
end

# asinc is not in Julia, but in C is asinc(x) = asin(x)/x
# To give similiar behavior, define asinc(x) = asin(pi*x)/(pi*x)
@inline function asinhc(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, pi), ct.tpsa)
  mad_ctpsa_asinhc!(ct.tpsa, ct.tpsa)
  return t
end

# These functions are not implemented in the GTPSA C library, so they 
# are implemented below without creating unnecessary temporaries
@inline function csc(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_sin!(ct1.tpsa, ct.tpsa)
  mad_ctpsa_inv!(ct.tpsa, 1.0+0.0*im, ct.tpsa)
  return ct
end

@inline function sec(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_cos!(ct1.tpsa, ct.tpsa)
  mad_ctpsa_inv!(ct.tpsa, 1.0+0.0*im, ct.tpsa)
  return ct
end

@inline function csch(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_sinh!(ct1.tpsa, ct.tpsa)
  mad_ctpsa_inv!(ct.tpsa, 1.0+0.0*im, ct.tpsa)
  return ct
end

@inline function sech(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_cosh!(ct1.tpsa, ct.tpsa)
  mad_ctpsa_inv!(ct.tpsa, 1.0+0.0*im, ct.tpsa)
  return ct
end

@inline function acsc(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_inv!(ct1.tpsa, 1.0+0.0*im, ct.tpsa)
  mad_ctpsa_asin!(ct.tpsa, ct.tpsa)
  return ct
end

@inline function asec(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_inv!(ct1.tpsa, 1.0+0.0*im, ct.tpsa)
  mad_ctpsa_acos!(ct.tpsa, ct.tpsa)
  return ct
end

@inline function acsch(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_inv!(ct1.tpsa, 1.0+0.0*im, ct.tpsa)
  mad_ctpsa_asinh!(ct.tpsa, ct.tpsa)
  return ct
end

@inline function asech(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_inv!(ct1.tpsa, 1.0+0.0*im, ct.tpsa)
  mad_ctpsa_acosh!(ct.tpsa, ct.tpsa)
  return ct
end