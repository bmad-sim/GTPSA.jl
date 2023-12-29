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
  t = TPS(Base.unsafe_convert(Ptr{Desc}, unsafe_load(ct1).d))
  mad_ctpsa_real!(ct1.tpsa, t.tpsa)
  return t
end

@inline function imag(ct1::ComplexTPS)::TPS
  t = TPS(Base.unsafe_convert(Ptr{Desc}, unsafe_load(ct1).d))
  mad_ctpsa_imag!(ct1.tpsa, t.tpsa)
  return t
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
  ct = ComplexTPS(mad_ctpsa_new(Base.unsafe_convert(Ptr{CTPSA}, t1.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_scl!(ct1.tpsa, -1.0+0.0*im, ct.tpsa)
  mad_ctpsa_set0!(ct.tpsa, 1.0+0.0*im, convert(ComplexF64,a))
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
  return ct1
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
  mad_ctpsa_inv!(ct1.tpsa, convert(Cdouble, 1), ct.tpsa)
  return ct
end

# TPS to ComplexTPS promotion, w/o creating temp ComplexTPS:
@inline function ^(ct1::ComplexTPS, t1::TPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_pow!(ct1.tpsa, ct.tpsa, ct.tpsa)
  return ct
end

@inline function ^(t1::TPS, a::Complex)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_pown!(ct.tpsa, convert(ComplexF64, a), ct.tpsa)
  return ct
end

@inline function ^(a::Complex, t1::TPS)::ComplexTPS
  ct = ComplexTPS(mad_ctpsa_new(Base.unsafe_convert(Ptr{CTPSA}, t1.tpsa), MAD_TPSA_SAME))
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

@inline function hypot(t1::TPS, t2::TPS)::TPS
  t = zero(t1)
  mad_tpsa_hypot!(t1.tpsa, t2.tpsa, t.tpsa)
  return t
end

@inline function norm(t1::TPS)::Float64
  return mad_tpsa_nrm(t1.tpsa)
end

# ComplexTPS:
@inline function hypot(ct1::ComplexTPS, ct2::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_hypot!(ct1.tpsa, ct2.tpsa, ct.tpsa)
  return ct
end

@inline function norm(ct1::ComplexTPS)::Float64
  return mad_ctpsa_nrm(ct1.tpsa)
end

# TPS to ComplexTPS promotion, w/o creating temp ComplexTPS:
@inline function hypot(ct1::ComplexTPS, t1::TPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_hypot!(ct1.tpsa, ct.tpsa, ct.tpsa)
  return ct
end

@inline function hypot(t1::TPS, ct1::ComplexTPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_hypot!(ct.tpsa, ct1.tpsa, ct.tpsa)
  return ct
end

# --- rest of unary functions ---
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
@FUN("sinc"  )
@FUN("sinh"  )
@FUN("cosh"  )
@FUN("tanh"  )
@FUN("coth"  )
@FUN("sinhc" )
@FUN("asin"  )
@FUN("acos"  )
@FUN("atan"  )
@FUN("acot"  )
@FUN("asinc" )
@FUN("asinh")
@FUN("acosh" )
@FUN("atanh" )
@FUN("acoth" )
@FUN("asinhc")
@FUN("erf"  )
@FUN("erfc"  )

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

@FUNC("unit"  )
@FUNC("sqrt"  )
@FUNC("exp"  )
@FUNC("log"  )
@FUNC("sin"  )
@FUNC("cos"  )
@FUNC("tan"  )
@FUNC("cot"  )
@FUNC("sinc"  )
@FUNC("sinh"  )
@FUNC("cosh"  )
@FUNC("tanh"  )
@FUNC("coth"  )
@FUNC("sinhc" )
@FUNC("asin"  )
@FUNC("acos"  )
@FUNC("atan"  )
@FUNC("acot"  )
@FUNC("asinc" )
@FUNC("asinh" )
@FUNC("acosh" )
@FUNC("atanh" )
@FUNC("acoth" )
@FUNC("asinhc")
@FUNC("erf"  )
@FUNC("erfc"  )