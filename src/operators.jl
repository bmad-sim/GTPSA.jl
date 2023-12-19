# --- temp test ---
#=
@inline function get_temp(t1::TPSA)::Ptr{RTPSA}
  return Base.unsafe_convert(Ptr{RTPSA}, unsafe_load(unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t1.tpsa).d)).t))
end

@inline function +(a::TPSA, b_tpsa::Ptr{RTPSA})::Ptr{RTPSA}
  c_tpsa = get_temp(a)
  mad_tpsa_add!(a.tpsa, b_tpsa, c_tpsa)
  return c_tpsa
end

@inline function +(a_tpsa::Ptr{RTPSA}, b::TPSA)::Ptr{RTPSA}
  c_tpsa = get_temp(b)
  mad_tpsa_add!(a_tpsa, b.tpsa, c_tpsa)
  return c_tpsa
end

@inline function +(a_tpsa::Ptr{RTPSA}, b_tpsa::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_add!(a.tpsa, b_tpsa, b_tpsa)
  return b_tpsa
end

@inline function +(a::TPSA, b::TPSA)::Ptr{RTPSA}
  c_tpsa = get_temp(a)
  mad_tpsa_add!(a.tpsa, b.tpsa, c_tpsa)
  return c_tpsa
end
=#
# --------------------------

# --- unary ---
@inline function +(a::TPSA)::TPSA
  c = TPSA(a)
  return c
end

@inline function -(a::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_scl!(a.tpsa, -1., c.tpsa)
  return c
end


# --- add ---

@inline function +(a::TPSA, b::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_add!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function +(a::Real, b::TPSA)::TPSA
  c = TPSA(b)
  mad_tpsa_copy!(b.tpsa, c.tpsa)
  mad_tpsa_set0!(c.tpsa, 1., convert(Float64,a))
  return c
end

@inline function +(a::TPSA, b::Real)::TPSA
  return b+a
end


# --- sub ---
@inline function -(a::TPSA, b::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_sub!(a.tpsa, b.tpsa, c.tpsa)
  return c
end


@inline function -(a::TPSA, b::Real)::TPSA
  c = TPSA(a)
  mad_tpsa_copy!(a.tpsa, c.tpsa)
  mad_tpsa_set0!(c.tpsa, 1., convert(Float64, -b))
  return c
end

@inline function -(a::Real, b::TPSA)::TPSA
  c = TPSA(b)
  mad_tpsa_scl!(b.tpsa, -1., c.tpsa)
  mad_tpsa_set0!(c.tpsa, 1., convert(Float64, a))
  return c
end


# --- mul ---
@inline function *(a::TPSA, b::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_mul!(a.tpsa, b.tpsa, c.tpsa) 
  return c
end

@inline function *(a::Real, b::TPSA)::TPSA
  c = TPSA(b)
  mad_tpsa_scl!(b.tpsa, convert(Float64, a), c.tpsa)
  return c
end

@inline function *(a::TPSA, b::Real)::TPSA
  return b*a
end


# --- div ---
@inline function /(a::TPSA, b::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_div!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function /(a::TPSA, b::Real)::TPSA
  c = TPSA(a)
  mad_tpsa_scl!(a.tpsa, convert(Float64, 1/b), c.tpsa)
  return c
end

@inline function /(a::Real, b::TPSA)::TPSA
  c = TPSA(b)
  mad_tpsa_inv!(b.tpsa, convert(Float64,a), c.tpsa)
  return c
end


# --- pow ---
@inline function ^(a::TPSA, b::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_pow!(a.tpsa, b.tpsa, c.tpsa)
  return c
end


@inline function ^(a::TPSA, b::Integer)::TPSA
  c = TPSA(a) 
  mad_tpsa_powi!(a.tpsa, convert(Cint, b), c.tpsa)
  return c
end

@inline function ^(a::TPSA, b::Real)::TPSA
  c = TPSA(a)
  mad_tpsa_pown!(a.tpsa, convert(Float64,b), c.tpsa)
  return c
end

@inline function ^(a::Real, b::TPSA)::TPSA
  c = TPSA(b)
  mad_tpsa_scl!(b.tpsa, convert(Float64,log(a)), c.tpsa)
  mad_tpsa_exp!(c.tpsa, c.tpsa)
  return c
end

@inline function inv(a::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_inv!(a.tpsa, convert(Cdouble, 1), c.tpsa)
  return c
end

# --- atan2, hypot ---
@inline function atan(a::TPSA, b::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_atan2!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function hypot(a::TPSA, b::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_hypot!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function norm(a::TPSA)::Float64
  return mad_tpsa_nrm(a.tpsa)
end

# --- unary ---
macro FUN(F)
  fn = Symbol("mad_tpsa_" * F * "!")
  quote
      @inline function $(esc(Symbol(F)))(a::TPSA)::TPSA
        c = TPSA(a)
        $(esc(fn))(a.tpsa, c.tpsa)
        return c
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




# CTPSA Operators:
@inline function real(a::ComplexTPSA)::TPSA
  c = TPSA(a)
  mad_ctpsa_real!(a.tpsa,c.tpsa)
  return c
end

@inline function imag(a::ComplexTPSA)::TPSA
  c = TPSA(a)
  mad_ctpsa_imag!(a.tpsa,c.tpsa)
  return c
end

# --- unary ---
@inline function +(a::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(a) 
  return c
end

@inline function -(a::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_scl!(a.tpsa, -1., c.tpsa)
  return c
end


#--- add ---
@inline function +(a::ComplexTPSA, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_add!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function +(a::Number, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(b)
  mad_ctpsa_copy!(b.tpsa, c.tpsa)
  mad_ctpsa_set0!(c.tpsa, 1.0+0.0*im, convert(ComplexF64, a))
  return c
end

@inline function +(a::ComplexTPSA, b::Number)::ComplexTPSA
  return b+a
end


# --- sub ---
@inline function -(a::ComplexTPSA, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_sub!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function -(a::ComplexTPSA, b::Number)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_copy!(a.tpsa, c.tpsa)
  mad_ctpsa_set0!(c.tpsa, 1.0+0.0*im, convert(ComplexF64, -b))
  return c
end

@inline function -(a::Number, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(b)
  mad_ctpsa_scl!(b.tpsa,-1., c.tpsa)
  mad_ctpsa_set0!(c.tpsa, 1.0+0.0*im, convert(ComplexF64,a))
  return c
end


# --- mul ---
@inline function *(a::ComplexTPSA, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_mul!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function *(a::Number, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(b)
  mad_ctpsa_scl!(b.tpsa, convert(ComplexF64,a), c.tpsa)
  return c
end

@inline function *(a::ComplexTPSA, b::Number)::ComplexTPSA
  return b*a
end


# --- div ---

@inline function /(a::ComplexTPSA, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_div!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function /(a::ComplexTPSA, b::Number)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_scl!(a.tpsa, convert(ComplexF64, 1.0/b), c.tpsa)
  return c
end

@inline function /(a::Number, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(b)
  mad_ctpsa_inv!(b.tpsa, convert(ComplexF64, a), c.tpsa)
  return c
end


# --- pow ---
@inline function ^(a::ComplexTPSA, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_pow!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function ^(a::ComplexTPSA, b::Integer)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_powi!(a.tpsa, convert(Cint, b), c.tpsa)
  return c
end

@inline function ^(a::ComplexTPSA, b::Number)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_pown!(a.tpsa, convert(ComplexF64, b), c.tpsa)
  return c
end

@inline function ^(a::Number, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(b)
  mad_ctpsa_scl!(b.tpsa, convert(ComplexF64, log(a)), c.tpsa)
  mad_ctpsa_exp!(c.tpsa, c.tpsa)
  return c
end

@inline function inv(a::ComplexTPSA)::TPSA
  c = ComplexTPSA(a)
  mad_ctpsa_inv!(a.tpsa, convert(Cdouble, 1), c.tpsa)
  return c
end

# --- hypot ---
@inline function hypot(a::ComplexTPSA, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_hypot!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function norm(a::ComplexTPSA)::Float64
  return mad_ctpsa_nrm(a.tpsa)
end


# --- unary ---
macro FUNC(F)
  fn = Symbol("mad_ctpsa_" * F * "!")
  quote
      @inline function $(esc(Symbol(F)))(a::ComplexTPSA)::ComplexTPSA
        c = ComplexTPSA(a)
        $(esc(fn))(a.tpsa, c.tpsa)
        return c
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