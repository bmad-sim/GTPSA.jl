# --- temp test ---
#=
@inline function get_temp(t1::TPS)::Ptr{RTPSA}
  return Base.unsafe_convert(Ptr{RTPSA}, unsafe_load(unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t1.tpsa).d)).t))
end

@inline function +(a::TPS, b_tpsa::Ptr{RTPSA})::Ptr{RTPSA}
  c_tpsa = get_temp(a)
  mad_tpsa_add!(a.tpsa, b_tpsa, c_tpsa)
  return c_tpsa
end

@inline function +(a_tpsa::Ptr{RTPSA}, b::TPS)::Ptr{RTPSA}
  c_tpsa = get_temp(b)
  mad_tpsa_add!(a_tpsa, b.tpsa, c_tpsa)
  return c_tpsa
end

@inline function +(a_tpsa::Ptr{RTPSA}, b_tpsa::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_add!(a.tpsa, b_tpsa, b_tpsa)
  return b_tpsa
end

@inline function +(a::TPS, b::TPS)::Ptr{RTPSA}
  c_tpsa = get_temp(a)
  mad_tpsa_add!(a.tpsa, b.tpsa, c_tpsa)
  return c_tpsa
end
=#
# --------------------------

# --- unary ---
@inline function +(a::TPS)::TPS
  c = TPS(a)
  return c
end

@inline function -(a::TPS)::TPS
  c = TPS(a)
  mad_tpsa_scl!(a.tpsa, -1., c.tpsa)
  return c
end

# --- Compare ---

@inline function ==(a::TPS, b::TPS)::Bool
  return convert(Bool, mad_tpsa_equ(a.tpsa, b.tpsa, convert(Cdouble, 0.)))
end

@inline function ==(a::Real, b::TPS)::Bool
  c = TPS(b)
  mad_tpsa_set0!(c.tpsa, 1., convert(Float64,a))
  return a == c
end

@inline function ==(a::TPS, b::Real)::Bool
  return b == a
end

@inline function ==(a::ComplexTPS, b::ComplexTPS)::Bool
  return convert(Bool, mad_ctpsa_equ(a.tpsa, b.tpsa, convert(Cdouble, 0.)))
end

@inline function ==(a::Number, b::ComplexTPS)::Bool
  c = TPS(b)
  mad_ctpsa_set0!(c.tpsa, 1., convert(ComplexF64,a))
  return a == c
end

@inline function ==(a::TPS, b::Number)::Bool
  return b == a
end


# ---------------


#=
mutability(::Type{TPS}) = IsMutable()

function promote_operation(::typeof(+), ::Type{TPS}, ::Type{TPS}) 
  return TPS
end

function operate!(::typeof(+), a::TPS, b::TPS)
  mad_tpsa_add!(a.tpsa, b.tpsa, a.tpsa)
  return a
end

function operate_to!(output::TPS, ::typeof(+), a::TPS, b::TPS)
  mad_tpsa_add!(a.tpsa, b.tpsa, output.tpsa)
  return output
end

function promote_operation(::typeof(sin), ::Type{TPS}, ::Type{TPS}) 
  return TPS
end

function operate!(::typeof(sin), a::TPS)
  mad_tpsa_sin!(a.tpsa, a.tpsa)
  return a
end

function operate_to!(output::TPS, ::typeof(sin), a::TPS)
  mad_tpsa_sin!(a.tpsa, output.tpsa)
  return output
end

function promote_operation(::typeof(cos), ::Type{TPS}, ::Type{TPS}) 
  return TPS
end

function operate!(::typeof(cos), a::TPS)
  mad_tpsa_cos!(a.tpsa, a.tpsa)
  return a
end

function operate_to!(output::TPS, ::typeof(cos), a::TPS)
  mad_tpsa_cos!(a.tpsa, output.tpsa)
  return output
end


function mutable_copy(a::TPS)
  b = TPS(a)
  mad_tpsa_copy!(a.tpsa,b.tpsa)
  return b
end
=#

# -----

# --- add ---
@inline function +(a::TPS, b::TPS)::TPS
  c = TPS(a)
  mad_tpsa_add!(a.tpsa, b.tpsa, c.tpsa)
  return c
end


@inline function +(a::Real, b::TPS)::TPS
  c = TPS(b)
  mad_tpsa_copy!(b.tpsa, c.tpsa)
  mad_tpsa_set0!(c.tpsa, 1., convert(Float64,a))
  return c
end

@inline function +(a::TPS, b::Real)::TPS
  return b+a
end


# --- sub ---
@inline function -(a::TPS, b::TPS)::TPS
  c = TPS(a)
  mad_tpsa_sub!(a.tpsa, b.tpsa, c.tpsa)
  return c
end


@inline function -(a::TPS, b::Real)::TPS
  c = TPS(a)
  mad_tpsa_copy!(a.tpsa, c.tpsa)
  mad_tpsa_set0!(c.tpsa, 1., convert(Float64, -b))
  return c
end

@inline function -(a::Real, b::TPS)::TPS
  c = TPS(b)
  mad_tpsa_scl!(b.tpsa, -1., c.tpsa)
  mad_tpsa_set0!(c.tpsa, 1., convert(Float64, a))
  return c
end


# --- mul ---
@inline function *(a::TPS, b::TPS)::TPS
  c = TPS(a)
  mad_tpsa_mul!(a.tpsa, b.tpsa, c.tpsa) 
  return c
end

@inline function *(a::Real, b::TPS)::TPS
  c = TPS(b)
  mad_tpsa_scl!(b.tpsa, convert(Float64, a), c.tpsa)
  return c
end

@inline function *(a::TPS, b::Real)::TPS
  return b*a
end


# --- div ---
@inline function /(a::TPS, b::TPS)::TPS
  c = TPS(a)
  mad_tpsa_div!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function /(a::TPS, b::Real)::TPS
  c = TPS(a)
  mad_tpsa_scl!(a.tpsa, convert(Float64, 1/b), c.tpsa)
  return c
end

@inline function /(a::Real, b::TPS)::TPS
  c = TPS(b)
  mad_tpsa_inv!(b.tpsa, convert(Float64,a), c.tpsa)
  return c
end


# --- pow ---
@inline function ^(a::TPS, b::TPS)::TPS
  c = TPS(a)
  mad_tpsa_pow!(a.tpsa, b.tpsa, c.tpsa)
  return c
end


@inline function ^(a::TPS, b::Integer)::TPS
  c = TPS(a) 
  mad_tpsa_powi!(a.tpsa, convert(Cint, b), c.tpsa)
  return c
end

@inline function ^(a::TPS, b::Real)::TPS
  c = TPS(a)
  mad_tpsa_pown!(a.tpsa, convert(Float64,b), c.tpsa)
  return c
end

@inline function ^(a::Real, b::TPS)::TPS
  c = TPS(b)
  mad_tpsa_scl!(b.tpsa, convert(Float64,log(a)), c.tpsa)
  mad_tpsa_exp!(c.tpsa, c.tpsa)
  return c
end

@inline function inv(a::TPS)::TPS
  c = TPS(a)
  mad_tpsa_inv!(a.tpsa, convert(Cdouble, 1), c.tpsa)
  return c
end

# --- atan2, hypot ---
@inline function atan(a::TPS, b::TPS)::TPS
  c = TPS(a)
  mad_tpsa_atan2!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function hypot(a::TPS, b::TPS)::TPS
  c = TPS(a)
  mad_tpsa_hypot!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function norm(a::TPS)::Float64
  return mad_tpsa_nrm(a.tpsa)
end

# --- unary ---
macro FUN(F)
  fn = Symbol("mad_tpsa_" * F * "!")
  quote
      @inline function $(esc(Symbol(F)))(a::TPS)::TPS
        c = TPS(a)
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
@inline function real(a::ComplexTPS)::TPS
  c = TPS(a)
  mad_ctpsa_real!(a.tpsa,c.tpsa)
  return c
end

@inline function imag(a::ComplexTPS)::TPS
  c = TPS(a)
  mad_ctpsa_imag!(a.tpsa,c.tpsa)
  return c
end

# --- unary ---
@inline function +(a::ComplexTPS)::ComplexTPS
  c = ComplexTPS(a) 
  return c
end

@inline function -(a::ComplexTPS)::ComplexTPS
  c = ComplexTPS(a)
  mad_ctpsa_scl!(a.tpsa, -1., c.tpsa)
  return c
end


#--- add ---
@inline function +(a::ComplexTPS, b::ComplexTPS)::ComplexTPS
  c = ComplexTPS(a)
  mad_ctpsa_add!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function +(a::Number, b::ComplexTPS)::ComplexTPS
  c = ComplexTPS(b)
  mad_ctpsa_copy!(b.tpsa, c.tpsa)
  mad_ctpsa_set0!(c.tpsa, 1.0+0.0*im, convert(ComplexF64, a))
  return c
end

@inline function +(a::ComplexTPS, b::Number)::ComplexTPS
  return b+a
end


# --- sub ---
@inline function -(a::ComplexTPS, b::ComplexTPS)::ComplexTPS
  c = ComplexTPS(a)
  mad_ctpsa_sub!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function -(a::ComplexTPS, b::Number)::ComplexTPS
  c = ComplexTPS(a)
  mad_ctpsa_copy!(a.tpsa, c.tpsa)
  mad_ctpsa_set0!(c.tpsa, 1.0+0.0*im, convert(ComplexF64, -b))
  return c
end

@inline function -(a::Number, b::ComplexTPS)::ComplexTPS
  c = ComplexTPS(b)
  mad_ctpsa_scl!(b.tpsa,-1., c.tpsa)
  mad_ctpsa_set0!(c.tpsa, 1.0+0.0*im, convert(ComplexF64,a))
  return c
end


# --- mul ---
@inline function *(a::ComplexTPS, b::ComplexTPS)::ComplexTPS
  c = ComplexTPS(a)
  mad_ctpsa_mul!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function *(a::Number, b::ComplexTPS)::ComplexTPS
  c = ComplexTPS(b)
  mad_ctpsa_scl!(b.tpsa, convert(ComplexF64,a), c.tpsa)
  return c
end

@inline function *(a::ComplexTPS, b::Number)::ComplexTPS
  return b*a
end


# --- div ---

@inline function /(a::ComplexTPS, b::ComplexTPS)::ComplexTPS
  c = ComplexTPS(a)
  mad_ctpsa_div!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function /(a::ComplexTPS, b::Number)::ComplexTPS
  c = ComplexTPS(a)
  mad_ctpsa_scl!(a.tpsa, convert(ComplexF64, 1.0/b), c.tpsa)
  return c
end

@inline function /(a::Number, b::ComplexTPS)::ComplexTPS
  c = ComplexTPS(b)
  mad_ctpsa_inv!(b.tpsa, convert(ComplexF64, a), c.tpsa)
  return c
end


# --- pow ---
@inline function ^(a::ComplexTPS, b::ComplexTPS)::ComplexTPS
  c = ComplexTPS(a)
  mad_ctpsa_pow!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function ^(a::ComplexTPS, b::Integer)::ComplexTPS
  c = ComplexTPS(a)
  mad_ctpsa_powi!(a.tpsa, convert(Cint, b), c.tpsa)
  return c
end

@inline function ^(a::ComplexTPS, b::Number)::ComplexTPS
  c = ComplexTPS(a)
  mad_ctpsa_pown!(a.tpsa, convert(ComplexF64, b), c.tpsa)
  return c
end

@inline function ^(a::Number, b::ComplexTPS)::ComplexTPS
  c = ComplexTPS(b)
  mad_ctpsa_scl!(b.tpsa, convert(ComplexF64, log(a)), c.tpsa)
  mad_ctpsa_exp!(c.tpsa, c.tpsa)
  return c
end

@inline function inv(a::ComplexTPS)::TPS
  c = ComplexTPS(a)
  mad_ctpsa_inv!(a.tpsa, convert(Cdouble, 1), c.tpsa)
  return c
end

# --- hypot ---
@inline function hypot(a::ComplexTPS, b::ComplexTPS)::ComplexTPS
  c = ComplexTPS(a)
  mad_ctpsa_hypot!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function norm(a::ComplexTPS)::Float64
  return mad_ctpsa_nrm(a.tpsa)
end


# --- unary ---
macro FUNC(F)
  fn = Symbol("mad_ctpsa_" * F * "!")
  quote
      @inline function $(esc(Symbol(F)))(a::ComplexTPS)::ComplexTPS
        c = ComplexTPS(a)
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