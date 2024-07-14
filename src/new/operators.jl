# --- copy ---
function copy!(t::NewTPS{Float64}, t1::NewTPS{Float64})
  mad_tpsa_copy!(t1, t)
  return t
end

function copy!(t::NewTPS{ComplexF64}, t1::NewTPS{ComplexF64})
  mad_ctpsa_copy!(t1, t)
  return t
end

function copy!(t::NewTPS{ComplexF64}, t1::NewTPS{Float64})
  mad_ctpsa_cplx!(t1, C_NULL, t)
  return t
end

# --- zero ---
zero(t::NewTPS) = typeof(t)(getdesc(t).desc, MAD_TPSA_SAME)

# --- one ---
function one(t1::NewTPS)
  t = zero(t1)
  t[0] = 1
  return t
end

# --- zeros and ones (taken from Base.array.jl) --- 
# We overload this because we want each element of the array to be a separate allocated TPS
for (fname, felt) in ((:zeros, :zero), (:ones, :one))
  @eval begin
      $fname(::Type{T}, dims::Base.DimOrInd...) where {T<:NewTPS} = $fname(T, dims)
      $fname(::Type{T}, dims::NTuple{N, Union{Integer, Base.OneTo}}) where {T<:NewTPS,N} = $fname(T, map(to_dim, dims))
      function $fname(::Type{T}, dims::NTuple{N, Integer}) where {T<:NewTPS,N}
          a = Array{T,N}(undef, dims)
          for idx in eachindex(a)
            a[idx] = $felt(T)
          end
          return a
      end
      function $fname(::Type{T}, dims::Tuple{}) where {T<:NewTPS}
          a = Array{T}(undef)
          for idx in eachindex(a)
            a[idx] = $felt(T)
          end
          return a
      end
  end
end

# --- rand ---
"""
    rand(::Type{T}; use::Union{Descriptor,TPS,ComplexTPS}=GTPSA.desc_current) where {T<:Union{TPS,ComplexTPS}}

Generate a `TPS`/`ComplexTPS` with all random coefficients.
"""
function rand(::Type{T}; use::Union{Descriptor,NewTPS}=GTPSA.desc_current) where {T<:NewTPS}
  t = T(use=use)
  len = numcoefs(t)
  for i=0:len-1
    t[i] = rand(eltype(t))
  end
  return t
end

# --- Basic unary ---
# TPS:
function +(t1::NewTPS)
  t = typeof(t1)(t1)
  return t
end

function -(t1::NewTPS)
  t = zero(t1)
  mul!(t, -1, t1)
  return t
end

# --- Compare ---
# For the general comparison operators (==, >, <, >=, etc) the scalar part is used
# For comparing every coefficient in a TPS, we use isequal, just as Dual Numbers. 

for t = (:(<), :(>), :(<=), :(>=), :(==))
@eval begin

function $t(t1::NewTPS, t2::NewTPS)
  return ($t)(t1[0], t2[0])
end

function $t(t1::NewTPS, a::Number)
  return ($t)(t1[0], a)
end

function $t(a::Number, t1::NewTPS)
  return ($t)(a, t1[0])
end

end
end

# --- Compare entire TPS ---
isequal(t1::NewTPS{Float64},    t2::NewTPS{Float64})    = equ(t1, t2, 0)
isequal(t1::NewTPS{ComplexF64}, t2::NewTPS{ComplexF64}) = equ(t1, t2, 0)
isequal(t1::NewTPS{ComplexF64}, t2::NewTPS{Float64})    = equ(t1, t2, 0)
isequal(t1::NewTPS{Float64},    t2::NewTPS{ComplexF64}) = equ(t2, t1, 0)

isequal(t1::NewTPS, a::Number) = (t1.hi == 0x0 ? t1 == a : false)
isequal(a::Number, t1::NewTPS) = isequal(t1, a)

# --- mul ---
# TPS, TPS:
mul!(c::NewTPS{Float64},    a::NewTPS{Float64},    b::NewTPS{Float64})    = mad_tpsa_mul!(a, b, c)
mul!(c::NewTPS{ComplexF64}, a::NewTPS{ComplexF64}, b::NewTPS{ComplexF64}) = mad_ctpsa_mul!(a, b, c)
mul!(c::NewTPS{ComplexF64}, a::NewTPS{ComplexF64}, b::NewTPS{Float64})    = mad_ctpsa_mult!(a, b, c)
mul!(c::NewTPS{ComplexF64}, a::NewTPS{Float64},    b::NewTPS{ComplexF64}) = mad_ctpsa_mult!(b, a, c)

# TPS, scalar:
mul!(c::NewTPS{Float64},    a::NewTPS{Float64},    v) = mad_tpsa_scl!( a, Float64(v),    c)
mul!(c::NewTPS{ComplexF64}, a::NewTPS{ComplexF64}, v) = mad_ctpsa_scl!(a, ComplexF64(v), c)
mul!(c::NewTPS{ComplexF64}, a::NewTPS{Float64},    v) = (copy!(c, a); mul!(c, c, v))
mul!(c::NewTPS, v, a::NewTPS) = mul!(c, a, v)

for t = ((NewTPS,NewTPS),(NewTPS,Number),(Number,NewTPS))
@eval begin
function *(t1::$t[1], t2::$t[2])
  use = $(t[1] == NewTPS ? :t1 : :t2)
  t = (promote_type(typeof(t1),typeof(t2)))(use=use)
  mul!(t, t1, t2)
  return t
end
end
end

# --- add ---
# TPS, TPS:
add!(c::NewTPS{Float64},    a::NewTPS{Float64},    b::NewTPS{Float64})    = mad_tpsa_add!(a, b, c)
add!(c::NewTPS{ComplexF64}, a::NewTPS{ComplexF64}, b::NewTPS{ComplexF64}) = mad_ctpsa_add!(a, b, c)
add!(c::NewTPS{ComplexF64}, a::NewTPS{ComplexF64}, b::NewTPS{Float64})    = mad_ctpsa_addt!(a, b, c)
add!(c::NewTPS{ComplexF64}, a::NewTPS{Float64},    b::NewTPS{ComplexF64}) = mad_ctpsa_addt!(b, a, c)

# TPS, scalar:
add!(c::NewTPS, a::NewTPS, v) = (copy!(c, a); seti!(c, 0, 1, v))
add!(c::NewTPS, v, a::NewTPS) = add!(c, a, v)

for t = ((NewTPS,NewTPS),(NewTPS,Number),(Number,NewTPS))
@eval begin
function +(t1::$t[1], t2::$t[2])
  use = $(t[1] == NewTPS ? :t1 : :t2)
  t = (promote_type(typeof(t1),typeof(t2)))(use=use)
  add!(t, t1, t2)
  return t
end
end
end


# --- sub ---
# TPS, TPS:
sub!(c::NewTPS{Float64},    a::NewTPS{Float64},    b::NewTPS{Float64})    = mad_tpsa_sub!(a, b, c)
sub!(c::NewTPS{ComplexF64}, a::NewTPS{ComplexF64}, b::NewTPS{ComplexF64}) = mad_ctpsa_sub!(a, b, c)
sub!(c::NewTPS{ComplexF64}, a::NewTPS{ComplexF64}, b::NewTPS{Float64})    = mad_ctpsa_subt!(a, b, c)
sub!(c::NewTPS{ComplexF64}, a::NewTPS{Float64},    b::NewTPS{ComplexF64}) = mad_ctpsa_tsub!(a, b, c)

# TPS, scalar:
sub!(c::NewTPS, a::NewTPS, v) = add!(c, a, -v)
sub!(c::NewTPS, v, a::NewTPS) = (mul!(c, -1, a); seti!(c, 0, 1, v))

for t = ((NewTPS,NewTPS),(NewTPS,Number),(Number,NewTPS))
@eval begin
function -(t1::$t[1], t2::$t[2])
  use = $(t[1] == NewTPS ? :t1 : :t2)
  t = (promote_type(typeof(t1),typeof(t2)))(use=use)
  sub!(t, t1, t2)
  return t
end
end
end



#= --- muladd ---
# Right now we will only include a*x + b where a and b are scalars and x is TPS/ComplexTPS
axpb!(a::Cdouble, x::NewTPS{Float64}, b::Cdouble, r::NewTPS{Float64}) = mad_tpsa_axpb!(a, x, b, r)
axpb!(a::ComplexF64, x::NewTPS{ComplexF64}, b::ComplexF64, r::NewTPS{ComplexF64}) = mad_tpsa_axpb!(a, x, b, r)

muladd!(t::TPS, a::Real, t1::TPS, b::Real) = axpb!(a, t1.tpsa, b, t.tpsa)
muladd!(t::ComplexTPS, a::Number, t1::ComplexTPS, b::Number) = axpb!(a, t1.tpsa, b, t.tpsa)
function muladd!(t::ComplexTPS, a::Number, t1::TPS, b::Number) # promotion
  mul!(t, a, t1)
  add!(t, t, b)
end
=#

# --- div/inv ---
# TPS, TPS:
div!(c::NewTPS{Float64},    a::NewTPS{Float64},    b::NewTPS{Float64})    = mad_tpsa_div!(a, b, c)
div!(c::NewTPS{ComplexF64}, a::NewTPS{ComplexF64}, b::NewTPS{ComplexF64}) = mad_ctpsa_div!(a, b, c)
div!(c::NewTPS{ComplexF64}, a::NewTPS{ComplexF64}, b::NewTPS{Float64})    = mad_ctpsa_divt!(a, b, c)
div!(c::NewTPS{ComplexF64}, a::NewTPS{Float64},    b::NewTPS{ComplexF64}) = mad_ctpsa_tdiv!(a, b, c)

# TPS, scalar:
inv!(c::NewTPS{Float64},    a::NewTPS{Float64},     v=1) = mad_tpsa_inv!(a, Float64(v), c)
inv!(c::NewTPS{ComplexF64}, a::NewTPS{ComplexF64},  v=1) = mad_ctpsa_inv!(a, ComplexF64(v), c)
inv!(c::NewTPS{ComplexF64}, a::NewTPS{Float64},     v=1) = (copy!(c, a); inv!(c, c, v))
div!(c::NewTPS, a::NewTPS, v) = mul!(c, a, 1/v)
div!(c::NewTPS, v, a::NewTPS) = inv!(c, a, v)

for t =  ((NewTPS,NewTPS),(NewTPS,Number),(Number,NewTPS))
@eval begin
function /(t1::$t[1], t2::$t[2])
  use = $(t[1] == NewTPS ? :t1 : :t2)
  t = (promote_type(typeof(t1),typeof(t2)))(use=use)
  div!(t, t1, t2)
  return t
end
end
end


# --- pow ---
# TPS, TPS:
pow!(c::NewTPS{Float64},    a::NewTPS{Float64},    b::NewTPS{Float64})    = mad_tpsa_pow!(a, b, c)
pow!(c::NewTPS{ComplexF64}, a::NewTPS{ComplexF64}, b::NewTPS{ComplexF64}) = mad_ctpsa_pow!(a, b, c)
pow!(c::NewTPS{ComplexF64}, a::NewTPS{ComplexF64}, b::NewTPS{Float64})    = mad_ctpsa_powt!(a, b, c)
pow!(c::NewTPS{ComplexF64}, a::NewTPS{Float64},    b::NewTPS{ComplexF64}) = mad_ctpsa_tpow!(a, b, c)

# TPS, scalar:
pow!(c::NewTPS{Float64},    a::NewTPS{Float64}, v)    = mad_tpsa_pown!(a, Float64(v), c)
pow!(c::NewTPS{ComplexF64}, a::NewTPS{ComplexF64}, v) = mad_ctpsa_pown!(a, ComplexF64(v), c)

pow!(c::NewTPS{Float64},    a::NewTPS{Float64},    v::Integer) = mad_tpsa_powi!(a, Cint(v), c)
pow!(c::NewTPS{ComplexF64}, a::NewTPS{ComplexF64}, v::Integer) = mad_ctpsa_powi!(a, Cint(v), c)

pow!(c::NewTPS{ComplexF64}, a::NewTPS{Float64}, v) = (copy!(c,a); pow!(c, c, v))
pow!(c::NewTPS, v, a::NewTPS) = (mul!(c, log(v), a); exp!(c,c))

for t =  ((NewTPS,NewTPS),(NewTPS,Number),(Number,NewTPS),(NewTPS,Integer),(Integer,NewTPS),)
@eval begin
function ^(t1::$t[1], t2::$t[2])
  use = $(t[1] == NewTPS ? :t1 : :t2)
  t = (promote_type(typeof(t1),typeof(t2)))(use=use)
  pow!(t, t1, t2)
  return t
end
end
end


# --- atan2 ---

atan!(c::NewTPS{Float64}, a::NewTPS{Float64}, b::NewTPS{Float64}) = mad_tpsa_atan2!(a, b, c)

function atan(t1::NewTPS{Float64}, t2::NewTPS{Float64})
  t = zero(t1)
  atan!(t, t1, t2)
  return t
end

function atan(t1::NewTPS{Float64}, a::Real)
  t = NewTPS{Float64}(a,use=t1)
  atan!(t, t1, t)
  return t
end

function atan(a::Real, t1::NewTPS{Float64})
  t = NewTPS{Float64}(a,use=t1)
  atan!(t, t, t1)
  return t
end


# --- rest of unary functions ---

# Every function provides in place and out of place version
# Let's first do all in place. For unary functions we assume input type == output type
# unless otherwise specified
for t = (:unit, :sqrt, :exp, :log, :sin, :cos, :tan, :cot, :sinh, :cosh, :tanh, 
         :coth, :asin, :acos, :atan, :acot, :asinh, :acosh, :atanh, :acoth, :erf, :erfc)
@eval begin
$(Symbol(t,:!))(t::NewTPS{Float64},    t1::NewTPS{Float64})    = $(Symbol("mad_tpsa_",t,:!))(t1, t)
$(Symbol(t,:!))(t::NewTPS{ComplexF64}, t1::NewTPS{ComplexF64}) = $(Symbol("mad_ctpsa_",t,:!))(t1, t)
end
end

# sinc in Julia has different definition than GTPSA
# In Julia: sinc(x) = sin(pi*x)/(pi*x)
# in C GTPSA: sinc(x) = sin(x)/x
# To make sinc agree:
sinc!(t::NewTPS{Float64},    t1::NewTPS{Float64})    = (mul!(t, t1, pi); mad_tpsa_sinc!(t, t))
sinc!(t::NewTPS{ComplexF64}, t1::NewTPS{ComplexF64}) = (mul!(t, t1, pi); mad_ctpsa_sinc!(t, t))

sinhc!(t::NewTPS{Float64},    t1::NewTPS{Float64})    = (mul!(t, t1, pi); mad_tpsa_sinhc!(t, t))
sinhc!(t::NewTPS{ComplexF64}, t1::NewTPS{ComplexF64}) = (mul!(t, t1, pi); mad_ctpsa_sinhc!(t, t))

# asinc is not in Julia, but in C is asinc(x) = asin(x)/x
# To give similiar behavior, define asinc(x) = asin(pi*x)/(pi*x)
asinc!(t::NewTPS{Float64},    t1::NewTPS{Float64})    = (mul!(t, t1, pi); mad_tpsa_asinc!(t, t))
asinc!(t::NewTPS{ComplexF64}, t1::NewTPS{ComplexF64}) = (mul!(t, t1, pi); mad_ctpsa_asinc!(t, t))
asinc(t1::NewTPS) = (t = zero(t1); asinc!(t, t1); return t)

asinhc!(t::NewTPS{Float64},    t1::NewTPS{Float64})    = (mul!(t, t1, pi); mad_tpsa_asinhc!(t, t))
asinhc!(t::NewTPS{ComplexF64}, t1::NewTPS{ComplexF64}) = (mul!(t, t1, pi); mad_ctpsa_asinhc!(t, t))
asinhc(t1::NewTPS) = (t = zero(t1); asinhc!(t, t1); return t)

# These functions are not implemented in the GTPSA C library:
csc!(t::NewTPS{T}, t1::NewTPS{T}) where {T} = (sin!(t, t1); inv!(t, t, 1))
csch!(t::NewTPS{T}, t1::NewTPS{T}) where {T} = (sinh!(t, t1); inv!(t, t, 1))
acsc!(t::NewTPS{T}, t1::NewTPS{T}) where {T} = (inv!(t, t1, 1); asin!(t, t))
acsch!(t::NewTPS{T}, t1::NewTPS{T}) where {T} = (inv!(t, t1, 1); asinh!(t, t))

sec!(t::NewTPS{T}, t1::NewTPS{T}) where {T} = (cos!(t, t1); inv!(t, t, 1))
sech!(t::NewTPS{T}, t1::NewTPS{T}) where {T} = (cosh!(t, t1); inv!(t, t, 1))
asec!(t::NewTPS{T}, t1::NewTPS{T}) where {T} = (inv!(t, t1, 1); acos!(t, t))
asech!(t::NewTPS{T}, t1::NewTPS{T}) where {T} = (inv!(t, t1, 1); acosh!(t, t))

conj!(t::NewTPS{ComplexF64}, t1::NewTPS{ComplexF64}) = mad_ctpsa_conj!(t1, t)
conj!(t::NewTPS, t1::NewTPS{Float64}) = copy!(t, t1)

polar!(t::NewTPS{ComplexF64}, t1::NewTPS{ComplexF64}) = mad_ctpsa_polar!(t1, t)
polar!(t::NewTPS{Float64}, t1::NewTPS{Float64}) = abs!(t,t1)

rect!(t::NewTPS{ComplexF64}, t1::NewTPS{ComplexF64}) = mad_ctpsa_rect!(t1, t)
rect!(t::NewTPS{Float64}, t1::NewTPS{Float64}) = copy!(t,t1)

# Now let's finally define all out of place versions:
for t = (:abs, :unit, :sqrt, :exp, :log, :sin, :cos, :tan, :cot, :sinh, :cosh, :tanh, 
  :coth, :asin, :acos, :atan, :acot, :asinh, :acosh, :atanh, :acoth, :erf, :erfc, :sinc,
  :sinhc, :asinc, :asinhc, :csc, :csch, :acsc, :acsch, :sec, :sech, :asec, :asech, :conj,
  :polar, :rect)
@eval begin
($t)(t1::NewTPS) = (t = zero(t1); $(Symbol(t,:!))(t, t1); return t)
end
end

# Now special complex unary:
# The following will always return a NewTPS{Float64}:
real!(t::NewTPS{Float64}, t1::NewTPS{ComplexF64}) = mad_ctpsa_real!(t1, t)
real!(t::NewTPS{Float64}, t1::NewTPS{Float64})    = copy!(t, t1) 

imag!(t::NewTPS{Float64}, t1::NewTPS{ComplexF64}) = mad_ctpsa_imag!(t1, t)
imag!(t::NewTPS{Float64}, t1::NewTPS{Float64})    = clear!(t);

abs!(t::NewTPS{Float64},    t1::NewTPS{Float64}) = mad_tpsa_abs!(t1, t)
abs!(t::NewTPS{Float64}, t1::NewTPS{ComplexF64}) = mad_ctpsa_cabs!(t1, t)

angle!(t::NewTPS{Float64}, t1::NewTPS{ComplexF64}) = mad_ctpsa_carg!(t1, t)
angle!(t::NewTPS{Float64}, t1::NewTPS{Float64}) = (clear!(t); t1[0] < 0 && (t[0] = pi); return)

for t = (:real,  :imag, :angle, :abs)
@eval begin
($t)(t1::NewTPS) = (t = NewTPS{real(eltype(t1))}(use=t1); $(Symbol(t,:!))(t, t1); return t)
end
end

# And finally, complex will always return NewTPS{ComplexF64}:


#=

function angle(ct1::ComplexTPS)::TPS
  t = TPS(mad_tpsa_new(Base.unsafe_convert(NewTPS{Float64}, ct1.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_carg!(ct1.tpsa, t.tpsa)
  return t
end

function angle(t1::TPS)::TPS
  ct = ComplexTPS(t1)
  t = zero(t1)
  mad_ctpsa_carg!(ct.tpsa, t.tpsa)
  return t
end

function complex(t1::TPS)::ComplexTPS
  return ComplexTPS(t1)
end

function complex(ct1::ComplexTPS)::ComplexTPS
  return ComplexTPS(ct1)
end

function complex(t1::TPS, t2::TPS)::ComplexTPS
  return ComplexTPS(t1, t2)
end

function complex(t1::TPS, a::Real)::ComplexTPS
  return ComplexTPS(t1, a)
end

function complex(a::Real, t1::TPS)::ComplexTPS
  return ComplexTPS(a, t1)
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
function sinc(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, pi), ct.tpsa)
  mad_ctpsa_sinc!(ct.tpsa, ct.tpsa)
  return ct
end

# asinc is not in Julia, but in C is asinc(x) = asin(x)/x
# To give similiar behavior, define asinc(x) = asin(pi*x)/(pi*x)
function asinc(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, pi), ct.tpsa)
  mad_ctpsa_asinc!(ct.tpsa, ct.tpsa)
  return ct
end

function sinhc(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, pi), ct.tpsa)
  mad_ctpsa_sinhc!(ct.tpsa, ct.tpsa)
  return ct
end

# asinc is not in Julia, but in C is asinc(x) = asin(x)/x
# To give similiar behavior, define asinc(x) = asin(pi*x)/(pi*x)
function asinhc(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, pi), ct.tpsa)
  mad_ctpsa_asinhc!(ct.tpsa, ct.tpsa)
  return ct
end

# These functions are not implemented in the GTPSA C library, so they 
# are implemented below without creating unnecessary temporaries
function csc(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_sin!(ct1.tpsa, ct.tpsa)
  mad_ctpsa_inv!(ct.tpsa, convert(ComplexF64, 1), ct.tpsa)
  return ct
end

function sec(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_cos!(ct1.tpsa, ct.tpsa)
  mad_ctpsa_inv!(ct.tpsa, convert(ComplexF64, 1), ct.tpsa)
  return ct
end

function csch(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_sinh!(ct1.tpsa, ct.tpsa)
  mad_ctpsa_inv!(ct.tpsa, convert(ComplexF64, 1), ct.tpsa)
  return ct
end

function sech(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_cosh!(ct1.tpsa, ct.tpsa)
  mad_ctpsa_inv!(ct.tpsa, convert(ComplexF64, 1), ct.tpsa)
  return ct
end

function acsc(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_inv!(ct1.tpsa, convert(ComplexF64, 1), ct.tpsa)
  mad_ctpsa_asin!(ct.tpsa, ct.tpsa)
  return ct
end

function asec(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_inv!(ct1.tpsa, convert(ComplexF64, 1), ct.tpsa)
  mad_ctpsa_acos!(ct.tpsa, ct.tpsa)
  return ct
end

function acsch(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_inv!(ct1.tpsa, convert(ComplexF64, 1), ct.tpsa)
  mad_ctpsa_asinh!(ct.tpsa, ct.tpsa)
  return ct
end

function asech(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_inv!(ct1.tpsa, convert(ComplexF64, 1), ct.tpsa)
  mad_ctpsa_acosh!(ct.tpsa, ct.tpsa)
  return ct
end
=#



#= Skipping hypot for now
function hypot(t1::TPS, t2::TPS)::TPS
  t = zero(t1)
  mad_tpsa_hypot!(t1.tpsa, t2.tpsa, t.tpsa)
  return t
end

function hypot(t1::TPS, a::Number)::TPS
  t = TPS(abs(a),use=t1)
  mad_tpsa_hypot!(t1.tpsa, t.tpsa, t.tpsa)
  return t
end

function hypot(a::Number, t1::TPS)::TPS
  return hypot(t1, a)
end

function hypot(t1::TPS, t2::TPS, t3::TPS)::TPS
  t = zero(t1)
  mad_tpsa_hypot3!(t1.tpsa, t2.tpsa, t3.tpsa, t.tpsa)
  return t
end

function hypot(t1::TPS, t2::TPS, a::Number)::TPS
  t3 = TPS(abs(a),use=t1)
  return hypot(t1, t2, t3)
end

function hypot(t1::TPS, a::Number, t2::TPS)::TPS
  return hypot(t1, t2, a)
end

function hypot(a::Number, t1::TPS, t2::TPS)::TPS
  return hypot(t1, t2, a)
end

function hypot(t1::TPS, a::Number, b::Number)::TPS
  t2 = TPS(abs(a),use=t1)
  return hypot(t1, t2, b)
end

function hypot(a::Number, t1::TPS, b::Number)::TPS
  return hypot(t1, a, b)
end

function hypot(a::Number, b::Number, t1::TPS)::TPS
  return hypot(t1, a, b)
end

"""
    norm(t1::TPS)::Float64

Calculates the 1-norm of the `TPS`, which is the sum of 
the `abs` of all coefficients.
"""
function norm(t1::TPS)::Float64
  return mad_tpsa_nrm(t1.tpsa)
end


# ComplexTPS:
function hypot(ct1::ComplexTPS, ct2::ComplexTPS)::TPS
  t1 = TPS(mad_tpsa_new(Base.unsafe_convert(NewTPS{Float64}, ct1.tpsa), MAD_TPSA_SAME))
  t = zero(t1)
  mad_ctpsa_cabs!(ct1.tpsa, t1.tpsa)
  mad_ctpsa_cabs!(ct2.tpsa, t.tpsa)
  mad_tpsa_hypot!(t1.tpsa, t.tpsa, t.tpsa)
  return t
end

function hypot(ct1::ComplexTPS, a::Number)::TPS
  t1 = TPS(mad_tpsa_new(Base.unsafe_convert(NewTPS{Float64}, ct1.tpsa), MAD_TPSA_SAME)) 
  t = zero(t1)
  t[0] = abs(a)
  mad_ctpsa_cabs!(ct1.tpsa, t1.tpsa)
  mad_tpsa_hypot!(t1.tpsa, t.tpsa, t.tpsa)
  return t
end

function hypot(a::Number, ct1::ComplexTPS)::TPS
  return hypot(ct1, a)
end

function hypot(ct1::ComplexTPS, ct2::ComplexTPS, ct3::ComplexTPS)::TPS
  t1 = TPS(mad_tpsa_new(Base.unsafe_convert(NewTPS{Float64}, ct1.tpsa), MAD_TPSA_SAME))
  t2 = zero(t1)
  t3 = zero(t1)
  t = zero(t1)
  mad_ctpsa_cabs!(ct1.tpsa, t1.tpsa)
  mad_ctpsa_cabs!(ct2.tpsa, t2.tpsa)
  mad_ctpsa_cabs!(ct3.tpsa, t3.tpsa)
  mad_tpsa_hypot3!(t1.tpsa, t2.tpsa, t3.tpsa, t.tpsa)
  return t
end

function hypot(ct1::ComplexTPS, ct2::ComplexTPS, a::Number)::TPS
  t1 = TPS(mad_tpsa_new(Base.unsafe_convert(NewTPS{Float64}, ct1.tpsa), MAD_TPSA_SAME))
  t2 = zero(t1)
  t3 = zero(t1)
  t3[0] = abs(a)
  t = zero(t1)
  mad_ctpsa_cabs!(ct1.tpsa, t1.tpsa)
  mad_ctpsa_cabs!(ct2.tpsa, t2.tpsa)
  mad_tpsa_hypot3!(t1.tpsa, t2.tpsa, t3.tpsa, t.tpsa)
  return t
end

function hypot(ct1::ComplexTPS, a::Number, ct2::ComplexTPS)::TPS
  return hypot(ct1, ct2, a)
end

function hypot(a::Number, ct1::ComplexTPS, ct2::ComplexTPS)::TPS
  return hypot(ct1, ct2, a)
end

function hypot(ct1::ComplexTPS, a::Number, b::Number)::TPS
  t1 = TPS(mad_tpsa_new(Base.unsafe_convert(NewTPS{Float64}, ct1.tpsa), MAD_TPSA_SAME))
  t2 = zero(t1)
  t3 = zero(t1)
  t = zero(t1)
  t2[0] = abs(a)
  t3[0] = abs(b)
  mad_ctpsa_cabs!(ct1.tpsa, t1.tpsa)
  mad_tpsa_hypot3!(t1.tpsa, t2.tpsa, t3.tpsa, t.tpsa)
  return t
end

function hypot(a::Number, ct1::ComplexTPS, b::Number)::TPS
  return hypot(ct1, a, b)
end

function hypot(a::Number, b::Number, ct1::ComplexTPS)::TPS
  return hypot(ct1, a, b)
end

"""
    norm(t1::ComplexTPS)::Float64

Calculates the 1-norm of the `ComplexTPS`, which is the sum of 
the `abs` of all coefficients.
"""
function norm(ct1::ComplexTPS)::Float64
  return mad_ctpsa_nrm(ct1.tpsa)
end

# Hypot mixing ComplexTPS and TPS w/o unnecessary temporaries
function hypot(ct1::ComplexTPS, t1::TPS)::TPS
  t = zero(t1)
  mad_ctpsa_cabs!(ct1.tpsa, t.tpsa)
  mad_ctpsa_hypot!(t.tpsa, t1.tpsa, t.tpsa)
  return t
end

function hypot(t1::TPS, ct1::ComplexTPS)::TPS
  return hypot(ct1, t1)
end

function hypot(ct1::ComplexTPS, ct2::ComplexTPS, t1::TPS)::TPS
  t2 = zero(t1)
  t3 = zero(t1)
  t = zero(t1)
  mad_ctpsa_cabs!(ct1.tpsa, t2.tpsa)
  mad_ctpsa_cabs!(ct2.tpsa, t3.tpsa)
  mad_tpsa_hypot3!(t1.tpsa, t2.tpsa, t3.tpsa, t.tpsa)
  return t
end

function hypot(ct1::ComplexTPS, t1::TPS, ct2::ComplexTPS)::TPS
  return hypot(ct1, ct2, t1)
end

function hypot(t1::TPS, ct1::ComplexTPS, ct2::ComplexTPS)::TPS
  return hypot(ct1, ct2, t1)
end

function hypot(ct1::ComplexTPS, t1::TPS, t2::TPS)::TPS
  t3 = zero(t1)
  t = zero(t1)
  mad_ctpsa_cabs!(ct1.tpsa, t3.tpsa)
  mad_tpsa_hypot3!(t1.tpsa, t2.tpsa, t3.tpsa, t.tpsa)
  return t
end

function hypot(t1::TPS, ct1::ComplexTPS, t2::TPS)::TPS
  return hypot(ct1, t1, t2)
end

function hypot(t1::TPS, t2::TPS, ct1::ComplexTPS)::TPS
  return hypot(ct1, t1, t2)
end

function hypot(ct1::ComplexTPS, t1::TPS, a::Number)::TPS
  t2 = zero(t1)
  t3 = zero(t1)
  t3[0] = abs(a)
  t = zero(t1)
  mad_ctpsa_cabs!(ct1.tpsa, t2.tpsa)
  mad_tpsa_hypot3!(t1.tpsa, t2.tpsa, t3.tpsa, t.tpsa)
  return t
end

function hypot(t1::TPS, ct1::ComplexTPS, a::Number)::TPS
  return hypot(ct1, t1, a)
end

function hypot(ct1::ComplexTPS, a::Number, t1::TPS)::TPS
  return hypot(ct1, t1, a)
end

function hypot(t1::TPS, a::Number, ct1::ComplexTPS)::TPS
  return hypot(ct1, t1, a)
end

function hypot(a::Number, ct1::ComplexTPS, t1::TPS)::TPS
  return hypot(ct1, t1, a)
end

function hypot(a::Number, t1::TPS, ct1::ComplexTPS)::TPS
  return hypot(ct1, t1, a)
end
=#