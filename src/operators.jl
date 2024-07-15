# --- copy ---
function copy!(t::TPS{Float64}, t1::TPS{Float64})
  mad_tpsa_copy!(t1, t)
  return t
end

function copy!(t::TPS{ComplexF64}, t1::TPS{ComplexF64})
  mad_ctpsa_copy!(t1, t)
  return t
end

function copy!(t::TPS{ComplexF64}, t1::TPS{Float64})
  mad_ctpsa_cplx!(t1, C_NULL, t)
  return t
end

# --- zero ---
zero(t::TPS) = typeof(t)(getdesc(t).desc, t.mo)

function zero(t::AbstractArray{<:TPS{<:T}}) where {T}
  out = similar(t)
  d = getdesc(first(t))
  for i in eachindex(out)
    out[i] = eltype(t)(d.desc, MAD_TPSA_DEFAULT)
  end
  return out
end

function one(t::AbstractArray{<:TPS{<:T}}) where {T}
  out = similar(t)
  d = getdesc(first(t))
  for i in eachindex(out)
    out[i] = eltype(t)(d.desc, MAD_TPSA_DEFAULT)
    out[i][0] = 1
  end
  return out
end

# --- one ---
function one(t1::TPS)
  t = zero(t1)
  t[0] = 1
  return t
end

# --- zeros and ones (taken from Base.array.jl) --- 
# We overload this because we want each element of the array to be a separate allocated TPS
for (fname, felt) in ((:zeros, :zero), (:ones, :one))
  @eval begin
      $fname(::Type{T}, dims::Base.DimOrInd...) where {T<:TPS} = $fname(T, dims)
      $fname(::Type{T}, dims::NTuple{N, Union{Integer, Base.OneTo}}) where {T<:TPS,N} = $fname(T, map(to_dim, dims))
      function $fname(::Type{T}, dims::NTuple{N, Integer}) where {T<:TPS,N}
          a = Array{T,N}(undef, dims)
          for idx in eachindex(a)
            a[idx] = $felt(T)
          end
          return a
      end
      function $fname(::Type{T}, dims::Tuple{}) where {T<:TPS}
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
function rand(::Type{T}; use::Union{Descriptor,TPS}=GTPSA.desc_current) where {T<:TPS}
  t = T(use=use)
  len = numcoefs(t)
  for i=0:len-1
    t[i] = rand(eltype(t))
  end
  return t
end

# --- Basic unary ---
# TPS:
function +(t1::TPS)
  t = typeof(t1)(t1)
  return t
end

function -(t1::TPS)
  t = zero(t1)
  mul!(t, -1, t1)
  return t
end

# --- Compare ---
# For the general comparison operators (==, >, <, >=, etc) the scalar part is used
# For comparing every coefficient in a TPS, we use isequal, just as Dual Numbers. 

for t = (:(<), :(>), :(<=), :(>=), :(==))
@eval begin

function $t(t1::TPS, t2::TPS)
  return ($t)(t1[0], t2[0])
end

function $t(t1::TPS, a::Number)
  return ($t)(t1[0], a)
end

function $t(a::Number, t1::TPS)
  return ($t)(a, t1[0])
end

end
end

isless(t1::TPS, t2::TPS) = t1 < t2
isinf(t1::TPS) = isinf(t1[0])
isnan(t1::TPS) = isnan(t1[0])
eps(t1::TPS) = eltype(t1)
floatmin(t1::TPS) = floatmin(eltype(t1))
floatmax(t1::TPS) = floatmax(eltype(t1))

# --- Compare entire TPS ---
isequal(t1::TPS{Float64},    t2::TPS{Float64})    = equ(t1, t2, 0)
isequal(t1::TPS{ComplexF64}, t2::TPS{ComplexF64}) = equ(t1, t2, 0)
isequal(t1::TPS{ComplexF64}, t2::TPS{Float64})    = equ(t1, t2, 0)
isequal(t1::TPS{Float64},    t2::TPS{ComplexF64}) = equ(t2, t1, 0)

isequal(t1::TPS, a::Number) = (t1.hi == 0x0 ? t1 == a : false)
isequal(a::Number, t1::TPS) = isequal(t1, a)

# --- mul ---
# TPS, TPS:
mul!(c::TPS{Float64},    a::TPS{Float64},    b::TPS{Float64})    = mad_tpsa_mul!(a, b, c)
mul!(c::TPS{ComplexF64}, a::TPS{ComplexF64}, b::TPS{ComplexF64}) = mad_ctpsa_mul!(a, b, c)
mul!(c::TPS{ComplexF64}, a::TPS{ComplexF64}, b::TPS{Float64})    = mad_ctpsa_mult!(a, b, c)
mul!(c::TPS{ComplexF64}, a::TPS{Float64},    b::TPS{ComplexF64}) = mad_ctpsa_mult!(b, a, c)

# TPS, scalar:
mul!(c::TPS{Float64},    a::TPS{Float64},    v) = mad_tpsa_scl!( a, Float64(v),    c)
mul!(c::TPS{ComplexF64}, a::TPS{ComplexF64}, v) = mad_ctpsa_scl!(a, ComplexF64(v), c)
mul!(c::TPS{ComplexF64}, a::TPS{Float64},    v) = (copy!(c, a); mul!(c, c, v))
mul!(c::TPS, v, a::TPS) = mul!(c, a, v)

for t = ((TPS,TPS),(TPS,Number),(Number,TPS))
@eval begin
function *(t1::$t[1], t2::$t[2])
  use = $(t[1] == TPS ? :t1 : :t2)
  t = (promote_type(typeof(t1),typeof(t2)))(use=use)
  mul!(t, t1, t2)
  return t
end
end
end

# --- add ---
# TPS, TPS:
add!(c::TPS{Float64},    a::TPS{Float64},    b::TPS{Float64})    = mad_tpsa_add!(a, b, c)
add!(c::TPS{ComplexF64}, a::TPS{ComplexF64}, b::TPS{ComplexF64}) = mad_ctpsa_add!(a, b, c)
add!(c::TPS{ComplexF64}, a::TPS{ComplexF64}, b::TPS{Float64})    = mad_ctpsa_addt!(a, b, c)
add!(c::TPS{ComplexF64}, a::TPS{Float64},    b::TPS{ComplexF64}) = mad_ctpsa_addt!(b, a, c)

# TPS, scalar:
add!(c::TPS, a::TPS, v) = (copy!(c, a); seti!(c, 0, 1, v))
add!(c::TPS, v, a::TPS) = add!(c, a, v)

for t = ((TPS,TPS),(TPS,Number),(Number,TPS))
@eval begin
function +(t1::$t[1], t2::$t[2])
  use = $(t[1] == TPS ? :t1 : :t2)
  t = (promote_type(typeof(t1),typeof(t2)))(use=use)
  add!(t, t1, t2)
  return t
end
end
end


# --- sub ---
# TPS, TPS:
sub!(c::TPS{Float64},    a::TPS{Float64},    b::TPS{Float64})    = mad_tpsa_sub!(a, b, c)
sub!(c::TPS{ComplexF64}, a::TPS{ComplexF64}, b::TPS{ComplexF64}) = mad_ctpsa_sub!(a, b, c)
sub!(c::TPS{ComplexF64}, a::TPS{ComplexF64}, b::TPS{Float64})    = mad_ctpsa_subt!(a, b, c)
sub!(c::TPS{ComplexF64}, a::TPS{Float64},    b::TPS{ComplexF64}) = mad_ctpsa_tsub!(a, b, c)

# TPS, scalar:
sub!(c::TPS, a::TPS, v) = add!(c, a, -v)
sub!(c::TPS, v, a::TPS) = (mul!(c, -1, a); seti!(c, 0, 1, v))

for t = ((TPS,TPS),(TPS,Number),(Number,TPS))
@eval begin
function -(t1::$t[1], t2::$t[2])
  use = $(t[1] == TPS ? :t1 : :t2)
  t = (promote_type(typeof(t1),typeof(t2)))(use=use)
  sub!(t, t1, t2)
  return t
end
end
end



#= --- muladd ---
# Right now we will only include a*x + b where a and b are scalars and x is TPS/ComplexTPS
axpb!(a::Cdouble, x::TPS{Float64}, b::Cdouble, r::TPS{Float64}) = mad_tpsa_axpb!(a, x, b, r)
axpb!(a::ComplexF64, x::TPS{ComplexF64}, b::ComplexF64, r::TPS{ComplexF64}) = mad_tpsa_axpb!(a, x, b, r)

muladd!(t::TPS, a::Real, t1::TPS, b::Real) = axpb!(a, t1.tpsa, b, t.tpsa)
muladd!(t::ComplexTPS, a::Number, t1::ComplexTPS, b::Number) = axpb!(a, t1.tpsa, b, t.tpsa)
function muladd!(t::ComplexTPS, a::Number, t1::TPS, b::Number) # promotion
  mul!(t, a, t1)
  add!(t, t, b)
end
=#

# --- div/inv ---
# TPS, TPS:
div!(c::TPS{Float64},    a::TPS{Float64},    b::TPS{Float64})    = mad_tpsa_div!(a, b, c)
div!(c::TPS{ComplexF64}, a::TPS{ComplexF64}, b::TPS{ComplexF64}) = mad_ctpsa_div!(a, b, c)
div!(c::TPS{ComplexF64}, a::TPS{ComplexF64}, b::TPS{Float64})    = mad_ctpsa_divt!(a, b, c)
div!(c::TPS{ComplexF64}, a::TPS{Float64},    b::TPS{ComplexF64}) = mad_ctpsa_tdiv!(a, b, c)

# TPS, scalar:
inv!(c::TPS{Float64},    a::TPS{Float64},     v=1) = mad_tpsa_inv!(a, Float64(v), c)
inv!(c::TPS{ComplexF64}, a::TPS{ComplexF64},  v=1) = mad_ctpsa_inv!(a, ComplexF64(v), c)
inv!(c::TPS{ComplexF64}, a::TPS{Float64},     v=1) = (copy!(c, a); inv!(c, c, v))
div!(c::TPS, a::TPS, v) = mul!(c, a, 1/v)
div!(c::TPS, v, a::TPS) = inv!(c, a, v)

for t =  ((TPS,TPS),(TPS,Number),(Number,TPS))
@eval begin
function /(t1::$t[1], t2::$t[2])
  use = $(t[1] == TPS ? :t1 : :t2)
  t = (promote_type(typeof(t1),typeof(t2)))(use=use)
  div!(t, t1, t2)
  return t
end
end
end


# --- pow ---
# TPS, TPS:
pow!(c::TPS{Float64},    a::TPS{Float64},    b::TPS{Float64})    = mad_tpsa_pow!(a, b, c)
pow!(c::TPS{ComplexF64}, a::TPS{ComplexF64}, b::TPS{ComplexF64}) = mad_ctpsa_pow!(a, b, c)
pow!(c::TPS{ComplexF64}, a::TPS{ComplexF64}, b::TPS{Float64})    = mad_ctpsa_powt!(a, b, c)
pow!(c::TPS{ComplexF64}, a::TPS{Float64},    b::TPS{ComplexF64}) = mad_ctpsa_tpow!(a, b, c)

# TPS, scalar:
pow!(c::TPS{Float64},    a::TPS{Float64}, v)    = mad_tpsa_pown!(a, Float64(v), c)
pow!(c::TPS{ComplexF64}, a::TPS{ComplexF64}, v) = mad_ctpsa_pown!(a, ComplexF64(v), c)

pow!(c::TPS{Float64},    a::TPS{Float64},    v::Integer) = mad_tpsa_powi!(a, Cint(v), c)
pow!(c::TPS{ComplexF64}, a::TPS{ComplexF64}, v::Integer) = mad_ctpsa_powi!(a, Cint(v), c)

pow!(c::TPS{ComplexF64}, a::TPS{Float64}, v) = (copy!(c,a); pow!(c, c, v))
pow!(c::TPS, v, a::TPS) = (mul!(c, log(v), a); exp!(c,c))

for t =  ((TPS,TPS),(TPS,Number),(Number,TPS),(TPS,Integer),(Integer,TPS),)
@eval begin
function ^(t1::$t[1], t2::$t[2])
  use = $(t[1] == TPS ? :t1 : :t2)
  t = (promote_type(typeof(t1),typeof(t2)))(use=use)
  pow!(t, t1, t2)
  return t
end
end
end


# --- atan2 ---

atan!(c::TPS{Float64}, a::TPS{Float64}, b::TPS{Float64}) = mad_tpsa_atan2!(a, b, c)

function atan(t1::TPS{Float64}, t2::TPS{Float64})
  t = zero(t1)
  atan!(t, t1, t2)
  return t
end

function atan(t1::TPS{Float64}, a::Real)
  t = TPS{Float64}(a,use=t1)
  atan!(t, t1, t)
  return t
end

function atan(a::Real, t1::TPS{Float64})
  t = TPS{Float64}(a,use=t1)
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
$(Symbol(t,:!))(t::TPS{Float64},    t1::TPS{Float64})    = $(Symbol("mad_tpsa_",t,:!))(t1, t)
$(Symbol(t,:!))(t::TPS{ComplexF64}, t1::TPS{ComplexF64}) = $(Symbol("mad_ctpsa_",t,:!))(t1, t)
end
end

# sinc in Julia has different definition than GTPSA
# In Julia: sinc(x) = sin(pi*x)/(pi*x)
# in C GTPSA: sinc(x) = sin(x)/x
# To make sinc agree:
sinc!(t::TPS{Float64},    t1::TPS{Float64})    = (mul!(t, t1, pi); mad_tpsa_sinc!(t, t))
sinc!(t::TPS{ComplexF64}, t1::TPS{ComplexF64}) = (mul!(t, t1, pi); mad_ctpsa_sinc!(t, t))

sinhc!(t::TPS{Float64},    t1::TPS{Float64})    = (mul!(t, t1, pi); mad_tpsa_sinhc!(t, t))
sinhc!(t::TPS{ComplexF64}, t1::TPS{ComplexF64}) = (mul!(t, t1, pi); mad_ctpsa_sinhc!(t, t))

# asinc is not in Julia, but in C is asinc(x) = asin(x)/x
# To give similiar behavior, define asinc(x) = asin(pi*x)/(pi*x)
asinc!(t::TPS{Float64},    t1::TPS{Float64})    = (mul!(t, t1, pi); mad_tpsa_asinc!(t, t))
asinc!(t::TPS{ComplexF64}, t1::TPS{ComplexF64}) = (mul!(t, t1, pi); mad_ctpsa_asinc!(t, t))

asinhc!(t::TPS{Float64},    t1::TPS{Float64})    = (mul!(t, t1, pi); mad_tpsa_asinhc!(t, t))
asinhc!(t::TPS{ComplexF64}, t1::TPS{ComplexF64}) = (mul!(t, t1, pi); mad_ctpsa_asinhc!(t, t))

# These functions are not implemented in the GTPSA C library:
csc!(t::TPS{T}, t1::TPS{T}) where {T} = (sin!(t, t1); inv!(t, t, 1))
csch!(t::TPS{T}, t1::TPS{T}) where {T} = (sinh!(t, t1); inv!(t, t, 1))
acsc!(t::TPS{T}, t1::TPS{T}) where {T} = (inv!(t, t1, 1); asin!(t, t))
acsch!(t::TPS{T}, t1::TPS{T}) where {T} = (inv!(t, t1, 1); asinh!(t, t))

sec!(t::TPS{T}, t1::TPS{T}) where {T} = (cos!(t, t1); inv!(t, t, 1))
sech!(t::TPS{T}, t1::TPS{T}) where {T} = (cosh!(t, t1); inv!(t, t, 1))
asec!(t::TPS{T}, t1::TPS{T}) where {T} = (inv!(t, t1, 1); acos!(t, t))
asech!(t::TPS{T}, t1::TPS{T}) where {T} = (inv!(t, t1, 1); acosh!(t, t))

conj!(t::TPS{ComplexF64}, t1::TPS{ComplexF64}) = mad_ctpsa_conj!(t1, t)
conj!(t::TPS, t1::TPS{Float64}) = copy!(t, t1)

rect!(t::TPS{ComplexF64}, t1::TPS{ComplexF64}) = mad_ctpsa_rect!(t1, t)
rect!(t::TPS{Float64}, t1::TPS{Float64}) = copy!(t,t1)

# Now let's finally define all out of place versions:
for t = (:unit, :sqrt, :exp, :log, :sin, :cos, :tan, :cot, :sinh, :cosh, :tanh, 
  :coth, :asin, :acos, :atan, :acot, :asinh, :acosh, :atanh, :acoth, :erf, :erfc, :sinc,
  :sinhc, :asinc, :asinhc, :csc, :csch, :acsc, :acsch, :sec, :sech, :asec, :asech, :conj, :rect)
@eval begin
($t)(t1::TPS) = (t = zero(t1); $(Symbol(t,:!))(t, t1); return t)
end
end

# Now special complex unary:
# The following will always return a TPS{Float64}:
real!(t::TPS{Float64}, t1::TPS{ComplexF64}) = mad_ctpsa_real!(t1, t)
real!(t::TPS{Float64}, t1::TPS{Float64})    = copy!(t, t1) 

imag!(t::TPS{Float64}, t1::TPS{ComplexF64}) = mad_ctpsa_imag!(t1, t)
imag!(t::TPS{Float64}, t1::TPS{Float64})    = clear!(t);

abs!(t::TPS{Float64},    t1::TPS{Float64}) = mad_tpsa_abs!(t1, t)
abs!(t::TPS{Float64}, t1::TPS{ComplexF64}) = mad_ctpsa_cabs!(t1, t)

angle!(t::TPS{Float64}, t1::TPS{ComplexF64}) = mad_ctpsa_carg!(t1, t)
angle!(t::TPS{Float64}, t1::TPS{Float64}) = (clear!(t); t1[0] < 0 && (t[0] = pi); return)

for t = (:real,  :imag, :angle, :abs)
@eval begin
($t)(t1::TPS) = (t = TPS{real(eltype(t1))}(use=t1); $(Symbol(t,:!))(t, t1); return t)
end
end

# And finally, these will always return TPS{ComplexF64}:
polar!(t::TPS{ComplexF64}, t1::TPS{ComplexF64}) = mad_ctpsa_polar!(t1, t)
polar!(t::TPS{ComplexF64}, t1::TPS{Float64}) = (copy!(t,t1); polar!(t, t) )
polar(t1::TPS) = (t=TPS{ComplexF64}(use=t1); polar!(t,t1); return t)

complex(t1::TPS) = TPS{ComplexF64}(t1)
complex!(t::TPS{ComplexF64}; tre=nothing, tim=nothing) = low_cplx!(t, tre, tim)

# TPS:
function low_cplx!(t::TPS{ComplexF64}, tre::Union{TPS{Float64},Nothing}, tim::Union{TPS{Float64},Nothing})
  return mad_ctpsa_cplx!(!isnothing(tre) ? tre : C_NULL, !isnothing(tim) ? tim : C_NULL, t)
end

# TPS, Number:
function low_cplx(t::TPS{ComplexF64}, tre::Union{TPS{Float64},Nothing}, tim::Number)
  mad_ctpsa_cplx!(!isnothing(tre) ? tre : C_NULL, C_NULL, t)
  t[0] += complex(0,tim)
end

# Number, TPS:
function low_cplx!(t::TPS{ComplexF64}, tre::Number, tim::Union{TPS{Float64},Nothing})
  mad_ctpsa_cplx!(C_NULL, !isnothing(tim) ? tim : t)
  t[0] += tre
end

# Number, Number
function low_cplx!(t::TPS{ComplexF64}, tre::Number, tim::Number)
  clear!(t)
  t[0] = complex(tre, tim)
end

complex(tre::TPS{Float64}, tim::TPS{Float64}) = (t = TPS{ComplexF64}(use=tre); complex!(t, tre=tre, tim=tim); return t)
complex(tre::TPS{Float64}, tim::Number)          = (t = TPS{ComplexF64}(use=tre); complex!(t, tre=tre, tim=tim); return t)
complex(tre::Number,          tim::TPS{Float64}) = (t = TPS{ComplexF64}(use=tim); complex!(t, tre=tre, tim=tim); return t)


hypot!(r::TPS{Float64}, a::TPS{Float64}, b::TPS{Float64}) = mad_tpsa_hypot!(a, b, r)
hypot!(r::TPS{Float64}, a::TPS{Float64}, b::TPS{Float64}, c::TPS{Float64}) = mad_tpsa_hypot3!(a, b, c, r)

function hypot!(d::TPS, a...)
  clear!(d)
  for n in a
    add!(d, d, abs(n)^2)
  end
  sqrt!(d,d)
end

function hypot(a::TPS, b::Number...)
  t = TPS{Float64}(use=a)
  hypot!(t, a, b...)
  return t
end

function hypot(a::TPS...)
  t = TPS{Float64}(use=a[1])
  hypot!(t, a...)
  return t
end
#=
function hypot(a::Number, b::TPS, c::Number, d::Number...)
  t = TPS{Float64}(use=b)
  hypot!(t, a, b, c, d...)
  return t
end

function hypot(a::Number, b::Number, c::TPS, d::Number...)
  t = TPS{Float64}(use=c)
  hypot!(t,a,b,c,d...)
  return t
end
=#


#=
hypot!(r::TPS{ComplexF64}, a::TPS{ComplexF64}, b::TPS{ComplexF64}) = mad_ctpsa_hypot!(a, b, r)


hypot!(r::TPS{ComplexF64}, a::TPS{ComplexF64}, b::TPS{ComplexF64}, c::TPS{ComplexF64}) = mad_ctpsa_hypot3!(a, b, c, r)


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
  t1 = TPS(mad_tpsa_new(Base.unsafe_convert(TPS{Float64}, ct1.tpsa), MAD_TPSA_SAME))
  t = zero(t1)
  mad_ctpsa_cabs!(ct1.tpsa, t1.tpsa)
  mad_ctpsa_cabs!(ct2.tpsa, t.tpsa)
  mad_tpsa_hypot!(t1.tpsa, t.tpsa, t.tpsa)
  return t
end

function hypot(ct1::ComplexTPS, a::Number)::TPS
  t1 = TPS(mad_tpsa_new(Base.unsafe_convert(TPS{Float64}, ct1.tpsa), MAD_TPSA_SAME)) 
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
  t1 = TPS(mad_tpsa_new(Base.unsafe_convert(TPS{Float64}, ct1.tpsa), MAD_TPSA_SAME))
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
  t1 = TPS(mad_tpsa_new(Base.unsafe_convert(TPS{Float64}, ct1.tpsa), MAD_TPSA_SAME))
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
  t1 = TPS(mad_tpsa_new(Base.unsafe_convert(TPS{Float64}, ct1.tpsa), MAD_TPSA_SAME))
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