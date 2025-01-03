# Here, all out of place operators are defined ONLY for TPS
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
    seti!(out[i], 0, 0, 1)
  end
  return out
end

# --- one ---
function one(t1::TPS)
  t = zero(t1)
  seti!(t, 0, 0, 1)
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
    rand(::Type{T}; use::Union{Descriptor,TPS}=GTPSA.desc_current) where {T<:TPS}

Generate a `TPS`/`ComplexTPS64` with all random coefficients.
"""
function rand(::Type{T}; use::Union{Descriptor,TPS}=GTPSA.desc_current) where {T<:TPS}
  t = T(use=use)
  len = numcoefs(t)
  for i=0:len-1
    t[i] = rand(numtype(t))
  end
  return t
end

# --- Compare ---
# For the general comparison operators (==, >, <, >=, etc) the scalar part is used
# For comparing every coefficient in a TPS, we use isequal, just as Dual Numbers. 

for t = (:(<), :(>), :(<=), :(>=), :(==))
@eval begin

function $t(t1::TPS, t2::TPS)
  return ($t)(geti(t1, 0), geti(t2, 0))
end

function $t(t1::TPS, a::Number)
  return ($t)(geti(t1, 0), a)
end

function $t(a::Number, t1::TPS)
  return ($t)(a, geti(t1, 0))
end

end
end

isless(t1::TPS, t2::TPS) = t1 < t2
isinf(t1::TPS) = isinf(geti(t1, 0))
isnan(t1::TPS) = isnan(geti(t1, 0))
eps(t1::TPS) = eps(numtype(t1))
floatmin(t1::TPS) = floatmin(numtype(t1))
floatmax(t1::TPS) = floatmax(numtype(t1))

# --- Compare entire TPS (monomial-by-monomial) ---
isequal(t1::TPS{Float64},    t2::TPS{Float64})    = equ(t1, t2, 0)
isequal(t1::TPS{ComplexF64}, t2::TPS{ComplexF64}) = equ(t1, t2, 0)
isequal(t1::TPS{ComplexF64}, t2::TPS{Float64})    = equ(t1, t2, 0)
isequal(t1::TPS{Float64},    t2::TPS{ComplexF64}) = equ(t2, t1, 0)

isequal(t1::TPS, a::Number) = (t1.hi == 0x0 ? t1 == a : false)
isequal(a::Number, t1::TPS) = isequal(t1, a)

# --- Basic unary ---
# TPS:
function +(t1::TPS)
  #t = zero(t1)
  #copy!(t,t1)
  return t1
end

function -(t1::TPS)
  t = zero(t1)
  mul!(t, -1, t1)
  return t
end

norm(t1::TPS) = abs(t1)

# --- Arithmetic +,-,*,/,^ ---
for t = ((TPS,TPS),(TPS,Number),(Number,TPS))
@eval begin
function +(t1::$t[1], t2::$t[2])
  use = $(t[1] == TPS ? :t1 : :t2)
  t = (promote_type(typeof(t1),typeof(t2)))(use=use)
  add!(t, t1, t2)
  return t
end

function -(t1::$t[1], t2::$t[2])
  use = $(t[1] == TPS ? :t1 : :t2)
  t = (promote_type(typeof(t1),typeof(t2)))(use=use)
  sub!(t, t1, t2)
  return t
end

function *(t1::$t[1], t2::$t[2])
  use = $(t[1] == TPS ? :t1 : :t2)
  t = (promote_type(typeof(t1),typeof(t2)))(use=use)
  mul!(t, t1, t2)
  return t
end

function /(t1::$t[1], t2::$t[2])
  use = $(t[1] == TPS ? :t1 : :t2)
  t = (promote_type(typeof(t1),typeof(t2)))(use=use)
  div!(t, t1, t2)
  return t
end
end
end

for t = ((TPS,TPS),(TPS,Number),(Number,TPS),(Integer,TPS),(TPS,Integer))
@eval begin
function ^(t1::$t[1], t2::$t[2])
  use = $(t[1] == TPS ? :t1 : :t2)
  t = (promote_type(typeof(t1),typeof(t2)))(use=use)
  pow!(t, t1, t2)
  return t
end
end
end

# --- Rest of unary functions ---
for t = (:unit, :sqrt, :exp, :log, :sin, :cos, :tan, :cot, :sinh, :cosh, :tanh, :inv,
  :coth, :asin, :acos, :atan, :acot, :asinh, :acosh, :atanh, :acoth, :erf, :erfc, :erfcx, 
  :erfi, :wf, :sinc, :sincu, :sinhc, :sinhcu, :asinc, :asincu, :asinhc, :asinhcu, :csc, 
  :csch, :acsc, :acsch, :sec, :sech, :asec, :asech, :conj, :rect, :log10)
@eval begin
($t)(t1::TPS) = (t = zero(t1); $(Symbol(t,:!))(t, t1); return t)
end
end

# --- atan2 ---
atan(t1::TPS{Float64}, t2::TPS{Float64}) = (t = zero(t1); atan!(t, t1, t2); return t)
atan(t1::TPS{Float64}, a::Real)     = (t = TPS{Float64}(a,use=t1); atan!(t, t1, t);  return t)
atan(a::Real, t1::TPS{Float64})     = (t = TPS{Float64}(a,use=t1); atan!(t, t, t1);  return t)

# --- Unary functions that return TPS{Float64} ---
for t = (:real,  :imag, :angle, :abs)
@eval begin
($t)(t1::TPS) = (t = TPS{real(numtype(t1))}(use=t1); $(Symbol(t,:!))(t, t1); return t)
end
end

# --- Unary functions that return TPS{ComplexF64} --- 
polar(t1::TPS) = (t=TPS{ComplexF64}(use=t1); polar!(t,t1); return t)
complex(t1::TPS) = TPS{ComplexF64}(t1)
complex(tre::TPS{Float64}, tim::TPS{Float64}) = (t = TPS{ComplexF64}(use=tre); complex!(t, tre=tre, tim=tim); return t)
complex(tre::TPS{Float64}, tim::Real)  = (t = TPS{ComplexF64}(use=tre); complex!(t, tre=tre, tim=tim); return t)
complex(tre::Real,    tim::TPS{Float64}) = (t = TPS{ComplexF64}(use=tim); complex!(t, tre=tre, tim=tim); return t)

# --- hypot ---
hypot(a::TPS, b::Number...) = (t = TPS{Float64}(use=a); hypot!(t, a, b...); return t)
hypot(a::TPS...) = (t = TPS{Float64}(use=a[1]); hypot!(t, a...); return t)

