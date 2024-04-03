# --- copy ---
function copy!(t::TPS, t1::TPS)
  mad_tpsa_copy!(t1.tpsa, t.tpsa)
end

function copy!(ct::ComplexTPS, ct1::ComplexTPS)
  mad_ctpsa_copy!(ct1.tpsa, ct.tpsa)
end

# --- zero ---
function zero(t::TPS)::TPS
  return TPS(mad_tpsa_new(t.tpsa, MAD_TPSA_SAME))
end

function zero(ct::ComplexTPS)::ComplexTPS
  return ComplexTPS(mad_ctpsa_new(ct.tpsa, MAD_TPSA_SAME))
end

# --- one ---
function one(t1::TPS)::TPS
  t = TPS(mad_tpsa_new(t1.tpsa, MAD_TPSA_SAME))
  mad_tpsa_set0!(t.tpsa, 0.0, 1.0)
  return t
end

function one(ct1::ComplexTPS)::ComplexTPS
  ct = ComplexTPS(mad_ctpsa_new(ct1.tpsa, MAD_TPSA_SAME))
  mad_ctpsa_set0!(ct.tpsa, ComplexF64(0.0), ComplexF64(1.0))
  return ct
end

# --- zeros and ones (taken from Base.array.jl) --- 
# We overload this because we want each element of the array to be a separate allocated TPS
for (fname, felt) in ((:zeros, :zero), (:ones, :one))
  @eval begin
      $fname(::Type{T}, dims::Base.DimOrInd...) where {T<:Union{TPS,ComplexTPS}} = $fname(T, dims)
      $fname(::Type{T}, dims::NTuple{N, Union{Integer, Base.OneTo}}) where {T<:Union{TPS,ComplexTPS},N} = $fname(T, map(to_dim, dims))
      function $fname(::Type{T}, dims::NTuple{N, Integer}) where {T<:Union{TPS,ComplexTPS},N}
          a = Array{T,N}(undef, dims)
          for idx in eachindex(a)
            a[idx] = $felt(T)
          end
          return a
      end
      function $fname(::Type{T}, dims::Tuple{}) where {T<:Union{TPS,ComplexTPS}}
          a = Array{T}(undef)
          for idx in eachindex(a)
            a[idx] = $felt(T)
          end
          return a
      end
  end
end

# --- Unary ---
# TPS:
function +(t1::TPS)::TPS
  t = TPS(t1)
  return t
end

function -(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_scl!(t1.tpsa, -1., t.tpsa)
  return t
end

# ComplexTPS:
function +(ct1::ComplexTPS)::ComplexTPS
  ct = ComplexTPS(ct1)
  return ct
end

function -(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, -1), ct.tpsa)
  return ct
end

function real(ct1::ComplexTPS)::TPS
  t = TPS(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA}, ct1.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_real!(ct1.tpsa, t.tpsa)
  return t
end

function imag(ct1::ComplexTPS)::TPS
  t = TPS(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA}, ct1.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_imag!(ct1.tpsa, t.tpsa)
  return t
end

function real(t1::TPS)::TPS
  return TPS(t1)
end

function imag(t1::TPS)::TPS
  return zero(t1)
end


# --- Compare ---
# For the general comparison operators (==, >, <, >=, etc) the scalar part is used
# for equal behavior to Dual numbers. For comparing every coefficient in a TPS, we use
# isequal, just as Dual Numbers. 

function <(t1::TPS, t2::TPS)::Bool
  return mad_tpsa_get0(t1.tpsa) < mad_tpsa_get0(t2.tpsa)
end

function <(t1::TPS, a::Real)::Bool
  return mad_tpsa_get0(t1.tpsa) < a
end

function <(a::Real, t1::TPS)::Bool
  return a < mad_tpsa_get0(t1.tpsa)
end

function >(t1::TPS, t2::TPS)::Bool
  return t2 < t1
end

function >(t1::TPS, a::Real)::Bool
  return a < t1
end

function >(a::Real, t1::TPS)::Bool
  return t1 < a
end

function <=(t1::TPS, t2::TPS)::Bool
  return mad_tpsa_get0(t1.tpsa) <= mad_tpsa_get0(t2.tpsa)
end

function <=(t1::TPS, a::Real)::Bool
  return mad_tpsa_get0(t1.tpsa) <= a
end

function <=(a::Real, t1::TPS)::Bool
  return a <= mad_tpsa_get0(t1.tpsa)
end

function >=(t1::TPS, t2::TPS)::Bool
  return t2 <= t1
end

function >=(t1::TPS, a::Real)::Bool
  return a <= t1
end

function >=(a::Real, t1::TPS)::Bool
  return t1 <= a
end

# Note Complex numbers/TPSs have no defined >, <

function ==(t1::TPS, t2::TPS)::Bool
  return mad_tpsa_get0(t1.tpsa) == mad_tpsa_get0(t2.tpsa)
end

function ==(t1::TPS, a::Number)::Bool
  return mad_tpsa_get0(t1.tpsa) == a
end

function ==(a::Number,t1::TPS)::Bool
  return mad_tpsa_get0(t1.tpsa) == a
end

# ---

function ==(t1::TPS, a::Complex)::Bool
  return mad_tpsa_get0(t1.tpsa) == a
end

function ==(a::Complex,t1::TPS)::Bool
  return mad_tpsa_get0(t1.tpsa) == a
end

# ---


function ==(ct1::ComplexTPS, ct2::ComplexTPS)::Bool
  return mad_ctpsa_get0(ct1.tpsa) == mad_ctpsa_get0(ct2.tpsa)
end

function ==(ct1::ComplexTPS, a::Number)::Bool
  return mad_ctpsa_get0(ct1.tpsa) == a
end

function ==(a::Number, ct1::ComplexTPS)::Bool
  return mad_ctpsa_get0(ct1.tpsa) == a
end

function ==(ct1::ComplexTPS, t1::TPS)::Bool
  return mad_ctpsa_get0(ct1.tpsa) == mad_tpsa_get0(t1.tpsa)
end

function ==(t1::TPS, ct1::ComplexTPS)::Bool
  return ct1 == t1
end


# --- Compare entire TPS ---
# TPS:
function isequal(t1::TPS, t2::TPS)::Bool
  return convert(Bool, mad_tpsa_equ(t1.tpsa, t2.tpsa, convert(Cdouble, 0.)))
end

function isequal(t1::TPS, a::Real)::Bool
  t2 = TPS(a,use=t1)
  return isequal(t1,t2)
end

function isequal(a::Real, t1::TPS)::Bool
  return isequal(t1, a)
end

# ComplexTPS:
function isequal(ct1::ComplexTPS, ct2::ComplexTPS)::Bool
  return convert(Bool, mad_ctpsa_equ(ct1.tpsa, ct2.tpsa, convert(Cdouble, 0.)))
end

function isequal(ct1::ComplexTPS, a::Number)::Bool
  ct2 = ComplexTPS(a,use=ct1)
  return isequal(ct1, ct2)
end

function isequal(a::Number, ct1::ComplexTPS)::Bool
  return isequal(ct1, a)
end

# TPS/ComplexTPS and Real/Complex conversion:
function isequal(t1::TPS, a::Complex)::Bool
  if (imag(a) != 0)
    return false
  else
    return isequal(t1, real(a))
  end
end

function isequal(a::Complex, t1::TPS)::Bool
  return isequal(t1, a)
end

function isequal(ct1::ComplexTPS, t1::TPS)::Bool
  return convert(Bool, mad_ctpsa_equt(ct1.tpsa, t1.tpsa, convert(Cdouble, 0.)))
end

function isequal(t1::TPS, ct1::ComplexTPS)::Bool
  return isequal(ct1, t1)
end


# --- add ---
# TPS:
function +(t1::TPS, t2::TPS)::TPS
  t = zero(t1)
  mad_tpsa_add!(t1.tpsa, t2.tpsa, t.tpsa)
  return t
end

function +(t1::TPS, a::Real)::TPS
  t = TPS(t1)
  mad_tpsa_set0!(t.tpsa, 1., convert(Float64,a))
  return t
end

function +(a::Real, t1::TPS)::TPS
  return t1 + a
end

# ComplexTPS:
function +(ct1::ComplexTPS, ct2::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_add!(ct1.tpsa, ct2.tpsa, ct.tpsa)
  return ct
end

function +(ct1::ComplexTPS, a::Number)::ComplexTPS
  ct = ComplexTPS(ct1)
  mad_ctpsa_set0!(ct.tpsa, convert(ComplexF64, 1), convert(ComplexF64, a))
  return ct
end

function +(a::Number, ct1::ComplexTPS)::ComplexTPS
  return ct1 + a
end

# TPS to ComplexTPS promotion, w/o creating temp ComplexTPS:
function +(ct1::ComplexTPS, t1::TPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_addt!(ct1.tpsa, t1.tpsa, ct.tpsa)
  return ct
end

function +(t1::TPS, ct1::ComplexTPS)::ComplexTPS
  return ct1 + t1
end

function +(t1::TPS, a::Complex)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_set0!(ct.tpsa, convert(ComplexF64, 1), convert(ComplexF64, a))
  return ct
end

function +(a::Complex, t1::TPS)::ComplexTPS
  return t1 + a
end


# --- sub ---
# TPS:
function -(t1::TPS, t2::TPS)::TPS
  t = zero(t1)
  mad_tpsa_sub!(t1.tpsa, t2.tpsa, t.tpsa)
  return t
end


function -(t1::TPS, a::Real)::TPS
  t = TPS(t1)
  mad_tpsa_set0!(t.tpsa, 1., convert(Float64, -a))
  return t
end

function -(a::Real, t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_scl!(t1.tpsa, -1., t.tpsa)
  mad_tpsa_set0!(t.tpsa, 1., convert(Float64, a))
  return t
end

# ComplexTPS:
function -(ct1::ComplexTPS, ct2::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_sub!(ct1.tpsa, ct2.tpsa, ct.tpsa)
  return ct
end

function -(ct1::ComplexTPS, a::Number)::ComplexTPS
  ct = ComplexTPS(ct1)
  mad_ctpsa_set0!(ct.tpsa, convert(ComplexF64, 1), convert(ComplexF64, -a))
  return ct
end

function -(a::Number, ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, -1), ct.tpsa)
  mad_ctpsa_set0!(ct.tpsa, convert(ComplexF64, 1), convert(ComplexF64,a))
  return ct
end

# TPS to ComplexTPS promotion, w/o creating temp ComplexTPS:
function -(ct1::ComplexTPS, t1::TPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_subt!(ct1.tpsa, t1.tpsa, ct.tpsa)
  return ct
end

function -(t1::TPS, ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_tsub!(t1.tpsa, ct1.tpsa, ct.tpsa)
  return ct
end

function -(t1::TPS, a::Complex)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_set0!(ct.tpsa, convert(ComplexF64, 1), convert(ComplexF64, -a))
  return ct
end

function -(a::Complex, t1::TPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_scl!(ct.tpsa, convert(ComplexF64, -1), ct.tpsa)
  mad_ctpsa_set0!(ct.tpsa, convert(ComplexF64, 1), convert(ComplexF64,a))
  return ct
end


# --- mul ---
# TPS:
function *(t1::TPS, t2::TPS)::TPS
  t = zero(t1)
  mad_tpsa_mul!(t1.tpsa, t2.tpsa, t.tpsa) 
  return t
end

function *(t1::TPS, a::Real)::TPS
  t = zero(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Float64, a), t.tpsa)
  return t
end

function *(a::Real, t1::TPS)::TPS
  return t1 * a
end

# ComplexTPS:
function *(ct1::ComplexTPS, ct2::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_mul!(ct1.tpsa, ct2.tpsa, ct.tpsa)
  return ct
end

function *(ct1::ComplexTPS, a::Number)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64,a), ct.tpsa)
  return ct
end

function *(a::Number, ct1::ComplexTPS)::ComplexTPS
  return ct1 * a
end

# TPS to ComplexTPS promotion, w/o creating temp ComplexTPS:
function *(ct1::ComplexTPS, t1::TPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_mult!(ct1.tpsa, t1.tpsa, ct.tpsa)
  return ct
end

function *(t1::TPS, ct1::ComplexTPS)::ComplexTPS
  return ct1 * t1
end

function *(t1::TPS, a::Complex)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_scl!(ct.tpsa, convert(ComplexF64,a), ct.tpsa)
  return ct
end

function *(a::Complex, t1::TPS)::ComplexTPS
  return t1 * a
end


# --- div ---
# TPS:
function /(t1::TPS, t2::TPS)::TPS
  t = zero(t1)
  mad_tpsa_div!(t1.tpsa, t2.tpsa, t.tpsa)
  return t
end

function /(t1::TPS, a::Real)::TPS
  t = zero(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Float64, 1/a), t.tpsa)
  return t
end

function /(a::Real, t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_inv!(t1.tpsa, convert(Float64,a), t.tpsa)
  return t
end

# ComplexTPS:
function /(ct1::ComplexTPS, ct2::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_div!(ct1.tpsa, ct2.tpsa, ct.tpsa)
  return ct
end

function /(ct1::ComplexTPS, a::Number)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, 1/a), ct.tpsa)
  return ct
end

function /(a::Number, ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_inv!(ct1.tpsa, convert(ComplexF64, a), ct.tpsa)
  return ct
end

# TPS to ComplexTPS promotion, w/o creating temp ComplexTPS:
function /(ct1::ComplexTPS, t1::TPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_divt!(ct1.tpsa, t1.tpsa, ct.tpsa)
  return ct
end

function /(t1::TPS, ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_tdiv!(t1.tpsa, ct1.tpsa, ct.tpsa)
  return ct
end

function /(t1::TPS, a::Complex)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_scl!(ct.tpsa, convert(ComplexF64, 1/a), ct.tpsa)
  return ct
end

function /(a::Complex, t1::TPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_inv!(ct.tpsa, convert(ComplexF64, a), ct.tpsa)
  return ct
end


# --- pow ---
# TPS:
function ^(t1::TPS, t2::TPS)::TPS
  t = zero(t1)
  mad_tpsa_pow!(t1.tpsa, t2.tpsa, t.tpsa)
  return t
end

function ^(t1::TPS, i::Integer)::TPS
  t = zero(t1)
  mad_tpsa_powi!(t1.tpsa, convert(Cint, i), t.tpsa)
  return t
end

function ^(t1::TPS, a::Real)::TPS
  t = zero(t1)
  mad_tpsa_pown!(t1.tpsa, convert(Float64, a), t.tpsa)
  return t
end

function ^(a::Real, t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Float64, log(a)), t.tpsa)
  mad_tpsa_exp!(t.tpsa, t.tpsa)
  return t
end

function inv(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_inv!(t1.tpsa, convert(Cdouble, 1), t.tpsa)
  return t
end

# ComplexTPS:
function ^(ct1::ComplexTPS, ct2::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_pow!(ct1.tpsa, ct2.tpsa, ct.tpsa)
  return ct
end

function ^(ct1::ComplexTPS, i::Integer)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_powi!(ct1.tpsa, convert(Cint, i), ct.tpsa)
  return ct
end

function ^(ct1::ComplexTPS, a::Number)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_pown!(ct1.tpsa, convert(ComplexF64, a), ct.tpsa)
  return ct
end

function ^(a::Number, ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, log(a)), ct.tpsa)
  mad_ctpsa_exp!(ct.tpsa, ct.tpsa)
  return ct
end

function inv(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_inv!(ct1.tpsa, convert(ComplexF64, 1), ct.tpsa)
  return ct
end

# TPS to ComplexTPS promotion, w/o creating temp ComplexTPS:
function ^(ct1::ComplexTPS, t1::TPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_powt!(ct1.tpsa, t1.tpsa, ct.tpsa)
  return ct
end

function ^(t1::TPS, ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_tpow!(t1.tpsa, ct1.tpsa, ct.tpsa)
  return ct
end

function ^(t1::TPS, a::Complex)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_pown!(ct.tpsa, convert(ComplexF64, a), ct.tpsa)
  return ct
end

function ^(a::Complex, t1::TPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_scl!(ct.tpsa, convert(ComplexF64, log(a)), ct.tpsa)
  mad_ctpsa_exp!(ct.tpsa, ct.tpsa)
  return ct
end


# --- atan2, hypot, norm ---
# TPS:
function atan(t1::TPS, t2::TPS)::TPS
  t = zero(t1)
  mad_tpsa_atan2!(t1.tpsa, t2.tpsa, t.tpsa)
  return t
end

function atan(t1::TPS, a::Real)::TPS
  t = TPS(a,use=t1)
  mad_tpsa_atan2!(t1.tpsa, t.tpsa, t.tpsa)
  return t
end

function atan(a::Real, t1::TPS)::TPS
  t = TPS(a,use=t1)
  mad_tpsa_atan2!(t.tpsa, t1.tpsa, t.tpsa)
  return t
end

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
  t1 = TPS(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA}, ct1.tpsa), MAD_TPSA_SAME))
  t = zero(t1)
  mad_ctpsa_cabs!(ct1.tpsa, t1.tpsa)
  mad_ctpsa_cabs!(ct2.tpsa, t.tpsa)
  mad_tpsa_hypot!(t1.tpsa, t.tpsa, t.tpsa)
  return t
end

function hypot(ct1::ComplexTPS, a::Number)::TPS
  t1 = TPS(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA}, ct1.tpsa), MAD_TPSA_SAME)) 
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

function hypot(ct1::ComplexTPS, ct2::ComplexTPS, a::Number)::TPS
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

function hypot(ct1::ComplexTPS, a::Number, ct2::ComplexTPS)::TPS
  return hypot(ct1, ct2, a)
end

function hypot(a::Number, ct1::ComplexTPS, ct2::ComplexTPS)::TPS
  return hypot(ct1, ct2, a)
end

function hypot(ct1::ComplexTPS, a::Number, b::Number)::TPS
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

# --- rest of unary functions ---
# TPS:
macro FUN(F)
  fn = Symbol("mad_tpsa_" * F * "!")
  quote
      function $(esc(Symbol(F)))(t1::TPS)::TPS
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
function sinc(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Cdouble, pi), t.tpsa)
  mad_tpsa_sinc!(t.tpsa, t.tpsa)
  return t
end

# asinc is not in Julia, but in C is asinc(x) = asin(x)/x
# To give similiar behavior, define asinc(x) = asin(pi*x)/(pi*x)
function asinc(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Cdouble, pi), t.tpsa)
  mad_tpsa_asinc!(t.tpsa, t.tpsa)
  return t
end

function sinhc(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Cdouble, pi), t.tpsa)
  mad_tpsa_sinhc!(t.tpsa, t.tpsa)
  return t
end

# asinc is not in Julia, but in C is asinc(x) = asin(x)/x
# To give similiar behavior, define asinc(x) = asin(pi*x)/(pi*x)
function asinhc(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Cdouble, pi), t.tpsa)
  mad_tpsa_asinhc!(t.tpsa, t.tpsa)
  return t
end

# These functions are not implemented in the GTPSA C library, so they 
# are implemented below without creating unnecessary temporaries
function csc(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_sin!(t1.tpsa, t.tpsa)
  mad_tpsa_inv!(t.tpsa, 1.0, t.tpsa)
  return t
end

function sec(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_cos!(t1.tpsa, t.tpsa)
  mad_tpsa_inv!(t.tpsa, 1.0, t.tpsa)
  return t
end

function csch(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_sinh!(t1.tpsa, t.tpsa)
  mad_tpsa_inv!(t.tpsa, 1.0, t.tpsa)
  return t
end

function sech(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_cosh!(t1.tpsa, t.tpsa)
  mad_tpsa_inv!(t.tpsa, 1.0, t.tpsa)
  return t
end

function acsc(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_inv!(t1.tpsa, 1.0, t.tpsa)
  mad_tpsa_asin!(t.tpsa, t.tpsa)
  return t
end

function asec(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_inv!(t1.tpsa, 1.0, t.tpsa)
  mad_tpsa_acos!(t.tpsa, t.tpsa)
  return t
end

function acsch(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_inv!(t1.tpsa, 1.0, t.tpsa)
  mad_tpsa_asinh!(t.tpsa, t.tpsa)
  return t
end

function asech(t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_inv!(t1.tpsa, 1.0, t.tpsa)
  mad_tpsa_acosh!(t.tpsa, t.tpsa)
  return t
end


# ComplexTPS:
macro FUNC(F)
  fn = Symbol("mad_ctpsa_" * F * "!")
  quote
      function $(esc(Symbol(F)))(ct1::ComplexTPS)::ComplexTPS
        ct = zero(ct1)
        $(esc(fn))(ct1.tpsa, ct.tpsa)
        return ct
      end
  end
end

function abs(ct1::ComplexTPS)::TPS
  t = TPS(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA}, ct1.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_cabs!(ct1.tpsa, t.tpsa)
  return t
end

function conj(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_conj!(ct1.tpsa, ct.tpsa)
  return ct
end

function conj(t1::TPS)::TPS
  return TPS(t1)
end

function angle(ct1::ComplexTPS)::TPS
  t = TPS(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA}, ct1.tpsa), MAD_TPSA_SAME))
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

function polar(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_polar!(ct1.tpsa, ct.tpsa)
  return ct
end

function polar(t1::TPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_polar!(ct.tpsa, ct.tpsa)
  return ct
end

function rect(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_rect!(ct1.tpsa, ct.tpsa)
  return ct
end

function rect(t1::TPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_rect!(ct.tpsa, ct.tpsa)
  return ct
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
