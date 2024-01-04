macro usetemps(expr)
  return :(to_TPS($(to_temp_form(esc(expr)))))
end 

function to_TPS(tpsa::Ptr{RTPSA})::TPS
  t = TPS(mad_tpsa_new(tpsa, MAD_TPSA_SAME))
  mad_tpsa_copy!(tpsa,t.tpsa)
  rel_temp!(tpsa)
  return t
end

function to_TPS(ctpsa::Ptr{CTPSA})::ComplexTPS
  ct = TPS(mad_tpsa_new(ctpsa, MAD_TPSA_SAME))
  mad_ctpsa_copy!(ctpsa,ct.tpsa)
  rel_temp!(ctpsa)
  return ct
end

# Fallback for non-TPSA types
to_TPS(a) = (@inline; a)

function to_temp_form(expr)
  fcns = [:inv, :atan, :abs, :sqrt, :exp, :log, :sin, :cos, :tan, :csc, :sec, :cot, :sinc, :sinh, :cosh,
          :tanh, :csch, :sech, :coth, :asin, :acos, :atan, :acsc, :asec, :acot, :asinh, :acosh, :atanh, :acsch, 
          :asech, :acoth, :real, :imag, :conj, :angle, :complex, :sinhc, :asinc, :asinhc, :erf, :erfc, :norm,
          :polar, :rect] # hypot not included bc appears does not support in-place input = output
  for i in eachindex(expr.args)
    if expr.args[i] isa Expr
      to_temp_form(expr.args[i])
    elseif expr.args[i] == :+
      expr.args[i] = :±  # \pm  (also allowed as unary operator)
    elseif expr.args[i] == :-
      expr.args[i] = :∓  # \mp  (also allowed as unary operator)
    elseif expr.args[i] == :*
      expr.args[i] = :⨰  # \dottimes
    elseif expr.args[i] == :/
      expr.args[i] = :⨱  # \timesbar
    elseif expr.args[i] == :^
      expr.args[i] = :⤊  # \Uuparrow
    elseif expr.args[i] in fcns
      str = "__t_" * string(expr.args[i])
      expr.args[i] = Symbol(str)
    end
  end
  return expr
end

function get_rtemp!(t1::Union{ComplexTPS,TPS})::Ptr{RTPSA}
  return get_rtemp_low!(t1.tpsa)
end

function get_rtemp_low!(tpsa::Ptr{<:Union{RTPSA,CTPSA}})::Ptr{RTPSA}
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(tpsa).d))
  tmpidx = unsafe_load(desc.ti)
  if tmpidx == DESC_MAX_TMP
    # Run out of temporaries... no choice but to throw error (this must return temporary Ptr{RTPSA} for type stability)
    # First release all temporaries
    unsafe_store!(desc.cti, Cint(0))
    error("Permanent temporaries buffer out of memory (max 8). Please divide expression into subexpressions to use @usetemps.")
  end
  t = unsafe_load(Base.unsafe_convert(Ptr{Ptr{RTPSA}}, desc.t), tmpidx+1)
  # Increment tmp idx in Descriptor
  unsafe_store!(desc.ti, tmpidx+Cint(1))
  return t
end

function get_ctemp!(ct1::Union{ComplexTPS,TPS})::Ptr{CTPSA}
  return get_ctemp_low!(ct1.tpsa)
end

function get_ctemp_low!(ctpsa::Ptr{<:Union{RTPSA,CTPSA}})::Ptr{CTPSA}
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(ctpsa).d))
  tmpidx = unsafe_load(desc.cti)
  if tmpidx == DESC_MAX_TMP
    # Run out of temporaries... no choice but to throw error (this must return temporary Ptr{CTPSA} for type stability)
    # First release all temporaries
    unsafe_store!(desc.cti, Cint(0))
    error("Permanent temporaries buffer out of memory (max 8). Please divide expression into subexpressions to use @usetemps.")
  end
  ct = unsafe_load(Base.unsafe_convert(Ptr{Ptr{CTPSA}}, desc.ct), tmpidx+1)
  # Increment tmp idx in Descriptor
  unsafe_store!(desc.cti, tmpidx+Cint(1))
  return ct
end

function rel_temp!(tpsa::Ptr{RTPSA})
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(tpsa).d))
  tmpidx = unsafe_load(desc.ti)
  # Decrement tmp idx in Descriptor
  unsafe_store!(desc.ti, tmpidx-Cint(1))
  return
end

function rel_temp!(ctpsa::Ptr{CTPSA})
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(ctpsa).d))
  tmpidx = unsafe_load(desc.cti)
  # Decrement tmp idx in Descriptor
  unsafe_store!(desc.cti, tmpidx-Cint(1))
  return
end

# --- Unary ---
# TPS:
function ±(t1::TPS)::TPS
  return t1
end

function ±(tpsa::Ptr{RTPSA})::Ptr{RTPSA}
  return tpsa
end

function ∓(t1::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_scl!(t1.tpsa, -1., tpsa)
  return tpsa
end

function ∓(tpsa::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_scl!(tpsa, -1., tpsa)
  return tpsa
end

# ComplexTPS:
function ±(ct1::ComplexTPS)::ComplexTPS
  return ct1
end

function ±(ctpsa::Ptr{CTPSA})::Ptr{CTPSA}
  return ctpsa
end

function ∓(ct1::ComplexTPS)::Ptr{CTPSA}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, -1), ctpsa)
  return ctpsa
end

function ∓(ctpsa::Ptr{CTPSA})::Ptr{CTPSA}
  mad_ctpsa_scl!(ctpsa, convert(ComplexF64, -1), ctpsa)
  return ctpsa
end

function __t_real(ct1::ComplexTPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(ct1)
  mad_ctpsa_real!(ct1.tpsa, tpsa)
  return tpsa
end

function __t_imag(ct1::ComplexTPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(ct1)
  mad_ctpsa_imag!(ct1.tpsa, tpsa)
  return t
end

function __t_real(t1::TPS)::TPS
  return t1
end

function __t_imag(t1::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_setval!(tpsa, 0.0)
  return tpsa
end

# Temps:
function __t_real(ctpsa::Ptr{CTPSA})::Ptr{RTPSA}
  tpsa = get_rtemp!(ct1)
  mad_ctpsa_real!(ctpsa, tpsa)
  rel_ctemp!(ctpsa)
  return tpsa
end

function __t_imag(ctpsa::Ptr{CTPSA})::Ptr{RTPSA}
  tpsa = get_rtemp!(ct1)
  mad_ctpsa_imag!(ctpsa, tpsa)
  rel_temp!(ctpsa)
  return t
end

function __t_real(tpsa::Ptr{RTPSA})::TPS
  return t1
end

function __t_imag(tpsa::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_setval!(tpsa, 0.0)
  return tpsa
end

# Fallbacks for regular types
±(a) = +a
∓(a) = -a
__t_real(a) = real(a)
__t_imag(a) = imag(a)

# --- add ---
# TPS:
function ±(t1::TPS, t2::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_add!(t1.tpsa, t2.tpsa, tpsa)
  return tpsa
end

function ±(t1::TPS, a::Real)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_copy!(t1.tpsa, tpsa)
  mad_tpsa_set0!(tpsa, 1., convert(Float64,a))
  return tpsa
end

function ±(a::Real, t1::TPS)::Ptr{RTPSA}
  return t1 ± a
end

function ±(tpsa1::Ptr{RTPSA}, tpsa2::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_add!(tpsa1, tpsa2, tpsa1)
  rel_temp!(tpsa2)
  return tpsa1
end

function ±(t1::TPS, tpsa1::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_add!(t1.tpsa, tpsa1, tpsa1)
  return tpsa1
end

function ±(tpsa1::Ptr{RTPSA},t1::TPS)::Ptr{RTPSA}
  return t1 ± tpsa1
end

function ±(tpsa1::Ptr{RTPSA}, a::Real)::Ptr{RTPSA}
  mad_tpsa_set0!(tpsa1, 1., convert(Float64,a))
  return tpsa1
end

function ±(a::Real, tpsa1::Ptr{RTPSA})::Ptr{RTPSA}
  return tpsa1 ± a
end

# ComplexTPS:
function ±(ct1::ComplexTPS, ct2::ComplexTPS)::Ptr{CTPSA}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_add!(ct1.ctpsa, ct2.ctpsa, ctpsa)
  return ctpsa
end

function ±(ct1::ComplexTPS, a::Number)::Ptr{CTPSA}
  ctpsa = get_ctemp!(t1)
  mad_ctpsa_copy!(ct1.tpsa, ctpsa)
  mad_ctpsa_set0!(ctpsa, convert(ComplexF64,1), convert(ComplexF64,a))
  return ctpsa
end

function ±(a::Number, ct1::ComplexTPS)::Ptr{CTPSA}
  return ct1 ± a
end

function ±(ctpsa1::Ptr{CTPSA}, ctpsa2::Ptr{CTPSA})::Ptr{CTPSA}
  mad_ctpsa_add!(ctpsa1, ctpsa2, ctpsa1)
  rel_temp!(ctpsa2)
  return ctpsa1
end

function ±(ct1::ComplexTPS, ctpsa1::Ptr{CTPSA})::Ptr{CTPSA}
  mad_ctpsa_add!(ct1.tpsa, ctpsa1, ctpsa1)
  return ctpsa1
end

function ±(ctpsa1::Ptr{CTPSA}, ct1::ComplexTPS)::Ptr{CTPSA}
  return  ct1 ± ctpsa1
end

function ±(ctpsa1::Ptr{CTPSA}, a::Number)::Ptr{CTPSA}
  mad_ctpsa_set0!(ctpsa1, convert(ComplexF64,1), convert(ComplexF64,a))
  return ctpsa1
end

function ±(a::Number, ctpsa1::Ptr{CTPSA})::Ptr{CTPSA}
  return ctpsa1 ± a
end

#= MIXING ComplexTPS with TPS NOT CURRENTLY IMPLEMENTED FOR @usetemps
# TPS to ComplexTPS promotion:
function ±(ct1::ComplexTPS, t1::TPS)::Ptr{CTPSA}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_cplx!(t1.tpsa , Base.unsafe_convert(Ptr{RTPSA}, C_NULL), ctpsa)
  mad_ctpsa_add!(ct1.tpsa, ctpsa, ctpsa)
  return ctpsa
end

function ±(t1::TPS, ct1::ComplexTPS)::Ptr{CTPSA}
  return ct1 ± t1
end

function ±(t1::TPS, a::Complex)::Ptr{CTPSA}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_cplx!(t1.tpsa, Base.unsafe_convert(Ptr{RTPSA}, C_NULL), ctpsa)
  mad_ctpsa_set0!(ctpsa, convert(ComplexF64, 1), convert(ComplexF64, a))
  return ctpsa
end

function ±(a::Complex, t1::TPS)::Ptr{CTPSA}
  return t1 ± a
end

function ±(tpsa1::Ptr{RTPSA}, a::Complex)::Ptr{CTPSA}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_cplx!(tpsa1, Base.unsafe_convert(Ptr{RTPSA}, C_NULL), ctpsa)
  mad_ctpsa_set0!(ctpsa, convert(ComplexF64, 1), convert(ComplexF64, a))
  rel_rtemp!(tpsa1)
  return ctpsa
end

function ±(a::Complex, tpsa1::Ptr{RTPSA})::Ptr{CTPSA}
  return tpsa1 ± a
end

function ±(ctpsa1::Ptr{RTPSA}, tpsa1::Ptr{CTPSA})::Ptr{CTPSA}
=#

# All other types should just be +
±(a, b) =(@inline; +(a,b))
±(a, b, c, xs...) = (@inline; Base.afoldl(±, (±)((±)(a,b),c), xs...))


# --- sub ---
# TPS:
function ∓(t1::TPS, t2::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_sub!(t1.tpsa, t2.tpsa, tpsa)
  return tpsa
end

function ∓(t1::TPS, a::Real)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_copy!(t1.tpsa, tpsa)
  mad_tpsa_set0!(tpsa, 1., convert(Float64, -a))
  return tpsa
end

function ∓(a::Real, t1::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_scl!(t1.tpsa, -1., tpsa)
  mad_tpsa_set0!(tpsa, 1., convert(Float64, a))
  return tpsa
end

function ∓(tpsa1::Ptr{RTPSA}, tpsa2::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_sub!(tpsa1, tpsa2, tpsa1)
  rel_temp!(tpsa2)
  return tpsa1
end

function ∓(t1::TPS, tpsa1::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_sub!(t1.tpsa, tpsa1, tpsa1)
  return tpsa1
end

function ∓(tpsa1::Ptr{RTPSA}, t1::TPS)::Ptr{RTPSA}
  mad_tpsa_sub!(tpsa1, t1.tpsa, tpsa1)
  return tpsa1
end

function ∓(tpsa1::Ptr{RTPSA}, a::Real)::Ptr{RTPSA}
  mad_tpsa_set0!(tpsa1, 1., convert(Float64, -a))
  return tpsa1
end

function ∓(a::Real, tpsa1::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_scl!(tpsa1, -1., tpsa1)
  mad_tpsa_set0!(tpsa1, 1., convert(Float64, a))
  return tpsa1
end

# ComplexTPS:
function ∓(ct1::ComplexTPS, ct2::ComplexTPS)::Ptr{CTPSA}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_sub!(ct1.tpsa, ct2.tpsa, ctpsa)
  return ctpsa
end

function ∓(ct1::ComplexTPS, a::Number)::Ptr{CTPSA}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_copy!(ct1.tpsa, ctpsa)
  mad_ctpsa_set0!(ctpsa, convert(ComplexF64, 1.), convert(ComplexF64, -a))
  return ctpsa
end

function ∓(a::Number, ct1::ComplexTPS)::Ptr{CTPSA}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64,-1.), ctpsa)
  mad_ctpsa_set0!(ctpsa, convert(ComplexF64,1.), convert(ComplexF64, a))
  return ctpsa
end

function ∓(ctpsa1::Ptr{CTPSA}, ctpsa2::Ptr{CTPSA})::Ptr{CTPSA}
  mad_ctpsa_sub!(ctpsa1, ctpsa2, ctpsa1)
  rel_temp!(ctpsa2)
  return ctpsa1
end

function ∓(ct1::ComplexTPS, ctpsa1::Ptr{CTPSA})::Ptr{CTPSA}
  mad_ctpsa_sub!(ct1.tpsa, ctpsa1, ctpsa1)
  return ctpsa1
end

function ∓(ctpsa1::Ptr{CTPSA}, ct1::ComplexTPS)::Ptr{CTPSA}
  mad_ctpsa_sub!(ctpsa1, ct1.tpsa, ctpsa1)
  return ctpsa1
end

function ∓(ctpsa1::Ptr{CTPSA}, a::Number)::Ptr{CTPSA}
  mad_ctpsa_set0!(ctpsa1, convert(ComplexF64,1.), convert(ComplexF64, -a))
  return ctpsa1
end

function ∓(a::Number, ctpsa1::Ptr{CTPSA})::Ptr{CTPSA}
  mad_ctpsa_scl!(tpsa1, convert(ComplexF64, -1.), tpsa1)
  mad_ctpsa_set0!(tpsa1, convert(ComplexF64, 1.), convert(ComplexF64, a))
  return ctpsa1
end

# All other types should just be -
∓(a, b) = (@inline; -(a,b))
# afoldl only for +,*

# --- mul ---
# TPS:
function ⨰(t1::TPS, t2::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_mul!(t1.tpsa, t2.tpsa, tpsa) 
  return tpsa
end

function ⨰(t1::TPS, a::Real)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Float64, a), tpsa)
  return tpsa
end

function ⨰(a::Real, t1::TPS)::Ptr{RTPSA}
  return t1 ⨰ a
end

function ⨰(tpsa1::Ptr{RTPSA}, tpsa2::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_mul!(tpsa1, tpsa2, tpsa1)
  rel_temp!(tpsa2) 
  return tpsa1
end

function ⨰(t1::TPS, tpsa1::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_mul!(t1.tpsa, tpsa1, tpsa1)
  return tpsa1
end

function ⨰(tpsa1::Ptr{RTPSA}, t1::TPS)::Ptr{RTPSA}
  return t1 ⨰ tpsa1
end

function ⨰(tpsa1::Ptr{RTPSA}, a::Real)::Ptr{RTPSA}
  mad_tpsa_scl!(tpsa1, convert(Float64, a), tpsa1)
  return tpsa1
end

function ⨰(a::Real, tpsa1::Ptr{RTPSA})::Ptr{RTPSA}
  return tpsa1 ⨰ a
end

# ComplexTPS:
function ⨰(ct1::ComplexTPS, ct2::ComplexTPS)::Ptr{CTPSA}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_mul!(ct1.tpsa, ct2.tpsa, ctpsa) 
  return ctpsa
end

function ⨰(ct1::ComplexTPS, a::Number)::Ptr{CTPSA}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, a), ctpsa)
  return ctpsa
end

function ⨰(a::Number, ct1::ComplexTPS)::Ptr{CTPSA}
  return ct1 ⨰ a
end

function ⨰(ctpsa1::Ptr{CTPSA}, ctpsa2::Ptr{CTPSA})::Ptr{CTPSA}
  mad_ctpsa_mul!(ctpsa1, ctpsa2, ctpsa1)
  rel_temp!(ctpsa2) 
  return ctpsa1
end

function ⨰(ct1::ComplexTPS, ctpsa1::Ptr{CTPSA})::Ptr{CTPSA}
  mad_ctpsa_mul!(ct1.tpsa, ctpsa1, ctpsa1)
  return ctpsa1
end

function ⨰(ctpsa1::Ptr{CTPSA}, ct1::ComplexTPS)::Ptr{CTPSA}
  return ct1 ⨰ ctpsa1
end

function ⨰(ctpsa1::Ptr{CTPSA}, a::Number)::Ptr{CTPSA}
  mad_ctpsa_scl!(ctpsa1, convert(ComplexF64, a), ctpsa)
  return ctpsa1
end

function ⨰(a::Number, ctpsa1::Ptr{CTPSA})::Ptr{CTPSA}
  return ctpsa1 ⨰ a
end

# Fallbacks
# All other types should just be *
⨰(a, b) = (@inline; *(a,b))
⨰(a, b, c, xs...) = (@inline; Base.afoldl(⨰, (⨰)((⨰)(a,b),c), xs...))

# --- div ---
# TPS:
function ⨱(t1::TPS, t2::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_div!(t1.tpsa, t2.tpsa, tpsa)
  return tpsa
end

function ⨱(t1::TPS, a::Real)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Float64, 1/a), tpsa)
  return tpsa
end

function ⨱(a::Real, t1::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_inv!(t1.tpsa, convert(Float64,a), tpsa)
  return tpsa
end

function ⨱(tpsa1::Ptr{RTPSA}, tpsa2::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_div!(tpsa1, tpsa2, tpsa1)
  rel_temp!(tpsa2)
  return tpsa1
end

function ⨱(tpsa1::Ptr{RTPSA}, a::Real)::Ptr{RTPSA}
  mad_tpsa_scl!(tpsa1, convert(Float64, 1/a), tpsa1)
  return tpsa1
end

function ⨱(a::Real, tpsa1::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_inv!(tpsa1, convert(Float64, a), tpsa1)
  return tpsa1
end

function ⨱(tpsa1::Ptr{RTPSA}, t1::TPS)::Ptr{RTPSA}
  mad_tpsa_div!(tpsa1, t1.tpsa, tpsa1)
  return tpsa1
end

function ⨱(t1::TPS,tpsa1::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_div!(t1.tpsa, tpsa1, tpsa1)
  return tpsa1
end


# ComplexTPS:
function ⨱(ct1::ComplexTPS, ct2::ComplexTPS)::Ptr{CTPSA}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_div!(ct1.tpsa, ct2.tpsa, ctpsa)
  return cvtpsa
end

function ⨱(ct1::ComplexTPS, a::Number)::Ptr{CTPSA}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, 1/a), ctpsa)
  return ctpsa
end

function ⨱(a::Number, ct1::ComplexTPS)::Ptr{CTPSA}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_inv!(ct1.tpsa, convert(ComplexF64,a), ctpsa)
  return ctpsa
end

function ⨱(ctpsa1::Ptr{CTPSA}, ctpsa2::Ptr{CTPSA})::Ptr{CTPSA}
  mad_ctpsa_div!(ctpsa1, ctpsa2, ctpsa1)
  rel_temp!(ctpsa2)
  return ctpsa1
end

function ⨱(ctpsa1::Ptr{CTPSA}, a::Number)::Ptr{CTPSA}
  mad_ctpsa_scl!(ctpsa1, convert(ComplexF64, 1/a), ctpsa1)
  return ctpsa1
end

function ⨱(a::Number, ctpsa1::Ptr{CTPSA})::Ptr{CTPSA}
  mad_ctpsa_inv!(ctpsa1, convert(ComplexF64, a), ctpsa1)
  return ctpsa1
end

function ⨱(ctpsa1::Ptr{CTPSA}, ct1::ComplexTPS)::Ptr{CTPSA}
  mad_ctpsa_div!(ctpsa1, ct1.tpsa, ctpsa1)
  return ctpsa1
end

function ⨱(ct1::ComplexTPS,ctpsa1::Ptr{CTPSA})::Ptr{CTPSA}
  mad_ctpsa_div!(ct1.tpsa, ctpsa1, ctpsa1)
  return ctpsa1
end

# Fallbacks
# All other types should just be /
⨱(a, b) = (@inline; /(a,b))

# --- pow ---
# TPS:
function ⤊(t1::TPS, t2::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_pow!(t1.tpsa, t2.tpsa, tpsa)
  return tpsa
end

function ⤊(t1::TPS, i::Integer)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_powi!(t1.tpsa, convert(Cint, i), tpsa)
  return tpsa
end

function ⤊(t1::TPS, a::Real)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_pown!(t1.tpsa, convert(Float64, a), tpsa)
  return tpsa
end

function ⤊(a::Real, t1::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Float64, log(a)),  tpsa)
  mad_tpsa_exp!(tpsa, tpsa)
  return tpsa
end

function __t_inv(t1::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_inv!(t1.tpsa, convert(Cdouble, 1), tpsa)
  return tpsa
end

function ⤊(tpsa1::Ptr{RTPSA}, tpsa2::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_pow!(tpsa1, tpsa2, tpsa1)
  rel_temp!(tpsa2)
  return tpsa1
end

function ⤊(tpsa1::Ptr{RTPSA}, i::Integer)::Ptr{RTPSA}
  mad_tpsa_powi!(tpsa1, convert(Cint, i), tpsa1)
  return tpsa1
end

function ⤊(tpsa1::Ptr{RTPSA}, a::Real)::Ptr{RTPSA}
  mad_tpsa_pown!(tpsa1, convert(Float64, a), tpsa1)
  return tpsa1
end

function ⤊(a::Real,tpsa1::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_scl!(tpsa1, convert(Float64, log(a)),  tpsa1)
  mad_tpsa_exp!(tpsa1, tpsa1)
  return tpsa1
end

function __t_inv(tpsa1::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_inv!(tpsa1, convert(Cdouble, 1), tpsa1)
  return tpsa1
end

function ⤊(t1::TPS, tpsa1::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_pow!(t1.tpsa, tpsa1, tpsa1)
  return tpsa1
end

function ⤊(tpsa1::Ptr{RTPSA},t1::TPS)::Ptr{RTPSA}
  mad_tpsa_pow!(tpsa1, t1.tpsa, tpsa1)
  return tpsa1
end

# ComplexTPS:
function ⤊(ct1::ComplexTPS, ct2::ComplexTPS)::Ptr{CTPSA}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_pow!(ct1.tpsa, ct2.tpsa, ctpsa)
  return ctpsa
end

function ⤊(ct1::ComplexTPS, i::Integer)::Ptr{CTPSA}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_powi!(ct1.tpsa, convert(Cint, i), ctpsa)
  return ctpsa
end

function ⤊(ct1::ComplexTPS, a::Number)::Ptr{CTPSA}
  tpsa = get_ctemp!(ct1)
  mad_ctpsa_pown!(ct1.tpsa, convert(ComplexF64, a), ctpsa)
  return tpsa
end

function ⤊(a::Number, ct1::ComplexTPS)::Ptr{CTPSA}
  tpsa = get_ctemp!(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, log(a)),  ctpsa)
  mad_ctpsa_exp!(tpsa, tpsa)
  return tpsa
end

function __t_inv(ct1::ComplexTPS)::Ptr{CTPSA}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_inv!(ct1.tpsa, convert(Cdouble, 1), ctpsa)
  return ctpsa
end

function ⤊(ctpsa1::Ptr{CTPSA}, ctpsa2::Ptr{CTPSA})::Ptr{CTPSA}
  mad_ctpsa_pow!(ctpsa1, ctpsa2, ctpsa1)
  rel_temp!(ctpsa2)
  return ctpsa1
end

function ⤊(ctpsa1::Ptr{CTPSA}, i::Integer)::Ptr{CTPSA}
  mad_ctpsa_powi!(ctpsa1, convert(Cint, i), ctpsa1)
  return ctpsa1
end

function ⤊(ctpsa1::Ptr{CTPSA}, a::Number)::Ptr{CTPSA}
  mad_ctpsa_pown!(ctpsa1, convert(ComplexF64, a), ctpsa1)
  return ctpsa1
end

function ⤊(a::Number,ctpsa1::Ptr{CTPSA})::Ptr{CTPSA}
  mad_ctpsa_scl!(ctpsa1, convert(ComplexF64, log(a)),  ctpsa1)
  mad_ctpsa_exp!(ctpsa1, ctpsa1)
  return ctpsa1
end

function __t_inv(ctpsa1::Ptr{CTPSA})::Ptr{CTPSA}
  mad_ctpsa_inv!(ctpsa1, convert(Cdouble, 1), ctpsa1)
  return ctpsa1
end

function ⤊(ct1::ComplexTPS, ctpsa1::Ptr{CTPSA})::Ptr{CTPSA}
  mad_ctpsa_pow!(ct1.tpsa, ctpsa1, ctpsa1)
  return ctpsa1
end

function ⤊(ctpsa1::Ptr{CTPSA},ct1::ComplexTPS)::Ptr{CTPSA}
  mad_ctpsa_pow!(ctpsa1, ct1.tpsa, ctpsa1)
  return ctpsa1
end

⤊(a,b) = (@inline; ^(a,b))

# --- atan, norm ---
# TPS:
function __t_atan(t1::TPS, t2::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_atan2!(t1.tpsa, t2.tpsa, tpsa)
  return tpsa
end

function __t_atan(t1::TPS, a::Real)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_setval!(tpsa, convert(Float64, a))
  mad_tpsa_atan2!(t1.tpsa, tpsa, tpsa)
  return tpsa
end

function __t_atan(a::Real, t1::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_setval!(tpsa, convert(Float64, a))
  mad_tpsa_atan2!(tpsa,t1.tpsa, tpsa)
  return tpsa
end

function __t_atan(tpsa1::Ptr{RTPSA}, tpsa2::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_atan2!(tpsa1, tpsa2, tpsa1)
  rel_temp!(tpsa2)
  return tpsa1
end

function __t_atan(tpsa1::Ptr{RTPSA}, a::Real)::Ptr{RTPSA}
  tpsa2 = get_rtemp_low!(tpsa1)
  mad_tpsa_setval!(tpsa2, convert(Float64, a))
  mad_tpsa_atan2!(tpsa1, tpsa2, tpsa1)
  rel_temp!(tpsa2)
  return tpsa1
end

function __t_atan(a::Real, tpsa1::Ptr{RTPSA})::Ptr{RTPSA}
  tpsa2 = get_rtemp_low!(tpsa1)
  mad_tpsa_setval!(tpsa2, convert(Float64, a))
  mad_tpsa_atan2!(tpsa2,tpsa1, tpsa1)
  rel_temp!(tpsa2)
  return tpsa1
end


function __t_norm(tpsa1::Ptr{RTPSA})::Float64
  nrm = mad_tpsa_nrm(tpsa1)
  rel_temp!(tpsa1)
  return nrm 
end

function __t_norm(ctpsa1::Ptr{CTPSA})::Float64
  nrm = mad_ctpsa_nrm(ctpsa1)
  rel_temp!(ctpsa1)
  return nrm 
end


# ∓∓∓ rest of unary functions ∓∓∓
# TPS:
macro FUNT(F)
  F1 = Symbol("__t_" * F)
  fn = Symbol("mad_tpsa_" * F * "!")
  quote
      function $(esc(F1))(t1::TPS)::Ptr{RTPSA}
        tpsa = get_rtemp!(t1)
        $(esc(fn))(t1.tpsa, tpsa)
        return tpsa
      end

      function $(esc(F1))(tpsa1::Ptr{RTPSA})::Ptr{RTPSA}
        $(esc(fn))(tpsa1, tpsa1)
        return tpsa1
      end

      # Fallback
      $(esc(F1))(a) = $(esc(Symbol(F)))(a)
  end
end

@FUNT("abs"  )
@FUNT("unit"  )
@FUNT("sqrt"  )
@FUNT("exp"  )
@FUNT("log"  )
@FUNT("sin"  )
@FUNT("cos"  )
@FUNT("tan"  )
@FUNT("cot"  )
@FUNT("sinh"  )
@FUNT("cosh"  )
@FUNT("tanh"  )
@FUNT("coth"  )
@FUNT("asin"  )
@FUNT("acos"  )
@FUNT("atan"  )
@FUNT("acot"  )
@FUNT("asinh")
@FUNT("acosh" )
@FUNT("atanh" )
@FUNT("acoth" )
@FUNT("erf"  )
@FUNT("erfc"  )
#=
# sinc in Julia has different definition than GTPSA
# In Julia: sinc(x) = sin(pi*x)/(pi*x)
# in C GTPSA: sinc(x) = sin(x)/x
# To make sinc agree:
function __t_sinc(t1::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Cdouble, pi), tpsa)
  mad_tpsa_sinc!(tpsa, tpsa)
  return tpsa
end

function __t_sinc(tpsa1::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_scl!(tpsa1, convert(Cdouble, pi), tpsa1)
  mad_tpsa_sinc!(tpsa1, tpsa1)
  return tpsa1
end


# asinc is not in Julia, but in C is asinc(x) = asin(x)⨱x
# To give similiar behavior, define asinc(x) = asin(pi⨰x)⨱(pi⨰x)
function __t_asinc(t1::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Cdouble, pi), tpsa)
  mad_tpsa_asinc!(tpsa, tpsa)
  return tpsa
end

function __t_asinc(tpsa1::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_scl!(tpsa1, convert(Cdouble, pi), tpsa1)
  mad_tpsa_asinc!(tpsa1, tpsa1)
  return tpsa1
end

function __t_sinhc(t1::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Cdouble, pi), tpsa)
  mad_tpsa_sinhc!(tpsa, tpsa)
  return tpsa
end

function __t_sinhc(tpsa1::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_scl!(tpsa1, convert(Cdouble, pi), tpsa1)
  mad_tpsa_sinhc!(tpsa1, tpsa1)
  return tpsa1
end

# asinc is not in Julia, but in C is asinc(x) = asin(x)⨱x
# To give similiar behavior, define asinc(x) = asin(pi⨰x)⨱(pi⨰x)
function __t_asinhc(t1::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_scl!(t1.tpsa, convert(Cdouble, pi), tpsa)
  mad_tpsa_asinhc!(tpsa, tpsa)
  return tpsa
end

function __t_asinhc(tpsa1::Ptr{RTPSA})::Ptr{RTPSA}
  mad_tpsa_scl!(tpsa1, convert(Cdouble, pi), tpsa1)
  mad_tpsa_asinhc!(tpsa1, tpsa1)
  return tpsa1
end

# These functions are not implemented in the GTPSA C library, so they 
# are implemented below without creating unnecessary temporaries
function __t_csc(t1::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_sin!(t1.tpsa, t.tpsa)
  mad_tpsa_inv!(t.tpsa, 1.0, t.tpsa)
  return t
end

function __t_sec(t1::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_cos!(t1.tpsa, t.tpsa)
  mad_tpsa_inv!(t.tpsa, 1.0, t.tpsa)
  return t
end

function __t_csch(t1::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_sinh!(t1.tpsa, t.tpsa)
  mad_tpsa_inv!(t.tpsa, 1.0, t.tpsa)
  return t
end

function __t_sech(t1::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_cosh!(t1.tpsa, t.tpsa)
  mad_tpsa_inv!(t.tpsa, 1.0, t.tpsa)
  return t
end

function __t_acsc(t1::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_inv!(t1.tpsa, 1.0, t.tpsa)
  mad_tpsa_asin!(t.tpsa, t.tpsa)
  return t
end

function __t_asec(t1::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_inv!(t1.tpsa, 1.0, t.tpsa)
  mad_tpsa_acos!(t.tpsa, t.tpsa)
  return t
end

function __t_acsch(t1::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_inv!(t1.tpsa, 1.0, t.tpsa)
  mad_tpsa_asinh!(t.tpsa, t.tpsa)
  return t
end

function __t_asech(t1::TPS)::Ptr{RTPSA}
  tpsa = get_rtemp!(t1)
  mad_tpsa_inv!(t1.tpsa, 1.0, t.tpsa)
  mad_tpsa_acosh!(t.tpsa, t.tpsa)
  return t
end


# ComplexTPS:
macro FUNC(F)
  fn = Symbol("mad_ctpsa_" ⨰ F ⨰ "!")
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

function complex(ct1::ComplexTPS)
  return ComplexTPS(ct1)
end

function complex(t1::TPS, t2::TPS)::ComplexTPS
  return ComplexTPS(t1, t2)
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
# In Julia: sinc(x) = sin(pi⨰x)⨱(pi⨰x)
# in C GTPSA: sinc(x) = sin(x)⨱x
# To make sinc agree:
function sinc(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, pi), ct.tpsa)
  mad_ctpsa_sinc!(ct.tpsa, ct.tpsa)
  return ct
end

# asinc is not in Julia, but in C is asinc(x) = asin(x)⨱x
# To give similiar behavior, define asinc(x) = asin(pi⨰x)⨱(pi⨰x)
function asinc(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, pi), ct.tpsa)
  mad_ctpsa_asinc!(ct.tpsa, ct.tpsa)
  return t
end

function sinhc(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, pi), ct.tpsa)
  mad_ctpsa_sinhc!(ct.tpsa, ct.tpsa)
  return ct
end

# asinc is not in Julia, but in C is asinc(x) = asin(x)⨱x
# To give similiar behavior, define asinc(x) = asin(pi⨰x)⨱(pi⨰x)
function asinhc(ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_scl!(ct1.tpsa, convert(ComplexF64, pi), ct.tpsa)
  mad_ctpsa_asinhc!(ct.tpsa, ct.tpsa)
  return t
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