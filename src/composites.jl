# For safety, just don't do "aliasing" (output=input) with composites 
# because inconsistent behavior here + also not always faster (1/11/2023). 
# --- muladd --- 
# a*t1 + b
function muladd(a::Real, t1::TPS, b::Real)::TPS
  t = zero(t1)
  mad_tpsa_axpb!(convert(Cdouble, a), t1.tpsa, convert(Cdouble, b), t.tpsa)
  return t
end

function muladd(a::Number, ct1::ComplexTPS, b::Number)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_axpb!(convert(ComplexF64, a), ct1.tpsa, convert(ComplexF64, b), ct.tpsa)
  return ct
end

function muladd(t1::TPS, a::Real, b::Real)
  return muladd(a,t1,b)
end

function muladd(ct1::ComplexTPS, a::Number, b::Number)
  return muladd(a,ct1,b)
end

# t1*t2 + a
function muladd(t1::TPS, t2::TPS, a::Real)::TPS
  t = zero(t1)
  mad_tpsa_axypb!(1.0, t1.tpsa, t2.tpsa, convert(Cdouble, a), t.tpsa)
  return t
end

function muladd(ct1::ComplexTPS, ct2::ComplexTPS, a::Number)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_axypb!(convert(ComplexF64, 1.0), ct1.tpsa, ct2.tpsa, convert(ComplexF64, a), ct.tpsa)
  return ct
end

# a*t1 + t2
function muladd(a::Real, t1::TPS, t2::TPS)::TPS
  t = zero(t1)
  mad_tpsa_axpbypc!(convert(Cdouble, a), t1.tpsa, 1.0, t2.tpsa, 0.0, t.tpsa)
  return t
end

function muladd(a::Number, ct1::ComplexTPS, ct2::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_axpbypc!(convert(ComplexF64, a), ct1.tpsa, convert(ComplexF64,1.0), ct2.tpsa, convert(ComplexF64,0.0), ct.tpsa)
  return ct
end

function muladd(t1::TPS, a::Real, t2::TPS)
  return muladd(a,t1,t2)
end

function muladd(ct1::ComplexTPS, a::Number, ct2::ComplexTPS)
  return muladd(a,ct1,ct2)
end

# t1*t2 + t3
function muladd(t1::TPS, t2::TPS, t3::TPS)::TPS
  t = zero(t1)
  mad_tpsa_axypbzpc!(1.0, t1.tpsa, t2.tpsa, 1.0, t3.tpsa, 0.0, t.tpsa)
  return t
end

function muladd(ct1::ComplexTPS, ct2::ComplexTPS, ct3::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_axypbzpc!(convert(ComplexF64,1), ct1.tpsa, ct2.tpsa, convert(ComplexF64,1), ct3.tpsa, convert(ComplexF64,0), ct.tpsa)
  return ct
end