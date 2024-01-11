# --- muladd --- 
# a*t1 + b or t1*t2 + a or a*t1 + t2 or t1*t2 + t3
# a*t1 + b
function muladd(a::Real, t1::TPS, b::Real)::TPS
  t = zero(t1)
  mad_tpsa_axpb!(convert(Cdouble, a), t1.tpsa, convert(Cdouble, b), t.tpsa)
  return t
end

function muladd(a::Number, t1::TPS, b::Number)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_axpb!(convert(ComplexF64, a), ct.tpsa, convert(ComplexF64, b), ct.tpsa)
  return ct
end

function muladd(a::Number, ct1::ComplexTPS, b::Number)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_axpb!(convert(ComplexF64, a), ct1.tpsa, convert(ComplexF64, b), ct.tpsa)
  return ct
end

function muladd(t1::TPS, a::Number, b::Number)
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

#= faster to not use
function muladd(t1::TPS, t2::TPS, a::Number)::ComplexTPS
  ct1 = ComplexTPS(t1)
  ct2 = ComplexTPS(t2)
  mad_ctpsa_axypb!(convert(ComplexF64, 1.0), ct1.tpsa, ct2.tpsa, convert(ComplexF64, a), ct1.tpsa)
  return ct1
end
=#

function muladd(ct1::ComplexTPS, t2::TPS, a::Number)::ComplexTPS
  ct = ComplexTPS(t2)
  mad_ctpsa_axypb!(convert(ComplexF64, 1.0), ct.tpsa, ct1.tpsa, convert(ComplexF64, a), ct.tpsa)
  return ct
end

function muladd(t1::TPS, ct1::ComplexTPS, a::Number)::ComplexTPS
  return muladd(ct1,t1,a)
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

function muladd(a::Number, t1::TPS, t2::TPS)::ComplexTPS
  ct1 = ComplexTPS(t1)
  ct2 = ComplexTPS(t2)
  mad_ctpsa_axpbypc!(convert(ComplexF64, a), ct1.tpsa, convert(ComplexF64,1.0), ct2.tpsa, convert(ComplexF64,0.0), ct1.tpsa)
  return ct1
end

function muladd(a::Number, t1::TPS, ct1::ComplexTPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_axpbypc!(convert(ComplexF64, a), ct.tpsa, convert(ComplexF64,1.0), ct1.tpsa, convert(ComplexF64,0.0), ct.tpsa)
  return ct
end

function muladd(a::Number, ct1::ComplexTPS, t1::TPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_axpbypc!(convert(ComplexF64, a), ct1.tpsa, convert(ComplexF64,1.0), ct.tpsa, convert(ComplexF64,0.0), ct.tpsa)
  return ct
end

function muladd(a::Number, ct1::ComplexTPS, ct2::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_axpbypc!(convert(ComplexF64, a), ct1.tpsa, convert(ComplexF64,1.0), ct2.tpsa, convert(ComplexF64,0.0), ct.tpsa)
  return ct
end

function muladd(t1::TPS, a::Number, t2::TPS)
  return muladd(a,t1,t2)
end

function muladd(ct1::ComplexTPS, a::Number, t1::TPS)
  return muladd(a,ct1,t1)
end

function muladd(t1::TPS, a::Number, ct1::ComplexTPS)
  return muladd(a,t1,ct1)
end

function muladd(ct1::ComplexTPS, a::Number, ct2::ComplexTPS)
  return muladd(a,ct1,ct2)
end

# t1*t2 + t3
function muladd(t1::TPS, t2::TPS, t3::TPS)::TPS
  t = zero(t1)
  mad_tpsa_axypbzpc!(1.0, t1.tpsa, t2.tpsa, 1.0, t3.tpsa, 0.0, t.tpsa)
  println("hi1"); return t
end

function muladd(ct1::ComplexTPS, t1::TPS, t2::TPS)::ComplexTPS
  ct = ComplexTPS(t1)
  ct2 = ComplexTPS(t2)
  mad_ctpsa_axypbzpc!(convert(ComplexF64,1), ct.tpsa, ct1.tpsa, convert(ComplexF64,1), ct2.tpsa, convert(ComplexF64,0), ct2.tpsa)
  println("hi2"); return ct2
end

function muladd(t1::TPS, ct1::ComplexTPS, t2::TPS)::ComplexTPS
  return muladd(ct1, t1, t2)
end

function muladd(t1::TPS, t2::TPS, ct1::ComplexTPS)::ComplexTPS
  ct = ComplexTPS(t1)
  ct2 = ComplexTPS(t2)
  mad_ctpsa_axypbzpc!(convert(ComplexF64,1), ct.tpsa, ct2.tpsa, convert(ComplexF64,1), ct1.tpsa, convert(ComplexF64,0), ct.tpsa)
  println("hi3"); return ct
end

function muladd(ct1::ComplexTPS, ct2::ComplexTPS, t1::TPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_axypbzpc!(convert(ComplexF64,1), ct1.tpsa, ct2.tpsa, convert(ComplexF64,1), ct.tpsa, convert(ComplexF64,0), ct.tpsa)
  println("hi4"); return ct
end

function muladd(ct1::ComplexTPS, t1::TPS, ct2::ComplexTPS)::ComplexTPS
  ct = ComplexTPS(t1)
  mad_ctpsa_axypbzpc!(convert(ComplexF64,1), ct1.tpsa, ct.tpsa, convert(ComplexF64,1), ct2.tpsa, convert(ComplexF64,0), ct.tpsa)
  println("hi5"); return ct
end

function muladd(t1::TPS, ct1::ComplexTPS, ct2::ComplexTPS)::ComplexTPS
  return muladd(ct1, t1, ct2)
end

function muladd(ct1::ComplexTPS, ct2::ComplexTPS, ct3::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_axypbzpc!(convert(ComplexF64,1), ct1.tpsa, ct2.tpsa, convert(ComplexF64,1), ct3.tpsa, convert(ComplexF64,0), ct.tpsa)
  println("hi6"); return ct
end