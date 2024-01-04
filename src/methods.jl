# --- Evaluate ---
# TPS:
function evaluate(m::Vector{TPS}, x::Vector{<:Real})::Vector{Float64}
  na = length(m)
  ma = map(t->t.tpsa, m)
  nb = Cint(length(x))
  tb = convert(Vector{Float64}, x)
  tc = Vector{Float64}(undef, nb)
  mad_tpsa_eval!(na, ma, nb, tb, tc)
  return tc
end

function evaluate(t1::TPS, x::Vector{<:Real})::Float64
  return evaluate([t1], x)[1]
end

function evaluate(t1::TPS, x::Real)::Float64
  return evaluate([t1], [x])[1]
end

# ComplexTPS:
function evaluate(m::Vector{ComplexTPS}, x::Vector{<:Number})::Vector{ComplexF64}
  na = length(m)
  ma = map(t->t.tpsa, m)
  nb = Cint(length(x))
  tb = convert(Vector{ComplexF64}, x)
  tc = Vector{ComplexF64}(undef, nb)
  mad_ctpsa_eval!(na, ma, nb, tb, tc)
  return tc
end

function evaluate(ct1::ComplexTPS, x::Vector{<:Number})::ComplexF64
  return evaluate([ct1], x)[1]
end

function evaluate(ct1::ComplexTPS, x::Real)::ComplexF64
  return evaluate([ct1], [x])[1]
end

# --- Integral/Derivative  ---
function integrate(t1::TPS, iv::Integer)::TPS
  t = zero(t1)
  mad_tpsa_integ!(t1.tpsa, t.tpsa, convert(Cint, iv))
  return t
end

function integrate(ct1::ComplexTPS, iv::Integer)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_integ!(ct1.tpsa, ct.tpsa, convert(Cint, iv))
  return ct
end

function derivative(t1::TPS, iv::Integer)::TPS
  t = zero(t1)
  mad_tpsa_deriv!(t1.tpsa, t.tpsa, convert(Cint, iv))
  return t
end

function derivative(ct1::ComplexTPS, iv::Integer)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_deriv!(ct1.tpsa, ct.tpsa, convert(Cint, iv))
  return ct
end

# Derivative wrt specific monomial
function derivative(t1::TPS, vars::Pair{<:Integer, <:Integer}...; params::Tuple{Vararg{Pair{<:Integer,<:Integer}}}=())::TPS
  t = zero(t1)
  # Need to create array of orders with length nv + np
  ords, n = pairs_to_m(vars,params=params)
  mad_tpsa_derivm!(t1.tpsa, t.tpsa, n, ords)
  return t
end

# Derivative wrt specific monomial
function derivative(ct1::ComplexTPS, vars::Pair{<:Integer, <:Integer}...; params::Tuple{Vararg{Pair{<:Integer,<:Integer}}}=())::ComplexTPS
  ct = zero(ct1)
  # Need to create array of orders with length nv + np
  ords, n = pairs_to_m(vars,params=params)
  mad_ctpsa_derivm!(ct1.tpsa, ct.tpsa, n, ords)
  return ct
end