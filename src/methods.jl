# --- Evaluate ---
# TPS:
function evaluate(m::Vector{TPS}, x::Vector{<:Real})::Vector{Float64}
  na = Cint(length(m))
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
  na = Cint(length(m))
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
function integrate(t1::TPS; var::Integer=1)::TPS
  t = zero(t1)
  mad_tpsa_integ!(t1.tpsa, t.tpsa, convert(Cint, var))
  return t
end

function integrate(ct1::ComplexTPS; var::Integer=1)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_integ!(ct1.tpsa, ct.tpsa, convert(Cint, var))
  return ct
end

function differentiate(t1::TPS, vars::Pair{<:Integer,<:Integer}...; var=0,param=0, params::Tuple{Vararg{Pair{<:Integer,<:Integer}}}=())::TPS
  t = zero(t1)
  if isempty(vars) && isempty(params)
    if param > 0
      if var > 0
        error("Use sparse monomial indexing to take derivative wrt higher order monomials.")
      end
      desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t1.tpsa).d))
      nv = desc.nv # TOTAL NUMBER OF VARS!!!!
      var = param + nv
    elseif var == 0
      var = 1
    end
    mad_tpsa_deriv!(t1.tpsa, t.tpsa, convert(Cint, var))
    return t
  else
    # Need to create array of orders with length nv + np
    ords, n = pairs_to_m(t1,vars...,params=params)
    mad_tpsa_derivm!(t1.tpsa, t.tpsa, n, ords)
    return t
  end
end

function differentiate(t1::ComplexTPS, vars::Pair{<:Integer,<:Integer}...; var=0,param=0, params::Tuple{Vararg{Pair{<:Integer,<:Integer}}}=())::ComplexTPS
  t = zero(t1)
  if isempty(vars) && isempty(params)
    if param > 0
      if var > 0
        error("Use sparse monomial indexing to take derivative wrt higher order monomials.")
      end
      desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t1.tpsa).d))
      nv = desc.nv # TOTAL NUMBER OF VARS!!!!
      var = param + nv
    elseif var == 0
      var = 1
    end
    mad_ctpsa_deriv!(t1.tpsa, t.tpsa, convert(Cint, var))
    return t
  else
    # Need to create array of orders with length nv + np
    ords, n = pairs_to_m(t1,vars...,params=params)
    mad_ctpsa_derivm!(t1.tpsa, t.tpsa, n, ords)
    return t
  end
end

"""
    getord(t::TPS, order::Integer)::TPS

Extracts one homogenous polynomial from the TPS of the given order.
"""
function getord(t1::TPS, order::Integer)::TPS
  t = zero(t1)
  mad_tpsa_getord!(t1.tpsa, t.tpsa, convert(Cuchar, order))
  return t
end


#=
function poissonbracket(t1::TPS, t2::TPS)::TPS
  t = zero(t1)
  mad_tpsa_poisbra!(t1.tpsa,t2.tpsa,t3.tpsa)
  return t
end
=#