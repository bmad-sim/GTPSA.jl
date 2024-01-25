# --- Evaluate ---
# TPS:
"""
    evaluate(m::Vector{TPS}, x::Vector{<:Real})::Vector{Float64}

Evaluates `m` at the point `x`.
"""
function evaluate(m::Vector{TPS}, x::Vector{<:Real})::Vector{Float64}
  na = Cint(length(m))
  ma = map(t->t.tpsa, m)
  nb = Cint(length(x))
  tb = convert(Vector{Float64}, x)
  tc = Vector{Float64}(undef, nb)
  mad_tpsa_eval!(na, ma, nb, tb, tc)
  return tc
end

"""
    evaluate(t1::TPS, x::Vector{<:Real})::Float64

Evaluates `t1` at the point `x`.
"""
function evaluate(t1::TPS, x::Vector{<:Real})::Float64
  return evaluate([t1], x)[1]
end

"""
    evaluate(t1::TPS, x::Real)::Float64

Evaluates `t1` at the point `x`.
"""
function evaluate(t1::TPS, x::Real)::Float64
  return evaluate([t1], [x])[1]
end

# ComplexTPS:
"""
    evaluate(m::Vector{ComplexTPS}, x::Vector{<:Number})::Vector{ComplexF64}

Evaluates `m` at the point `x`.
"""
function evaluate(m::Vector{ComplexTPS}, x::Vector{<:Number})::Vector{ComplexF64}
  na = Cint(length(m))
  ma = map(t->t.tpsa, m)
  nb = Cint(length(x))
  tb = convert(Vector{ComplexF64}, x)
  tc = Vector{ComplexF64}(undef, nb)
  mad_ctpsa_eval!(na, ma, nb, tb, tc)
  return tc
end

"""
    evaluate(ct1::ComplexTPS, x::Vector{<:Number})::ComplexF64

Evaluates `ct1` at the point `x`.
"""
function evaluate(ct1::ComplexTPS, x::Vector{<:Number})::ComplexF64
  return evaluate([ct1], x)[1]
end

"""
    evaluate(ct1::ComplexTPS, x::Real)::ComplexF64

Evaluates `ct1` at the point `x`.
"""
function evaluate(ct1::ComplexTPS, x::Real)::ComplexF64
  return evaluate([ct1], [x])[1]
end

# --- Integral/Derivative  ---

"""
    integrate(t1::TPS; var::Integer=1)::TPS

Integrates `t1` wrt the variable `var`. Note that integration wrt 
parameters is not allowed by definition.
"""
function integrate(t1::TPS; var::Integer=1)::TPS
  t = zero(t1)
  mad_tpsa_integ!(t1.tpsa, t.tpsa, convert(Cint, var))
  return t
end

"""
    integrate(ct1::ComplexTPS; var::Integer=1)::ComplexTPS

Integrates `ct1` wrt the variable `var`. Note that integration wrt 
parameters is not allowed by definition.
"""
function integrate(ct1::ComplexTPS; var::Integer=1)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_integ!(ct1.tpsa, ct.tpsa, convert(Cint, var))
  return ct
end


"""
    differentiate(t1::TPS, vars::Pair{<:Integer,<:Integer}...; var=0,param=0, params::Tuple{Vararg{Pair{<:Integer,<:Integer}}}=())::TPS

Differentiates `t1` wrt the variable or parameter specified by `var` or `param`, or alternatively 
any monomial specified by sparse monomial indexing.

# Examples
```julia-repl
julia> d = Descriptor(1,5,1,5);

julia> x1 = vars(d)[1]; k1 = params(d)[1];

julia> differentiate(x1*k1, var=1)
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        0    1


julia> differentiate(x1*k1, param=1)
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        1    0


julia> differentiate(x1*k1, 1=>1,params=(1=>1,))
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    0        0    0
```
"""
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

"""
    differentiate(ct1::ComplexTPS, vars::Pair{<:Integer,<:Integer}...; var=0,param=0, params::Tuple{Vararg{Pair{<:Integer,<:Integer}}}=())::ComplexTPS

Differentiates `ct1` wrt the variable or parameter specified by `var` or `param`, or alternatively 
any monomial specified by sparse monomial indexing.

# Examples
```julia-repl
julia> d = Descriptor(1,5,1,5);

julia> x1 = complexvars(d)[1]; k1 = complexparams(d)[1];

julia> differentiate(x1*k1, var=1)
ComplexTPS:
  Real                      Imag                     Order     Exponent
   1.0000000000000000e+00    0.0000000000000000e+00    1        0    1


julia> differentiate(x1*k1, param=1)
ComplexTPS:
  Real                      Imag                     Order     Exponent
   1.0000000000000000e+00    0.0000000000000000e+00    1        1    0


julia> differentiate(x1*k1, 1=>1,params=(1=>1,))
ComplexTPS:
  Real                      Imag                     Order     Exponent
   1.0000000000000000e+00    0.0000000000000000e+00    0        0    0
```
"""
function differentiate(ct1::ComplexTPS, vars::Pair{<:Integer,<:Integer}...; var=0,param=0, params::Tuple{Vararg{Pair{<:Integer,<:Integer}}}=())::ComplexTPS
  ct = zero(ct1)
  if isempty(vars) && isempty(params)
    if param > 0
      if var > 0
        error("Use sparse monomial indexing to take derivative wrt higher order monomials.")
      end
      desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(ct1.tpsa).d))
      nv = desc.nv # TOTAL NUMBER OF VARS!!!!
      var = param + nv
    elseif var == 0
      var = 1
    end
    mad_ctpsa_deriv!(ct1.tpsa, ct.tpsa, convert(Cint, var))
    return t
  else
    # Need to create array of orders with length nv + np
    ords, n = pairs_to_m(ct1,vars...,params=params)
    mad_ctpsa_derivm!(ct1.tpsa, ct.tpsa, n, ords)
    return ct
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

"""
    getord(ct1::ComplexTPS, order::Integer)::ComplexTPS

Extracts one homogenous polynomial from the ComplexTPS of the given order.
"""
function getord(ct1::ComplexTPS, order::Integer)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_getord!(ct1.tpsa, ct.tpsa, convert(Cuchar, order))
  return ct
end

"""
    poisbra(f::TPS, g::TPS)::TPS

Assuming the variables in the `TPSA` are canonical-conjugates, and ordered so that the canonically-
conjugate variables are consecutive (q1, p1, q2, p2, ...), computes the Poisson bracket 
of `f` and `g`. The Poisson bracket of two functions `[f, g]` is defined as 
`Σᵢ (∂f/∂qᵢ)(∂g/∂pᵢ) - (∂g/∂qᵢ)(∂f/∂pᵢ)`.

# Examples
```julia-repl
julia> d = Descriptor(4,10);

julia> x = vars(d);

julia> f = (x[1]^2 + x[2]^2)/2 + (x[3]^2 + x[4]^2)/2;

julia> poisbra(f,x[1])
TPS:
  Coefficient              Order     Exponent
  -1.0000000000000000e+00    1        0    1    0    0


julia> poisbra(f,x[2])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        1    0    0    0


julia> poisbra(f,x[3])
TPS:
  Coefficient              Order     Exponent
  -1.0000000000000000e+00    1        0    0    0    1


julia> poisbra(f,x[4])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        0    0    1    0
```
"""
function poisbra(f::TPS, g::TPS)::TPS
  t = zero(f)
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(f.tpsa).d))
  mad_tpsa_poisbra!(f.tpsa,g.tpsa,t.tpsa, desc.nv)
  return t
end

"""
    poisbra(f::ComplexTPS, g::ComplexTPS)::ComplexTPS

Assuming the variables in the `TPSA` are canonical-conjugates, and ordered so that the canonically-
conjugate variables are consecutive (q1, p1, q2, p2, ...), computes the Poisson bracket 
of `f` and `g`. The Poisson bracket of two functions `[f, g]` is defined as 
`Σᵢ (∂f/∂qᵢ)(∂g/∂pᵢ) - (∂g/∂qᵢ)(∂f/∂pᵢ)`.

```julia-repl
julia> d = Descriptor(4,10);

julia> x = complexvars(d);

julia> f = (x[1]^2 + x[2]^2)/2 + (x[3]^2 + x[4]^2)/2;

julia> poisbra(f,x[1])
ComplexTPS:
  Real                      Imag                     Order     Exponent
  -1.0000000000000000e+00   -0.0000000000000000e+00    1        0    1    0    0


julia> poisbra(f,x[2])
ComplexTPS:
  Real                      Imag                     Order     Exponent
   1.0000000000000000e+00    0.0000000000000000e+00    1        1    0    0    0


julia> poisbra(f,x[3])
ComplexTPS:
  Real                      Imag                     Order     Exponent
  -1.0000000000000000e+00   -0.0000000000000000e+00    1        0    0    0    1


julia> poisbra(f,x[4])
ComplexTPS:
  Real                      Imag                     Order     Exponent
   1.0000000000000000e+00    0.0000000000000000e+00    1        0    0    1    0
```
"""
function poisbra(f::ComplexTPS, g::ComplexTPS)::ComplexTPS
  ct = zero(f)
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(f.tpsa).d))
  mad_ctpsa_poisbra!(f.tpsa,g.tpsa,ct.tpsa, desc.nv)
  return ct
end

function poisbra(f::TPS, g::ComplexTPS)::ComplexTPS
  ct = zero(g)
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(f.tpsa).d))
  mad_ctpsa_tpoisbra!(f.tpsa,g.tpsa,ct.tpsa, desc.nv)
  return ct
end
function poisbra(f::ComplexTPS, g::TPS)::ComplexTPS
  ct = zero(f)
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(f.tpsa).d))
  mad_ctpsa_poisbrat!(f.tpsa,g.tpsa,ct.tpsa, desc.nv)
  return ct
end

function liebra(ma::Vector{TPS}, mb::Vector{TPS})::Vector{TPS}
  na = Cint(length(ma))
  m1 = map(t->t.tpsa, ma)
  m2 = map(t->t.tpsa, mb)
  mc = Vector{TPS}(undef, na)
  for i in eachindex(mc)
    mc[i] = zero(ma[1])
  end
  m3 = map(t->t.tpsa, mc)
  mad_tpsa_liebra!(na, m1, m2, m3)
  return mc
end

function liebra(ma::Vector{ComplexTPS}, mb::Vector{ComplexTPS})::Vector{ComplexTPS}
  na = Cint(length(ma))
  m1 = map(t->t.tpsa, ma)
  m2 = map(t->t.tpsa, mb)
  mc = Vector{ComplexTPS}(undef, na)
  for i in eachindex(mc)
    mc[i] = zero(ma[1])
  end
  m3 = map(t->t.tpsa, mc)
  mad_ctpsa_liebra!(na, m1, m2, m3)
  return mc
end