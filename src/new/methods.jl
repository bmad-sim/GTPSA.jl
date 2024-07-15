# --- clear ---
clear!(t::NewTPS{Float64})    = mad_tpsa_clear!(t)
clear!(t::NewTPS{ComplexF64}) = mad_ctpsa_clear!(t)

"""
    norm(t1::TPS)::Float64

Calculates the 1-norm of the `TPS`, which is the sum of 
the `abs` of all coefficients.
"""
norm(t1::NewTPS{Float64}) = mad_tpsa_nrm(t1)
norm(t1::NewTPS{ComplexF64}) = mad_ctpsa_nrm(t1)

# --- setTPS! ---

"""
    setTPS!(t::NewTPS, t1::Number; change::Bool=false) 

General function for setting a TPS/ComplexTPS `t` equal to `t1`. If `change` is `true`,
then `t` and `t1` can have different `Descriptor`s (with invalid monomials removed) so 
long as the number of variables + number of parameters are equal.
"""
function setTPS!(t::NewTPS, t1::Number; change::Bool=false) 
  # if just a regular number
  if !(t1 isa NewTPS)
    clear!(t)
    t[0] = t1
    return
  end

  olddesc = getdesc(t1)
  newdesc = getdesc(t)

  # if not changing descriptors
  if olddesc == newdesc || !change 
    copy!(t, t1)
    return
  end

  # else we have to get fancy
  numnn(t) == numnn(t1) || error("Number of variables + parameters in GTPSAs do not agree!")
  nn = numnn(t)
  coef = Ref{eltype(t1)}()
  mono = Vector{Cuchar}(undef, nn)
  idx = cycle!(t1, -1, np+nv, mono, coef)
  while idx >= 0
    # if valid monomial in new descriptor:
    if convert(Bool, mad_desc_isvalidm(newdesc, nn, mono))
      setm!(t, nn, mono, 0, eltype(t)(coef[])) # set new tpsa
    end
    idx = cycle!(t1, idx, nn, mono, coef)
  end
end



# --- Evaluate ---
eval!(na::Cint, ma::Vector{NewTPS{Float64}}, nb::Cint, tb::Vector{Float64}, tc::Vector{Float64}) = mad_tpsa_eval!(na, ma, nb, tb, tc)
eval!(na::Cint, ma::Vector{NewTPS{ComplexF64}}, nb::Cint, tb::Vector{ComplexF64}, tc::Vector{ComplexF64}) = mad_ctpsa_eval!(na, ma, nb, tb, tc)

"""
    evaluate!(y::Vector{T}, F::Vector{NewTPS{T}}, x::Vector{<:Number}) where {T}

Evaluates the vector function `F` at the point `x`, and fills `y` with the result. 
"""
evaluate!(y::Vector{T}, F::Vector{NewTPS{T}}, x::Vector{<:Number}) where {T} = eval!(Cint(length(F)), F, Cint(length(x)), convert(Vector{T}, x), convert(Vector{T}, y))
evaluate(F::Vector{NewTPS{T}}, x::Vector{<:Number}) where {T} = (y = zeros(T,length(F)); evaluate!(y, F, x); return y)


# --- Integral ---
# Low-level equivalent calls for TPS and ComplexTPS:

"""
    integ!(t::T, t1::T, var::Integer=1) where {T<:NewTPS}
    ∫!(t::T, t1::T, var::Integer=1) where {T<:NewTPS}

Integrates `t1` wrt the variable `var` and fills `t` with the result. 
Integration wrt parameters is not allowed, and integration wrt higher order 
monomials is not currently supported.
"""
integ!(t::NewTPS{Float64},   t1::NewTPS{Float64},    var=1) = mad_tpsa_integ!(t1, t, Cint(var))
integ!(t::NewTPS{ComplexF64},t1::NewTPS{ComplexF64}, var=1) = mad_ctpsa_integ!(ctpsa1, ctpsa, Cint(var))
∫! = integ!

"""
    integ(t1::NewTPS, var::Integer=1)
    ∫(t1::NewTPS, var::Integer=1)

Integrates `t1` wrt the variable `var`. Integration wrt 
parameters is not allowed, and integration wrt higher order 
monomials is not currently supported.
"""
integ(t1::NewTPS{Float64},    var=1) = (t = zero(t1); integ!(t, t1, var); return t)
∫ = integ

# --- Derivative ---
# Low-level equivalent calls for TPS and ComplexTPS:
deriv!(tpsa1::NewTPS{Float64}, tpsa::NewTPS{Float64}, var::Cint) = (@inline; mad_tpsa_deriv!(tpsa1, tpsa, var))
deriv!(ctpsa1::NewTPS{ComplexF64}, ctpsa::NewTPS{ComplexF64}, var::Cint) = (@inline; mad_ctpsa_deriv!(ctpsa1, ctpsa, var))
derivm!(tpsa1::NewTPS{Float64}, tpsa::NewTPS{Float64}, n::Cint, ords::Vector{Cuchar}) = (@inline; mad_tpsa_derivm!(tpsa1, tpsa, n, ords))
derivm!(ctpsa1::NewTPS{ComplexF64}, ctpsa::NewTPS{ComplexF64}, n::Cint, ords::Vector{Cuchar}) = (@inline; mad_ctpsa_derivm!(ctpsa1, ctpsa, n, ords))

"""
    deriv!(t::T, t1::T, v::Union{TPSIndexType, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing) where {T<:NewTPS}
    ∂!(t::T, t1::T, v::Union{TPSIndexType, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing) where {T<:NewTPS}

Differentiates `t1` wrt the variable/parameter specified by the variable/parameter index, or 
alternatively any monomial specified by indexing-by-order OR indexing-by-sparse monomial, and 
sets `t` equal to the result in-place. See the `deriv` documentation for examples.

### Input
- `v`      -- An integer (for variable index), vector/tuple of orders for each variable (for indexing-by-order), or vector/tuple of pairs (sparse monomial)
- `param`  -- (Keyword argument, optional) An integer for the parameter index
- `params` -- (Keyword argument, optional) Vector/tuple of pairs for sparse-monomial indexing
"""
function deriv!(t::T, t1::T, v::Union{TPSIndexType, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing) where {T<:NewTPS}
  low_deriv!(t, t1, v, param, params)
end

∂! = deriv!

"""
    deriv(t1::NewTPS, v::Union{TPSIndexType, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing)
    ∂(t1::NewTPS, v::Union{TPSIndexType, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing)

Differentiates `t1` wrt the variable/parameter specified by the variable/parameter index, or 
alternatively any monomial specified by indexing-by-order OR indexing-by-sparse monomial.

### Input
- `v`      -- An integer (for variable index), vector/tuple of orders for each variable (for indexing-by-order), or vector/tuple of pairs (sparse monomial)
- `param`  -- (Keyword argument, optional) An integer for the parameter index
- `params` -- (Keyword argument, optional) Vector/tuple of pairs for sparse-monomial indexing

# Examples: Variable/Parameter Index:
```julia-repl
julia> d = Descriptor(1,5,1,5);

julia> x1 = vars(d)[1]; k1 = params(d)[1];

julia> deriv(x1*k1, 1)
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        0    1


julia> deriv(x1*k1, param=1)
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        1    0
```

# Examples: Monomial Index-by-Order
```julia-repl
julia> deriv(x1*k1, [1,0])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        0    1


julia> deriv(x1*k1, [0,1])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        1    0


julia> deriv(x1*k1, [1,1])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    0        0    0
```

# Examples: Monomial Index-by-Sparse Monomial
```julia-repl
julia> deriv(x1*k1, [1=>1])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        0    1


julia> deriv(x1*k1, params=[1=>1])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        1    0


julia> deriv(x1*k1, [1=>1], params=[1=>1])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    0        0    0
```
"""
function deriv(t1::NewTPS, v::Union{TPSIndexType, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing)
  t = zero(t1)
  low_deriv!(t, t1, v, param, params)
  return t
end

# Variable/parameter:
function low_deriv!(t::T, t1::T, v::Integer, param::Nothing, params::Nothing) where {T<:NewTPS}
  deriv!(t1, t, convert(Cint, v))
end

function low_deriv!(t::T, t1::T, v::Nothing, param::Integer, params::Nothing) where {T<:NewTPS}
  nv = numvars(t1)
  deriv!(t1, t, Cint(param)+nv)
end

# Default to first variable if nothing passed:
function low_deriv!(t::T, t1::T, v::Nothing, param::Nothing, params::Nothing) where {T<:NewTPS}
  low_deriv!(t1, 1, nothing, nothing)
end

# Monomial by order:
function low_deriv!(t::T, t1::T, v::MIndexType, param::Nothing, params::Nothing) where {T<:NewTPS}
  derivm!(t1, t, Cint(length(v)), convert(Vector{Cuchar}, v))
end

# Monomial by sparse monomial:
function low_deriv!(t::T, t1::T, v::SMIndexType, param::Nothing, params::SMIndexType) where {T<:NewTPS}
  # Need to create array of orders with length nv + np
  ords, n = pairs_to_m(t1,v,params=params)
  derivm!(t1, t, n, ords)
end

function low_deriv!(t::T, t1::T, v::SMIndexType, param::Nothing, params::Nothing) where {T<:NewTPS}
  # Need to create array of orders with length nv + np
  ords, n = pairs_to_m(t1,v)
  derivm!(t1, t, n, ords)
end

function low_deriv!(t::T, t1::T, v::Nothing, param::Nothing, params::SMIndexType) where {T<:NewTPS}
  # Need to create array of orders with length nv + np
  ords, n = pairs_to_m(t1,Pair{Int,Int}[],params=params)
  derivm!(t1, t, n, ords)
end

# Throw error if no above use cases satisfied:
function low_deriv(t::T, t1::T, v, param, params) where {T<:NewTPS}
  error("Invalid monomial specified. Please use ONE of variable/parameter index, index by order, or index by sparse monomial.")
end

∂ = deriv

# --- getord ---
# Low-level equivalent calls for TPS and ComplexTPS:
getord!(tpsa1::NewTPS{Float64}, tpsa::NewTPS{Float64}, order::Cuchar) = (@inline;  mad_tpsa_getord!(tpsa1, tpsa, order))
getord!(ctpsa1::NewTPS{ComplexF64}, ctpsa::NewTPS{ComplexF64}, order::Cuchar) = (@inline;  mad_ctpsa_getord!(ctpsa1, ctpsa, order))

"""
    getord!(t::T, t1::T, order::Integer) where {T<:NewTPS}

Extracts one homogenous polynomial from `t1` of the given order and 
fills `t` with the result in-place.
"""
function getord!(t::T, t1::T, order::Integer) where {T<:NewTPS}
  getord!(t1, t, Cuchar(order))
end

"""
    getord(t1::NewTPS, order::Integer)

Extracts one homogenous polynomial from `t1` of the given order.
"""
function getord(t1::NewTPS, order::Integer)
  t = zero(t1)
  getord!(t, t1, order)
  return t
end

# --- cutord ---
cutord!(tpsa1::NewTPS{Float64}, tpsa::NewTPS{Float64}, order::Cint) = (@inline;  mad_tpsa_cutord!(tpsa1, tpsa, order))
cutord!(ctpsa1::NewTPS{ComplexF64}, ctpsa::NewTPS{ComplexF64}, order::Cint) = (@inline;  mad_ctpsa_cutord!(ctpsa1, ctpsa, order))

"""
    cutord!(t::T, t1::T, order::Integer) where {T<:NewTPS}

Cuts out the monomials in `t1` at the given order and above. Or, if `order` 
is negative, will cut monomials with orders at and below `abs(order)`. `t` 
is filled in-place with the result. See the documentation for `cutord` for examples.
"""
function cutord!(t::T, t1::T, order::Integer) where {T<:NewTPS}
  cutord!(t1, t, convert(Cint, order))
end

"""
    cutord(t1::NewTPS, order::Integer)

Cuts out the monomials in `t1` at the given order and above. Or, if `order` 
is negative, will cut monomials with orders at and below `abs(order)`.

# Examples
```julia-repl
julia> d = Descriptor(1,10);

julia> x = vars(d);

julia> cutord(sin(x[1]), 5)
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        1
  -1.6666666666666666e-01    3        3


julia> cutord(sin(x[1]), -5)
TPS:
  Coefficient              Order     Exponent
  -1.9841269841269841e-04    7        7
   2.7557319223985893e-06    9        9
```
"""
function cutord(t1::NewTPS, order::Integer)
  t = zero(t1)
  cutord!(t, t1, order)
  return t
end

# --- clearord! ---
clrord!(t::NewTPS{Float64}, ord::Cuchar) = mad_tpsa_clrord!(t, ord)
clrord!(t::NewTPS{ComplexF64}, ord::Cuchar) = mad_ctpsa_clrord!(t, ord)

"""
    clearord!(t::NewTPS, order::Integer)

Clears all monomial coefficients in `t` at order `order`.
"""
clearord!(t::NewTPS, order::Integer) = clrord!(t, convert(Cuchar, order))

"""
    clearord(t1::NewTPS, order::Integer)

Returns a new TPS equal to `t1` but with all monomial coefficients 
at the given `order` cleared (set equal to 0).
"""
function clearord(t1::NewTPS, order::Integer)
  t = zero(t1)
  copy!(t,t1)
  clearord!(t,order)
  return t
end


# --- scalar ---
"""
    scalar(t::NewTPS)

Extracts the scalar part of the TPS. Equivalent to `t[0]` but 
this can be easily broadcasted.
"""
function scalar(t::NewTPS)
  return t[0]
end

# --- composition ---
compose!(na::Cint, ma::Vector{NewTPS{Float64}}, nb::Cint, mb::Vector{NewTPS{Float64}}, mc::Vector{NewTPS{Float64}}) = (@inline; mad_tpsa_compose!(na, ma, nb, mb, mc))
compose!(na::Cint, ma::Vector{NewTPS{ComplexF64}}, nb::Cint, mb::Vector{NewTPS{ComplexF64}}, mc::Vector{NewTPS{ComplexF64}}) = (@inline; mad_ctpsa_compose!(na, ma, nb, mb, mc))


"""
    compose!(m::Vector{<:NewTPS}, m2::Vector{<:NewTPS}, m1::Vector{<:NewTPS}; work_low::Union{Nothing,Tuple{Vararg{Vector{<:Union{NewTPS{Float64},NewTPS{ComplexF64}}}}}}=nothing, work_prom::Union{Nothing,Tuple{Vararg{Vector{<:ComplexTPS}}}}=nothing)

Composes the vector functions `m2 ∘ m1` and stores the result in-place in `m`. Promotion is allowed, provided 
the output vector function `m` has the correct promoted type. 

For all compositions, 3 temporary vectors must be generated that contain NewTPS{Float64} or NewTPS{ComplexF64}
for each TPS in the map (depending on output type), to pass to the low-level C composition function in GTPSA. 
They are correspondingly referred to as `outx_low`, `m2x_low`, and `m1x_low`. These three temporaries containers 
can be optionally passed as a tuple in `work_low`, and must satisfy the following requirements:

```
work_low[1] = outx_low   # Length >= length(m) = length(m2)
work_low[2] = m2x_low    # Length >= length(m2) = length(m)
work_low[3] = m1x_low    # Length >= length(m1)
```

If promotion is occuring, then one of the input vectors must be promoted to `ComplexTPS`. A vector of pre-allocated 
`ComplexTPS`s can optionally provided as the first argument in the `work_prom` tuple, and has the requirement:

If `eltype(m.x) != eltype(m1.x)` (then `m1` must be promoted):
`work_prom[1] = m1x_prom  # Length >= length(m1), Vector{ComplexTPS}`

else if `eltype(m.x) != eltype(m2.x)` (then `m2` must be promoted):
`work_prom[1] = m2x_prom  # Length >= length(m2) = length(m), Vector{ComplexTPS}`

Note that the `ComplexTPS`s in the vectors must be allocated and have the same `Descriptor`.
"""
function compose!(m::Vector{<:NewTPS}, m2::Vector{<:NewTPS}, m1::Vector{<:NewTPS}; work_low::Union{Nothing,Tuple{Vararg{Vector{<:Union{NewTPS{Float64},NewTPS{ComplexF64}}}}}}=nothing, work_prom::Union{Nothing,Tuple{Vararg{Vector{<:ComplexTPS}}}}=nothing)
  #desc = getdesc(first(m))
  n = length(m)
  n2 = length(m2)
  n1 = length(m1)

  #@assert n == n2 "Incorrect output length, received length $(length(m)) but need $(length(m2))"
  #@assert numnn(first(m2)) == n1 "Not enough input arguments"
  #@assert !(m === m1) "Cannot compose!(m, m2, m1) with m === m1"
  #@assert eltype(m) == promote_type(eltype(m2),eltype(m1)) "Cannot compose: output vector type $(eltype(m)) must be $(promote_type(eltype(m2),eltype(m1)))"
  outT = eltype(m)

  if !isnothing(work_low)
    outx_low = work_low[1]
    m2x_low = work_low[2]
    m1x_low = work_low[3]
    @assert length(outx_low) >= n "Incorrect length for work_low[1] = outx_low. Received $(length(outx_low)), should be >=$n"
    @assert length(m2x_low) >= n2 "Incorrect length for work_low[2] = m2x_low. Received $(length(m2x_low)), should be >=$n2"
    @assert length(m1x_low) >= n1 "Incorrect length for work_low[3] = m1x_low. Received $(length(m1x_low)), should be >=$n1"
  else
    outx_low = Vector{lowtype(outT)}(undef, n)
    m2x_low = Vector{lowtype(outT)}(undef, n)
    m1x_low = Vector{lowtype(outT)}(undef, n1)
  end

  if !isnothing(work_prom)
    if outT != eltype(m1)
      m1x_prom = work_prom[1]
      m2x_prom = nothing
      @assert length(m1x_prom) >= n1 "Incorrect length for work_prom[1] = m1x_prom: Received $(length(m1x_prom)), should be >=$n1"
    elseif outT != eltype(m2)
      m1x_prom = nothing
      m2x_prom = work_prom[1]
      @assert length(m2x_prom) >= n "Incorrect length for work_prom[1] = m2x_prom: Received $(length(m2x_prom)), should be >=$n"
    else
      m1x_prom = nothing
      m2x_prom = nothing
    end
  else
    if outT != eltype(m1)
      m1x_prom = Vector{ComplexTPS}(undef, n1)
      for i=1:n1  # Allocate
        @inbounds m1x_prom[i] = ComplexTPS(use=first(m))
      end
      m2x_prom = nothing
    elseif outT != eltype(m2)
      m1x_prom = nothing
      m2x_prom = Vector{ComplexTPS}(undef, n)
      for i=1:n
        @inbounds m2x_prom[i] = ComplexTPS(use=first(m))
      end
    else
      m1x_prom = nothing
      m2x_prom = nothing
    end
  end

  # Do the composition, promoting if necessary
  if outT != eltype(m1) 
    # Promote to ComplexTPS:
    for i=1:n1
      @inbounds complex!(m1x_prom[i], m1[i])
    end
    map!(t->t, m1x_low, m1x_prom)
  else
    map!(t->t, m1x_low, m1)
  end

  if outT != eltype(m2)
    # Promote to ComplexTPS:
    for i=1:n
      @inbounds complex!(m2x_prom[i], m2[i])
    end
    map!(t->t, m2x_low, m2x_prom)
  else
    map!(t->t, m2x_low, m2)
  end

  # go low
  map!(t->t, outx_low, m)

  GC.@preserve m1x_prom m2x_prom compose!(Cint(n), m2x_low, Cint(n1), m1x_low, outx_low)

  return
end

function ∘(ma::Vector{<:NewTPS}, mb::Vector{<:NewTPS})
  na = Cint(length(ma))
  nb = Cint(length(mb))
  # Ensure mb is length = input
  desc = getdesc(first(ma))
  if numnn(desc) != nb
    error("Not enough input arguments")
  end
  outT = promote_type(eltype(ma),eltype(mb))
  mc = Vector{outT}(undef, na)
  for i=1:na
    @inbounds mc[i] = outT(use=desc)
  end
  compose!(mc, ma, mb)
  return mc
end

# --- translate ---
translate!(na::Cint, ma::Vector{NewTPS{Float64}}, nb::Cint, tb::Vector{Float64}, mc::Vector{NewTPS{Float64}}) = mad_tpsa_translate!(na, ma, nb, tb, mc)
translate!(na::Cint, ma::Vector{NewTPS{ComplexF64}}, nb::Cint, tb::Vector{ComplexF64}, mc::Vector{NewTPS{ComplexF64}}) = mad_ctpsa_translate!(na, ma, nb, tb, mc)


"""
    translate!(mc::Vector{<:T}, ma::Vector{<:T}, tb::Vector{<:Number}) where {T<:NewTPS}

Fills `ma` with the vector function equal to `ma` with its expansion point translated by `tb`.

Two temporary vectors of either `NewTPS{Float64}` or `NewTPS{ComplexF64}` must be created, or they can optionally 
be passed as a tuple to the kwarg `work_low` where

`ma_low` = low corresponding to `ma` = `work_low[1]`
`mb_low` = low corresponding to `mb` = `work_low[2]`
"""
function translate!(mc::Vector{<:T}, ma::Vector{<:T}, tb::Vector{<:Number}; work_low::Union{Nothing,Tuple{Vararg{Vector{<:Union{NewTPS{Float64},NewTPS{ComplexF64}}}}}}=nothing) where {T<:NewTPS}
  desc = getdesc(first(ma))
  nb = Cint(length(tb))
  na = Cint(length(ma))
  numnn(desc) == nb || error("Not enough input arguments")
  length(mc) == na || error("Output vector length != input vector length")
  eltype(tb) == numtype(T) || error("Translation vector must have eltype $(numtype(T))")
  if !isnothing(work_low)
    ma_low = work_low[1]
    mc_low = work_low[2]
  else
    ma_low = map(t->t, ma)
    mc_low = map(t->t, mc)
  end
  translate!(na, ma_low, nb, tb, mc_low)
end

"""
Returns a vector function equal to `ma` with its expansion point translated by `tb`
"""
function translate(ma::Vector{<:T}, tb::Vector{<:Number}) where {T<:NewTPS}
  desc = getdesc(first(ma))
  nc = length(ma)
  mc = Vector{T}(undef, nc)
  for i=1:nc
    mc[i] = T(use=desc)
  end
  translate!(mc, ma, convert(Vector{numtype(T)}, tb))
  return mc
end