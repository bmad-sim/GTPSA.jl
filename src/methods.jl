# --- clear ---
clear!(t::Ptr{RTPSA}) = mad_tpsa_clear!(t)
clear!(t::Ptr{CTPSA}) = mad_ctpsa_clear!(t)

"""
    clear!(t::Union{TPS,ComplexTPS})

Clears the TPS (sets all monomial coefficients to 0).
"""
function clear!(t::Union{TPS,ComplexTPS})
  clear!(t.tpsa)
end

# --- complex! ---

"""
    complex!(ct::ComplexTPS, t::TPS)

Sets the pre-allocated `ComplexTPS` `ct` equal to `t`.
"""
function complex!(ct::ComplexTPS, t::TPS)
  mad_ctpsa_cplx!(t.tpsa, Base.unsafe_convert(Ptr{RTPSA}, C_NULL), ct.tpsa)
end

# --- Evaluate ---
eval!(na::Cint, ma::Vector{Ptr{RTPSA}}, nb::Cint, tb::Vector{Float64}, tc::Vector{Float64}) = mad_tpsa_eval!(na, ma, nb, tb, tc)
eval!(na::Cint, ma::Vector{Ptr{CTPSA}}, nb::Cint, tb::Vector{ComplexF64}, tc::Vector{ComplexF64}) = mad_ctpsa_eval!(na, ma, nb, tb, tc)

"""
    evaluate!(tc::Vector{<:Number}, m::Vector{T}, tb::Vector{<:Number}; work_low::Vector{<:Union{Ptr{RTPSA},Ptr{CTPSA}}}=Vector{lowtype(T)}(undef, length(m))) where {T<:Union{TPS,ComplexTPS}}

Evaluates the vector function `m` at the point `tb`, and fills `tc` with the result. 
An optional container `work_low` can be provided for containing the low-level TPS 
structs for zero allocations.    
"""
function evaluate!(tc::Vector{<:Number}, m::Vector{T}, tb::Vector{<:Number}; work_low::Vector{<:Union{Ptr{RTPSA},Ptr{CTPSA}}}=Vector{lowtype(T)}(undef, length(m))) where {T<:Union{TPS,ComplexTPS}}
  na = Cint(length(m))
  nb = Cint(length(tb))
  @assert na == nb "Vector lengths for TPSs and evaluation point disagree"
  @assert length(tc) == nb "Output vector length disagrees with input vector length"
  @assert length(work_low) == na "work_low vector length != input vector length"
  @assert eltype(tc) == numtype(T) "Output vector eltype should be $(numtype(T)), received $(typeof(tc))"

  map!(t->t.tpsa, work_low, m)
  eval!(na, work_low, nb, tb, tc)
  return
end

"""
    evaluate(m::Vector{T}, tb::Vector{<:Number}) where {T<:Union{TPS,ComplexTPS}}

Evaluates the vector function `m` at the point `tb`.
"""
function evaluate(m::Vector{T}, tb::Vector{<:Number}) where {T<:Union{TPS,ComplexTPS}}
  na = Cint(length(m))
  tc = Vector{numtype(T)}(undef, na)
  evaluate!(tc, m, tb)
  return tc
end

# --- Integral ---
# Low-level equivalent calls for TPS and ComplexTPS:
integ!(tpsa1::Ptr{RTPSA},  tpsa::Ptr{RTPSA}, var::Cint) = (@inline; mad_tpsa_integ!(tpsa1, tpsa, var))
integ!(ctpsa1::Ptr{CTPSA}, ctpsa::Ptr{CTPSA}, var::Cint) = (@inline; mad_ctpsa_integ!(ctpsa1, ctpsa, var))

"""
    integ!(t::T, t1::T, var::Integer=1) where {T<:Union{TPS,ComplexTPS}}
    ∫!(t::T, t1::T, var::Integer=1) where {T<:Union{TPS,ComplexTPS}}

Integrates `t1` wrt the variable `var` and fills `t` with the result. 
Integration wrt parameters is not allowed, and integration wrt higher order 
monomials is not currently supported.
"""
function integ!(t::T, t1::T, var::Integer=1) where {T<:Union{TPS,ComplexTPS}}
  integ(t1.tpsa, t.tpsa, Cint(var))
  return
end

∫! = integ!

"""
    integ(t1::Union{TPS, ComplexTPS}, var::Integer=1)
    ∫(t1::Union{TPS, ComplexTPS}, var::Integer=1)

Integrates `t1` wrt the variable `var`. Integration wrt 
parameters is not allowed, and integration wrt higher order 
monomials is not currently supported.
"""
function integ(t1::Union{TPS, ComplexTPS}, var::Integer=1)
  t = zero(t1)
  integ!(t, t1, var)
  return t
end

∫ = integ

# --- Derivative ---
# Low-level equivalent calls for TPS and ComplexTPS:
deriv!(tpsa1::Ptr{RTPSA}, tpsa::Ptr{RTPSA}, var::Cint) = (@inline; mad_tpsa_deriv!(tpsa1, tpsa, var))
deriv!(ctpsa1::Ptr{CTPSA}, ctpsa::Ptr{CTPSA}, var::Cint) = (@inline; mad_ctpsa_deriv!(ctpsa1, ctpsa, var))
derivm!(tpsa1::Ptr{RTPSA}, tpsa::Ptr{RTPSA}, n::Cint, ords::Vector{Cuchar}) = (@inline; mad_tpsa_derivm!(tpsa1, tpsa, n, ords))
derivm!(ctpsa1::Ptr{CTPSA}, ctpsa::Ptr{CTPSA}, n::Cint, ords::Vector{Cuchar}) = (@inline; mad_ctpsa_derivm!(ctpsa1, ctpsa, n, ords))

"""
    deriv!(t::T, t1::T, v::Union{TPSIndexType, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing) where {T<:Union{TPS,ComplexTPS}}
    ∂!(t::T, t1::T, v::Union{TPSIndexType, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing) where {T<:Union{TPS,ComplexTPS}}

Differentiates `t1` wrt the variable/parameter specified by the variable/parameter index, or 
alternatively any monomial specified by indexing-by-order OR indexing-by-sparse monomial, and 
sets `t` equal to the result in-place. See the `deriv` documentation for examples.

### Input
- `v`      -- An integer (for variable index), vector/tuple of orders for each variable (for indexing-by-order), or vector/tuple of pairs (sparse monomial)
- `param`  -- (Keyword argument, optional) An integer for the parameter index
- `params` -- (Keyword argument, optional) Vector/tuple of pairs for sparse-monomial indexing
"""
function deriv!(t::T, t1::T, v::Union{TPSIndexType, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing) where {T<:Union{TPS,ComplexTPS}}
  low_deriv!(t, t1, v, param, params)
end

∂! = deriv!

"""
    deriv(t1::Union{TPS,ComplexTPS}, v::Union{TPSIndexType, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing)
    ∂(t1::Union{TPS,ComplexTPS}, v::Union{TPSIndexType, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing)

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
function deriv(t1::Union{TPS,ComplexTPS}, v::Union{TPSIndexType, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing)
  t = zero(t1)
  low_deriv!(t, t1, v, param, params)
  return t
end

# Variable/parameter:
function low_deriv!(t::T, t1::T, v::Integer, param::Nothing, params::Nothing) where {T<:Union{TPS,ComplexTPS}}
  deriv!(t1.tpsa, t.tpsa, convert(Cint, v))
end

function low_deriv!(t::T, t1::T, v::Nothing, param::Integer, params::Nothing) where {T<:Union{TPS,ComplexTPS}}
  nv = numvars(t1)
  deriv!(t1.tpsa, t.tpsa, Cint(param)+nv)
end

# Default to first variable if nothing passed:
function low_deriv!(t::T, t1::T, v::Nothing, param::Nothing, params::Nothing) where {T<:Union{TPS,ComplexTPS}}
  low_deriv!(t1, 1, nothing, nothing)
end

# Monomial by order:
function low_deriv!(t::T, t1::T, v::MIndexType, param::Nothing, params::Nothing) where {T<:Union{TPS,ComplexTPS}}
  derivm!(t1.tpsa, t.tpsa, Cint(length(v)), convert(Vector{Cuchar}, v))
end

# Monomial by sparse monomial:
function low_deriv!(t::T, t1::T, v::SMIndexType, param::Nothing, params::SMIndexType) where {T<:Union{TPS,ComplexTPS}}
  # Need to create array of orders with length nv + np
  ords, n = pairs_to_m(t1,v,params=params)
  derivm!(t1.tpsa, t.tpsa, n, ords)
end

function low_deriv!(t::T, t1::T, v::SMIndexType, param::Nothing, params::Nothing) where {T<:Union{TPS,ComplexTPS}}
  # Need to create array of orders with length nv + np
  ords, n = pairs_to_m(t1,v)
  derivm!(t1.tpsa, t.tpsa, n, ords)
end

function low_deriv!(t::T, t1::T, v::Nothing, param::Nothing, params::SMIndexType) where {T<:Union{TPS,ComplexTPS}}
  # Need to create array of orders with length nv + np
  ords, n = pairs_to_m(t1,Pair{Int,Int}[],params=params)
  derivm!(t1.tpsa, t.tpsa, n, ords)
end

# Throw error if no above use cases satisfied:
function low_deriv(t::T, t1::T, v, param, params) where {T<:Union{TPS,ComplexTPS}}
  error("Invalid monomial specified. Please use ONE of variable/parameter index, index by order, or index by sparse monomial.")
end

∂ = deriv

# --- getord ---
# Low-level equivalent calls for TPS and ComplexTPS:
getord!(tpsa1::Ptr{RTPSA}, tpsa::Ptr{RTPSA}, order::Cuchar) = (@inline;  mad_tpsa_getord!(tpsa1, tpsa, order))
getord!(ctpsa1::Ptr{CTPSA}, ctpsa::Ptr{CTPSA}, order::Cuchar) = (@inline;  mad_ctpsa_getord!(ctpsa1, ctpsa, order))

"""
    getord!(t::T, t1::T, order::Integer) where {T<:Union{TPS,ComplexTPS}}

Extracts one homogenous polynomial from `t1` of the given order and 
fills `t` with the result in-place.
"""
function getord!(t::T, t1::T, order::Integer) where {T<:Union{TPS,ComplexTPS}}
  getord!(t1.tpsa, t.tpsa, Cuchar(order))
end

"""
    getord(t1::Union{TPS, ComplexTPS}, order::Integer)

Extracts one homogenous polynomial from `t1` of the given order.
"""
function getord(t1::Union{TPS, ComplexTPS}, order::Integer)
  t = zero(t1)
  getord!(t, t1, order)
  return t
end

# --- cutord ---
cutord!(tpsa1::Ptr{RTPSA}, tpsa::Ptr{RTPSA}, order::Cint) = (@inline;  mad_tpsa_cutord!(tpsa1, tpsa, order))
cutord!(ctpsa1::Ptr{CTPSA}, ctpsa::Ptr{CTPSA}, order::Cint) = (@inline;  mad_ctpsa_cutord!(ctpsa1, ctpsa, order))

"""
    cutord!(t::T, t1::T, order::Integer) where {T<:Union{TPS,ComplexTPS}}

Cuts out the monomials in `t1` at the given order and above. Or, if `order` 
is negative, will cut monomials with orders at and below `abs(order)`. `t` 
is filled in-place with the result. See the documentation for `cutord` for examples.
"""
function cutord!(t::T, t1::T, order::Integer) where {T<:Union{TPS,ComplexTPS}}
  cutord!(t1.tpsa, t.tpsa, convert(Cint, order))
end

"""
    cutord(t1::Union{TPS, ComplexTPS}, order::Integer)

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
function cutord(t1::Union{TPS, ComplexTPS}, order::Integer)
  t = zero(t1)
  cutord!(t, t1, order)
  return t
end


# --- scalar ---
"""
    scalar(t::Union{TPS,ComplexTPS})

Extracts the scalar part of the TPS. Equivalent to `t[0]` but 
this can be easily broadcasted.
"""
function scalar(t::Union{TPS,ComplexTPS})
  return t[0]
end

# --- composition ---
compose!(na::Cint, ma::Vector{Ptr{RTPSA}}, nb::Cint, mb::Vector{Ptr{RTPSA}}, mc::Vector{Ptr{RTPSA}}) = (@inline; mad_tpsa_compose!(na, ma, nb, mb, mc))
compose!(na::Cint, ma::Vector{Ptr{CTPSA}}, nb::Cint, mb::Vector{Ptr{CTPSA}}, mc::Vector{Ptr{CTPSA}}) = (@inline; mad_ctpsa_compose!(na, ma, nb, mb, mc))


"""
    compose!(m::Vector{<:Union{TPS,ComplexTPS}}, m2::Vector{<:Union{TPS,ComplexTPS}}, m1::Vector{<:Union{TPS,ComplexTPS}}; work_low::Union{Nothing,Tuple{Vararg{Vector{<:Union{Ptr{RTPSA},Ptr{CTPSA}}}}}}=nothing, work_prom::Union{Nothing,Tuple{Vararg{Vector{<:ComplexTPS}}}}=nothing)

Composes the vector functions `m2 ∘ m1` and stores the result in-place in `m`. Promotion is allowed, provided 
the output vector function `m` has the correct promoted type. 

For all compositions, 3 temporary vectors must be generated that contain Ptr{RTPSA} or Ptr{CTPSA}
for each TPS in the map (depending on output type), to pass to the low-level C composition function in GTPSA. 
They are correspondingly referred to as `outx_low`, `m2x_low`, and `m1x_low`. These three temporaries containers 
can be optionally passed as a tuple in `work_low`, and must satisfy the following requirements:

work_low[1] = outx_low   # Length >= length(m) = length(m2)
work_low[2] = m2x_low    # Length >= length(m2) = length(m)
work_low[3] = m1x_low    # Length >= length(m1)

If promotion is occuring, then one of the input vectors must be promoted to `ComplexTPS`. A vector of pre-allocated 
`ComplexTPS`s can optionally provided as the first argument in the `work_prom` tuple, and has the requirement:

If `eltype(m.x) != eltype(m1.x)` (then `m1` must be promoted):
work_prom[1] = m1x_prom  # Length >= length(m1), Vector{ComplexTPS}

else if `eltype(m.x) != eltype(m2.x)` (then `m2` must be promoted):
work_prom[1] = m2x_prom  # Length >= length(m2) = length(m), Vector{ComplexTPS}

Note that the `ComplexTPS`s in the vectors must be allocated and have the same `Descriptor`.
"""
function compose!(m::Vector{<:Union{TPS,ComplexTPS}}, m2::Vector{<:Union{TPS,ComplexTPS}}, m1::Vector{<:Union{TPS,ComplexTPS}}; work_low::Union{Nothing,Tuple{Vararg{Vector{<:Union{Ptr{RTPSA},Ptr{CTPSA}}}}}}=nothing, work_prom::Union{Nothing,Tuple{Vararg{Vector{<:ComplexTPS}}}}=nothing)
  desc = getdesc(first(m))
  n = length(m)
  n2 = length(m2)
  n1 = length(m1)

  @assert n == n2 "Incorrect output length, received length $(length(m)) but need $(length(m2))"
  @assert numnn(first(m2)) == n1 "Not enough input arguments"
  @assert !(m === m1) "Cannot compose!(m, m2, m1) with m === m1"
  @assert eltype(m) == promote_type(eltype(m2),eltype(m1)) "Cannot compose: output vector type $(eltype(m)) must be $(promote_type(eltype(m2),eltype(m1)))"
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
        @inbounds m1x_prom[i] = ComplexTPS(use=desc)
      end
      m2x_prom = nothing
    elseif outT != eltype(m2)
      m1x_prom = nothing
      m2x_prom = Vector{ComplexTPS}(undef, n)
      for i=1:n
        @inbounds m2x_prom[i] = ComplexTPS(use=desc)
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
    map!(t->t.tpsa, m1x_low, m1x_prom)
  else
    map!(t->t.tpsa, m1x_low, m1)
  end

  if outT != eltype(m2)
    # Promote to ComplexTPS:
    for i=1:n
      @inbounds complex!(m2x_prom[i], m2[i])
    end
    map!(t->t.tpsa, m2x_low, m2x_prom)
  else
    map!(t->t.tpsa, m2x_low, m2)
  end

  # go low
  map!(t->t.tpsa, outx_low, m)

  GC.@preserve m1x_prom m2x_prom compose!(Cint(n), m2x_low, Cint(n1), m1x_low, outx_low)

  return
end

function ∘(ma::Vector{<:Union{TPS,ComplexTPS}}, mb::Vector{<:Union{TPS,ComplexTPS}})
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

compose = ∘

# --- translate ---
translate!(na::Cint, ma::Vector{Ptr{RTPSA}}, nb::Cint, tb::Vector{Float64}, mc::Vector{Ptr{RTPSA}}) = mad_tpsa_translate!(na, ma, nb, tb, mc)
translate!(na::Cint, ma::Vector{Ptr{CTPSA}}, nb::Cint, tb::Vector{ComplexF64}, mc::Vector{Ptr{CTPSA}}) = mad_ctpsa_translate!(na, ma, nb, tb, mc)


"""
    translate!(mc::Vector{<:T}, ma::Vector{<:T}, tb::Vector{<:Number}) where {T<:Union{TPS,ComplexTPS}}

Fills `ma` with the vector function equal to `ma` with its expansion point translated by `tb`.

Two temporary vectors of either `Ptr{RTPSA}` or `Ptr{CTPSA}` must be created, or they can optionally 
be passed as a tuple to the kwarg `work_low` where

`ma_low` = low corresponding to `ma` = `work_low[1]`
`mb_low` = low corresponding to `mb` = `work_low[2]`
"""
function translate!(mc::Vector{<:T}, ma::Vector{<:T}, tb::Vector{<:Number}; work_low::Union{Nothing,Tuple{Vararg{Vector{<:Union{Ptr{RTPSA},Ptr{CTPSA}}}}}}=nothing) where {T<:Union{TPS,ComplexTPS}}
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
    ma_low = map(t->t.tpsa, ma)
    mc_low = map(t->t.tpsa, mc)
  end
  translate!(na, ma_low, nb, tb, mc_low)
end

"""
Returns a vector function equal to `ma` with its expansion point translated by `tb`
"""
function translate(ma::Vector{<:T}, tb::Vector{<:Number}) where {T<:Union{TPS,ComplexTPS}}
  desc = getdesc(first(ma))
  nc = length(ma)
  mc = Vector{T}(undef, nc)
  for i=1:nc
    mc[i] = T(use=desc)
  end
  translate!(mc, ma, convert(Vector{numtype(T)}, tb))
  return mc
end