# --- norm --- 

"""
    normTPS(t1::TPS)

Calculates the 1-norm of the `TPS`, which is the sum of 
the `abs` of all coefficients.
"""
normTPS(t1::RealTPS) = mad_tpsa_nrm(t1)
normTPS(t1::ComplexTPS) = mad_ctpsa_nrm(t1)

# --- setTPS! ---

"""
    setTPS!(t::TPS, t1::Number; change::Bool=false) -> t

General function for setting a TPS `t` equal to `t1`. If `change` is `true`,
then `t` and `t1` can have different `Descriptor`s (with invalid monomials removed) so 
long as the number of variables + number of parameters are equal.
"""
function setTPS!(t::TPS, t1::Number; change::Bool=false) 
  # if just a regular number
  if !(t1 isa TPS)
    clear!(t)
    t[0] = t1
    return t
  end

  olddesc = getdesc(t1)
  newdesc = getdesc(t)

  # if not changing descriptors
  if olddesc == newdesc || !change 
    copy!(t, t1)
    return t
  end

  # else we have to get fancy
  numnn(t) == numnn(t1) || error("Number of variables + parameters in GTPSAs do not agree!")
  nn = numnn(t)
  coef = Ref{numtype(t1)}()
  mono = Vector{Cuchar}(undef, nn)
  idx = cycle!(t1, -1, nn, mono, coef)
  while idx >= 0
    # if valid monomial in new descriptor:
    if convert(Bool, mad_desc_isvalidm(newdesc.desc, nn, mono))
      setm!(t, nn, mono, 0, numtype(t)(coef[])) # set new tpsa
    end
    idx = cycle!(t1, idx, nn, mono, coef)
  end
  return t
end



# --- Evaluate ---
mad_eval!(na, ma::Union{AbstractArray{TPS{T}},TPS{T}}, nb, tb::AbstractArray{T}, tc::Union{Ref{T},AbstractArray{T}}) where {T<:Float64} = mad_tpsa_eval!(Cint(na), ma, Cint(nb), tb, tc)
mad_eval!(na, ma::Union{AbstractArray{TPS{T}},TPS{T}}, nb, tb::AbstractArray{T}, tc::Union{Ref{T},AbstractArray{T}}) where {T<:ComplexF64} = mad_ctpsa_eval!(Cint(na), ma, Cint(nb), tb, tc)

#mad_eval!(na, ma::TPS{T}, nb, tb::AbstractArray{T}, tc::Ref{T}) where {T<:Float64} = mad_tpsa_eval!(Cint(na), ma, Cint(nb), tb, tc)
#mad_eval!(na, ma::TPS{T}, nb, tb::AbstractArray{T}, tc::Ref{T}) where {T<:ComplexF64} = mad_ctpsa_eval!(Cint(na), ma, Cint(nb), tb, tc)


"""
    evaluate!(y::Union{Ref{T},AbstractArray{T}}, m::Union{TPS{T},AbstractArray{TPS{T}}}, x::AbstractArray{T}) where {T<:Union{Float64,ComplexF64}} -> y
  
Evaluates the `TPS` function `m` at the point `x` and fills `y` with the result.
"""
function evaluate!(y::Union{Ref{T},AbstractArray{T}}, m::Union{TPS{T},AbstractArray{TPS{T}}}, x::AbstractArray{T}) where {T<:Union{Float64,ComplexF64}}
  ny = length(y)
  n = length(m)
  nx = length(x)

  ny == n || error("Incorrect output length, received length $ny but need $n")
  numnn(first(m)) == nx || error("Not enough input arguments")
  ismutable(y) || error("Output y must be mutable!")
  !(y === x) || error("Cannot evaluate!(y, m, x) with y === x")

  mad_eval!(n, m, nx, x, y)
  return y
end


"""
    evaluate(m::Union{TPS,AbstractArray{<:TPS}}, x::AbstractArray{<:Number})

Evaluates the `TPS` function `m` at the result `x`.
"""
evaluate

function evaluate(m::AbstractArray{<:TPS}, x::AbstractArray{<:Number})
  (mprom, xprom) = promote_arrays_numtype(m, x)
  y = copymutable_oftype(xprom, eltype(xprom))
  evaluate!(y, mprom, xprom)
  return y
end

function evaluate(f::TPS, x::AbstractArray{<:Number})
  T = promote_type(typeof(f), eltype(x))
  T == typeof(f) ? fprom = f : fprom = T(f)
  numtype(T) == eltype(x) ? xprom = x : xprom = numtype(T).(x)
  g = Ref{numtype(T)}() #(use=f)
  evaluate!(g, fprom, xprom)
  return g[]
end


# --- F . grad ---
"""
    fgrad!(g::T, F::AbstractArray{<:T}, h::T) where {T<:TPS} -> g

Calculates `F⋅∇h` and sets `g` equal to the result.
"""
function fgrad!(g::T, F::AbstractArray{<:T}, h::T) where {T<:TPS} 
  nv = numvars(h)
  length(F) == nv || error("Incorrect length of F; received $(length(F)), should be $nv")
  !(g === h) || error("Cannot fgrad!(g, F, h) with g === h")
  
  if numtype(T) != Float64
      GTPSA.mad_ctpsa_fgrad!(Cint(length(F)), F, h, g)
  else
      GTPSA.mad_tpsa_fgrad!(Cint(length(F)), F, h, g)
  end
  return g
end
  
"""
    fgrad(F::AbstractArray{<:TPS}, h::TPS)

Calculates `F⋅∇h`.
"""
function fgrad(F::AbstractArray{<:TPS}, h::TPS)
  T = promote_type(typeof(h), eltype(F))
  T == typeof(h) ? hprom = h : hprom = T(h)
  numtype(T) == eltype(F) ? Fprom = F : Fprom = numtype(T).(F)
  g = zero(hprom)
  fgrad!(g, Fprom, hprom)
  return g
end


# --- Integral ---
"""
    integ!(t::TPS{T}, t1::TPS{T}, var::Integer=1) where {T} -> t
    ∫!(t::TPS{T}, t1::TPS{T}, var::Integer=1)     where {T} -> t

Integrates `t1` wrt the variable `var` and fills `t` with the result. 
Integration wrt parameters is not allowed, and integration wrt higher order 
monomials is not currently supported.
"""
integ!(t::TPS{Float64},   t1::TPS{Float64},    var=1) = (mad_tpsa_integ!(t1, t, Cint(var)); return t)
integ!(t::TPS{ComplexF64},t1::TPS{ComplexF64}, var=1) = (mad_ctpsa_integ!(ctpsa1, ctpsa, Cint(var)); return t)
const ∫! = integ!

"""
    integ(t1::TPS, var::Integer=1)
    ∫(t1::TPS, var::Integer=1)

Integrates `t1` wrt the variable `var`. Integration wrt 
parameters is not allowed, and integration wrt higher order 
monomials is not currently supported.
"""
integ(t1::TPS, var=1) = (t = zero(t1); integ!(t, t1, var); return t)
const ∫ = integ

# --- Derivative ---
# Low-level equivalent calls for TPS and ComplexTPS64:
mad_deriv!( t1::TPS{Float64},    t::TPS{Float64},    var) = mad_tpsa_deriv!(t1, t, Cint(var))
mad_deriv!( t1::TPS{ComplexF64}, t::TPS{ComplexF64}, var) = mad_ctpsa_deriv!(t1, t, Cint(var))
mad_derivm!(t1::TPS{Float64},    t::TPS{Float64},    n, ords) = mad_tpsa_derivm!(t1, t, Cint(n), convert(Vector{UInt8},ords))
mad_derivm!(t1::TPS{ComplexF64}, t::TPS{ComplexF64}, n, ords) = mad_ctpsa_derivm!(t1, t, Cint(n), convert(Vector{UInt8}, ords))

"""
    deriv!(t::TPS{T}, t1::TPS{T}, v::Union{TPSIndexType, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing) where {T} -> t 
    ∂!(t::TPS{T}, t1::TPS{T}, v::Union{TPSIndexType, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing)     where {T} -> t

Differentiates `t1` wrt the variable/parameter specified by the variable/parameter index, or 
alternatively any monomial specified by indexing-by-order OR indexing-by-sparse monomial, and 
sets `t` equal to the result in-place. See the `deriv` documentation for examples.

### Input
- `v`      -- An integer (for variable index), vector/tuple of orders for each variable (for indexing-by-order), or vector/tuple of pairs (sparse monomial)
- `param`  -- (Keyword argument, optional) An integer for the parameter index
- `params` -- (Keyword argument, optional) Vector/tuple of pairs for sparse-monomial indexing
"""
function deriv!(t::TPS{T}, t1::TPS{T}, v::Union{TPSIndexType, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing) where {T}
  low_deriv!(t, t1, v, param, params)
  return t
end

# Variable/parameter:
low_deriv!(t::TPS{T}, t1::TPS{T}, v::Integer, param::Nothing, params::Nothing) where {T} = mad_deriv!(t1, t, v)
low_deriv!(t::TPS{T}, t1::TPS{T}, v::Nothing, param::Integer, params::Nothing) where {T} = mad_deriv!(t1, t, param+numvars(t))

# Default to first variable if nothing passed:
low_deriv!(t::TPS{T}, t1::TPS{T}, v::Nothing, param::Nothing, params::Nothing) where {T} = low_deriv!(t1, 1, nothing, nothing)

# Monomial by order:
low_deriv!(t::TPS{T}, t1::TPS{T}, v::MIndexType, param::Nothing, params::Nothing) where {T} = mad_derivm!(t1, t, length(v), v)

# Monomial by sparse monomial:
function low_deriv!(t::TPS{T}, t1::TPS{T}, v::SMIndexType, param::Nothing, params::SMIndexType) where {T}
  # Need to create array of orders with length nv + np
  ords, n = pairs_to_m(t1,v,params=params)
  mad_derivm!(t1, t, n, ords)
end

function low_deriv!(t::TPS{T}, t1::TPS{T}, v::SMIndexType, param::Nothing, params::Nothing) where {T}
  # Need to create array of orders with length nv + np
  ords, n = pairs_to_m(t1,v)
  mad_derivm!(t1, t, n, ords)
end

function low_deriv!(t::TPS{T}, t1::TPS{T}, v::Nothing, param::Nothing, params::SMIndexType) where {T}
  # Need to create array of orders with length nv + np
  ords, n = pairs_to_m(t1,Pair{Int,Int}[],params=params)
  mad_derivm!(t1, t, n, ords)
end

# Throw error if no above use cases satisfied:
low_deriv(t::TPS{T}, t1::TPS{T}, v, param, params) where {T} = error("Invalid monomial specified. Please use ONE of variable/parameter index, index by order, or index by sparse monomial.")

const ∂! = deriv!

"""
    deriv(t1::TPS, v::Union{TPSIndexType, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing)
    ∂(t1::TPS, v::Union{TPSIndexType, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing)

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
function deriv(t1::TPS, v::Union{TPSIndexType, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing)
  t = zero(t1)
  low_deriv!(t, t1, v, param, params)
  return t
end

const ∂ = deriv

# --- getord ---

"""
    getord!(t::TPS{T}, t1::TPS{T}, order::Integer) where {T} -> t

Extracts one homogenous polynomial from `t1` of the given order and 
fills `t` with the result in-place.
"""
getord!(t::TPS{Float64},    t1::TPS{Float64},    order::Integer) = (mad_tpsa_getord!(t1, t, UInt8(order)); return t)
getord!(t::TPS{ComplexF64}, t1::TPS{ComplexF64}, order::Integer) = (mad_ctpsa_getord!(t1, t, UInt8(order)); return t)

"""
    getord(t1::TPS, order::Integer)

Extracts one homogenous polynomial from `t1` of the given order.
"""
getord(t1::TPS, order::Integer) = (t = zero(t1); getord!(t, t1, order); return t)


# --- cutord ---
"""
    cutord!(t::TPS{T}, t1::TPS{T}, order::Integer) where {T<:TPS} -> t

Cuts out the monomials in `t1` at the given order and above. Or, if `order` 
is negative, will cut monomials with orders at and below `abs(order)`. `t` 
is filled in-place with the result. See the documentation for `cutord` for examples.
"""
cutord!(t::TPS{Float64},    t1::TPS{Float64},    order::Integer) = (mad_tpsa_cutord!(t1, t, Cint(order)); return t)
cutord!(t::TPS{ComplexF64}, t1::TPS{ComplexF64}, order::Integer) = (mad_ctpsa_cutord!(t1, t, Cint(order)); return t)

"""
    cutord(t1::TPS, order::Integer)

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
cutord(t1::TPS, order::Integer) = (t = zero(t1); cutord!(t, t1, order); return t)

# --- clearord! ---
"""
    clearord!(t::TPS, order::Integer) -> t

Clears all monomial coefficients in `t` at order `order`.
"""
clearord!(t::TPS{Float64},    ord::Integer) = (mad_tpsa_clrord!(t, UInt8(ord)); return t)
clearord!(t::TPS{ComplexF64}, ord::Integer) = (mad_ctpsa_clrord!(t, UInt8(ord)); return t)


"""
    clearord(t1::TPS, order::Integer)

Returns a new TPS equal to `t1` but with all monomial coefficients 
at the given `order` cleared (set equal to 0).
"""
clearord(t1::TPS, order::Integer) = (t = zero(t1); copy!(t,t1); clearord!(t,order); return t)


# --- scalar ---
"""
    scalar(t::TPS)

Extracts the scalar part of the TPS. Equivalent to `t[0]` but 
this can be easily broadcasted.
"""
scalar(t::TPS) = t[0]

# fallback for non TPS types 
scalar(t::Number) = t[1]

# --- composition ---
mad_compose!(na, ma::Union{AbstractArray{T},T}, nb, mb::AbstractArray{T}, mc::Union{AbstractArray{T},T}) where {T<:TPS64}        = mad_tpsa_compose!(Cint(na), ma, Cint(nb), mb, mc)
mad_compose!(na, ma::Union{AbstractArray{T},T}, nb, mb::AbstractArray{T}, mc::Union{AbstractArray{T},T}) where {T<:ComplexTPS64} = mad_ctpsa_compose!(Cint(na), ma, Cint(nb), mb, mc)

"""
    compose!(m::Union{AbstractArray{T},T}, m2::Union{AbstractArray{T},T}, m1::AbstractArray{T}) where {T<:Union{TPS64,ComplexTPS64}} -> m

Composes the `TPS`s `m2 ∘ m1`, and stores the result in-place in `m`. 
"""
function compose!(m::Union{AbstractArray{T},T}, m2::Union{AbstractArray{T},T}, m1::AbstractArray{T}) where {T<:Union{TPS64,ComplexTPS64}}
  n = length(m)
  n2 = length(m2)
  n1 = length(m1)

  # Checks:
  n == n2 || error("Incorrect output length, received length $n but need $n2")
  numnn(first(m2)) == n1 || error("Not enough input arguments")
  !(m === m1) || error("Cannot compose!(m, m2, m1) with m === m1")

  mad_compose!(-n, m2, n1, m1, m)
  return m
end

"""
    compose(m2::Union{TPS,AbstractArray{<:TPS}}, m1::AbstractArray{<:TPS})

Composes the `TPS`s `m2 ∘ m1`
"""
compose

function compose(m2::AbstractArray{<:TPS}, m1::AbstractArray{<:TPS})
  (m2prom, m1prom) = promote_arrays_numtype(m2, m1)
  m = similar(m2prom)
  for i in eachindex(m)
    m[i] = eltype(m2prom)(use=first(m2prom))
  end
  compose!(m, m2prom, m1prom)
  return m
end

function compose(f::TPS, m1::AbstractArray{<:TPS})
  T = promote_type(typeof(f), eltype(m1))
  T == typeof(f) ? fprom = f : fprom = T(f)
  T == eltype(m1) ? m1prom = m1 : m1prom = T.(m1)
  g = T(use=f)
  compose!(g, fprom, m1prom)
  return g
end

∘(m2::Union{TPS,AbstractArray{<:TPS}}, m1::AbstractArray{<:TPS}) = compose(m2, m1)

# --- translate ---
mad_translate!(na, ma::Union{AbstractArray{T},T}, nb, tb::AbstractArray{Float64},    mc::Union{AbstractArray{T},T}) where {T<:TPS64}        = mad_tpsa_translate!(Cint(na), ma, Cint(nb), tb, mc)
mad_translate!(na, ma::Union{AbstractArray{T},T}, nb, tb::AbstractArray{ComplexF64}, mc::Union{AbstractArray{T},T}) where {T<:ComplexTPS64} = mad_ctpsa_translate!(Cint(na), ma, Cint(nb), tb, mc)

"""
    translate!(m::Union{AbstractArray{T},T}, m1::Union{AbstractArray{T},T}, x::AbstractArray{U}) where {U<:Union{Float64,ComplexF64},T<:TPS{U}} -> m

Sets `m` to `m1` with its expansion point translated by `x`.
"""
function translate!(m::Union{AbstractArray{T},T}, m1::Union{AbstractArray{T},T}, x::AbstractArray{U}) where {U<:Union{Float64,ComplexF64},T<:TPS{U}}
  n = length(m)
  n1 = length(m1)
  nx = length(x)

  # Checks:
  n == n1 || error("Incorrect output length, received length $n but need $n1")
  numnn(first(m1)) == nx || error("Not enough input arguments")
  !(m === m1) || error("Cannot translate!(m, m1, x) with m === m1")

  mad_translate!(n1, m1, nx, x, m)
  return m
end

"""
    translate(m1::AbstractArray{<:TPS}, x::AbstractArray{<:Number})

Translates the expansion point of `m1` by `x`.
"""
translate

function translate(m1::AbstractArray{<:TPS}, x::AbstractArray{<:Number})
  (m1prom, xprom) = promote_arrays_numtype(m1, x)
  m = similar(m1prom)
  for i in eachindex(m)
    m[i] = eltype(m1prom)(use=first(m1prom))
  end
  translate!(m, m1prom, xprom)
  return m
end

function translate(f::TPS, x::AbstractArray{<:Number})
  T = promote_type(typeof(f), eltype(x))
  T == typeof(f) ? fprom = f : fprom = T(f)
  numtype(T) == eltype(x) ? xprom = x : xprom = numtype(T).(x)
  g = T(use=f)
  translate!(g, fprom, xprom)
  return g
end
