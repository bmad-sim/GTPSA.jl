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
"""
    evaluate!(y, m, x) -> y
  
Evaluates the `TPS` function `m` at the point `x` and mutates `y` to contain the result.
  
# Arguments
- `y::Union{Ref{T},AbstractArray{T}}`          -- Container to store result of evaluating `m` at `x`
- `m::Union{TPS{T,D},AbstractArray{TPS{T,D}}}` -- `TPS` or `TPS` map to evaluate
- `x::Union{Ref{T},AbstractArray{T}}`          -- Value(s) of the variables to evaluate `m` at
"""
function evaluate!(
  y::Union{Ref{T},AbstractArray{T}}, 
  m::Union{TPS{T,D},AbstractArray{TPS{T,D}}}, 
  x::Union{Ref{T},AbstractArray{T}}
) where {T<:Union{Float64,ComplexF64},D}
  ny = length(y)
  n = length(m)
  nx = length(x)

  ny == n || error("Incorrect output length, received length $ny but need $n")
  numnn(first(m)) == nx || error("Not enough input arguments")
  ismutable(y) || error("Output y must be mutable!")
  !(y === x) || error("Cannot evaluate!(y, m, x) with y === x")
  T == Float64 ? mad_tpsa_eval!(Cint(n), m, Cint(nx), x, y) : mad_ctpsa_eval!(Cint(n), m, Cint(nx), x, y)
  return y
end


"""
    evaluate(m, x)

Evaluates the `TPS` function `m` at the result `x`.

# Arguments
- `m::Union{TPS,AbstractArray{TPS{T,D}}}`    -- `TPS` or `TPS` map to evaluate
- `x::Union{Number,AbstractArray{<:Number}}` -- Value(s) of the variable(s) to evaluate `m` at

`Evaluate` can also be called by "calling" `m`, e.g. `m(x)`, `m([x[1], x[2], ...])`, or 
`m(x[1], x[2], ...)` is also allowed.
"""
function evaluate(m::Union{TPS,AbstractArray{TPS{T1,D}}}, x::Union{Number,AbstractArray{<:Number}}) where {T1,D}
  T = promote_type(eltype(m), eltype(x))
  T == eltype(m) ? mprom = m : mprom = T.(m)
  GTPSA.numtype(T) == eltype(x) ? xprom = x : xprom = (GTPSA.numtype(T)).(x)
  if xprom isa Number
    xprom = Ref{GTPSA.numtype(T)}(xprom)
  end
  m isa TPS ? y = Ref{GTPSA.numtype(T)}() : y = similar(mprom, GTPSA.numtype(T))
  evaluate!(y, mprom, xprom)
  return m isa TPS ? y[] : y
end

(f::TPS)(x::AbstractArray{<:Number}) = evaluate(f, x)
(f::TPS)(x::Number) = evaluate(f, x)
(f::TPS)(x::Number, xs::Number...) = evaluate(f, vcat(x, collect(xs)))

(f::AbstractArray{TPS{T,D}})(x::AbstractArray{<:Number}) where {T,D} = evaluate(f, x)
(f::AbstractArray{TPS{T,D}})(x::Number) where {T,D} = evaluate(f, x)
(f::AbstractArray{TPS{T,D}})(x::Number, xs::Number...) where {T,D} = evaluate(f, vcat(x, collect(xs)))



# --- F . grad ---
"""
    fgrad!(g::TPS{T}, F::AbstractArray{TPS{T,D}}, h::TPS{T}) where {T,D} -> g

Calculates `F⋅∇h` and sets `g` equal to the result.
"""
function fgrad!(g::TPS{T}, F::AbstractArray{TPS{T,D}}, h::TPS{T}) where {T,D} 
  nv = numvars(h)
  length(F) == nv || error("Incorrect length of F; received $(length(F)), should be $nv")
  !(g === h) || error("Cannot fgrad!(g, F, h) with g === h")
  T == Float64 ? mad_tpsa_fgrad!(Cint(length(F)), F, h, g) : mad_ctpsa_fgrad!(Cint(length(F)), F, h, g)
  return g
end
  
"""
    fgrad(F::AbstractArray{TPS{T,D}}, h::TPS) where {T,D}

Calculates `F⋅∇h`.
"""
function fgrad(F::AbstractArray{TPS{T,D}}, h::TPS) where {T,D}
  T1 = promote_type(typeof(h), eltype(F))
  T1 == typeof(h) ? hprom = h : hprom = T1(h)
  T1 == eltype(F) ? Fprom = F : Fprom = T1.(F)
  g = zero(hprom)
  fgrad!(g, Fprom, hprom)
  return g
end


# --- Integral ---
"""
    integ!(t::TPS{T}, t1::TPS{T}, var=1) where {T} -> t
    ∫!(t::TPS{T}, t1::TPS{T}, var=1) where {T} -> t

Integrates `t1` wrt the variable `var` and sets `t` equal to the result. 
Integration with respect to parameters is not allowed.
"""
function integ!(t::TPS{T}, t1::TPS{T}, var=1) where {T}
  T == Float64 ? mad_tpsa_integ!(t1, t, Cint(var)) : mad_ctpsa_integ!(t1, t, Cint(var))
  return t
end
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
    deriv!(t::TPS{T}, t1::TPS{T}, monomialindex) where {T} -> t 
    ∂!(t::TPS{T}, t1::TPS{T}, monomialindex)     where {T} -> t

Differentiates `t1` wrt the variable/parameter specified by the variable/parameter index, or 
alternatively any monomial specified by indexing-by-order OR indexing-by-sparse monomial, and 
sets `t` equal to the result in-place. See the `deriv` documentation for examples.
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
    deriv(t1::TPS, monomialindex)
    ∂(t1::TPS, monomialindex)

Differentiates `t1` wrt the variable/parameter specified by the variable/parameter index, or 
alternatively any monomial specified by indexing-by-order OR indexing-by-sparse monomial.

# Examples: Variable/Parameter Index:
```julia-repl
julia> d = Descriptor(2, 5);

julia> Δx = @vars(d);

julia> deriv(Δx[1] + 2*Δx[2] + 3*Δx[1]*Δx[2], 1)
TPS64{Descriptor(NV=2, MO=5)}:
 Coefficient                Order   Exponent
  1.0000000000000000e+00      0      0   0
  3.0000000000000000e+00      1      0   1


julia> deriv(Δx[1] + 2*Δx[2] + 3*Δx[1]*Δx[2], 2)
TPS64{Descriptor(NV=2, MO=5)}:
 Coefficient                Order   Exponent
  2.0000000000000000e+00      0      0   0
  3.0000000000000000e+00      1      1   0
```

# Examples: Monomial Index-by-Order
```julia-repl
julia> deriv(Δx[1] + 2*Δx[2] + 3*Δx[1]*Δx[2], [1,0])
TPS64{Descriptor(NV=2, MO=5)}:
 Coefficient                Order   Exponent
  1.0000000000000000e+00      0      0   0
  3.0000000000000000e+00      1      0   1


julia> deriv(Δx[1] + 2*Δx[2] + 3*Δx[1]*Δx[2], [1,1])
TPS64{Descriptor(NV=2, MO=5)}:
 Coefficient                Order   Exponent
  3.0000000000000000e+00      0      0   0
```

# Examples: Monomial Index-by-Sparse Monomial
```julia-repl
julia> deriv(Δx[1] + 2*Δx[2] + 3*Δx[1]*Δx[2], [1=>1])
TPS64{Descriptor(NV=2, MO=5)}:
 Coefficient                Order   Exponent
  1.0000000000000000e+00      0      0   0
  3.0000000000000000e+00      1      0   1


julia> deriv(Δx[1] + 2*Δx[2] + 3*Δx[1]*Δx[2], [2=>1])
TPS64{Descriptor(NV=2, MO=5)}:
 Coefficient                Order   Exponent
  2.0000000000000000e+00      0      0   0
  3.0000000000000000e+00      1      1   0
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
sets `t` to the result in-place.
"""
function getord!(t::TPS{T},    t1::TPS{T},    order::Integer) where {T}
  if T == Float64
    mad_tpsa_getord!(t1, t, UInt8(order))
  else
    mad_ctpsa_getord!(t1, t, UInt8(order))
  end
  return t
end

"""
    getord(t1::TPS, order::Integer)

Extracts one homogenous polynomial from `t1` of the given order.
"""
getord(t1::TPS, order::Integer) = (t = zero(t1); getord!(t, t1, order); return t)


# --- cutord ---
"""
    cutord!(t::TPS{T}, t1::TPS{T}, order::Integer) where {T} -> t

Cuts out the monomials in `t1` at the given order and above. Or, if `order` 
is negative, will cut monomials with orders at and below `abs(order)`. `t` 
is set to the result. See the documentation for `cutord` for examples.
"""
function cutord!(t::TPS{T},    t1::TPS{T},    order::Integer) where {T}
  if T == Float64
    mad_tpsa_cutord!(t1, t, Cint(order))
  else
    mad_ctpsa_cutord!(t1, t, Cint(order))
  end
  return t
end


"""
    cutord(t1::TPS, order::Integer)

Cuts out the monomials in `t1` at the given order and above. Or, if `order` 
is negative, will cut monomials with orders at and below `abs(order)`.

# Examples
```julia-repl
julia> d = Descriptor(1, 10);

julia> Δx = first(@vars(d));

julia> cutord(sin(Δx), 5)
TPS64{Descriptor(NV=1, MO=10)}:
 Coefficient                Order   Exponent
  1.0000000000000000e+00      1      1
 -1.6666666666666666e-01      3      3


julia> cutord(sin(Δx), -5)
TPS64{Descriptor(NV=1, MO=10)}:
 Coefficient                Order   Exponent
 -1.9841269841269841e-04      7      7
  2.7557319223985893e-06      9      9
```
"""
cutord(t1::TPS, order::Integer) = (t = zero(t1); cutord!(t, t1, order); return t)

# --- clearord! ---
"""
    clearord!(t::TPS, order::Integer) -> t

Clears all monomial coefficients in `t` at order `order`.
"""
function clearord!(t::TPS{T}, ord::Integer) where {T} 
  if T == Float64 
    mad_tpsa_clrord!(t, UInt8(ord))
  else
    mad_ctpsa_clrord!(t, UInt8(ord))
  end
  return t
end

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
"""
    compose!(m, m2, m1) -> m

Composes the `TPS`s (or `TPS` maps) `m2 ∘ m1`, and sets `m` equal to the result. If 
the GTPSA has more than 1 variable or parameter, then `m1` must be an array with length 
equal to the total number of variables and parameters.

# Arguments
- `m::Union{AbstractArray{TPS{T,D}},TPS{T,D}}`    
- `m2::Union{AbstractArray{TPS{T,D2}},TPS{T,D2}}`
- `m1::Union{AbstractArray{TPS{T,D1}},TPS{T,D1}}`
"""
function compose!(
  m::Union{AbstractArray{TPS{T,D}},TPS{T,D}}, 
  m2::Union{AbstractArray{TPS{T,D2}},TPS{T,D2}},
  m1::Union{AbstractArray{TPS{T,D1}},TPS{T,D1}}
) where {T<:Union{Float64,ComplexF64},D,D1,D2}
  n = length(m)
  n2 = length(m2)
  n1 = length(m1)

  # Checks:
  n == n2 || error("Incorrect output length, received length $n but need $n2")
  numnn(first(m2)) == n1 || error("Not enough input arguments")
  !(m === m1) || error("Cannot compose!(m, m2, m1) with m === m1")
  if T == Float64
    mad_tpsa_compose!(Cint(-n), m2, Cint(n1), m1, m)
  else
    mad_ctpsa_compose!(Cint(-n), m2, Cint(n1), m1, m)
  end
  return m
end

"""
    compose(m2, m1)

Composes the `TPS` (map) functions `m2 ∘ m1`

# Arguments
- `m2::Union{AbstractArray{TPS{T2,D}},TPS{T2,D}}`
- `m1::Union{AbstractArray{TPS{T1,D}},TPS{T1,D}}`

`Compose` can also be called by "calling" `m2` with `m1`, e.g. `m2(m1)`, or using the 
composition operator `m2 ∘ m1`.
"""
function compose(m2::Union{AbstractArray{TPS{T2,D}},TPS{T2,D}}, m1::Union{AbstractArray{TPS{T1,D}},TPS{T1,D}}) where {T2,T1,D}
  T = promote_type(eltype(m1), eltype(m2))
  T == eltype(m1) ? m1prom = m1 : m1prom = T.(m1)
  T == eltype(m2) ? m2prom = m2 : m2prom = T.(m2)
  m = zero(m2prom)
  compose!(m, m2prom, m1prom)
  return m
end

∘(m2::Union{AbstractArray{TPS{T2,D}},TPS}, m1::Union{AbstractArray{TPS{T1,D}},TPS}) where {T2,T1,D} = compose(m2, m1)

(f::TPS)(g::AbstractArray{TPS{T,D}}) where {T,D} = compose(f, g)
(f::TPS)(g::TPS) = compose(f, g)
(f::TPS)(g::TPS, gs::TPS...) = compose(f, vcat(g, collect(gs)))

(f::AbstractArray{TPS{TF,DF}})(g::AbstractArray{TPS{TG,DG}}) where {TF,DF,TG,DG} = compose(f, g)
(f::AbstractArray{TPS{TF,DF}})(g::TPS) where {TF,DF}= compose(f, g)
(f::AbstractArray{TPS{TF,DF}})(g::TPS, gs::TPS...) where {TF,DF}= compose(f, vcat(g, collect(gs)))


# --- translate ---
"""
    translate!(m, m1, x) -> m

Sets `m` to `m1` with its expansion point translated by `x`.

# Arguments
- `m::Union{AbstractArray{TPS{T,D}},TPS{T,D}}`    -- Output `TPS` or `TPS` map containing `m` translated by `x`
- `m1::Union{AbstractArray{TPS{T,D1}},TPS{T,D1}}` -- `TPS` or `TPS` map to translate by `x`
- `x::Union{Ref{T},AbstractArray{T}}`             -- Amount to translate the expansion point of `m1` by each variable. If the GTPSA has only 1 variable, then this may be a `Ref`.
"""
function translate!(
  m::Union{AbstractArray{TPS{T,D}},TPS{T,D}}, 
  m1::Union{AbstractArray{TPS{T,D1}},TPS{T,D1}}, 
  x::Union{Ref{T},AbstractArray{T}}
) where {T<:Union{Float64,ComplexF64},D,D1}
  n = length(m)
  n1 = length(m1)
  nx = length(x)

  # Checks:
  n == n1 || error("Incorrect output length, received length $n but need $n1")
  numnn(first(m1)) == nx || error("Not enough input arguments")
  !(m === m1) || error("Cannot translate!(m, m1, x) with m === m1")
  if T == Float64
    mad_tpsa_translate!(Cint(n1), m1, Cint(nx), x, m)
  else
    mad_ctpsa_translate!(Cint(n1), m1, Cint(nx), x, m)
  end
  return m
end

"""
    translate(m1, x) where {T1,D}

Translates the expansion point of `m1` by `x`.

# Arguments
- `m1::Union{AbstractArray{TPS{T1,D}},TPS}`  -- `TPS` or `TPS` map to translate
- `x::Union{Number,AbstractArray{<:Number}}` -- Amount(s) to translate the expansion point of each variable
"""
function translate(m1::Union{AbstractArray{TPS{T1,D}},TPS}, x::Union{Number,AbstractArray{<:Number}}) where {T1,D}
  T = promote_type(eltype(m1), eltype(x))
  T == eltype(m1) ? m1prom = m1 : m1prom = T.(m1)
  T == eltype(x) ? xprom = x : xprom = (GTPSA.numtype(T)).(x)
  if xprom isa Number
    xprom = Ref{GTPSA.numtype(T)}(xprom)
  end
  m = zero(m1prom)
  translate!(m, m1prom, xprom)
  return m
end


# --- inv! ---

"""
    inv!(m, m1)

Inverts the `TPS` map `m1` and sets `m` equal to the result. The scalar part of 
`m1` is ignored, so the user is responsible for ensuring the coordinate system 
is correct by either translating `m` or translating the coordinate system.

# Arguments
- `m::Union{AbstractArray{TPS{T,D}},TPS{T,D}}`
- `m1::Union{AbstractArray{TPS{T,D1}},TPS{T,D1}}`
"""
function inv!(m::Union{AbstractArray{TPS{T,D}},TPS{T,D}}, m1::Union{AbstractArray{TPS{T,D1}},TPS{T,D1}}) where {T,D,D1}
  !(m === m1) || error("Aliasing `m === m1` not allowed for `inv!`")
  n = length(m)
  n1 = length(m1)
  nn = numnn(first(m1))
  nv = numvars(first(m1))
  n1 == nn || error("Length of `m1` must equal the number of variables + parameters in the GTPSA to invert")
  n  >= nv || error("Length of result `m` must be at least equal to the number of variables in the GTPSA to invert")
  
  if T == Float64
    mad_tpsa_minv!(Cint(nn), m1, Cint(nv), m)
  else
    mad_ctpsa_minv!(Cint(nn), m1, Cint(nv), m)
  end

  return m
end

"""
    inv(m1::AbstractArray{TPS{T,D}}) where {T,D}

Returns the inverse of the `TPS` map `m1`. The scalar part of `m1` is ignored, 
so the user is responsible for ensuring the coordinate system is correct by 
either translating `m` or translating the coordinate system.

`inv` is not defined for a single `TPS` because it will clash with the 
regular `Number` function `inv`, which should calculate `1/x` instead.
Use `inv!` to invert a single `TPS` without allocating an array, or call 
`first(inv([f]))`.
"""
function inv(m1::AbstractArray{TPS{T,D}}) where {T,D}
  m = zero(m1)
  inv!(m, m1)
  return m
end


