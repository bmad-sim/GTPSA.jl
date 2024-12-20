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
mad_eval!(na, ma::AbstractArray{<:RealTPS},    nb, tb::AbstractArray{Float64},    tc::AbstractArray{Float64})    = mad_tpsa_eval!(Cint(na), ma, Cint(nb), tb, tc)
mad_eval!(na, ma::AbstractArray{<:ComplexTPS}, nb, tb::AbstractArray{ComplexF64}, tc::AbstractArray{ComplexF64}) = mad_ctpsa_eval!(Cint(na), ma, Cint(nb), tb, tc)

# Note that the cconvert here only will  work on TPS, not TempTPS
# tc here will be a Ref
mad_eval!(na, ma::TPS64,        nb, tb::AbstractArray{Float64},    tc::Ref{Float64})    = GC.@preserve ma @ccall MAD_TPSA.mad_tpsa_eval(Cint(na)::Cint, Ref(pointer_from_objref(ma))::Ptr{Cvoid}, Cint(nb)::Cint, tb::Ptr{Float64}, tc::Ptr{Float64})::Cvoid
mad_eval!(na, ma::ComplexTPS64, nb, tb::AbstractArray{ComplexF64}, tc::Ref{ComplexF64}) = GC.@preserve ma @ccall MAD_TPSA.mad_ctpsa_eval(Cint(na)::Cint, Ref(pointer_from_objref(ma))::Ptr{Cvoid}, Cint(nb)::Cint, tb::Ptr{Float64}, tc::Ptr{ComplexF64})::Cvoid

"""
    evaluate!(y::Union{AbstractVector{T},Ref{T}}, F::Union{AbstractVector{
      TPS{T}},TPS{T}}, x::AbstractVector{<:Number}) where {T} -> y

Evaluates the scalar or vector function `F` at the point `x`, and fills `y` with the result. 
"""
evaluate!

"""
    evaluate(F::Union{AbstractVector{TPS{T}},TPS{T}}, x::AbstractVector{T}) where {T}

Evaluates the scalar or vector function `F` at the point `x`, and returns the result.
"""
evaluate

# Vector function ---
function evaluate!(y::AbstractVector{T}, F::AbstractVector{TPS{T}}, x::AbstractVector{T}) where {T}
  Base.require_one_based_indexing(y, F, x)
  require_mutable(y, x)
  length(x) == numnn(first(F)) || error("Not enough input arguments")
  length(y) == length(F) || error("Not enough output arguments")
  # If the TPS vector function is immutable, we can just evaluate each (mutable) TPS element
  if ismutable(F)
    mad_eval!(length(F), F, length(x), x, y)
  else
    for i=1:length(y)

    end
  end
  return y
end

function evaluate(F::AbstractVector{TPS{T}}, x::AbstractVector{<:Number}) where {T}
  y = similar(x, T, length(F)); 
  # Mutable input x is necessary for the C library
  x1 = (eltype(x) == T && ismutable(x)) ? x : LinearAlgebra.copymutable_oftype(x, T)
  evaluate!(y, F, x1); 
  return y
end

# Scalar function ---
function evaluate!(y::Ref{T}, f::TPS{T}, x::Vector{<:Number}) where {T}
  length(x) == numnn(f) || error("Not enough input arguments")
  mad_eval!(1, f, length(x), convert(Vector{T}, x), y)
  return y
end

evaluate(f::TPS{T}, x::Vector{<:Number}) where {T} = (y = Ref{T}(); evaluate!(y, f, x); return y[])



# --- F . grad ---
"""
    fgrad!(g::T, F::AbstractVector{<:T}, h::T) where {T<:Union{TPS64,ComplexTPS64}} -> g

Calculates `F⋅∇h` and sets `g` equal to the result.
"""
function fgrad!(g::T, F::AbstractVector{<:T}, h::T) where {T<:Union{RealTPS, ComplexTPS}} 
  Base.require_one_based_indexing(F)
  nv = numvars(h)
  @assert length(F) == nv "Incorrect length of F; received $(length(F)), should be $nv"
  @assert !(g === h) "Aliasing g === h not allowed for fgrad!"
  if numtype(T) != Float64
      GTPSA.mad_ctpsa_fgrad!(Cint(length(F)), F, h, g)
  else
      GTPSA.mad_tpsa_fgrad!(Cint(length(F)), F, h, g)
  end
  return g
end
  
"""
    fgrad(F::AbstractVector{<:T}, h::T) where {T<:Union{TPS64,ComplexTPS64}}

Calculates `F⋅∇h`.
"""
function fgrad(F::AbstractVector{<:T}, h::T) where {T<:Union{RealTPS, ComplexTPS}} 
  g = zero(h)
  fgrad!(g, F, h)
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
mad_compose!(na, ma::T, nb, mb::AbstractArray{TPS64}, mc::T) where {T<:Union{AbstractArray{TPS64},TPS64}} = mad_tpsa_compose!(Cint(na), ma, Cint(nb), mb, mc)
mad_compose!(na, ma::T, nb, mb::AbstractArray{ComplexTPS64}, mc::T) where {T<:Union{AbstractArray{ComplexTPS64},ComplexTPS64}} = mad_ctpsa_compose!(Cint(na), ma, Cint(nb), mb, mc)

function compose!(m::T, m2::T, m1::AbstractVector{U}) where {U<:Union{TPS64,ComplexTPS64}, T<:Union{AbstractVector{U},U}}
  Base.require_one_based_indexing(m, m2, m1)
  n = length(m)
  n2 = length(m2)
  n1 = length(m1)

  # Checks:
  n == n2 || error("Incorrect output length, received length $(length(m)) but need $(length(m2))")
  numnn(first(m2)) == n1 || error("Not enough input arguments")
  !(m === m1) || error("Cannot compose!(m, m2, m1) with m === m1")

  mad_compose!(-n, m2, n1, m1, m)
  return m
end

function compose(m2::AbstractArray{<:TPS}, m1::AbstractArray{<:TPS})
  (m2prom, m1prom) = promote_to_mutable_arrays(m2, m1)
  m = similar(m2prom)
  for i in eachindex(m)
    m[i] = eltype(m2prom)(use=first(m2prom))
  end
  compose!(m, m2, m1)
  return m
end

function compose(f::TPS, m1::AbstractArray{<:TPS})
  T = promote_type(typeof(f), eltype(m1))
  g = T(use=f)
  compose!(g, f, m1)
end

∘(m2::Union{TPS,AbstractVector{<:TPS}}, m1::AbstractVector{<:TPS}) = compose(m2, m1)

# --- translate ---
mad_translate!(na, ma::Vector{TPS{Float64}},    nb, tb, mc::Vector{TPS{Float64}})    = mad_tpsa_translate!(Cint(na), ma, Cint(nb), convert(Vector{Float64}, tb), mc)
mad_translate!(na, ma::Vector{TPS{ComplexF64}}, nb, tb, mc::Vector{TPS{ComplexF64}}) = mad_ctpsa_translate!(Cint(na), ma, Cint(nb), convert(Vector{ComplexF64}, tb), mc)

# This is for translating single TPSs with ZERO allocations:
#mad_compose!(na, ma::TPS64,        nb, tb::AbstractVector{TPS64},        mc::TPS64)        = GC.@preserve ma mc @ccall MAD_TPSA.mad_tpsa_compose(Cint(na)::Cint, Ref(pointer_from_objref(ma))::Ptr{Cvoid}, Cint(nb)::Cint, mb::Ptr{TPS64}, Ref(pointer_from_objref(mc))::Ptr{Cvoid})::Cvoid
#mad_compose!(na, ma::ComplexTPS64, nb, tb::AbstractVector{ComplexTPS64}, mc::ComplexTPS64) = GC.@preserve ma mc @ccall MAD_TPSA.mad_tpsa_compose(Cint(na)::Cint, Ref(pointer_from_objref(ma))::Ptr{Cvoid}, Cint(nb)::Cint, mb::Ptr{ComplexTPS64}, Ref(pointer_from_objref(mc))::Ptr{Cvoid})::Cvoid

"""
    translate!(m::Vector{<:TPS{T}}, m1::Vector{<:TPS{T}}, x::Vector{<:Number}) where {T} -> m

Fills `m` with the vector function equal to `m1` with its expansion point translated by `x`.
"""
function translate!(m::Vector{<:TPS{T}}, m1::Vector{<:TPS{T}}, x::Vector{<:Number}) where {T}
  numnn(first(m1)) == length(x) || error("Not enough input arguments!")
  length(m) == length(m1) || error("Incorrect output length (should be $(length(m1)), received $(length(m)))")
  mad_translate!(length(m1), m1, length(x), x, m)
  return m
end

"""
    translate(m1::Vector{<:TPS{T}}, x::Vector{<:Number}) where {T}

returns a vector function equal to `m1` with its expansion point translated by `x`.
"""
function translate(m1::Vector{<:TPS{T}}, x::Vector{<:Number}) where {T}
  n = length(m1)
  m = Vector{TPS{T}}(undef, n)
  desc = getdesc(first(m1))
  for i=1:n
    @inbounds m[i] = TPS{T}(use=desc)
  end
  translate!(m, m1, x)
  return m
end