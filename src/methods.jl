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

# --- Integral ---
# Low-level equivalent calls for TPS and ComplexTPS:
integ!(tpsa1::Ptr{RTPSA},  tpsa::Ptr{RTPSA}, var::Cint) = (@inline; mad_tpsa_integ!(tpsa1, tpsa, var))
integ!(ctpsa1::Ptr{CTPSA}, ctpsa::Ptr{CTPSA}, var::Cint) = (@inline; mad_ctpsa_integ!(ctpsa1, ctpsa, var))

"""
    ∫(t1::Union{TPS, ComplexTPS}, var::Integer=1)::typeof(t1)

Integrates `t1` wrt the variable `var`. Integration wrt 
parameters is not allowed, and integration wrt higher order 
monomials is not currently supported.
"""
function ∫(t1::Union{TPS, ComplexTPS}, var::Integer=1)::typeof(t1)
  t = zero(t1)
  integ!(t1.tpsa, t.tpsa, Cint(var))
  return t
end


# --- Derivative ---
# Low-level equivalent calls for TPS and ComplexTPS:
deriv!(tpsa1::Ptr{RTPSA}, tpsa::Ptr{RTPSA}, var::Cint) = (@inline; mad_tpsa_deriv!(tpsa1, tpsa, var))
deriv!(ctpsa1::Ptr{CTPSA}, ctpsa::Ptr{CTPSA}, var::Cint) = (@inline; mad_ctpsa_deriv!(ctpsa1, ctpsa, var))
derivm!(tpsa1::Ptr{RTPSA}, tpsa::Ptr{RTPSA}, n::Cint, ords::Vector{Cuchar}) = (@inline; mad_tpsa_derivm!(tpsa1, tpsa, n, ords))
derivm!(ctpsa1::Ptr{CTPSA}, ctpsa::Ptr{CTPSA}, n::Cint, ords::Vector{Cuchar}) = (@inline; mad_ctpsa_derivm!(ctpsa1, ctpsa, n, ords))

"""
    ∂(t1::Union{TPS,ComplexTPS}, v::Union{Integer, Vector{<:Union{<:Pair{<:Integer,<:Integer},<:Integer}}, Nothing}=nothing; param::Union{<:Integer,Nothing}=nothing, params::Union{Vector{<:Pair{<:Integer,<:Integer}}, Nothing}=nothing)

Differentiates `t1` wrt the variable/parameter specified by the variable/parameter index, or 
alternatively any monomial specified by indexing-by-order OR indexing-by-sparse monomial.

# Examples: Variable/Parameter Index:
```julia-repl
julia> d = Descriptor(1,5,1,5);

julia> x1 = vars(d)[1]; k1 = params(d)[1];

julia> ∂(x1*k1, 1)
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        0    1


julia> ∂(x1*k1, param=1)
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        1    0
```

# Examples: Monomial Index-by-Order
```julia-repl
julia> ∂(x1*k1, [1,0])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        0    1


julia> ∂(x1*k1, [0,1])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        1    0


julia> ∂(x1*k1, [1,1])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    0        0    0
```

# Examples: Monomial Index-by-Sparse Monomial
```julia-repl
julia> ∂(x1*k1, [1=>1])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        0    1


julia> ∂(x1*k1, params=[1=>1])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        1    0


julia> ∂(x1*k1, [1=>1], params=[1=>1])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    0        0    0
```
"""
function ∂(t1::Union{TPS,ComplexTPS}, v::Union{Integer, Vector{<:Union{<:Pair{<:Integer,<:Integer},<:Integer}}, Nothing}=nothing; param::Union{<:Integer,Nothing}=nothing, params::Union{Vector{<:Pair{<:Integer,<:Integer}}, Nothing}=nothing)
  return deriv(t1, v, param, params)
end

# Variable/parameter:
function deriv(t1::Union{TPS,ComplexTPS}, v::Integer, param::Nothing, params::Nothing)::typeof(t1)
  t = zero(t1)
  deriv!(t1.tpsa, t.tpsa, convert(Cint, v))
  return t
end

function deriv(t1::Union{TPS,ComplexTPS}, v::Nothing, param::Integer, params::Nothing)::typeof(t1)
  t = zero(t1)
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t1.tpsa).d))
  nv = desc.nv # TOTAL NUMBER OF VARS!!!!
  deriv!(t1.tpsa, t.tpsa, Cint(param)+nv)
  return t
end

# Default to first variable if nothing passed:
function deriv(t1::Union{TPS,ComplexTPS}, v::Nothing, param::Nothing, params::Nothing)::typeof(t1)
  return deriv(t1, 1, nothing, nothing)
end

# Monomial by order:
function deriv(t1::Union{TPS,ComplexTPS}, v::Vector{<:Integer}, param::Nothing, params::Nothing)::typeof(t1)
  t = zero(t1)
  derivm!(t1.tpsa, t.tpsa, Cint(length(v)), convert(Vector{Cuchar}, v))
  return t
end

# Monomial by sparse monomial:
function deriv(t1::Union{TPS,ComplexTPS}, v::Vector{<:Pair{<:Integer,<:Integer}}, param::Nothing, params::Vector{<:Pair{<:Integer,<:Integer}})::typeof(t1)
  t = zero(t1)
  # Need to create array of orders with length nv + np
  ords, n = pairs_to_m(t1,v,params=params)
  derivm!(t1.tpsa, t.tpsa, n, ords)
  return t
end

function deriv(t1::Union{TPS,ComplexTPS}, v::Vector{<:Pair{<:Integer,<:Integer}}, param::Nothing, params::Nothing)::typeof(t1)
  t = zero(t1)
  # Need to create array of orders with length nv + np
  ords, n = pairs_to_m(t1,v)
  derivm!(t1.tpsa, t.tpsa, n, ords)
  return t
end

function deriv(t1::Union{TPS,ComplexTPS}, v::Nothing, param::Nothing, params::Vector{<:Pair{<:Integer,<:Integer}})::typeof(t1)
  t = zero(t1)
  # Need to create array of orders with length nv + np
  ords, n = pairs_to_m(t1,Pair{Int,Int}[],params=params)
  derivm!(t1.tpsa, t.tpsa, n, ords)
  return t
end

# Throw error if no above use cases satisfied:
function deriv(t1::Union{TPS,ComplexTPS}, v, param, params)
  error("Invalid monomial specified. Please use ONE of variable/parameter index, index by order, or index by sparse monomial.")
end

# --- getord and cutord ---
# Low-level equivalent calls for TPS and ComplexTPS:
getord!(tpsa1::Ptr{RTPSA}, tpsa::Ptr{RTPSA}, order::Cuchar) = (@inline;  mad_tpsa_getord!(tpsa1, tpsa, order))
getord!(ctpsa1::Ptr{CTPSA}, ctpsa::Ptr{CTPSA}, order::Cuchar) = (@inline;  mad_ctpsa_getord!(ctpsa1, ctpsa, order))
cutord!(tpsa1::Ptr{RTPSA}, tpsa::Ptr{RTPSA}, order::Cuchar) = (@inline;  mad_tpsa_cutord!(tpsa1, tpsa, order))
cutord!(ctpsa1::Ptr{CTPSA}, ctpsa::Ptr{CTPSA}, order::Cuchar) = (@inline;  mad_ctpsa_cutord!(ctpsa1, ctpsa, order))

"""
    getord(t1::Union{TPS, ComplexTPS}, order::Integer)::typeof(t1)

Extracts one homogenous polynomial from `t1` of the given order.
"""
function getord(t1::Union{TPS, ComplexTPS}, order::Integer)::typeof(t1)
  t = zero(t1)
  getord!(t1.tpsa, t.tpsa, convert(Cuchar, order))
  return t
end

"""
    cutord(t1::Union{TPS, ComplexTPS}, order::Integer)::typeof(t1)

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
function cutord(t1::Union{TPS, ComplexTPS}, order::Integer)::typeof(t1)
  t = zero(t1)
  cutord!(t1.tpsa, t.tpsa, convert(Cint, order))
  return t
end

# --- Poisson bracket ---
# Low-level calls
poisbra!(tpsa1::Ptr{RTPSA}, tpsa2::Ptr{RTPSA}, tpsa::Ptr{RTPSA}, nv::Cint) = (@inline; mad_tpsa_poisbra!(tpsa1, tpsa2, tpsa, nv))
poisbra!(tpsa1::Ptr{RTPSA}, ctpsa1::Ptr{CTPSA}, ctpsa::Ptr{CTPSA}, nv::Cint) = (@inline; mad_ctpsa_poisbrat!(ctpsa1,tpsa1,ctpsa, nv))
poisbra!(ctpsa1::Ptr{CTPSA}, tpsa1::Ptr{RTPSA}, ctpsa::Ptr{CTPSA}, nv::Cint) = (@inline; mad_ctpsa_tpoisbra!(tpsa1, ctpsa1, ctpsa, nv))
poisbra!(ctpsa1::Ptr{CTPSA}, ctpsa2::Ptr{CTPSA}, ctpsa::Ptr{CTPSA}, nv::Cint) = (@inline; mad_ctpsa_poisbra!(ctpsa1,ctpsa2, ctpsa, nv))

"""
    pb(f::Union{TPS, ComplexTPS}, g::Union{TPS, ComplexTPS})

Assuming the variables in the TPSA are canonically-conjugate, and ordered so that the canonically-
conjugate variables are consecutive (q₁, p₁, q₂, p₂, ...), computes the Poisson bracket 
of the scalar functions `f` and `g`. The Poisson bracket of two functions `{f, g}` is defined as 
`Σᵢ (∂f/∂qᵢ)(∂g/∂pᵢ) - (∂g/∂qᵢ)(∂f/∂pᵢ)`.

# Examples
```julia-repl
julia> d = Descriptor(4,10);

julia> x = vars(d);

julia> f = (x[1]^2 + x[2]^2)/2 + (x[3]^2 + x[4]^2)/2;

julia> pb(f,x[1])
TPS:
  Coefficient              Order     Exponent
  -1.0000000000000000e+00    1        0    1    0    0


julia> pb(f,x[2])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        1    0    0    0


julia> pb(f,x[3])
TPS:
  Coefficient              Order     Exponent
  -1.0000000000000000e+00    1        0    0    0    1


julia> pb(f,x[4])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        0    0    1    0
```
"""
function pb(f::Union{TPS, ComplexTPS}, g::Union{TPS, ComplexTPS})
  t = zero_promote(f,g)
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(f.tpsa).d))
  poisbra!(f.tpsa,g.tpsa,t.tpsa, desc.nv)
  return t
end

# --- Lie bracket ---
liebra!(na::Cint, m1::Vector{Ptr{RTPSA}}, m2::Vector{Ptr{RTPSA}}, m3::Vector{Ptr{RTPSA}}) = (@inline; mad_tpsa_liebra!(na, m1, m2, m3))
liebra!(na::Cint, m1::Vector{Ptr{CTPSA}}, m2::Vector{Ptr{CTPSA}}, m3::Vector{Ptr{CTPSA}}) = (@inline; mad_ctpsa_liebra!(na, m1, m2, m3))

"""
    lb(A::Vector{<:Union{TPS,ComplexTPS}}, F::Vector{<:Union{TPS,ComplexTPS}})

Computes the Lie bracket of the vector functions `A` and `F`
eq 3.42 3.43 in Etienne's book
"""
function lb(A::Vector{<:Union{TPS,ComplexTPS}}, F::Vector{<:Union{TPS,ComplexTPS}})
  mc, na = zero_promote(A, F)
  m1 = map(t->convert(typeof(mc[1]), t).tpsa, A)   # Promotes if necessary, else does nothing
  m2 = map(t->convert(typeof(mc[1]), t).tpsa, F)   # Promotes if necessary, else does nothing
  m3 = map(t->t.tpsa, mc)
  liebra!(na, m2, m1, m3)      # SIGN DIFFERENCE WITH ETIENNE'S BOOK!!!!
  return mc
end

# --- getvectorfield ---
vec2fld!(na::Cint, tpsa::Ptr{RTPSA}, m::Vector{Ptr{RTPSA}}) = (@inline; mad_tpsa_vec2fld!(na, tpsa, m))
vec2fld!(na::Cint, ctpsa::Ptr{CTPSA}, m::Vector{Ptr{CTPSA}}) = (@inline; mad_ctpsa_vec2fld!(na, ctpsa, m))

function getvectorfield(h::Union{TPS,ComplexTPS})::Vector{<:typeof(h)}
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(h.tpsa).d))
  na = desc.nv
  mc = Vector{typeof(h)}(undef, na) 
  for i in eachindex(mc)
    mc[i] = zero(h)
  end
  m = map(t->t.tpsa, mc)
  vec2fld!(na, h.tpsa, m)
  return mc
end

# --- exp([f, .]) m ---
exppb!(na::Cint, ma::Ptr{RTPSA}, mb::Ptr{RTPSA}, mc::Ptr{RTPSA}) = (@inline; mad_tpsa_exppb!(na, ma, mb, mc))
exppb!(na::Cint, ma::Ptr{CTPSA}, mb::Ptr{CTPSA}, mc::Ptr{CTPSA}) = (@inline; mad_ctpsa_exppb!(na, ma, mb, mc))

function exppb(ma::Vector{<:Union{TPS,ComplexTPS}}, mb::Vector{<:Union{TPS,ComplexTPS}})
  mc, na = zero_promote(ma, mb)
  m1 = map(t->convert(typeof(mc[1]), t).tpsa, ma)   # Promotes if necessary, else does nothing
  m2 = map(t->convert(typeof(mc[1]), t).tpsa, mb)   # Promotes if necessary, else does nothing
  m3 = map(t->t.tpsa, mc)
  exppb!(na, m1, m2, m3)  
  return mc
end

