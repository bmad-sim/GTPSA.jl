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
    integ(t1::Union{TPS, ComplexTPS}, var::Integer=1)
    ∫(t1::Union{TPS, ComplexTPS}, var::Integer=1)

Integrates `t1` wrt the variable `var`. Integration wrt 
parameters is not allowed, and integration wrt higher order 
monomials is not currently supported.
"""
function integ(t1::Union{TPS, ComplexTPS}, var::Integer=1)
  t = zero(t1)
  integ!(t1.tpsa, t.tpsa, Cint(var))
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
    deriv(t1::Union{TPS,ComplexTPS}, v::Union{Integer, Vector{<:Union{<:Pair{<:Integer,<:Integer},<:Integer}}, Nothing}=nothing; param::Union{<:Integer,Nothing}=nothing, params::Union{Vector{<:Pair{<:Integer,<:Integer}}, Nothing}=nothing)
    ∂(t1::Union{TPS,ComplexTPS}, v::Union{Integer, Vector{<:Union{<:Pair{<:Integer,<:Integer},<:Integer}}, Nothing}=nothing; param::Union{<:Integer,Nothing}=nothing, params::Union{Vector{<:Pair{<:Integer,<:Integer}}, Nothing}=nothing)

Differentiates `t1` wrt the variable/parameter specified by the variable/parameter index, or 
alternatively any monomial specified by indexing-by-order OR indexing-by-sparse monomial.

### Input
- `v`      -- An integer (for variable index), an array of orders for each variable (for indexing-by-order), or an array of pairs (sparse monomial)
- `param`  -- (Keyword argument, optional) An integer for the parameter index
- `params` -- (Keyword argument, optional) An array of pairs for sparse-monomial indexing

# Examples: Variable/Parameter Index:
```julia-repl
julia> d = Descriptor(1,5,1,5);

julia> x1 = vars(use=d)[1]; k1 = params(use=d)[1];

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
function deriv(t1::Union{TPS,ComplexTPS}, v::Union{Integer, Vector{<:Union{<:Pair{<:Integer,<:Integer},<:Integer}}, Nothing}=nothing; param::Union{<:Integer,Nothing}=nothing, params::Union{Vector{<:Pair{<:Integer,<:Integer}}, Nothing}=nothing)
  return low_deriv(t1, v, param, params)
end

# Variable/parameter:
function low_deriv(t1::Union{TPS,ComplexTPS}, v::Integer, param::Nothing, params::Nothing)
  t = zero(t1)
  deriv!(t1.tpsa, t.tpsa, convert(Cint, v))
  return t
end

function low_deriv(t1::Union{TPS,ComplexTPS}, v::Nothing, param::Integer, params::Nothing)
  t = zero(t1)
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t1.tpsa).d))
  nv = desc.nv # TOTAL NUMBER OF VARS!!!!
  deriv!(t1.tpsa, t.tpsa, Cint(param)+nv)
  return t
end

# Default to first variable if nothing passed:
function low_deriv(t1::Union{TPS,ComplexTPS}, v::Nothing, param::Nothing, params::Nothing)
  return low_deriv(t1, 1, nothing, nothing)
end

# Monomial by order:
function low_deriv(t1::Union{TPS,ComplexTPS}, v::Vector{<:Integer}, param::Nothing, params::Nothing)
  t = zero(t1)
  derivm!(t1.tpsa, t.tpsa, Cint(length(v)), convert(Vector{Cuchar}, v))
  return t
end

# Monomial by sparse monomial:
function low_deriv(t1::Union{TPS,ComplexTPS}, v::Vector{<:Pair{<:Integer,<:Integer}}, param::Nothing, params::Vector{<:Pair{<:Integer,<:Integer}})
  t = zero(t1)
  # Need to create array of orders with length nv + np
  ords, n = pairs_to_m(t1,v,params=params)
  derivm!(t1.tpsa, t.tpsa, n, ords)
  return t
end

function low_deriv(t1::Union{TPS,ComplexTPS}, v::Vector{<:Pair{<:Integer,<:Integer}}, param::Nothing, params::Nothing)
  t = zero(t1)
  # Need to create array of orders with length nv + np
  ords, n = pairs_to_m(t1,v)
  derivm!(t1.tpsa, t.tpsa, n, ords)
  return t
end

function low_deriv(t1::Union{TPS,ComplexTPS}, v::Nothing, param::Nothing, params::Vector{<:Pair{<:Integer,<:Integer}})
  t = zero(t1)
  # Need to create array of orders with length nv + np
  ords, n = pairs_to_m(t1,Pair{Int,Int}[],params=params)
  derivm!(t1.tpsa, t.tpsa, n, ords)
  return t
end

# Throw error if no above use cases satisfied:
function low_deriv(t1::Union{TPS,ComplexTPS}, v, param, params)
  error("Invalid monomial specified. Please use ONE of variable/parameter index, index by order, or index by sparse monomial.")
end

∂ = deriv

# --- getord and cutord ---
# Low-level equivalent calls for TPS and ComplexTPS:
getord!(tpsa1::Ptr{RTPSA}, tpsa::Ptr{RTPSA}, order::Cuchar) = (@inline;  mad_tpsa_getord!(tpsa1, tpsa, order))
getord!(ctpsa1::Ptr{CTPSA}, ctpsa::Ptr{CTPSA}, order::Cuchar) = (@inline;  mad_ctpsa_getord!(ctpsa1, ctpsa, order))
cutord!(tpsa1::Ptr{RTPSA}, tpsa::Ptr{RTPSA}, order::Cint) = (@inline;  mad_tpsa_cutord!(tpsa1, tpsa, order))
cutord!(ctpsa1::Ptr{CTPSA}, ctpsa::Ptr{CTPSA}, order::Cint) = (@inline;  mad_ctpsa_cutord!(ctpsa1, ctpsa, order))

"""
    getord(t1::Union{TPS, ComplexTPS}, order::Integer)

Extracts one homogenous polynomial from `t1` of the given order.
"""
function getord(t1::Union{TPS, ComplexTPS}, order::Integer)
  t = zero(t1)
  getord!(t1.tpsa, t.tpsa, convert(Cuchar, order))
  return t
end

"""
    cutord(t1::Union{TPS, ComplexTPS}, order::Integer)

Cuts out the monomials in `t1` at the given order and above. Or, if `order` 
is negative, will cut monomials with orders at and below `abs(order)`.

# Examples
```julia-repl
julia> d = Descriptor(1,10);

julia> x = vars(use=d);

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
  cutord!(t1.tpsa, t.tpsa, convert(Cint, order))
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

julia> x = vars(use=d);

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
  t = promote_type(typeof(f),typeof(g))(use=f)
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(f.tpsa).d))
  poisbra!(f.tpsa,g.tpsa,t.tpsa, desc.nv)
  return t
end

# --- Lie bracket ---
liebra!(na::Cint, m1::Vector{Ptr{RTPSA}}, m2::Vector{Ptr{RTPSA}}, m3::Vector{Ptr{RTPSA}}) = (@inline; mad_tpsa_liebra!(na, m1, m2, m3))
liebra!(na::Cint, m1::Vector{Ptr{CTPSA}}, m2::Vector{Ptr{CTPSA}}, m3::Vector{Ptr{CTPSA}}) = (@inline; mad_ctpsa_liebra!(na, m1, m2, m3))

"""
    lb(A::Vector{<:Union{TPS,ComplexTPS}}, F::Vector{<:Union{TPS,ComplexTPS}})

Computes the Lie bracket of the vector functions `A` and `F`, defined over N variables as 
`Σᵢᴺ Aᵢ (∂F/∂xᵢ) - Fᵢ (∂A/∂xᵢ)`

# Example
```julia-repl
julia> d = Descriptor(2,10); x = vars();

julia> A = [-x[2], x[1]]
2-element Vector{TPS}:
  Out  Coefficient                Order   Exponent
-------------------------------------------------
   1:  -1.0000000000000000e+00      1      0   1
-------------------------------------------------
   2:   1.0000000000000000e+00      1      1   0


julia> F = [-x[1]^2, 2*x[1]*x[2]]
2-element Vector{TPS}:
  Out  Coefficient                Order   Exponent
-------------------------------------------------
   1:  -1.0000000000000000e+00      2      2   0
-------------------------------------------------
   2:   2.0000000000000000e+00      2      1   1


julia> lb(A,F)
2-element Vector{TPS}:
  Out  Coefficient                Order   Exponent
-------------------------------------------------
   1:   4.0000000000000000e+00      2      1   1
-------------------------------------------------
   2:   3.0000000000000000e+00      2      2   0
   2:  -2.0000000000000000e+00      2      0   2
```
"""
function lb(A::Vector{<:Union{TPS,ComplexTPS}}, F::Vector{<:Union{TPS,ComplexTPS}})
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(A[1].tpsa).d))
  if length(A) != desc.nv || length(F) != desc.nv
    error("Vector length != number of variables in the GTPSA")
  end
  A1, F1 = promote(A, F)
  m1 = map(t->t.tpsa, A1)  
  m2 = map(t->t.tpsa, F1) 
  mc = zero.(A1)
  m3 = map(t->t.tpsa, mc)
  GC.@preserve A1 F1 liebra!(Cint(length(A)), m1, m2, m3)     
  return mc
end

# --- getvectorfield ---
vec2fld!(na::Cint, tpsa::Ptr{RTPSA}, m::Vector{Ptr{RTPSA}}) = (@inline; mad_tpsa_vec2fld!(na, tpsa, m))
vec2fld!(na::Cint, ctpsa::Ptr{CTPSA}, m::Vector{Ptr{CTPSA}}) = (@inline; mad_ctpsa_vec2fld!(na, ctpsa, m))

"""
    getvectorfield(h::Union{TPS,ComplexTPS})::Vector{<:typeof(h)}

Assuming the variables in the TPSA are canonically-conjugate, and ordered so that the canonically-
conjugate variables are consecutive (q₁, p₁, q₂, p₂, ...), calculates the vector field (Hamilton's 
equations) from the passed Hamiltonian, defined as `[∂h/∂p₁, -∂h/∂q₁, ...]`

# Example
```julia-repl
julia> d = Descriptor(2,10); x = vars();

julia> h = (x[1]^2 + x[2]^2)/2
TPS:
 Coefficient                Order   Exponent
  5.0000000000000000e-01      2      2   0
  5.0000000000000000e-01      2      0   2


julia> getvectorfield(h)
2-element Vector{TPS}:
  Out  Coefficient                Order   Exponent
-------------------------------------------------
   1:  -1.0000000000000000e+00      1      0   1
-------------------------------------------------
   2:   1.0000000000000000e+00      1      1   0
```
"""
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

# --- gethamiltonian ---
fld2vec!(na::Cint, ma::Vector{Ptr{RTPSA}}, tpsa::Ptr{RTPSA}) = (@inline; mad_tpsa_fld2vec!(na, ma, tpsa))
fld2vec!(na::Cint, ma::Vector{Ptr{CTPSA}},  ctpsa::Ptr{CTPSA}) = (@inline; mad_ctpsa_fld2vec!(na, ma, ctpsa))

"""
    gethamiltonian(F::Vector{<:Union{TPS,ComplexTPS}})

Assuming the variables in the TPSA are canonically-conjugate, and ordered so that the canonically-
conjugate variables are consecutive (q₁, p₁, q₂, p₂, ...), this function calculates the Hamiltonian 
from a vector field `F` that can be obtained from a Hamiltonian (e.g. by `getvectorfield`). Explicitly, 
`∫ F₁ dp₁ - ∫ F₂ dq₁ + ... + ∫ F₂ₙ₋₁ dpₙ - ∫ F₂ₙ dqₙ `

# Example
```julia-repl
julia> d = Descriptor(2,10); x = vars();

julia> h = (x[1]^2 + x[2]^2)/2
TPS:
 Coefficient                Order   Exponent
  5.0000000000000000e-01      2      2   0
  5.0000000000000000e-01      2      0   2


julia> F = getvectorfield(h)
2-element Vector{TPS}:
  Out  Coefficient                Order   Exponent
-------------------------------------------------
   1:  -1.0000000000000000e+00      1      0   1
-------------------------------------------------
   2:   1.0000000000000000e+00      1      1   0


julia> gethamiltonian(F)
TPS:
 Coefficient                Order   Exponent
  5.0000000000000000e-01      2      2   0
  5.0000000000000000e-01      2      0   2
```
"""
function gethamiltonian(F::Vector{<:Union{TPS,ComplexTPS}})
  descF = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(F[1].tpsa).d))
  if length(F) != descF.nv
    error("Vector length != number of variables in the GTPSA")
  end
  h = zero(F[1])
  m1 = map(t->t.tpsa, F)
  fld2vec!(Cint(length(m)), m1, h.tpsa)
  return h
end


# --- exp(F . grad) m ---
exppb!(na::Cint, ma::Vector{Ptr{RTPSA}}, mb::Vector{Ptr{RTPSA}}, mc::Vector{Ptr{RTPSA}}) = (@inline; mad_tpsa_exppb!(na, ma, mb, mc))
exppb!(na::Cint, ma::Vector{Ptr{CTPSA}}, mb::Vector{Ptr{CTPSA}}, mc::Vector{Ptr{CTPSA}}) = (@inline; mad_ctpsa_exppb!(na, ma, mb, mc))

"""
    exppb(F::Vector{<:Union{TPS,ComplexTPS}}, m::Vector{<:Union{TPS,ComplexTPS}}=vars(use=first(F)))

Calculates `exp(F⋅∇)m = m + F⋅∇m + (F⋅∇)²m/2! + ...`. If `m` is not provided, it is assumed 
to be the identity. 

# Example

```julia-repl
julia> d = Descriptor(2,10); x = vars()[1]; p = vars()[2];

julia> time = 0.01; k = 2; m = 0.01;

julia> h = p^2/(2m) + 1/2*k*x^2;

julia> hf = getvectorfield(h);

julia> map = exppb(-time*hf, [x, p])
2-element Vector{TPS}:
  Out  Coefficient                Order   Exponent
-------------------------------------------------
   1:   9.9001665555952290e-01      1      1   0
   1:   9.9666999841313930e-01      1      0   1
-------------------------------------------------
   2:  -1.9933399968262787e-02      1      1   0
   2:   9.9001665555952378e-01      1      0   1
```
"""
function exppb(F::Vector{<:Union{TPS,ComplexTPS}}, m::Vector{<:Union{TPS,ComplexTPS}}=vars(use=first(F)))
  descF = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(F[1].tpsa).d))
  if length(F) != descF.nv
    error("Vector length != number of variables in the GTPSA")
  end
  ma1, mb1 = promote(F, m)
  m1 = map(t->t.tpsa, ma1) 
  m2 = map(t->t.tpsa, mb1) 
  mc = zero.(ma1)
  m3 = map(t->t.tpsa, mc)
  GC.@preserve ma1 mb1 exppb!(Cint(length(F)), m1, m2, m3)  
  return mc
end

# --- logpb ---
logpb!(na::Cint, ma::Vector{Ptr{RTPSA}}, mb::Vector{Ptr{RTPSA}}, mc::Vector{Ptr{RTPSA}}) = (@inline; mad_tpsa_logpb!(na, ma, mb, mc))
logpb!(na::Cint, ma::Vector{Ptr{CTPSA}}, mb::Vector{Ptr{CTPSA}}, mc::Vector{Ptr{CTPSA}}) = (@inline; mad_ctpsa_logpb!(na, ma, mb, mc))

"""
    logpb(mf::Vector{<:Union{TPS,ComplexTPS}}, mi::Vector{<:Union{TPS,ComplexTPS}}=vars(use=first(F)))

Given a final map `mf` and initial map `mi`, this function calculates the vector field `F`
such that `mf=exp(F⋅∇)mi`. If `mi` is not provided, it is assumed to be the identity.

```julia-repl
julia> d = Descriptor(2,10); x = vars()[1]; p = vars()[2];

julia> time = 0.01; k = 2; m = 0.01;

julia> h = p^2/(2m) + 1/2*k*x^2;

julia> hf = getvectorfield(h);

julia> map = exppb(-time*hf);

julia> logpb(map) == -time*hf
true
```
"""
function logpb(mf::Vector{<:Union{TPS,ComplexTPS}}, mi::Vector{<:Union{TPS,ComplexTPS}}=vars(use=first(mf)))
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(mf[1].tpsa).d))
  if length(mf) != desc.nv || length(mi) != desc.nv
    error("Vector length != number of variables in the GTPSA")
  end
  ma1, mb1 = promote(mf, mi)
  m1 = map(t->t.tpsa, ma1) 
  m2 = map(t->t.tpsa, mb1) 
  mc = zero.(ma1)
  m3 = map(t->t.tpsa, mc)
  GC.@preserve ma1 mb1 logpb!(Cint(length(mf)), m1, m2, m3)  
  return mc
end

# --- F . grad ---
fgrad!(na::Cint, ma::Vector{Ptr{RTPSA}}, b::Ptr{RTPSA}, c::Ptr{RTPSA}) = (@inline; mad_tpsa_fgrad!(na, ma, b, c))
fgrad!(na::Cint, ma::Vector{Ptr{CTPSA}}, b::Ptr{CTPSA}, c::Ptr{CTPSA}) = (@inline; mad_ctpsa_fgrad!(na, ma, b, c))

"""
    fgrad(F::Vector{<:Union{TPS,ComplexTPS}}, g::Union{TPS,ComplexTPS})

Calculates `F⋅∇g`.
"""
function fgrad(F::Vector{<:Union{TPS,ComplexTPS}}, g::Union{TPS,ComplexTPS})
  descF = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(F[1].tpsa).d))
  if length(F) != descF.nv
    error("Vector length != number of variables in the GTPSA")
  end
  type = promote_type(typeof(F[1]), typeof(g))
  ma1 = convert(Vector{type},  F)
  b1 = convert(type, g)
  m1 = map(t->t.tpsa, ma1) 
  c = zero(b1)
  GC.@preserve ma1 fgrad!(Cint(length(ma)), m1, b1.tpsa, c.tpsa)  
  return c
end

# --- mnrm ---
mnrm(na::Cint, ma::Vector{Ptr{RTPSA}})::Float64 = mad_tpsa_mnrm(na, ma)
mnrm(na::Cint, ma::Vector{Ptr{CTPSA}})::ComplexF64 = mad_ctpsa_mnrm(na, ma)

"""
    norm(ma::Vector{<:Union{TPS,ComplexTPS}})

Calculates the norm of the map `ma`, defined as `sum(norm.(ma))` or the 
sum of the absolute value of all coefficients in each TPS.
"""
function norm(ma::Vector{<:Union{TPS,ComplexTPS}})
  return mnrm(Cint(length(ma)), map(x->x.tpsa, ma))
end

# --- map inversion ---
minv!(na::Cint, ma::Vector{Ptr{RTPSA}}, mc::Vector{Ptr{RTPSA}}) = (@inline; mad_tpsa_minv!(na, ma, mc))
minv!(na::Cint, ma::Vector{Ptr{CTPSA}}, mc::Vector{Ptr{CTPSA}}) = (@inline; mad_ctpsa_minv!(na, ma, mc))

"""
    inv(ma::Vector{<:Union{TPS,ComplexTPS}})

Inverts the map `ma` such that `ma ∘ inv(ma) = 1` in the variables.

# Example

```julia-repl

julia> d = Descriptor(2,10); x = vars()[1]; p = vars()[2];

julia> time = 0.01; k = 2; m = 0.01;

julia> h = p^2/(2m) + 1/2*k*x^2;

julia> hf = getvectorfield(h);

julia> map = exppb(-time*hf, [x, p]);

julia> map ∘ inv(map)
2-element Vector{TPS}:
  Out  Coefficient                Order   Exponent
-------------------------------------------------
   1:   1.0000000000000000e+00      1      1   0
-------------------------------------------------
   2:   1.0000000000000002e+00      1      0   1
```
"""
function inv(ma::Vector{<:Union{TPS,ComplexTPS}})
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(ma[1].tpsa).d))
  if length(ma) != desc.nv
    error("Map length != number of variables in the GTPSA")
  end
  mc = zero.(ma)
  ma1 = map(x->x.tpsa, ma)
  mc1 = map(x->x.tpsa, mc)
  minv!(Cint(length(ma)), ma1, mc1)
  return mc
end

# --- partial inversion ---
pminv!(na::Cint, ma::Vector{Ptr{RTPSA}}, mc::Vector{Ptr{RTPSA}}, select::Vector{Cint}) = (@inline; mad_tpsa_pminv!(na, ma, mc, select))
pminv!(na::Cint, ma::Vector{Ptr{CTPSA}}, mc::Vector{Ptr{CTPSA}}, select::Vector{Cint}) = (@inline; mad_ctpsa_pminv!(na, ma, mc, select))

"""
    ptinv(ma::Vector{<:Union{TPS,ComplexTPS}}, vars::Vector{<:Integer})

Partially-inverts the map `ma`, inverting only the variables specified by index
in `vars`.
"""
function ptinv(ma::Vector{<:Union{TPS,ComplexTPS}}, vars::Vector{<:Integer})
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(ma[1].tpsa).d))
  if length(ma) != desc.nv
    error("Map length != number of variables in the GTPSA")
  end
  mc = zero.(ma)
  ma1 = map(x->x.tpsa, ma)
  mc1 = map(x->x.tpsa, mc)
  na = Cint(length(ma))
  select = zeros(Cint, na)
  select[vars] .= Cint(1)
  pminv!(na, ma1, mc1, select)
  return mc
end

# --- composition ---
compose!(na::Cint, ma::Vector{Ptr{RTPSA}}, nb::Cint, mb::Vector{Ptr{RTPSA}}, mc::Vector{Ptr{RTPSA}}) = (@inline; mad_tpsa_compose!(na, ma, nb, mb, mc))
compose!(na::Cint, ma::Vector{Ptr{CTPSA}}, nb::Cint, mb::Vector{Ptr{CTPSA}}, mc::Vector{Ptr{CTPSA}}) = (@inline; mad_ctpsa_compose!(na, ma, nb, mb, mc))

function ∘(ma::Vector{<:Union{TPS,ComplexTPS}}, mb::Vector{<:Union{TPS,ComplexTPS}})
  na = Cint(length(ma))
  nb = Cint(length(mb))
  # Ensure mb is length = input
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(ma[1].tpsa).d))
  if desc.nv + desc.np != nb
    error("Not enough input arguments")
  end
  ma1, mb1 = promote(ma, mb)
  m1 = map(t->t.tpsa, ma1) 
  m2 = map(t->t.tpsa, mb1) 
  mc = Vector{typeof(ma1[1])}(undef, na)
  for i in eachindex(mc)
    mc[i] = zero(ma1[1])
  end
  m3 = map(t->t.tpsa, mc)
  GC.@preserve ma1 mb1 compose!(na, m1, nb, m2, m3)
  return mc
end

compose = ∘

# --- translate ---
"""
    translate(m::Vector{TPS}, x::Vector{<:Real})::Vector{TPS}

Translates the expansion point of the Vector of TPSs `m` by `x`.
"""
function translate(m::Vector{TPS}, x::Vector{<:Real})::Vector{TPS}
  na = Cint(length(m))
  nb = Cint(length(x))
  tb = convert(Vector{Float64}, x)
  ma1 = map(x->x.tpsa, m)
  mc = zero.(m)
  mc1 = map(x->x.tpsa, mc)
  mad_tpsa_translate!(na, ma1, nb, tb, mc1)
  return mc
end

"""
    translate(m::Vector{ComplexTPS}, x::Vector{<:Number})::Vector{ComplexTPS}

Translates the expansion point of the Vector of TPSs `m` by `x`.
"""
function translate(m::Vector{ComplexTPS}, x::Vector{<:Number})::Vector{ComplexTPS}
  na = Cint(length(m))
  nb = Cint(length(x))
  tb = convert(Vector{ComplexF64}, x)
  ma1 = map(x->x.tpsa, m)
  mc = zero.(m)
  mc1 = map(x->x.tpsa, mc)
  mad_ctpsa_translate!(na, ma1, nb, tb, mc1)
  return mc
end