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

### Input
- `v`      -- An integer (for variable index), an array of orders for each variable (for indexing-by-order), or an array of pairs (sparse monomial)
- `param`  -- (Keyword argument, optional) An integer for the parameter index
- `params` -- (Keyword argument, optional) An array of pairs for sparse-monomial indexing

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
  A1, F1 = promote(A, F)
  m1 = map(t->t.tpsa, A1)  
  m2 = map(t->t.tpsa, F1) 
  mc = zero.(A1)
  m3 = map(t->t.tpsa, mc)
  GC.@preserve A1 F1 liebra!(Cint(length(A)), m2, m1, m3)      # SIGN DIFFERENCE WITH ETIENNE'S BOOK!!!!
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

# --- gethamiltonian ---
fld2vec!(na::Cint, m::Vector{Ptr{RTPSA}}, tpsa::Ptr{RTPSA}) = (@inline; mad_tpsa_fld2vec!(na, ma, tpsa))
fld2vec!(na::Cint, m::Vector{Ptr{CTPSA}},  ctpsa::Ptr{CTPSA}) = (@inline; mad_ctpsa_fld2vec!(na, ma, ctpsa))

function gethamiltonian(m::Vector{<:Union{TPS,ComplexTPS}})
  h = zero(m[1])
  m1 = map(t->t.tpsa, m)
  fld2vec!(Cint(length(m)), m1, h)
  return h
end


# --- exp([ma, .]) mb ---
exppb!(na::Cint, ma::Vector{Ptr{RTPSA}}, mb::Vector{Ptr{RTPSA}}, mc::Vector{Ptr{RTPSA}}) = (@inline; mad_tpsa_exppb!(na, ma, mb, mc))
exppb!(na::Cint, ma::Vector{Ptr{CTPSA}}, mb::Vector{Ptr{CTPSA}}, mc::Vector{Ptr{CTPSA}}) = (@inline; mad_ctpsa_exppb!(na, ma, mb, mc))

function exppb(ma::Vector{<:Union{TPS,ComplexTPS}}, mb::Vector{<:Union{TPS,ComplexTPS}})
  ma1, mb1 = promote(ma, mb)
  m1 = map(t->t.tpsa, ma1) 
  m2 = map(t->t.tpsa, mb1) 
  mc = zero.(ma1)
  m3 = map(t->t.tpsa, mc)
  GC.@preserve ma1 mb1 exppb!(Cint(length(na)), m1, m2, m3)  
  return mc
end

# --- logpb ---
logpb!(na::Cint, ma::Vector{Ptr{RTPSA}}, mb::Vector{Ptr{RTPSA}}, mc::Vector{Ptr{RTPSA}}) = (@inline; mad_tpsa_logpb!(na, ma, mb, mc))
logpb!(na::Cint, ma::Vector{Ptr{CTPSA}}, mb::Vector{Ptr{CTPSA}}, mc::Vector{Ptr{CTPSA}}) = (@inline; mad_ctpsa_logpb!(na, ma, mb, mc))

function logpb(ma::Vector{<:Union{TPS,ComplexTPS}}, mb::Vector{<:Union{TPS,ComplexTPS}})
  ma1, mb1 = promote(ma, mb)
  m1 = map(t->t.tpsa, ma1) 
  m2 = map(t->t.tpsa, mb1) 
  mc = zero.(ma1)
  m3 = map(t->t.tpsa, mc)
  GC.@preserve ma1 mb1 logpb!(Cint(length(na)), m1, m2, m3)  
  return mc
end

# --- F . grad ---
fgrad!(na::Cint, ma::Vector{Ptr{RTPSA}}, b::Ptr{RTPSA}, c::Ptr{RTPSA}) = (@inline; mad_tpsa_fgrad!(na, ma, b, c))
fgrad!(na::Cint, ma::Vector{Ptr{CTPSA}}, b::Ptr{CTPSA}, c::Ptr{CTPSA}) = (@inline; mad_ctpsa_fgrad!(na, ma, b, c))


function fgrad(ma::Vector{<:Union{TPS,ComplexTPS}}, b::Union{TPS,ComplexTPS})
  type = promote_type(typeof(ma[1]), typeof(b))
  ma1 = convert(Vector{type},  ma)
  b1 = convert(type, b)
  m1 = map(t->t.tpsa, ma1) 
  c = zero(b1)
  GC.@preserve ma1 logpb!(Cint(length(na)), m1, b1.tpsa, c)  
  return c
end

# --- mnrm ---
mnrm(na::Cint, ma::Vector{Ptr{RTPSA}})::Float64 = mad_tpsa_mrnm(na, ma)
mnrm(na::Cint, ma::Vector{Ptr{CTPSA}})::ComplexF64 = mad_ctpsa_mrnm(na, ma)

function norm(ma::Vector{<:Union{TPS,ComplexTPS}})
  return mrnm(Cint(length(ma)), map(x->x.tpsa, ma))
end

# --- map inversion ---
minv!(na::Cint, ma::Vector{Ptr{RTPSA}}, mc::Vector{Ptr{RTPSA}}) = (@inline; mad_tpsa_minv!(na, ma, mc))
minv!(na::Cint, ma::Vector{Ptr{CTPSA}}, mc::Vector{Ptr{CTPSA}}) = (@inline; mad_ctpsa_minv!(na, ma, mc))

function inv(ma::Vector{<:Union{TPS,ComplexTPS}})
  mc = zero.(ma)
  ma1 = map(x->x.tpsa, ma)
  mc1 = map(x->x.tpsa, mc)
  minv!(Cint(length(ma)), ma1, mc1)
  return mc
end

# --- partial inversion ---
pminv!(na::Cint, ma::Vector{Ptr{RTPSA}}, mc::Vector{Ptr{RTPSA}}, select::Vector{Cint}) = (@inline; mad_tpsa_pminv!(na, ma, mc, select))
pminv!(na::Cint, ma::Vector{Ptr{CTPSA}}, mc::Vector{Ptr{CTPSA}}, select::Vector{Cint}) = (@inline; mad_ctpsa_pminv!(na, ma, mc, select))

function pinv(ma::Vector{<:Union{TPS,ComplexTPS}}, vars::Vector{<:Integer})
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
  ma1, mb1 = promote(ma, mb)
  m1 = map(t->t.tpsa, ma1) 
  m2 = map(t->t.tpsa, mb1) 
  na = Cint(length(ma))
  nb = Cint(length(mb))
  mc = Vector{typeof(ma1[1])}(undef, na)
  for i in eachindex(mc)
    mc[i] = zero(ma1[1])
  end
  m3 = map(t->t.tpsa, mc)
  GC.@preserve ma1 mb1 compose!(na, m1, nb, m2, m3)
  return mc
end

# --- translate ---
function translate(m::Vector{TPS}, x::Vector{<:Real})::TPS
  na = Cint(length(m))
  nb = Cint(length(x))
  tb = convert(Vector{Float64}, x)
  ma1 = map(x->x.tpsa, m)
  mc = zero.(m)
  mc1 = map(x->x.tpsa, mc1)
  mad_tpsa_translate!(na, ma1, nb, tb, mc1)
  return mc
end

function translate(m::Vector{ComplexTPS}, x::Vector{<:Number})::TPS
  na = Cint(length(m))
  nb = Cint(length(x))
  tb = convert(Vector{ComplexF64}, x)
  ma1 = map(x->x.tpsa, m)
  mc = zero.(m)
  mc1 = map(x->x.tpsa, mc1)
  mad_ctpsa_translate!(na, ma1, nb, tb, mc1)
  return mc
end
