# --- Setters ---
seti!(t::Ptr{RTPSA}, i::Cint, a::Cdouble, b::Cdouble) = mad_tpsa_seti!(t, i, a, b)
seti!(t::Ptr{CTPSA}, i::Cint, a::ComplexF64, b::ComplexF64) =  mad_ctpsa_seti!(t, i, a, b)
setm!(t::Ptr{RTPSA}, n::Cint, m::Vector{Cuchar}, a::Float64, b::Float64) = mad_tpsa_setm!(t, n, m, a, b)
setm!(t::Ptr{CTPSA}, n::Cint, m::Vector{Cuchar}, a::ComplexF64, b::ComplexF64) = mad_ctpsa_setm!(t, n, m, a, b)
setsm!(t::Ptr{RTPSA}, n::Cint, m::Vector{Cint}, a::Float64, b::Float64)= mad_tpsa_setsm!(t, n, m, a, b)
setsm!(t::Ptr{CTPSA}, n::Cint, m::Vector{Cint}, a::ComplexF64, b::ComplexF64)= mad_ctpsa_setsm!(t, n, m, a, b)

# All
function setindex!(t::Union{TPS,ComplexTPS}, v::Number, idx::Union{TPSIndexType,Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType,Nothing}=nothing)
  lowset!(t, v, idx, param, params)
end

# To override Base number.jl
#setindex!(t::Union{ComplexTPS, TPS}, v::Number, idx::Integer) = lowset!(t, v, tuple(idx), nothing, nothing)

# Flat index
function lowset!(t::Union{TPS,ComplexTPS}, v::Number, i::Union{Nothing,Integer}, param::Union{Nothing,Integer}, params::Nothing)
  if !xor(isnothing(i), isnothing(param))
    error("Invalid monomial index specified. Please use ONE of variable/parameter index, index by order, or index by sparse monomial.")
  end

  if isnothing(param)
    seti!(t.tpsa, Cint(i), (numtype(t))(0), (numtype(t))(v))
  else
    nv = numvars(t)
    seti!(t.tpsa, Cint(nv+param), (numtype(t))(0), (numtype(t))(v))
  end
end

# Monomial
function lowset!(t::Union{TPS,ComplexTPS}, v::Number, ords::MIndexType, param::Nothing, params::Nothing)
  setm!(t.tpsa, convert(Cint, length(ords)), collect(Cuchar, ords), (numtype(t))(0), (numtype(t))(v))
end

# By sparse monomial
function lowset!(t::Union{TPS,ComplexTPS}, v::Number, vars::SMIndexType, param::Nothing, params::Union{SMIndexType,Nothing})
  sm, n = pairs_to_sm(t, vars, params=params)
  setsm!(t.tpsa, n, sm, (numtype(t))(0), (numtype(t))(v))
end

# Sparse monomial with nothing for vars
function lowset!(t::Union{TPS,ComplexTPS}, v::Number, vars::Nothing, param::Nothing, params::SMIndexType)
  sm, n = pairs_to_sm(t, Pair{Int,Int}[], params=params)
  setsm!(t.tpsa, n, sm, (numtype(t))(0), (numtype(t))(v))
end

# In case someone forgets tuple:
lowset!(t, v, idx, param, params) = error("Invalid monomial index specified. Please use ONE of variable/parameter index, index by order, or index by sparse monomial.")


# --- Getters ---
geti(t::Ptr{RTPSA}, i::Cint) = mad_tpsa_geti(t, i)
geti(t::Ptr{CTPSA}, i::Cint) = mad_ctpsa_geti(t, i)
getm(t::Ptr{RTPSA}, n::Cint, m::Vector{Cuchar}) = mad_tpsa_getm(t, n, m)
getm(t::Ptr{CTPSA}, n::Cint, m::Vector{Cuchar}) = mad_ctpsa_getm(t, n, m)
getsm(t::Ptr{RTPSA}, n::Cint, m::Vector{Cint})= mad_tpsa_getsm(t, n, m)
getsm(t::Ptr{CTPSA}, n::Cint, m::Vector{Cint})= mad_ctpsa_getsm(t, n, m)

# All
function getindex(t::Union{TPS,ComplexTPS}, idx::Union{TPSIndexType,Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType,Nothing}=nothing)
  #println(typeof(t))
  #println(typeof(idx))
  #println(typeof(param))
  #println(typeof(params))
  return lowget(t, idx, param, params)
end

# To override Base number.jl
getindex(t::Union{ComplexTPS, TPS}, idx::Integer) = lowget(t, idx, nothing, nothing)

# Flat index
function lowget(t::Union{TPS,ComplexTPS}, i::Union{Nothing,Integer}, param::Union{Nothing,Integer}, params::Nothing)
  if !xor(isnothing(i), isnothing(param))
    error("Invalid monomial index specified. Please use ONE of variable/parameter index, index by order, or index by sparse monomial.")
  end

  if isnothing(param)
    return geti(t.tpsa, Cint(i))
  else
    nv = numvars(t)
    return geti(t.tpsa, Cint(nv+param))
  end
end

# Monomial
function lowget(t::Union{TPS,ComplexTPS}, ords::MIndexType, param::Nothing, params::Nothing)
  return getm(t.tpsa, convert(Cint, length(ords)), collect(Cuchar, ords))
end

# By sparse monomial
function lowget(t::Union{TPS,ComplexTPS}, vars::SMIndexType, param::Nothing, params::Union{SMIndexType,Nothing})
  sm, n = pairs_to_sm(t, vars, params=params)
  return getsm(t.tpsa, n, sm)
end

# Sparse monomial with nothing for vars
function lowget(t::Union{TPS,ComplexTPS}, vars::Nothing, param::Nothing, params::SMIndexType)
  sm, n = pairs_to_sm(t, Pair{Int,Int}[], params=params)
  return getsm(t.tpsa, n, sm)
end

lowget(t, idx, param, params) = error("Invalid monomial index specified. Please use ONE of variable/parameter index, index by order, or index by sparse monomial.")


# --- Slicing (getter) ---

# Vectors (which will be Vector{Any}) must be converted to tuples so SM or M indexing can be resolved:
const SMColonIndexType = Tuple{Vararg{Union{Pair{<:Integer,<:Integer},Colon}}}
const MColonIndexType =Tuple{Vararg{Union{Integer,Colon}}}
const TPSColonIndexType = Union{MColonIndexType,
                                SMColonIndexType,
                                Vector{<:Any}}

function getindex(t::Union{TPS,ComplexTPS}, v::TPSColonIndexType; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing)
  if (v isa Vector)
    par_mono = setup_mono(t, tuple(v...), param, params)
  else
    par_mono = setup_mono(t, v, param, params)
  end
  return slice(t, par_mono, false)
end

# For flat indexing:
function getindex(t::Union{TPS,ComplexTPS}, v::Integer, c::Colon)
  par_mono = setup_mono(t, v, nothing, nothing)
  return slice(t, par_mono,false)
end

function getindex(t::Union{TPS,ComplexTPS}, c::Colon; param::Union{Integer,Nothing}=nothing)
  if isnothing(param)
    return t
  else
    par_mono = setup_mono(t, nothing, param, nothing)
    return slice(t, par_mono,false)
  end
end


# --- par --- 
"""
    par(t::Union{TPS,ComplexTPS}, v::Union{TPSColonIndexType, Vector{Pair{<:Integer,<:Integer}}, Vector{<:Integer}, Integer, Colon, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing)

Extracts a polynomial from the TPS containing the specified monomial, and removes the monomial.

### Input
- `v`      -- An integer (for variable index), an array/tuple of orders for each variable (for indexing-by-order), or an array/tuple of pairs (sparse monomial)
- `param`  -- (Keyword argument, optional) An integer for the parameter index
- `params` -- (Keyword argument, optional) An array of pairs for sparse-monomial indexing

# Examples: Variable/Parameter Index:
```julia-repl
julia> d = Descriptor(5, 10, 2, 10); x = vars(d); k = params(d);

julia> f = 2*x[1]^2*x[3] + 3*x[1]^2*x[2]*x[3]*x[4]^2*x[5]*k[1] + 6*x[3] + 5
TPS:
 Coefficient                Order   Exponent
  5.0000000000000000e+00      0      0   0   0   0   0   |   0   0
  6.0000000000000000e+00      1      0   0   1   0   0   |   0   0
  2.0000000000000000e+00      3      2   0   1   0   0   |   0   0
  3.0000000000000000e+00      8      2   1   1   2   1   |   1   0


julia> par(f, 3)
TPS:
 Coefficient                Order   Exponent
  6.0000000000000000e+00      0      0   0   0   0   0   |   0   0
  2.0000000000000000e+00      2      2   0   0   0   0   |   0   0
  3.0000000000000000e+00      7      2   1   0   2   1   |   1   0


julia> par(f, param=1)
TPS:
 Coefficient                Order   Exponent
  3.0000000000000000e+00      7      2   1   1   2   1   |   0   0
```

# Examples: Monomial Index-by-Order
```julia-repl
julia> par(f, [2,:,1])
TPS:
 Coefficient                Order   Exponent
  2.0000000000000000e+00      0      0   0   0   0   0   |   0   0
  3.0000000000000000e+00      5      0   1   0   2   1   |   1   0


julia> par(f, [2,0,1])
TPS:
 Coefficient                Order   Exponent
  2.0000000000000000e+00      0      0   0   0   0   0   |   0   0
```

# Examples: Monomial Index-by-Sparse Monomial
```julia-repl
julia> par(f, [1=>2, 3=>1])
TPS:
 Coefficient                Order   Exponent
  2.0000000000000000e+00      0      0   0   0   0   0   |   0   0
  3.0000000000000000e+00      5      0   1   0   2   1   |   1   0

  
julia> par(f, params=[1=>1])
TPS:
 Coefficient                Order   Exponent
  3.0000000000000000e+00      7      2   1   1   2   1   |   0   0
```
"""
function par(t::Union{TPS,ComplexTPS}, v::Union{TPSColonIndexType, Vector{Pair{<:Integer,<:Integer}}, Vector{<:Integer}, Integer, Colon, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing)
  if (v isa Vector)
    tmpv = tuple(v...)
    if tmpv isa MColonIndexType # because par-ing, must add colon
      par_mono = setup_mono(t, tuple(tmpv...,:), param, params)
    else
      par_mono = setup_mono(t, tmpv, param, params)
    end
  else
    par_mono = setup_mono(t, v, param, params)
  end
  return slice(t, par_mono)
end

# Flat index:
function setup_mono(t1::Union{TPS,ComplexTPS}, v::Integer, param::Nothing, params::Nothing)::Vector{Cuchar}
  par_mono = ones(Cuchar, v+1).*0xff
  par_mono[v] = 0x1
  return par_mono
end

function setup_mono(t1::Union{TPS,ComplexTPS}, v::Union{Nothing,Colon}, param::Integer, params::Nothing)::Vector{Cuchar}
  nv = numvars(t1) # TOTAL NUMBER OF VARS!!!!
  par_mono = ones(Cuchar, param+nv+1).*0xff
  par_mono[nv+param] = 0x1
  return par_mono
end

# Monomial by order:
# This one should ALWAYS be called by par or splicing colon IS in the tuple or vector somewhere
function setup_mono(t1::Union{TPS,ComplexTPS}, v::MColonIndexType, param::Nothing, params::Nothing)::Vector{Cuchar}
  return collect(replace(x-> x isa Colon ? 0xff::Cuchar : convert(Cuchar, x)::Cuchar, v))
end

# By definition, sparse monomial makes everything else zero. SO if we reach this, it is automatically
# assumed that everything else is colon except those explictly made ix_var=>0
# Monomial by sparse monomial:
function setup_mono(t1::Union{TPS,ComplexTPS}, v::SMColonIndexType, param::Nothing, params::SMIndexType)::Vector{Cuchar}
  # Need to create array of orders with length nv + np
  ords, ___  = pairs_to_m(t1,filter(x->!(x isa Colon), v),params=params,zero_mono=false)
  return vcat(ords, 0xff)
end

function setup_mono(t1::Union{TPS,ComplexTPS}, v::SMColonIndexType, param::Nothing, params::Nothing)::Vector{Cuchar}
  # Need to create array of orders with length nv + np
  ords, ___  = pairs_to_m(t1,filter(x->!(x isa Colon), v),zero_mono=false)
  return vcat(ords, 0xff)
end

function setup_mono(t1::Union{TPS,ComplexTPS}, v::Nothing, param::Nothing, params::SMIndexType)::Vector{Cuchar}
  # Need to create array of orders with length nv + np
  ords, ___ = pairs_to_m(t1,Pair{Int,Int}[],params=params,zero_mono=false)
  return vcat(ords, 0xff)
end


# Throw error if no above use cases satisfied:
function setup_mono(t1::Union{TPS,ComplexTPS}, v, param, params)
  error("Invalid monomial specified. Please use ONE of variable/parameter index, index by order, or index by sparse monomial.")
end

idxm(t::Ptr{RTPSA}, n::Cint, m::Vector{Cuchar}) = (@inline; mad_tpsa_idxm(t, n, m))
idxm(t::Ptr{CTPSA}, n::Cint, m::Vector{Cuchar}) = (@inline; mad_ctpsa_idxm(t, n, m))

function slice(t1::Union{TPS,ComplexTPS}, par_mono::Vector{Cuchar}, par_it=true)
  nv = numvars(t1)
  np = numparams(t1)
  t = zero(t1)
  v = Cint(length(par_mono))
  coef = Ref{numtype(t)}()
  mono = Vector{Cuchar}(undef, np+nv)
  idx = idxm(t1.tpsa, v, replace(x->x==0xff ? 0x0 : x, par_mono))-Cint(1)
  idx = cycle!(t1.tpsa, idx, np+nv, mono, coef)
  valid_idxs = findall(x->x != 0xff, par_mono)
  invalid_idxs = findall(x->x == 0xff, par_mono[1:min(v,np+nv)])
  while idx >= 0
    if all(mono[valid_idxs] .== par_mono[valid_idxs])
      # if last index in par_mono is a colon, assume all the rest are colons, else check if all are zeros
      if last(par_mono) == 0xff || all(mono[valid_idxs[end]+1:end] .== 0x0)
        if par_it
          tmp = zeros(Cuchar, np+nv)
          tmp[invalid_idxs] .= mono[invalid_idxs]
          tmp[v+1:end] .= mono[v+1:end]
          t[tmp] = coef[]
        else
          t[idx] = coef[]
        end
      end
    end
    idx = cycle!(t1.tpsa, idx, np+nv, mono, coef)
  end
  return t
end

# --- gradient, jacobian, hessian getters ---
getv!(t::Ptr{RTPSA}, i::Cint, n::Cint, v::Union{Ptr{Cdouble},Vector{Cdouble}}) = (@inline; mad_tpsa_getv!(t, i, n, v))
getv!(t::Ptr{CTPSA}, i::Cint, n::Cint, v::Union{Ptr{ComplexF64},Vector{ComplexF64}}) = (@inline; mad_ctpsa_getv!(t, i, n, v))

"""
    gradient!(result::Vector{<:Union{Float64,ComplexF64}}, t::Union{TPS,ComplexTPS}; include_params=false)

Extracts the first-order partial derivatives (evaluated at 0) from the TPS and fills the `result` 
vector in-place. The partial derivatives wrt the parameters will also be extracted 
when the `include_params` flag is set to `true`. Note that this function is not 
calculating anything - just extracting the first-order monomial coefficients already 
in the TPS.

### Input
- `t`              -- `TPS`/`ComplexTPS` to extract the gradient from
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `result`         -- Preallocated `Vector` to fill with the gradient of the TPS
"""
function gradient!(result::Vector{<:Union{Float64,ComplexF64}}, t::Union{TPS,ComplexTPS}; include_params=false)
  n = numvars(t)
  if include_params
    n += numparams(t)
  end
  if length(result) != n
    error("Incorrect size for result")
  end
  getv!(t.tpsa, Cint(1), n, result)
end

"""
    gradient(t::Union{TPS,ComplexTPS}; include_params=false)

Extracts the first-order partial derivatives (evaluated at 0) from the TPS. The partial 
derivatives wrt the parameters will also be extracted when the `include_params` flag is 
set to `true`. Note that this function is not calculating anything - just extracting the 
first-order monomial coefficients already in the TPS.

### Input
- `t`              -- `TPS`/`ComplexTPS` to extract the gradient from
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `grad`           -- Gradient of the TPS
"""
function gradient(t::Union{TPS,ComplexTPS}; include_params=false)
  n = numvars(t)
  if include_params
    n += numparams(t)
  end
  grad = Vector{numtype(t)}(undef, n)
  getv!(t.tpsa, Cint(1), n, grad)
  return grad
end

"""
    jacobian!(result::Matrix{<:Union{Float64,ComplexF64}}, m::Vector{<:Union{TPS,ComplexTPS}}; include_params=false)

Extracts the first-order partial derivatives (evaluated at 0) from the Vector of TPSs. 
and fills the `result` matrix in-place. The partial derivatives wrt the parameters will 
also be extracted when the `include_params` flag is set to `true`. Note that this function 
is not calculating anything - just extracting the first-order monomial coefficients already 
in the TPSs.

### Input
- `m`              -- `Vector` of TPSs. to extract the Jacobian from
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `result`         -- Preallocated matrix to fill with the Jacobian of `m`
"""
function jacobian!(result::Matrix{<:Union{Float64,ComplexF64}}, m::Vector{<:Union{TPS,ComplexTPS}}; include_params=false)
  n = numvars(first(m))
  if include_params
    n += numparams(first(m))
  end
  if size(result)[2] != n
    error("Incorrect size for result")
  end
  grad = Vector{numtype(first(m))}(undef, n)
  # This is not fully in-place technically, bc Julia is column-major and 
  # filling each row in place without allocating temp would require row-major
  # So there are allocations for the array grad
  for i=1:length(m)
    getv!(m[i].tpsa, Cint(1), n, grad)
    result[i,:] = grad
  end
end

"""
    jacobian(m::Vector{<:Union{TPS,ComplexTPS}}; include_params=false)

Extracts the first-order partial derivatives (evaluated at 0) from the Vector of TPSs. 
The partial derivatives wrt the parameters will also be extracted when the `include_params` 
flag is set to `true`. Note that this function is not calculating anything - just extracting 
the first-order monomial coefficients already in the TPSs.

### Input
- `m`              -- `Vector` of TPSs. to extract the Jacobian from
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `J`              -- Jacobian of `m`
"""
function jacobian(m::Vector{<:Union{TPS,ComplexTPS}}; include_params=false)
  n = numvars(first(m))
  if include_params
    n += numparams(first(m))
  end
  J = Matrix{numtype(first(m))}(undef, length(m), n)
  grad = Vector{numtype(first(m))}(undef, n)
  for i=1:length(m)
    getv!(m[i].tpsa, Cint(1), n, grad)
    J[i,:] = grad
  end
  return J
end

"""
    jacobiant!(result::Matrix{<:Union{Float64,ComplexF64}}, m::Vector{<:Union{TPS,ComplexTPS}}; include_params=false)

Extracts the first-order partial derivatives (evaluated at 0) from the Vector of TPSs, 
as the transpose of the Jacobian. Because of Julia's column-major indexing vs. C's row-major, 
this routine will be slightly faster than using `jacobian!` if the transpose of the Jacobian is 
needed. The partial derivatives wrt the parameters will  also be extracted when the `include_params` 
flag is set to `true`. Note that this function is not calculating anything - just extracting the 
first-order monomial coefficients already in the TPSs and filling `result`.

### Input
- `m`              -- `Vector` of TPSs. to extract the Jacobian from
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `result`         -- Preallocated matrix to fill with the transpose of the Jacobian of `m`
"""
function jacobiant!(result::Matrix{<:Union{Float64,ComplexF64}}, m::Vector{<:Union{TPS,ComplexTPS}}; include_params=false)
  n = numvars(first(m))
  if include_params
    n += numparams(first(m))
  end
  if size(result)[2] != n
    error("Incorrect size for result")
  end
  for i=1:length(m)
    getv!(m[i].tpsa, Cint(1), n, pointer(result, length(m)*(i-1)+1))
  end
end

"""
    jacobiant(m::Vector{<:Union{TPS,ComplexTPS}}; include_params=false)

Extracts the first-order partial derivatives (evaluated at 0) from the Vector of TPSs, 
as the transpose of the Jacobian. Because of Julia's column-major indexing vs. C's row-major, 
this routine will be slightly faster than using `jacobian` if the transpose of the Jacobian is 
needed. The partial derivatives wrt the parameters will  also be extracted when the `include_params` 
flag is set to `true`. Note that this function is not calculating anything - just extracting the 
first-order monomial coefficients already in the TPSs.

### Input
- `m`              -- `Vector` of TPSs. to extract the Jacobian from
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `Jt`             -- Transpose of the Jacobian of `m`
"""
function jacobiant(m::Vector{<:Union{TPS,ComplexTPS}}; include_params=false)
  n = numvars(first(m))
  if include_params
    n += numparams(first(m))
  end
  result = Matrix{numtype(first(m))}(undef, length(m), n)
  jacobiant!(result, m, include_params=include_params)
  return result
end

"""
    hessian!(result::Matrix{<:Union{Float64,ComplexF64}},t::Union{TPS,ComplexTPS}; include_params=false)

Extracts the second-order partial derivatives (evaluated at 0) from the TPS 
and fills the `result` matrix in-place. The partial derivatives wrt the parameters will 
also be extracted when the `include_params` flag is set to `true`. Note that this function 
is not calculating anything - just extracting the second-order monomial coefficients already 
in the TPS.

### Input
- `t`              -- `TPS`/`ComplexTPS` to extract the Hessian from
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `result`         -- Preallocated matrix to fill with the Hessian of the TPS
"""
function hessian!(result::Matrix{<:Union{Float64,ComplexF64}},t::Union{TPS,ComplexTPS}; include_params=false)
  d = Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d)
  desc = unsafe_load(d)
  n = desc.nv
  if include_params
    n += desc.np
  end
  if size(result) != (n,n)
    error("Incorrect size for result")
  end
  # Check that all vars/params are >= 2nd orders
  for i=1:n
    if unsafe_load(desc.no, i) < 0x2
      error("Hessian undefined for TPSA with at least one variable/parameter of order < 2")
    end
  end
  result[:] .= 0.
  idx = Cint(desc.nv+desc.np)
  maxidx = Cint(floor(n*(n+1)/2))+n
  v = Ref{numtype(t)}()
  mono = Vector{UInt8}(undef, n)
  idx = cycle!(t.tpsa, idx, n, mono, v)
  while idx > 0 && idx <= maxidx
    i = findfirst(x->x==0x1, mono)
    if isnothing(i)
      i = findfirst(x->x==0x2, mono)
      H[i,i] = v[]*2    # Multiply by 2 because taylor coefficient on diagonal is 1/2!*d2f/dx2
    else 
      j = findlast(x->x==0x1, mono)
      H[i,j] = v[]
      H[j,i] = v[]
    end
    idx = cycle!(t.tpsa, idx, n, mono, v)
  end
end

"""
    hessian(t::Union{TPS,ComplexTPS}; include_params=false)

Extracts the second-order partial derivatives (evaluated at 0) from the TPS.
The partial derivatives wrt the parameters will also be extracted when the `include_params` 
flag is set to `true`. Note that this function is not calculating anything - just extracting 
the second-order monomial coefficients already in the TPS.

### Input
- `t`              -- `TPS`/`ComplexTPS` to extract the Hessian from
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `H`              -- Hessian of the TPS
"""
function hessian(t::Union{TPS,ComplexTPS}; include_params=false)
  d = Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d)
  desc = unsafe_load(d)
  n = desc.nv
  if include_params
    n += desc.np
  end
  # Check that all vars/params are >= 2nd orders
  for i=1:n
    if unsafe_load(desc.no, i) < 0x2
      error("Hessian undefined for TPSA with at least one variable/parameter of order < 2")
    end
  end
  H = zeros(numtype(t), n, n)
  idx = Cint(desc.nv+desc.np)
  maxidx = Cint(floor(n*(n+1)/2))+n
  v = Ref{numtype(t)}()
  mono = Vector{UInt8}(undef, n)
  idx = cycle!(t.tpsa, idx, n, mono, v)
  while idx > 0 && idx <= maxidx
    i = findfirst(x->x==0x1, mono)
    if isnothing(i)
      i = findfirst(x->x==0x2, mono)
      H[i,i] = v[]*2    # Multiply by 2 because taylor coefficient on diagonal is 1/2!*d2f/dx2
    else 
      j = findlast(x->x==0x1, mono)
      H[i,j] = v[]
      H[j,i] = v[]
    end
    idx = cycle!(t.tpsa, idx, n, mono, v)
  end
  return H
end
