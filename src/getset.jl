# Valid indexing types 
# Sparse monomial:
const SMIndexType = Union{Vector{<:Pair{<:Integer,<:Integer}}, Tuple{Vararg{Pair{<:Integer,<:Integer}}}}
# Monomial:
const MIndexType = Union{Vector{<:Integer}, Tuple{Vararg{Integer}}}
# TPS Index:
const TPSIndexType = Union{Integer,
                           MIndexType,
                           SMIndexType}

# --- Setters ---
# All
function setindex!(t::TPS, v::Union{AbstractVector{<:Number},Number}, idx::Union{TPSIndexType,AbstractVector{<:Integer},Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType,Nothing}=nothing)
  lowset!(t, v, idx, param, params)
end

# Flat index
function lowset!(t::TPS, v::Number, i::Union{Nothing,Integer}, param::Union{Nothing,Integer}, params::Nothing)
  if isnothing(i) && isnothing(param)
    return
  elseif !isnothing(i) && !isnothing(param)
    error("Invalid monomial index specified. Please use ONE of variable/parameter index, index by order, or index by sparse monomial.")
  elseif isnothing(param)
    seti!(t, i, 0, v)
  else
    nv = numvars(t)
    seti!(t, nv+param, 0, v)
  end
end

# Flat index vectorized
function lowset!(t::TPS, v::AbstractVector{<:Number}, idxs::AbstractVector{<:Integer}, param::Nothing, params::Nothing)
  Base.require_one_based_indexing(v,idxs)
  length(v) == length(idxs) || throw(DimensionMismatch("Tried to assign $(length(v)) elements to $(length(idxs)) destinations"))
  for i=1:length(idxs)
    seti!(t, idxs[i], 0, v[i])
  end
end

# Monomial
function lowset!(t::TPS, v::Number, ords::MIndexType, param::Nothing, params::Nothing)
  setm!(t, length(ords), ords isa Vector{Cuchar} ? ords : collect(Cuchar, ords), 0, v)
end

# By sparse monomial
function lowset!(t::TPS, v::Number, vars::SMIndexType, param::Nothing, params::Union{SMIndexType,Nothing})
  sm, n = pairs_to_sm(t, vars, params=params)
  setsm!(t, n, sm, 0, v)
end

# Sparse monomial with nothing for vars
function lowset!(t::TPS, v::Number, vars::Nothing, param::Nothing, params::SMIndexType)
  sm, n = pairs_to_sm(t, Pair{Int,Int}[], params=params)
  setsm!(t, n, sm, 0, v)
end

# In case someone forgets tuple:
lowset!(t, v, idx, param, params) = error("Invalid monomial index specified. Please use ONE of variable/parameter index, index by order, or index by sparse monomial.")


# --- Getters ---
# All
function getindex(t::TPS, idx::Union{AbstractVector{<:Integer},TPSIndexType,Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType,Nothing}=nothing)
  return lowget(t, idx, param, params)
end

# To override Base number.jl
getindex(t::TPS, idx::Integer) = lowget(t, idx, nothing, nothing)

# Flat index
function lowget(t::TPS, i::Union{Nothing,Integer}, param::Union{Nothing,Integer}, params::Nothing)
  if isnothing(i) && isnothing(param)
    return t
  elseif !isnothing(i) && !isnothing(param)
    error("Invalid monomial index specified. Please use ONE of variable/parameter index, index by order, or index by sparse monomial.")
  elseif isnothing(param)
    return geti(t, i)
  else
    nv = numvars(t)
    return geti(t, nv+param)
  end
end

# Flat index vectorized
function lowget(t::TPS, idxs::AbstractVector{<:Integer}, param::Nothing, params::Nothing)
  Base.require_one_based_indexing(idxs)
  v = Vector{numtype(t)}(undef, length(idxs))
  for i=1:length(idxs)
    v[i] = geti(t, idxs[i])
  end
  return v
end

# Monomial
function lowget(t::TPS, ords::MIndexType, param::Nothing, params::Nothing)
  return getm(t, length(ords), ords isa Vector{Cuchar} ? ords : collect(Cuchar, ords))
end

# By sparse monomial
function lowget(t::TPS, vars::SMIndexType, param::Nothing, params::Union{SMIndexType,Nothing})
  sm, n = pairs_to_sm(t, vars, params=params)
  return getsm(t, n, sm)
end

# Sparse monomial with nothing for vars
function lowget(t::TPS, vars::Nothing, param::Nothing, params::SMIndexType)
  sm, n = pairs_to_sm(t, Pair{Int,Int}[], params=params)
  return getsm(t, n, sm)
end

lowget(t, idx, param, params) = error("Invalid monomial index specified. Please use ONE of variable/parameter index, index by order, or index by sparse monomial.")

"""
    numcoefs(t::TPS)

Returns the maximum number of monomials (including the scalar part)
in the `TPS`/`ComplexTPS64` given its `Descriptor`. 
"""
numcoefs(t::TPS) = len(t)

firstindex(t::TPS) = 0
lastindex(t::TPS) = numcoefs(t)-1

# --- Slicing (getter) ---

# Vectors (which will be Vector{Any}) must be converted to tuples so SM or M indexing can be resolved:
const SMColonIndexType = Tuple{Vararg{Union{Pair{<:Integer,<:Integer},Colon}}}
const MColonIndexType =Tuple{Vararg{Union{Integer,Colon}}}
const TPSColonIndexType = Union{MColonIndexType,
                                SMColonIndexType,
                                Vector{<:Any}}

function getindex(t::TPS, v::TPSColonIndexType; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing)
  if (v isa Vector)
    par_mono = setup_mono(t, tuple(v...), param, params)
  else
    par_mono = setup_mono(t, v, param, params)
  end
  return slice(t, par_mono, false)
end

# For flat indexing:
function getindex(t::TPS, v::Integer, c::Colon)
  par_mono = setup_mono(t, v, nothing, nothing)
  return slice(t, par_mono,false)
end

function getindex(t::TPS, c::Colon; param::Union{Integer,Nothing}=nothing)
  if isnothing(param)
    return t
  else
    par_mono = setup_mono(t, nothing, param, nothing)
    return slice(t, par_mono,false)
  end
end


# --- par --- 
"""
    par(t::TPS, monomialindex)

Extracts a polynomial from the TPS containing the specified monomial, and removes the monomial. 
Any of the three monomial indexing schemes (by order, sparse monomial, or monomial index -- 
see the Monomial Indexing section of the GTPSA manual for more details) may be used to specify 
the monomial.

# Examples: Variable/Parameter Index:
```julia-repl
julia> d = Descriptor(5, 10, 2, 10); Δx = @vars(d); Δk = @params(d);

julia> f = 2*Δx[1]^2*Δx[3] + 3*Δx[1]^2*Δx[2]*Δx[3]*Δx[4]^2*Δx[5]*Δk[1] + 6*Δx[3] + 5
TPS64{Descriptor(NV=5, MO=10, NP=2, PO=10)}:
 Coefficient                Order   Exponent
  5.0000000000000000e+00      0      0   0   0   0   0   |   0   0
  6.0000000000000000e+00      1      0   0   1   0   0   |   0   0
  2.0000000000000000e+00      3      2   0   1   0   0   |   0   0
  3.0000000000000000e+00      8      2   1   1   2   1   |   1   0


julia> par(f, 3)
TPS64{Descriptor(NV=5, MO=10, NP=2, PO=10)}:
 Coefficient                Order   Exponent
  6.0000000000000000e+00      0      0   0   0   0   0   |   0   0
  2.0000000000000000e+00      2      2   0   0   0   0   |   0   0
  3.0000000000000000e+00      7      2   1   0   2   1   |   1   0


julia> par(f, param=1)
TPS64{Descriptor(NV=5, MO=10, NP=2, PO=10)}:
 Coefficient                Order   Exponent
  3.0000000000000000e+00      7      2   1   1   2   1   |   0   0
```

# Examples: Monomial Index-by-Order
```julia-repl
julia> par(f, [2,:,1])
TPS64{Descriptor(NV=5, MO=10, NP=2, PO=10)}:
 Coefficient                Order   Exponent
  2.0000000000000000e+00      0      0   0   0   0   0   |   0   0
  3.0000000000000000e+00      5      0   1   0   2   1   |   1   0


julia> par(f, [2,0,1])
TPS64{Descriptor(NV=5, MO=10, NP=2, PO=10)}:
 Coefficient                Order   Exponent
  2.0000000000000000e+00      0      0   0   0   0   0   |   0   0
```


# Examples: Monomial Index-by-Sparse Monomial
```julia-repl
julia> par(f, [1=>2, 3=>1])
TPS64{Descriptor(NV=5, MO=10, NP=2, PO=10)}:
 Coefficient                Order   Exponent
  2.0000000000000000e+00      0      0   0   0   0   0   |   0   0
  3.0000000000000000e+00      5      0   1   0   2   1   |   1   0


julia> par(f, params=[1=>1])
TPS64{Descriptor(NV=5, MO=10, NP=2, PO=10)}:
 Coefficient                Order   Exponent
  3.0000000000000000e+00      7      2   1   1   2   1   |   0   0
```
"""
function par(t::TPS, v::Union{TPSColonIndexType, Vector{Pair{<:Integer,<:Integer}}, Vector{<:Integer}, Integer, Colon, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType, Nothing}=nothing)
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
function setup_mono(t1::TPS, v::Integer, param::Nothing, params::Nothing)::Vector{Cuchar}
  par_mono = ones(Cuchar, v+1).*0xff
  par_mono[v] = 0x1
  return par_mono
end

function setup_mono(t1::TPS, v::Union{Nothing,Colon}, param::Integer, params::Nothing)::Vector{Cuchar}
  nv = numvars(t1) # TOTAL NUMBER OF VARS!!!!
  par_mono = ones(Cuchar, param+nv+1).*0xff
  par_mono[nv+param] = 0x1
  return par_mono
end

# Monomial by order:
# This one should ALWAYS be called by par or splicing colon IS in the tuple or vector somewhere
function setup_mono(t1::TPS, v::MColonIndexType, param::Nothing, params::Nothing)::Vector{Cuchar}
  return collect(replace(x-> x isa Colon ? 0xff::Cuchar : convert(Cuchar, x)::Cuchar, v))
end

# By definition, sparse monomial makes everything else zero. SO if we reach this, it is automatically
# assumed that everything else is colon except those explictly made ix_var=>0
# Monomial by sparse monomial:
function setup_mono(t1::TPS, v::SMColonIndexType, param::Nothing, params::SMIndexType)::Vector{Cuchar}
  # Need to create array of orders with length nv + np
  ords, ___  = pairs_to_m(t1,filter(x->!(x isa Colon), v),params=params,zero_mono=false)
  return vcat(ords, 0xff)
end

function setup_mono(t1::TPS, v::SMColonIndexType, param::Nothing, params::Nothing)::Vector{Cuchar}
  # Need to create array of orders with length nv + np
  ords, ___  = pairs_to_m(t1,filter(x->!(x isa Colon), v),zero_mono=false)
  return vcat(ords, 0xff)
end

function setup_mono(t1::TPS, v::Nothing, param::Nothing, params::SMIndexType)::Vector{Cuchar}
  # Need to create array of orders with length nv + np
  ords, ___ = pairs_to_m(t1,Pair{Int,Int}[],params=params,zero_mono=false)
  return vcat(ords, 0xff)
end


# Throw error if no above use cases satisfied:
function setup_mono(t1::TPS, v, param, params)
  error("Invalid monomial specified. Please use ONE of variable/parameter index, index by order, or index by sparse monomial.")
end

# assums par_mono ends in 0xff.
function slice(t1::TPS, par_mono::Vector{Cuchar}, par_it=true)
  nv = numvars(t1)
  np = numparams(t1)
  t = zero(t1)
  v = length(par_mono)-1
  coef = Ref{numtype(t)}()
  mono = Vector{Cuchar}(undef, np+nv)
  idx = idxm(t1, v, replace(x->x==0xff ? 0x0 : x, par_mono))-1
  idx = cycle!(t1, idx, np+nv, mono, coef)
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
    idx = cycle!(t1, idx, np+nv, mono, coef)
  end
  return t
end

# --- gradient, jacobian, hessian getters ---

"""
    GTPSA.gradient!(result, t::TPS; include_params=false, unsafe_inbounds=false)

Extracts the first-order partial derivatives (evaluated at 0) from the TPS and fills the `result` 
vector in-place. The partial derivatives wrt the parameters will also be extracted 
when the `include_params` flag is set to `true`. Note that this function is not 
calculating anything - just extracting the first-order monomial coefficients already 
in the TPS.

### Input
- `t`               -- `TPS`/`ComplexTPS64` to extract the gradient from
- `include_params`  -- (Optional) Extract partial derivatives wrt parameters. Default is false
- `unsafe_inbounds` -- (Optional) Flag which, if false, ignores checking the size of `result`. The size of `result` is used to index the `TPS`. Default is `false`

### Output
- `result`         -- Vector to fill with the gradient of the TPS, must be 1-based indexing
"""
function gradient!(result, t::TPS; include_params=false, unsafe_inbounds=false)
  Base.require_one_based_indexing(result)
  n = numvars(t)
  if include_params
    n += numparams(t)
  end
  if !unsafe_inbounds && length(result) != n
    error("Incorrect size for result")
  end
  getv!(t, 1, length(result), result)
  return
end

"""
    GTPSA.gradient(t::TPS; include_params=false)

Extracts the first-order partial derivatives (evaluated at 0) from the TPS. The partial 
derivatives wrt the parameters will also be extracted when the `include_params` flag is 
set to `true`. Note that this function is not calculating anything - just extracting the 
first-order monomial coefficients already in the TPS.

### Input
- `t`              -- `TPS`/`ComplexTPS64` to extract the gradient from
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `grad`           -- Gradient of the TPS
"""
function gradient(t::TPS; include_params=false)
  n = numvars(t)
  if include_params
    n += numparams(t)
  end
  grad = Vector{numtype(t)}(undef, n)
  getv!(t, 1, n, grad)
  return grad
end

"""
    GTPSA.jacobian!(result, m::AbstractArray{<:TPS}; include_params=false, unsafe_inbounds=false)

Extracts the first-order partial derivatives (evaluated at 0) from the array of TPSs. 
and fills the `result` matrix in-place. The partial derivatives wrt the parameters will 
also be extracted when the `include_params` flag is set to `true`. Note that this function 
is not calculating anything - just extracting the first-order monomial coefficients already 
in the TPSs.

### Input
- `m`               -- Array of TPSs to extract the Jacobian from, must be 1-based indexing
- `include_params`  -- (Optional) Extract partial derivatives wrt parameters. Default is false
- `unsafe_inbounds` -- (Optional) Flag which, if false, ignores checking the size of `result`. The size of `result` is used to index the `TPS`. Default is `false`

### Output
- `result`         -- Matrix to fill with the Jacobian of `m`, must be 1-based indexing
"""
function jacobian!(result, m::AbstractArray{<:TPS}; include_params=false, unsafe_inbounds=false)
  Base.require_one_based_indexing(result, m)
  n = numvars(first(m))
  if include_params
    n += numparams(first(m))
  end
  
  if !unsafe_inbounds && size(result) != (length(m), n)
    error("Incorrect size for result")
  end


  for i=1:size(result, 2)
    for j=1:size(result, 1)
      result[j,i] = geti(m[j], i)
    end
  end
  return
end

"""
    GTPSA.jacobian(m::AbstractArray{<:TPS}; include_params=false)

Extracts the first-order partial derivatives (evaluated at 0) from the array of TPSs. 
The partial derivatives wrt the parameters will also be extracted when the `include_params` 
flag is set to `true`. Note that this function is not calculating anything - just extracting 
the first-order monomial coefficients already in the TPSs.

### Input
- `m`              -- Array of TPSs to extract the Jacobian from, must be 1-based indexing
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `J`              -- Jacobian of `m`
"""
function jacobian(m::AbstractArray{<:TPS}; include_params=false)
  Base.require_one_based_indexing(m)
  n = numvars(first(m))
  if include_params
    n += numparams(first(m))
  end
  J = Matrix{numtype(first(m))}(undef, length(m), n)
  jacobian!(J, m; include_params=include_params)
  return J
end

"""
    GTPSA.jacobiant!(result, m::AbstractArray{<:TPS}; include_params=false, unsafe_inbounds=false)

Extracts the first-order partial derivatives (evaluated at 0) from the array of TPSs, 
as the transpose of the Jacobian. The partial derivatives wrt the parameters will also 
be extracted when the `include_params` flag is set to `true`. Note that this function is 
not calculating anything - just extracting the first-order monomial coefficients already 
in the TPSs and filling `result`.

### Input
- `m`               -- Vector of TPSs to extract the Jacobian from, must be 1-based indexing
- `include_params`  -- (Optional) Extract partial derivatives wrt parameters. Default is false
- `unsafe_inbounds` -- (Optional) Flag which, if false, ignores checking the size of `result`. The size of `result` is used to index the `TPS`. Default is `false`

### Output
- `result`         -- Matrix to fill with the transpose of the Jacobian of `m`, must be 1-based indexing
"""
function jacobiant!(result, m::AbstractArray{<:TPS}; include_params=false, unsafe_inbounds=false)
  Base.require_one_based_indexing(result, m)
  n = numvars(first(m))
  if include_params
    n += numparams(first(m))
  end
  if !unsafe_inbounds && size(result) != (n, length(m))
    error("Incorrect size for result")
  end
  for i=1:size(result, 2)
    getv!(m[i], 1, size(result, 1), pointer(result, size(result, 1)*(i-1)+1))
  end
end

"""
    GTPSA.jacobiant(m::AbstractArray{<:TPS}; include_params=false) where {N,P,I}

Extracts the first-order partial derivatives (evaluated at 0) from the array of TPSs, 
as the transpose of the Jacobian. The partial derivatives wrt the parameters will also 
be extracted when the `include_params` flag is set to `true`. Note that this function is 
not calculating anything - just extracting the first-order monomial coefficients already 
in the TPSs.

### Input
- `m`              -- Array of TPSs to extract the Jacobian from, must be 1-based indexing
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `Jt`             -- Transpose of the Jacobian of `m`
"""
function jacobiant(m::AbstractArray{<:TPS}; include_params=false)
  Base.require_one_based_indexing(m)
  n = numvars(first(m))
  if include_params
    n += numparams(first(m))
  end
  result = Matrix{numtype(first(m))}(undef, n, length(m))
  jacobiant!(result, m, include_params=include_params)
  return result
end


"""
    GTPSA.hessian!(result, t::TPS; include_params=false, tmp_mono::Union{Nothing,Vector{UInt8}}=nothing, unsafe_fast=false, unsafe_inbounds=false)

Extracts the second-order partial derivatives (evaluated at 0) from the TPS 
and fills the `result` matrix in-place. The partial derivatives wrt the parameters will 
also be extracted when the `include_params` flag is set to `true`. Note that this function 
is not calculating anything - just extracting the second-order monomial coefficients already 
in the TPS.

### Input
- `t`               -- `TPS`/`ComplexTPS64` to extract the Hessian from
- `include_params`  -- (Optional) Extract partial derivatives wrt parameters. Default is false
- `tmp_mono`        -- (Optional) `Vector{UInt8}` to store the monomial, when different orders of truncation are used
- `unsafe_fast`     -- (Optional) Flag to specify that "fast" indexing should be used without checking. This will give incorrect results if any variable has a TO < 2. Default is `false`.
- `unsafe_inbounds` -- (Optional) Flag which, if false, ignores checking the size of `result`. The size of `result` is used to index the `TPS`. Default is `false`

### Output
- `result`         -- Matrix to fill with the Hessian of the TPS, must be 1-based indexing
"""
function hessian!(result, t::TPS; include_params=false, tmp_mono::Union{Nothing,Vector{UInt8}}=nothing, unsafe_fast=false, unsafe_inbounds=false)
  Base.require_one_based_indexing(result)
  desc = unsafe_load(t.d)
  n = desc.nv
  if include_params
    n += desc.np
  end

  if !unsafe_inbounds && size(result) != (n, n)
    error("Incorrect size for result")
  end

  nn = desc.nn

  result .= 0.

  # If all variables/variable+parameters have truncation order > 2, then 
  # the indexing is known beforehand and we can do it faster
  fast = true
  if !unsafe_fast
    i = 1
    while fast && i <= n
      if unsafe_load(desc.no, i) < 0x2
        fast = false  # use "slow" indexing
      end
      i += 1
    end
  end
  
  if fast
    idx = desc.nv+desc.np
    endidx = floor(n*(n+1)/2)+nn
    curdiag = 1
    col = 1
    v = Ref{numtype(t)}()
    idx = cycle!(t, idx, 0, C_NULL, v)
    while idx <= endidx && idx > 0
      h_idx = idx-nn
      while h_idx > curdiag
        col += 1
        curdiag += col
      end
      row = col-(curdiag-h_idx)
      #println("row = ", row, ", col = ", col)
      if row==col
        result[row,col] = 2*v[]
      else
        result[row,col] = v[]
        result[col,row] = v[]
      end
      idx = cycle!(t, idx, 0, C_NULL, v)
    end
  else
    # If there are some variables/parameters with TO == 1, we have to do it "slow"
    # because the indexing of TPSA index -> hessian index can be very complicated.
    # I saw slow in quotes because it is likely still much faster than the calculation
    # of the Hessian itself (this is just a getter)  
    idx = desc.nv+desc.np # start at 2nd order
    v = Ref{numtype(t)}()
    if isnothing(tmp_mono)
      mono = Vector{UInt8}(undef, nn)
    else
      length(tmp_mono) == nn || error("length(tmp_mono) must be $nn, received $(length(tmp_mono))")
      mono = tmp_mono
    end
    idx = cycle!(t, idx, nn, mono, v)
    while idx > 0 
      if sum(mono) > 0x2
        return result
      end
      i = findfirst(x->x==0x1, mono)
      if isnothing(i)
        i = findfirst(x->x==0x2, mono)
        if isnothing(i)
          return result
        end
        if i <= n
          result[i,i] = 2*v[]   # Multiply by 2 because taylor coefficient on diagonal is 1/2!*d2f/dx2
        end
      else 
        j = findlast(x->x==0x1, mono)
        if isnothing(j)
          return result
        end
        if i <= n && j <= n
          result[i,j] = v[]
          result[j,i] = v[]
        end
      end
      idx = cycle!(t, idx, nn, mono, v)
    end
  end
  return result
end

"""
    GTPSA.hessian(t::TPS; include_params=false)

Extracts the second-order partial derivatives (evaluated at 0) from the TPS.
The partial derivatives wrt the parameters will also be extracted when the `include_params` 
flag is set to `true`. Note that this function is not calculating anything - just extracting 
the second-order monomial coefficients already in the TPS.

### Input
- `t`              -- `TPS`/`ComplexTPS64` to extract the Hessian from
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `H`              -- Hessian of the TPS
"""
function hessian(t::TPS; include_params=false)
  desc = unsafe_load(t.d)
  n = desc.nv
  if include_params
    n += desc.np
  end

  H = zeros(numtype(t), n, n)

  hessian!(H, t, include_params=include_params)
  return H
end
