
"""
    vars(use::Union{Descriptor,TPS}=GTPSA.desc_current)

Returns a vector of `TPS`s corresponding to the variables for the 
`Descriptor` specified by `use`. Default value is `GTPSA.desc_current`.

### Input
- `use` -- (Optional) Specify which `Descriptor` to use, default is `GTPSA.desc_current`

### Output
- `x`   -- `Vector` containing unit `TPS{Float64}`s corresponding to each variable
"""
function vars(use::Union{Descriptor,TPS}=GTPSA.desc_current)
  getdesc(use).desc != C_NULL || error("Descriptor not defined!")
  nv = numvars(use)
  x = Vector{TPS{Float64,Nothing}}(undef, nv)
  for i=1:nv
    t = TPS{Float64}(use=use)
    @inbounds t[i] = 1.0
    @inbounds x[i] = t
  end
  return x
end

"""
    params(use::Union{Descriptor,TPS}=GTPSA.desc_current)

Returns a vector of `TPS`s corresponding to the parameters for the 
`Descriptor` specified by `use`. Default value is `GTPSA.desc_current`.

### Input
- `use` -- (Optional) Specify which `Descriptor` to use, default is `GTPSA.desc_current`

### Output
- `x`   -- `Vector` containing unit `TPS{Float64}`s corresponding to each parameters
"""
function params(use::Union{Descriptor,TPS}=GTPSA.desc_current)
  getdesc(use).desc != C_NULL || error("Descriptor not defined!")
  np = numparams(use)
  nv = numvars(use)
  k = Vector{TPS{Float64,Nothing}}(undef, np)
  for i=1:np
    t = TPS{Float64}(use=use)
    @inbounds t[nv+i] = 1.0
    @inbounds k[i] = t
  end
  return k
end

"""
    complexvars(use::Union{Descriptor,TPS}=GTPSA.desc_current)

Returns a vector of `ComplexTPS64`s corresponding to the variables for the 
`Descriptor` specified by `use`. Default value is `GTPSA.desc_current`.

### Input
- `use` -- (Optional) Specify which `Descriptor` to use, default is `GTPSA.desc_current`

### Output
- `x`   -- `Vector` containing unit `ComplexTPS64`s corresponding to each variable
"""
function complexvars(use::Union{Descriptor,TPS}=GTPSA.desc_current)
  getdesc(use).desc != C_NULL || error("Descriptor not defined!")
  nv = numvars(use)
  x = Vector{TPS{ComplexF64}}(undef, nv)
  for i=1:nv
    t = TPS{ComplexF64}(use=use)
    @inbounds t[i] = 1.0
    @inbounds x[i] = t
  end
  return x
end

"""
    complexparams(use::Union{Descriptor,TPS}=GTPSA.desc_current)

Returns a vector of `ComplexTPS64`s corresponding to the parameters for the 
`Descriptor` specified by `use`. Default value is `GTPSA.desc_current`.

### Input
- `use` -- (Optional) Specify which `Descriptor` to use, default is `GTPSA.desc_current`

### Output
- `x`   -- `Vector` containing unit `ComplexTPS64`s corresponding to each parameters
"""
function complexparams(use::Union{Descriptor,TPS}=GTPSA.desc_current)
  getdesc(use).desc != C_NULL || error("Descriptor not defined!")
  np = numparams(use)
  nv = numvars(use)
  k = Vector{TPS{ComplexF64}}(undef, np)
  for i=1:np
    t = TPS{ComplexF64}(use=use)
    @inbounds t[nv+i] = 1.0
    @inbounds k[i] = t
  end
  return k
end

"""
    mono(v::Union{Integer, Vector{<:Union{<:Pair{<:Integer,<:Integer},<:Integer}}, Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{Vector{<:Pair{<:Integer,<:Integer}}, Nothing}=nothing, use::Descriptor=GTPSA.desc_current)

Returns a `TPS{Float64}` corresponding to a specific monomial, specified using the variable/parameter index, or 
monomial indexing-by-order OR monomial indexing-by-sparse monomial. 

### Input
- `v`      -- An integer (for variable index), an array of orders for each variable (for indexing-by-order), or an array of pairs (sparse monomial)
- `param`  -- (Keyword argument, optional) An integer for the parameter index
- `params` -- (Keyword argument, optional) An array of pairs for sparse-monomial indexing
- `use`    -- (Keyword argument, optional) The descriptor to use to generate the monomial. Default is most recently-defined.

# Examples: Variable/Parameter Index:
```julia-repl
julia> d = Descriptor(3,10,2,10);

julia> mono(1)
TPS{Float64}:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        1    0    0    0    0


julia> mono(2, use=d)
TPS{Float64}:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        0    1    0    0    0


julia> mono(param=2)
TPS{Float64}:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        0    0    0    0    1
```

# Examples: Monomial Index-by-Order
```julia-repl
julia> mono([1])
TPS{Float64}:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        1    0    0    0    0


julia> mono([0,1])
TPS{Float64}:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        0    1    0    0    0


julia> mono([0,0,0,0,1], use=d)
TPS{Float64}:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        0    0    0    0    1


julia> mono([1,0,0,0,1])
TPS{Float64}:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    2        1    0    0    0    1
```

# Examples: Monomial Index-by-Sparse Monomial
```julia-repl
julia> mono([1=>1])
TPS{Float64}:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        1    0    0    0    0


julia> mono([2=>1])
TPS{Float64}:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        0    1    0    0    0


julia> mono([1=>1], params=[2=>1], use=d)
TPS{Float64}:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    2        1    0    0    0    1
```
"""
function mono(v::Union{TPSIndexType,Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType,Nothing}=nothing, use::Union{Descriptor,TPS}=GTPSA.desc_current)
  return low_mono(Float64, use, v, param, params)
end

function complexmono(v::Union{TPSIndexType,Nothing}=nothing; param::Union{Integer,Nothing}=nothing, params::Union{SMIndexType,Nothing}=nothing, use::Union{Descriptor,TPS}=GTPSA.desc_current)
  return low_mono(ComplexF64, use, v, param, params)
end

# Variable/parameter:
function low_mono(T::Type, d::Union{Descriptor,TPS}, v::Integer, param::Nothing, params::Nothing)
  t = TPS{T}(use=d)
  t[v] = 1
  return t
end

function low_mono(T::Type, d::Union{Descriptor,TPS}, v::Nothing, param::Integer, params::Nothing)
  t = TPS{T}(use=d)
  t[param=param] = 1
  return t
end

# Default to scalar value if nothing passed
function low_mono(T::Type, d::Union{Descriptor,TPS}, v::Nothing, param::Nothing, params::Nothing)
  t = TPS{T}(use=d)
  t[0] = 1.0
  return t
end

# Monomial by order:
function low_mono(T::Type, d::Union{Descriptor,TPS}, v::MIndexType, param::Nothing, params::Nothing)
  t = TPS{T}(use=d)
  t[v] = 1.0
  return t
end

# Monomial by sparse monomial:
function low_mono(T::Type, d::Union{Descriptor,TPS}, v::SMIndexType, param::Nothing, params::SMIndexType)
  t = TPS{T}(use=d)
  t[v, params=params] = 1.0
  return t
end

function low_mono(T::Type, d::Union{Descriptor,TPS}, v::SMIndexType, param::Nothing, params::Nothing)
  t = TPS{T}(use=d)
  # Need to create array of orders with length nv + np
  t[v] = 1.0
  return t
end

function low_mono(T::Type, d::Union{Descriptor,TPS}, v::Nothing, param::Nothing, params::SMIndexType)
  t = TPS{T}(use=d)
  t[params=params] = 1.0
  return t
end

# Throw error if no above use cases satisfied:
function low_mono(T::Type, d::Union{Descriptor,TPS}, v, param, params)
  error("Invalid monomial specified. Please use ONE of variable/parameter index, index by order, or index by sparse monomial.")
end