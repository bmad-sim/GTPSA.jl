
function check_kwargs(fn, kwargs...)
  valid_kwargs = [:(dynamic)=>Bool, :(complex)=>Bool]
  #valid_types = [Bool, Bool]
  for k in kwargs
    if Meta.isexpr(k, :(=))
      pk = Pair(k.args...)
      idx = findfirst(t->t==pk[1], map(t->t[1], valid_kwargs))
      if isnothing(idx)
        error("Unrecognized input to @$(fn) macro: $(pk[1])")
      elseif typeof(pk[2]) != valid_kwargs[idx][2]
        error("Type for keyword argument `$(pk[1])` must be `$(valid_kwargs[idx][2])`")
      end
    else
      error("Unrecognized input to @$(fn) macro: $k")
    end
  end
end

"""
    @vars(descriptor [,complex=bool] [, dynamic=bool])

Constructs a vector of `TPS`s corresponding to each of the variables in the GTPSA `descriptor`

# Keyword Arguments
- `complex` -- If `true`, returns the variables as `ComplexTPS64`s. Default is `false`.
- `dynamic` -- If `true`, the variables will use dynamic `Descriptor` resolution. Default is `false`.
"""
macro vars(d, kwargs...)
  # Check each kwarg:
  check_kwargs(:vars, kwargs...)
  kwargnames = map(t->t[1], map(t->Pair(t.args...), kwargs))
  kwargvals = map(t->t[2],map(t->Pair(t.args...), kwargs))

  idx_dynamic = findfirst(t->t==:dynamic, kwargnames)
  idx_complex = findfirst(t->t==:complex, kwargnames)
  if isnothing(idx_dynamic) || !kwargvals[idx_dynamic]
    if !isnothing(idx_complex) && kwargvals[idx_complex]
      return :(_complexvars(Val{$(esc(d))}()))
    else
      return :(_vars(Val{$(esc(d))}()))
    end
  else
    if !isnothing(idx_complex) && kwargvals[idx_complex]
      return :(_complexvars($(esc(d))))
    else
      return :(_vars($(esc(d))))
    end
  end
end

"""
    @params(descriptor [,complex=bool] [, dynamic=bool])

Constructs a vector of `TPS`s corresponding to each of the parameters in the GTPSA `descriptor`

# Keyword Arguments
- `complex` -- If `true`, returns the parameters as `ComplexTPS64`s. Default is `false`.
- `dynamic` -- If `true`, the parameters will use dynamic `Descriptor` resolution. Default is `false`.
"""
macro params(d, kwargs...)
  kwargnames = map(t->t[1], map(t->Pair(t.args...), kwargs))
  kwargvals = map(t->t[2],map(t->Pair(t.args...), kwargs))

  idx_dynamic = findfirst(t->t==:dynamic, kwargnames)
  idx_complex = findfirst(t->t==:complex, kwargnames)
  if isnothing(idx_dynamic) ||!kwargvals[idx_dynamic]
    if !isnothing(idx_complex) && kwargvals[idx_complex]
      return :(_complexparams(Val{$(esc(d))}()))
    else
      return :(_params(Val{$(esc(d))}()))
    end
  else
    if !isnothing(idx_complex) && kwargvals[idx_complex]
      return :(_complexparams($(esc(d))))
    else
      return :(_params($(esc(d))))
    end
  end
end


function _vars(::Val{D}) where {D}
  nv = numvars(D)
  x = Vector{TPS{Float64,D}}(undef, nv)
  for i=1:nv
    t = TPS{Float64,D}()
    @inbounds t[i] = 1.0
    @inbounds x[i] = t
  end
  return x
end

function _vars(d::Descriptor)
  nv = numvars(d)
  x = Vector{TPS{Float64,GTPSA.Dynamic}}(undef, nv)
  for i=1:nv
    t = TPS{Float64,GTPSA.Dynamic}(use=d)
    @inbounds t[i] = 1.0
    @inbounds x[i] = t
  end
  return x
end

function _complexvars(::Val{D}) where {D}
  nv = numvars(D)
  x = Vector{TPS{ComplexF64,D}}(undef, nv)
  for i=1:nv
    t = TPS{ComplexF64,D}()
    @inbounds t[i] = 1.0
    @inbounds x[i] = t
  end
  return x
end

function _complexvars(d::Descriptor)
  nv = numvars(d)
  x = Vector{TPS{ComplexF64,GTPSA.Dynamic}}(undef, nv)
  for i=1:nv
    t = TPS{ComplexF64,GTPSA.Dynamic}(use=d)
    @inbounds t[i] = 1.0
    @inbounds x[i] = t
  end
  return x
end

function _params(::Val{D}) where {D}
  np = numparams(D)
  nv = numvars(D)
  k = Vector{TPS{Float64,D}}(undef, np)
  for i=1:np
    t = TPS{Float64,D}()
    @inbounds t[nv+i] = 1.0
    @inbounds k[i] = t
  end
  return k
end

function _params(d::Descriptor)
  np = numparams(d)
  nv = numvars(d)
  k = Vector{TPS{Float64,GTPSA.Dynamic}}(undef, np)
  for i=1:np
    t = TPS{Float64,GTPSA.Dynamic}(use=d)
    @inbounds t[nv+i] = 1.0
    @inbounds k[i] = t
  end
  return k
end

function _complexparams(::Val{D}) where {D}
  np = numparams(D)
  nv = numvars(D)
  k = Vector{TPS{ComplexF64,D}}(undef, np)
  for i=1:np
    t = TPS{ComplexF64,D}()
    @inbounds t[nv+i] = 1.0
    @inbounds k[i] = t
  end
  return k
end

function _complexparams(d::Descriptor)
  np = numparams(d)
  nv = numvars(d)
  k = Vector{TPS{ComplexF64,GTPSA.Dynamic}}(undef, np)
  for i=1:np
    t = TPS{ComplexF64,GTPSA.Dynamic}(use=d)
    @inbounds t[nv+i] = 1.0
    @inbounds k[i] = t
  end
  return k
end



"""
    mono([tpstype, ] monomialindex [, use=(descriptor|tps)])

Returns a `TPS` of type `tpstype` (which defaults to `TPS{Float64,GTPSA.Dynamic}`) with 
the specified monomial set to 1. Any of the three monomial indexing schemes (by order, sparse 
monomial, or monomial index -- see the Monomial Indexing section of the GTPSA manual for more 
details) may be used to specify the monomial to set to one. E.g. a call to this function is 
equivalent to doing `t = (tpstype)(use=use); t[monomialindex] = 1`

`use` may only be specified if `tpstype <: TPS{T where {T},<:GTPSA.Dynamic}`.

# Examples: Variable/Parameter Index:
```julia-repl
julia> d = Descriptor(3,10,2,10);

julia> mono(1)
TPS64{GTPSA.Dynamic}:
 Coefficient                Order   Exponent
  1.0000000000000000e+00      1      1   0   0   |   0   0


julia> mono(ComplexTPS64, 1)
ComplexTPS64{GTPSA.Dynamic}:
 Real                     Imag                       Order   Exponent
  1.0000000000000000e+00   0.0000000000000000e+00      1      1   0   0   |   0   0


julia> mono(ComplexTPS64{d}, param=2)
ComplexTPS64{Descriptor(NV=3, MO=10, NP=2, PO=10)}:
 Real                     Imag                       Order   Exponent
  1.0000000000000000e+00   0.0000000000000000e+00      1      0   0   0   |   0   1
```

# Examples: Monomial Index-by-Order
```julia-repl
julia> mono([1,2,3])
TPS64{GTPSA.Dynamic}:
 Coefficient                Order   Exponent
  1.0000000000000000e+00      6      1   2   3   |   0   0


julia> mono([0,0,3,2,1], use=d)
TPS64{GTPSA.Dynamic}:
 Coefficient                Order   Exponent
  1.0000000000000000e+00      6      0   0   3   |   2   1


julia> mono(TPS64{d}, [1,0,0,0,1])
TPS64{Descriptor(NV=3, MO=10, NP=2, PO=10)}:
 Coefficient                Order   Exponent
  1.0000000000000000e+00      2      1   0   0   |   0   1
```

# Examples: Monomial Index-by-Sparse Monomial
```julia-repl
julia> mono([1=>1])
TPS64{GTPSA.Dynamic}:
 Coefficient                Order   Exponent
  1.0000000000000000e+00      1      1   0   0   |   0   0


julia> mono([2=>1])
TPS64{GTPSA.Dynamic}:
 Coefficient                Order   Exponent
  1.0000000000000000e+00      1      0   1   0   |   0   0


julia> mono([1=>1], params=[2=>1], use=d)
TPS64{GTPSA.Dynamic}:
 Coefficient                Order   Exponent
  1.0000000000000000e+00      2      1   0   0   |   0   1
```
"""
mono

function mono(
  ::Type{TPS{T,D}}, 
  v::Union{TPSIndexType,Nothing}=nothing; 
  param::Union{Integer,Nothing}=nothing, 
  params::Union{SMIndexType,Nothing}=nothing, 
  use::Union{Descriptor,TPS,Nothing}=nothing
) where {T<:Union{Float64,ComplexF64},D}
  if D != Dynamic
    if !isnothing(use)
      error("`use` kwarg is incompatible with static Descriptor `TPS` constructor.")
    end  
  else
    if isnothing(use)
      use = GTPSA.desc_current
    end
  end
  return _mono(TPS{T,D}, v, param, params, use)
end

# These all use dynamic resolution:
mono(
  ::Type{TPS{T}}, 
  v::Union{TPSIndexType,Nothing}=nothing; 
  param::Union{Integer,Nothing}=nothing, 
  params::Union{SMIndexType,Nothing}=nothing, 
  use::Union{Descriptor,TPS}=GTPSA.desc_current
) where {T<:Union{Float64,ComplexF64}} = mono(TPS{T,Dynamic}, v, param=param, params=params, use=use)

mono(
  v::Union{TPSIndexType,Nothing}=nothing; 
  param::Union{Integer,Nothing}=nothing, 
  params::Union{SMIndexType,Nothing}=nothing, 
  use::Union{Descriptor,TPS}=GTPSA.desc_current
) = mono(TPS{Float64,Dynamic}, v, param=param, params=params, use=use)

# This is deprecated
complexmono(
  v::Union{TPSIndexType,Nothing}=nothing; 
  param::Union{Integer,Nothing}=nothing, 
  params::Union{SMIndexType,Nothing}=nothing, 
  use::Union{Descriptor,TPS}=GTPSA.desc_current
) = (Base.depwarn("`complexmono` is deprecated, use `mono` instead.", :complexmono, force=true);
     mono(TPS{ComplexF64,Dynamic}, v, param=param, params=params, use=use))



# Variable/parameter:
_mono(
  ::Type{TPS{T,D}}, 
  v::Integer,
  param::Nothing,
  params::Nothing,
  use::Union{Descriptor,TPS}
) where {T<:Union{Float64,ComplexF64},D<:Dynamic} = (t = TPS{T,D}(use=use); t[v] = 1; return t)

_mono(
  ::Type{TPS{T,D}}, 
  v::Integer,
  param::Nothing,
  params::Nothing,
  use::Nothing
) where {T<:Union{Float64,ComplexF64},D} = (t = TPS{T,D}(); t[v] = 1; return t)


_mono(
  ::Type{TPS{T,D}}, 
  v::Nothing, 
  param::Integer, 
  params::Nothing,
  use::Union{Descriptor,TPS}
) where {T<:Union{Float64,ComplexF64},D<:Dynamic} = (t = TPS{T,D}(use=use); t[param=param] = 1; return t)

_mono(
  ::Type{TPS{T,D}}, 
  v::Nothing, 
  param::Integer, 
  params::Nothing,
  use::Nothing
) where {T<:Union{Float64,ComplexF64},D} = (t = TPS{T,D}(); t[param=param] = 1; return t)


# Default to empty if nothing passed
_mono(
  ::Type{TPS{T,D}}, 
  v::Nothing, 
  param::Nothing, 
  params::Nothing,
  use::Union{Descriptor,TPS}
) where  {T<:Union{Float64,ComplexF64},D<:Dynamic} = TPS{T,D}(use=use)

_mono(
  ::Type{TPS{T,D}}, 
  v::Nothing, 
  param::Nothing, 
  params::Nothing,
  use::Nothing
) where  {T<:Union{Float64,ComplexF64},D} = TPS{T,D}()


# Monomial by order:
_mono(
  ::Type{TPS{T,D}}, 
  v::MIndexType, 
  param::Nothing, 
  params::Nothing,
  use::Union{Descriptor,TPS}
) where {T<:Union{Float64,ComplexF64},D<:Dynamic} = (t = TPS{T,D}(use=use); t[v] = 1.0; return t)

_mono(
  ::Type{TPS{T,D}}, 
  v::MIndexType, 
  param::Nothing, 
  params::Nothing,
  use::Nothing
) where {T<:Union{Float64,ComplexF64},D} = (t = TPS{T,D}(); t[v] = 1.0; return t)

# Monomial by sparse monomial:
_mono(
  ::Type{TPS{T,D}},
  v::SMIndexType, 
  param::Nothing,
  params::SMIndexType,
  use::Union{Descriptor,TPS}
) where {T<:Union{Float64,ComplexF64},D<:Dynamic} = (t = TPS{T,D}(use=use); t[v, params=params] = 1.0; return t)

_mono(
  ::Type{TPS{T,D}},
  v::SMIndexType, 
  param::Nothing,
  params::SMIndexType,
  use::Nothing
) where {T<:Union{Float64,ComplexF64},D} = (t = TPS{T,D}(); t[v, params=params] = 1.0; return t)

_mono(
  ::Type{TPS{T,D}}, 
  v::SMIndexType, 
  param::Nothing, 
  params::Nothing,
  use::Union{Descriptor,TPS}
) where {T<:Union{Float64,ComplexF64},D<:Dynamic} = (t = TPS{T,D}(use=use); t[v] = 1.0; return t)

_mono(
  ::Type{TPS{T,D}}, 
  v::SMIndexType, 
  param::Nothing, 
  params::Nothing,
  use::Nothing
) where {T<:Union{Float64,ComplexF64},D} = (t = TPS{T,D}(); t[v] = 1.0; return t)


_mono(
  ::Type{TPS{T,D}}, 
  v::Nothing, 
  param::Nothing, 
  params::SMIndexType,
  use::Union{Descriptor,TPS}
) where {T<:Union{Float64,ComplexF64},D<:Dynamic} = (t = TPS{T,D}(use=use); t[params=params] = 1.0; return t)

_mono(
  ::Type{TPS{T,D}}, 
  v::Nothing, 
  param::Nothing, 
  params::SMIndexType,
  use::Nothing
) where {T<:Union{Float64,ComplexF64},D} = (t = TPS{T,D}(); t[params=params] = 1.0; return t)

# Throw error if no above use cases satisfied:
_mono(::Type, v, param, params, use) = error("Invalid monomial specified. Please use ONE of variable/parameter index, index by order, or index by sparse monomial.")
