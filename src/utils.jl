getdesc(t::TPS{T,Dynamic}) where {T} = Descriptor(t.d)
numvars(t::TPS{T,Dynamic}) where {T} = unsafe_load(t.d).nv
numparams(t::TPS{T,Dynamic}) where {T} = unsafe_load(t.d).np
numnn(t::TPS{T,Dynamic}) where {T} = unsafe_load(t.d).nn

getdesc(t::TempTPS{Float64,Dynamic}) = Descriptor(mad_tpsa_desc(t))
getdesc(t::TempTPS{ComplexF64,Dynamic}) = Descriptor(mad_ctpsa_desc(t))

getdesc(::TPS{T,D}) where {T,D} = D
numvars(::TPS{T,D}) where {T,D} = unsafe_load(D.desc).nv
numparams(::TPS{T,D}) where {T,D} = unsafe_load(D.desc).np
numnn(::TPS{T,D}) where {T,D} = unsafe_load(D.desc).nn

getdesc(::TempTPS{T,D}) where {T,D} = D

getdesc(d::Descriptor) = d
numvars(d::Descriptor) = unsafe_load(d.desc).nv
numparams(d::Descriptor) = unsafe_load(d.desc).np
numnn(d::Descriptor) = unsafe_load(d.desc).nn

getdesc(n::Nothing) = GTPSA.desc_current
numvars(n::Nothing) = unsafe_load(GTPSA.desc_current.desc).nv
numparams(n::Nothing) = unsafe_load(GTPSA.desc_currentt.desc).np
numnn(n::Nothing) = unsafe_load(GTPSA.desc_current.desc).nn

# These are used only for "show":
desctype(::Type{TPS{T,D}}) where {T,D} = D
desctype(::Type{<:TPS{T}}) where {T} = Nothing

_promote_arrays_numtype(t::AbstractArray{T}, ::Type{T}) where {T} = t 
_promote_arrays_numtype(t::AbstractArray{T}, ::Type{U}) where {T,U} = U.(t)
_promote_arrays_numtype(t::AbstractArray{TPS{U,D}}, ::Type{U}) where {U,D} = t
_promote_arrays_numtype(t::AbstractArray{TPS{T,D}}, ::Type{U}) where {U,T,D} = TPS{U,D}.(t)

function promote_arrays_numtype(arrays...)
  return map(t->_promote_arrays_numtype(t, numtype(Base.promote_eltype(arrays...))), arrays)
end

# Function to convert var=>ord, params=(param=>ord,) to low level sparse monomial format (varidx1, ord1, varidx2, ord2, paramidx, ordp1,...)
# vars simply must be some kind of iterable with eltype Pair{<:Integer,<:Integer}, same for param if provided
function pairs_to_sm(t::TPS, vars; params=nothing)::Tuple{Vector{Cint}, Cint}
  eltype(vars) <: Pair{<:Integer,<:Integer} || error("Invalid input for vars!")
  isnothing(params) || eltype(params) <: Pair{<:Integer,<:Integer} || error("Invalid input for params!")
  # WE MUST Order THE VARIABLES !!!
  nv = numvars(t)
  numv = Cint(length(vars))
  if !isnothing(params) 
    nump = Cint(length(params))
    imin = min(minimum(x->x.first, vars,init=typemax(Int)), minimum(x->x.first+nv, params,init=typemax(Int)))
    imax = max(maximum(x->x.first, vars,init=0), maximum(x->x.first+nv, params,init=0))
  else
    nump = 0
    imin = minimum(x->x.first, vars,init=typemax(Int))
    imax = maximum(x->x.first, vars,init=0)
  end
  len = imax-imin+1
  sm = zeros(Cint, 2*len)
  sm[1:2:end] = imin:imax
  for i=1:numv
    sm[2*(vars[i].first-imin+1)] = convert(Cint, vars[i].second)
  end
  for i=1:nump
    sm[2*(params[i].first+nv-imin+1)] = convert(Cint, params[i].second)
  end

  return sm, 2*len
end

# Function to convert var=>ord, params=(param=>ord,) to monomial format (byte array of orders)
function pairs_to_m(t::TPS, vars; params=Pair{Int,Int}[],zero_mono=true)::Tuple{Vector{UInt8}, Cint}
  eltype(vars) <: Pair{<:Integer,<:Integer} || error("Invalid input for vars!")
  isnothing(params) || eltype(params) <: Pair{<:Integer,<:Integer} || error("Invalid input for params!")
  nv = numvars(t)
  n = Cint(0)
  if isempty(params)
    n = Cint(maximum(map(x->x.first, vars)))
  else
    n = Cint(maximum(map(x->x.first, params))) + nv
  end
  if zero_mono
    ords = zeros(Cuchar, n)
  else
    ords = ones(Cuchar, n).*0xff
  end
  for var in vars
    ords[var.first] = convert(Cuchar, var.second)
  end
  for param in params
    ords[nv + param.first] = convert(Cuchar, param.second)
  end
  return ords, n
end
