numtype(t::TPS) = Float64
numtype(ct::ComplexTPS) = ComplexF64
numtype(::Type{TPS}) = Float64
numtype(::Type{ComplexTPS}) = ComplexF64

lowtype(t::TPS) = Ptr{RTPSA}
lowtype(ct::ComplexTPS) = Ptr{CTPSA}
lowtype(::Type{TPS}) = Ptr{RTPSA}
lowtype(::Type{ComplexTPS}) = Ptr{CTPSA}

getdesc(t::Union{TPS,ComplexTPS}) = Descriptor(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d))
numvars(t::Union{TPS,ComplexTPS}) = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d)).nv
numparams(t::Union{TPS,ComplexTPS}) = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d)).np
numnn(t::Union{TPS,ComplexTPS}) = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d)).nn
getdesc(d::Descriptor) = d
numvars(d::Descriptor) = unsafe_load(d.desc).nv
numparams(d::Descriptor) = unsafe_load(d.desc).np
numnn(d::Descriptor) = unsafe_load(d.desc).nn
getdesc(d::Nothing) = nothing



const SMIndexType = Union{Vector{<:Pair{<:Integer,<:Integer}}, Tuple{Vararg{Pair{<:Integer,<:Integer}}}}
const MIndexType = Union{Vector{<:Integer}, Tuple{Vararg{Integer}}}
const TPSIndexType = Union{Integer,
                           MIndexType,
                           SMIndexType}


cycle!(t::Ptr{RTPSA}, i::Cint, n::Cint, m_::Vector{Cuchar}, v_::Ref{Cdouble}) = (@inline; mad_tpsa_cycle!(t, i, n, m_, v_))
cycle!(t::Ptr{CTPSA}, i::Cint, n::Cint, m_::Vector{Cuchar}, v_::Ref{ComplexF64}) = (@inline; mad_ctpsa_cycle!(t, i, n, m_, v_))

# Generic function to make new copy of TPS with different descriptor
function change(t1::Union{TPS,ComplexTPS}, newd::Descriptor; type::Type=typeof(t1), scl2::Number=1)
  # Quick check if actually changing
  if Base.unsafe_convert(Ptr{Desc}, unsafe_load(t1.tpsa).d) == newd.desc
    return type(t1)
  end

  # THE NUMBER OF VARIABLES and PARAMETERS MUST AGREE!!!
  unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t1.tpsa).d)).nv == unsafe_load(newd.desc).nv || error("Number of variables in GTPSAs do not agree!")
  unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t1.tpsa).d)).np == unsafe_load(newd.desc).np || error("Number of parameters in GTPSAs do not agree!")

  t = type(use=newd)
  change!(t, t1, 0, scl2)
  return t
end

function change!(t::Union{TPS,ComplexTPS},t1::Union{TPS,ComplexTPS}, scl1::Number=0, scl2::Number=1)
  desc =  unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t1.tpsa).d))
  nv = desc.nv
  np = desc.np
  coef = Ref{numtype(t1)}()
  mono = Vector{Cuchar}(undef, np+nv)
  idx = cycle!(t1.tpsa, Cint(-1), np+nv, mono, coef)
  while idx >= 0
    # if valid monomial in new descriptor:
    if convert(Bool, mad_desc_isvalidm(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d), np+nv, mono))
      setm!(t.tpsa, np+nv, mono, convert(numtype(t), scl1), convert(numtype(t), scl2*coef[])) # set new tpsa
    end
    idx = cycle!(t1.tpsa, idx, np+nv, mono, coef)
  end
end

# Function to convert var=>ord, params=(param=>ord,) to low level sparse monomial format (varidx1, ord1, varidx2, ord2, paramidx, ordp1,...)
function pairs_to_sm(t::Union{TPS,ComplexTPS}, vars::Union{Vector{<:Pair{<:Integer, <:Integer}},Tuple{Vararg{Pair{<:Integer,<:Integer}}}}; params::Union{Vector{<:Pair{<:Integer,<:Integer}},Tuple{Vararg{<:Pair{<:Integer,<:Integer}}},Nothing}=nothing)::Tuple{Vector{Cint}, Cint}
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
function pairs_to_m(t::Union{TPS,ComplexTPS}, vars::Union{Vector{<:Pair{<:Integer, <:Integer}},Tuple{Vararg{Pair{<:Integer,<:Integer}}}}; params::Union{Vector{<:Pair{<:Integer, <:Integer}},Tuple{Vararg{Pair{<:Integer,<:Integer}}}}=Pair{Int,Int}[],zero_mono=true)::Tuple{Vector{UInt8}, Cint}
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

# Prevent undefined behavior
# Until AbstractComplex is implemented, I make the ctor return error because this should never happen 
# asumming I wrapped enough
#=
Complex(t1::TPS) = complex(t1) 
Complex(t1::TPS, t2::TPS) = complex(t1, t2)
Complex(t1::TPS, a::Real) = complex(t1, a)
Complex(a::Real, t1::TPS) = complex(a, t1)
Complex{TPS}(t1::TPS) = complex(t1) 
Complex{TPS}(t1::TPS, t2::TPS) = complex(t1, t2)
Complex{TPS}(t1::TPS, a::Real) = complex(t1, a)
Complex{TPS}(a::Real, t1::TPS) = complex(a, t1)=#
Complex(t1::TPS) = error("ComplexTPS can only be defined as an AbstractComplex type (to be implemented in Julia PR #35587). If this error was reached without explicitly attempting to create a Complex{TPS}, please submit an issue to GTPSA.jl with an example.")
Complex(t1::TPS, t2::TPS) = error("ComplexTPS can only be defined as an AbstractComplex type (to be implemented in Julia PR #35587). If this error was reached without explicitly attempting to create a Complex{TPS}, please submit an issue to GTPSA.jl with an example.")
Complex(t1::TPS, a::Real) = error("ComplexTPS can only be defined as an AbstractComplex type (to be implemented in Julia PR #35587). If this error was reached without explicitly attempting to create a Complex{TPS}, please submit an issue to GTPSA.jl with an example.")
Complex(a::Real, t1::TPS) = error("ComplexTPS can only be defined as an AbstractComplex type (to be implemented in Julia PR #35587). If this error was reached without explicitly attempting to create a Complex{TPS}, please submit an issue to GTPSA.jl with an example.")
Complex{TPS}(t1::TPS) = error("ComplexTPS can only be defined as an AbstractComplex type (to be implemented in Julia PR #35587). If this error was reached without explicitly attempting to create a Complex{TPS}, please submit an issue to GTPSA.jl with an example.")
Complex{TPS}(t1::TPS, t2::TPS) = error("ComplexTPS can only be defined as an AbstractComplex type (to be implemented in Julia PR #35587). If this error was reached without explicitly attempting to create a Complex{TPS}, please submit an issue to GTPSA.jl with an example.")
Complex{TPS}(t1::TPS, a::Real) = error("ComplexTPS can only be defined as an AbstractComplex type (to be implemented in Julia PR #35587). If this error was reached without explicitly attempting to create a Complex{TPS}, please submit an issue to GTPSA.jl with an example.")
Complex{TPS}(a::Real, t1::TPS) = error("ComplexTPS can only be defined as an AbstractComplex type (to be implemented in Julia PR #35587). If this error was reached without explicitly attempting to create a Complex{TPS}, please submit an issue to GTPSA.jl with an example.")

promote_rule(::Type{TPS}, ::Type{T}) where {T<:Real} = TPS #::Union{Type{<:AbstractFloat}, Type{<:Integer}, Type{<:Rational}, Type{<:AbstractIrrational}}) = TPS
promote_rule(::Type{ComplexTPS}, ::Type{T}) where {T<:Number} = ComplexTPS #::Union{Type{Complex{<:Real}},Type{<:AbstractFloat}, Type{<:Integer}, Type{<:Rational}, Type{<:AbstractIrrational}}) = ComplexTPS
promote_rule(::Type{TPS}, ::Type{T}) where {T<:Number}= ComplexTPS

# Handle bool which is special for some reason
+(t::TPS, z::Complex{Bool}) = t + Complex{Int}(z)
+(z::Complex{Bool}, t::TPS) = Complex{Int}(z) + t
-(t::TPS, z::Complex{Bool}) = t - Complex{Int}(z)
-(z::Complex{Bool}, t::TPS) = Complex{Int}(z) - t
*(t::TPS, z::Complex{Bool}) = t * Complex{Int}(z)
*(z::Complex{Bool}, t::TPS) = Complex{Int}(z) * t
/(t::TPS, z::Complex{Bool}) = t / Complex{Int}(z)
/(z::Complex{Bool}, t::TPS) = Complex{Int}(z) / t
^(t::TPS, z::Complex{Bool}) = t ^ Complex{Int}(z)
^(z::Complex{Bool}, t::TPS) = Complex{Int}(z) ^ t