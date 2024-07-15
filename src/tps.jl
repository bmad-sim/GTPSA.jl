Base.unsafe_convert(::Type{Ptr{TPS{T}}}, t::TPS{T}) where {T} = Base.unsafe_convert(Ptr{TPS{T}},pointer_from_objref(t))
Base.eltype(::Type{TPS{T}}) where {T} = T
Base.eltype(::TPS{T}) where {T} = T

getdesc(t::TPS) = Descriptor(t.d)
getdesc(d::Descriptor) = d
getdesc(n::Nothing) = GTPSA.desc_current

numvars(t::TPS) = unsafe_load(t.d).nv
numvars(d::Descriptor) = unsafe_load(d.desc).nv
numvars(n::Nothing) = unsafe_load(GTPSA.desc_current.desc).nv

numparams(t::TPS) = unsafe_load(t.d).np
numparams(d::Descriptor) = unsafe_load(d.desc).np
numparams(n::Nothing) = unsafe_load(GTPSA.desc_current.desc).np

numnn(t::TPS) = unsafe_load(t.d).nn
numnn(d::Descriptor) = unsafe_load(d.desc).nn
numnn(n::Nothing) = unsafe_load(GTPSA.desc_current.desc).nn

"""
    TPS(ta::Union{Number,Nothing}=nothing; use::Union{Descriptor,TPS,Nothing}=nothing)
    TPS{T}(ta::Union{Number,Nothing}=nothing; use::Union{Descriptor,TPS,Nothing}=nothing) where {T<:Union{Float64,ComplexF64}}

Constructor to create a new `TPS` equal to the real value `ta`. If `ta` is a `TPS`, this 
is equivalent to a copy constructor, with the result by default having the same `Descriptor` as `ta`.
If `ta` is not a `TPS`, then the `Descriptor` used will by default be `GTPSA.desc_current`. The `Descriptor` 
for the constructed `TPS` can be set using `use`. If a `TPS` or `ComplexTPS` is passed to `use`, 
the `Descriptor` for that TPS will be used.

The constructor can also be used to create a copy of a `TPS` under one `Descriptor` to instead 
have a different `Descriptor`. In this case, invalid monomials under the new `Descriptor` are removed.

### Input
- `ta`  -- Any `Number`
- `use` -- (Optional) specify which `Descriptor` to use, default is `nothing` which uses the `Descriptor` for `ta` if `ta isa TPS`, else uses `GTPSA.desc_current`

### Output
- `ret` -- New `TPS` equal to `ta`, with removal of invalid monomials if `ta` is a `TPS` and a new `Descriptor` is specified
"""
TPS
TPS{T}(ta::Union{Number,Nothing}=nothing; use::Union{Descriptor,TPS,Nothing}=nothing) where {T<:Union{Float64,ComplexF64}} = low_TPS(T, ta,use)
TPS{T}(ta::TPS;                        use::Union{Descriptor,TPS,Nothing}=nothing) where {T<:Union{Float64,ComplexF64}} = low_TPS(T, ta,use)

TPS(ta::Number;          use::Union{Descriptor,TPS,Nothing}=nothing) = TPS{promote_type(Float64,typeof(ta))}(ta, use=use)
TPS(ta::Nothing=nothing; use::Union{Descriptor,TPS,Nothing}=nothing) = TPS{Float64}(ta, use=use)
TPS(ta::TPS;          use::Union{Descriptor,TPS,Nothing}=nothing) = TPS{eltype(ta)}(ta, use=use)

const ComplexTPS = TPS{ComplexF64}

function low_TPS(T, ta, use)
  if ta isa Nothing          # --- Blank TPS ---
    return TPS{T}(getdesc(use).desc, use isa TPS ? use.mo : MAD_TPSA_DEFAULT)
  elseif ta isa TPS
    if use isa Nothing       # --- Copy ctor ---
      t = TPS{T}(getdesc(ta).desc, ta.mo)
      copy!(t, ta)
    else                     # --- Change descriptor ---
      t = TPS{T}(getdesc(use).desc, ta.mo)
      setTPS!(t, t1, change=true)
      return t
    end
  else                       # --- promote number ---
    t = TPS{T}(getdesc(use).desc, use isa TPS ? use.mo : MAD_TPSA_DEFAULT)
    t[0] = ta
  end
  return t
end


promote_rule(::Type{TPS{Float64}}, ::Type{T}) where {T<:Real} = TPS{Float64} 
promote_rule(::Type{TPS{Float64}}, ::Type{TPS{ComplexF64}}) = TPS{ComplexF64}
promote_rule(::Type{TPS{ComplexF64}}, ::Type{T}) where {T<:Number} = TPS{ComplexF64}
promote_rule(::Type{TPS{Float64}}, ::Type{T}) where {T<:Number} = TPS{ComplexF64}

promote_rule(::Type{T}, ::Type{TPS{Float64}}) where {T<:AbstractIrrational} = (T <: Real ? TPS{Float64} : TPS{ComplexF64})
promote_rule(::Type{T}, ::Type{TPS{ComplexF64}}) where {T<:AbstractIrrational} = TPS{ComplexF64}

promote_rule(::Type{T}, ::Type{TPS{Float64}}) where {T<:Rational} = (T <: Real ? TPS{Float64} : TPS{ComplexF64})
promote_rule(::Type{T}, ::Type{TPS{ComplexF64}}) where {T<:Rational} = TPS{ComplexF64}

complex(::Type{TPS}) = TPS{ComplexF64}
complex(::Type{TPS{T}}) where{T} = TPS{ComplexF64}

eps(::Type{TPS{T}}) where {T} = eps(T)
floatmin(::Type{TPS{T}}) where {T} = floatmin(T)
floatmax(::Type{TPS{T}}) where {T} = floatmax(T)

#promote_rule(::Type{TPS{Float64}}, ::Type{T}) where {T<:Union{AbstractFloat, Integer, Rational, Irrational}} = TPS{Float64}
#promote_rule(::Type{TPS{ComplexF64}}, ::Type{T}) where {T<:Union{Complex{<:Real},AbstractFloat,Integer,Rational,Irrational}} = TPS{ComplexF64}
#promote_rule(::Type{TPS{Float64}}, ::Type{Irrational}) = TPS{Float64}