"""
    `mutable struct NewTPS{T<:Union{Float64,ComplexF64}} <: Number`

Struct representing a truncated power series. The definition here is 1-to-1 
with the GTPSA C library definition.

### Fields
- `d::Ptr{Desc}`             -- Ptr to low-level descriptor struct
- `lo::UInt8`                -- Lowest used order
- `hi::UInt8`                -- Highest used order
- `mo::UInt8`                -- Max order
- `ao::UInt8`                -- Allocated order
- `uid::Cint`                -- Special user field for external use (and padding)
- `nam::NTuple{NAMSZ,UInt8}` -- TPS name, max string length = GTPSA.NAMSZ = 15 chars
- `coef::Ptr{T}`             -- An array containing all of the monomial coefficients up to the TPS max order
"""
mutable struct NewTPS{T<:Union{Float64,ComplexF64}} <: Number
  d::Ptr{Desc}                                            
  lo::UInt8                
  hi::UInt8     
  mo::UInt8  
  ao::UInt8
  uid::Cint            
  nam::NTuple{NAMSZ,UInt8} 
  coef::Ptr{T} # CRITICAL: Flexible array members in C must NOT BE USED! Change [] to * and fix malloc
  
  # Analog to C code mad_tpsa_newd
  function NewTPS{T}(d::Ptr{Desc}, mo::UInt8) where {T}
    d != C_NULL || error("No Descriptor defined!")
    mo = min(mo, unsafe_load(d).mo)
    sz = unsafe_load(unsafe_load(d).ord2idx, mo+2)*sizeof(T)
    coef = @ccall jl_malloc(sz::Csize_t)::Ptr{T}
    ao = mo
    uid = Cint(0)
    nam = (0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
    
    lo = 0x1
    hi = 0x0
    unsafe_store!(coef, T(0))

    t = new{T}(d, lo, hi, mo, ao, uid, nam, coef)

    f(t) = @ccall jl_free(t.coef::Ptr{Cvoid})::Cvoid
    finalizer(f, t)

    return t
  end
end

Base.unsafe_convert(::Type{Ptr{NewTPS{T}}}, t::NewTPS{T}) where {T} = Base.unsafe_convert(Ptr{NewTPS{T}},pointer_from_objref(t))
Base.eltype(::Type{NewTPS{T}}) where {T} = T
Base.eltype(::NewTPS{T}) where {T} = T

getdesc(t::NewTPS) = Descriptor(t.d)
getdesc(d::Descriptor) = d
getdesc(n::Nothing) = GTPSA.desc_current

numvars(t::NewTPS) = unsafe_load(t.d).nv
numvars(d::Descriptor) = unsafe_load(d.desc).nv
numvars(n::Nothing) = unsafe_load(GTPSA.desc_current.desc).nv

numparams(t::NewTPS) = unsafe_load(t.d).np
numparams(d::Descriptor) = unsafe_load(d.desc).np
numparams(n::Nothing) = unsafe_load(GTPSA.desc_current.desc).np

numnn(t::NewTPS) = unsafe_load(t.d).nn
numnn(d::Descriptor) = unsafe_load(d.desc).nn
numnn(n::Nothing) = unsafe_load(GTPSA.desc_current.desc).nn

function NewTPS{T}(;use::Union{Descriptor,NewTPS}=GTPSA.desc_current) where {T<:Number}
    
end

"""
    NewTPS(ta::Union{Number,Nothing}=nothing; use::Union{Descriptor,NewTPS,Nothing}=nothing)
    NewTPS{T}(ta::Union{Number,Nothing}=nothing; use::Union{Descriptor,NewTPS,Nothing}=nothing) where {T<:Union{Float64,ComplexF64}}

Constructor to create a new `TPS` equal to the real value `ta`. If `ta` is a `TPS`, this 
is equivalent to a copy constructor, with the result by default having the same `Descriptor` as `ta`.
If `ta` is not a `TPS`, then the `Descriptor` used will by default be `GTPSA.desc_current`. The `Descriptor` 
for the constructed `TPS` can be set using `use`. If a `TPS` or `ComplexTPS` is passed to `use`, 
the `Descriptor` for that TPS will be used.

The constructor can also be used to create a copy of a `TPS` under one `Descriptor` to instead 
have a different `Descriptor`. In this case, invalid monomials under the new `Descriptor` are removed.

### Input
- `ta`  -- Any `Real`
- `use` -- (Optional) specify which `Descriptor` to use, default is `nothing` which uses the `Descriptor` for `ta` if `ta isa TPS`, else uses `GTPSA.desc_current`

### Output
- `ret` -- New `TPS` equal to `ta`, with removal of invalid monomials if `ta` is a `TPS` and a new `Descriptor` is specified
"""
NewTPS
NewTPS{T}(ta::Union{Number,Nothing}=nothing; use::Union{Descriptor,NewTPS,Nothing}=nothing) where {T<:Union{Float64,ComplexF64}} = low_NewTPS(T, ta,use)
NewTPS{T}(ta::NewTPS;                        use::Union{Descriptor,NewTPS,Nothing}=nothing) where {T<:Union{Float64,ComplexF64}} = low_NewTPS(T, ta,use)

NewTPS(ta::Number;          use::Union{Descriptor,NewTPS,Nothing}=nothing) = NewTPS{promote_type(Float64,typeof(ta))}(ta, use=use)
NewTPS(ta::Nothing=nothing; use::Union{Descriptor,NewTPS,Nothing}=nothing) = NewTPS{Float64}(ta, use=use)
NewTPS(ta::NewTPS;          use::Union{Descriptor,NewTPS,Nothing}=nothing) = NewTPS{eltype(ta)}(ta, use=use)

const ComplexNewTPS = NewTPS{ComplexF64}

function low_NewTPS(T, ta, use)
  if ta isa Nothing          # --- Blank TPS ---
    return NewTPS{T}(getdesc(use).desc, use isa NewTPS ? MAD_TPSA_SAME : MAD_TPSA_DEFAULT)
  elseif ta isa NewTPS
    if use isa Nothing       # --- Copy ctor ---
      t = NewTPS{T}(getdesc(ta).desc, MAD_TPSA_SAME)
      copy!(t, ta)
    else                     # --- Change descriptor ---
      error("not implemented yet")
    end
  else                       # --- promote number ---
    t = NewTPS{T}(getdesc(use).desc, use isa NewTPS ? MAD_TPSA_SAME : MAD_TPSA_DEFAULT)
    t[0] = ta
  end
  return t
end

#=
# --- Change descriptor ---
function low_TPS(t1::TPS, use::Descriptor)
  t = TPS(use=use)
  setTPS!(t,t1,change=true)
  return t
end

function low_TPS(t1::TPS, use::Union{TPS,ComplexTPS})
  t = TPS(use=use)
  setTPS!(t,t1,change=true)
  return t
end
=#

promote_rule(::Type{NewTPS{Float64}}, ::Type{T}) where {T<:Real} = NewTPS{Float64} 
promote_rule(::Type{NewTPS{Float64}}, ::Type{NewTPS{ComplexF64}}) = NewTPS{ComplexF64}
promote_rule(::Type{NewTPS{ComplexF64}}, ::Type{T}) where {T<:Number} = NewTPS{ComplexF64}
promote_rule(::Type{NewTPS{Float64}}, ::Type{T}) where {T<:Number} = NewTPS{ComplexF64}

promote_rule(::Type{T}, ::Type{NewTPS{Float64}}) where {T<:AbstractIrrational} = (T <: Real ? NewTPS{Float64} : NewTPS{ComplexF64})
promote_rule(::Type{T}, ::Type{NewTPS{ComplexF64}}) where {T<:AbstractIrrational} = NewTPS{ComplexF64}

#promote_rule(::Type{NewTPS{Float64}}, ::Type{T}) where {T<:Union{AbstractFloat, Integer, Rational, Irrational}} = NewTPS{Float64}
#promote_rule(::Type{NewTPS{ComplexF64}}, ::Type{T}) where {T<:Union{Complex{<:Real},AbstractFloat,Integer,Rational,Irrational}} = NewTPS{ComplexF64}
#promote_rule(::Type{NewTPS{Float64}}, ::Type{Irrational}) = NewTPS{Float64}