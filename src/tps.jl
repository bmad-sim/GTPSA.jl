#=
    `mutable struct TPS{T<:Union{Float64,ComplexF64}} <: Number`

Struct representing a truncated power series. The definition here is 1-to-1 
with the GTPSA C library definition.

### Fields
- `d::Ptr{Desc}`             -- Ptr to low-level descriptor struct
- `lo::UInt8`                -- Lowest used order
- `hi::UInt8`                -- Highest used order
- `mo::UInt8`                -- Max order
- `ao::UInt8`                -- Allocated order
- `uid::Cint`                -- Special user field for external use (and padding)
- `nam::NTuple{16,UInt8}` -- TPS name, max string length = GTPSA.NAMSZ = 15 chars
- `coef::Ptr{T}`             -- An array containing all of the monomial coefficients up to the TPS max order
=#
mutable struct TPS{T<:Union{Float64,ComplexF64}} <: Number
  d::Ptr{Desc}                                            
  lo::UInt8                
  hi::UInt8     
  mo::UInt8  
  ao::UInt8
  uid::Cint            
  nam::NTuple{16,UInt8}  # NAMSZ = 16
  coef::Ptr{T} # CRITICAL: Flexible array members in C must NOT BE USED! 
               # In the C code: change [] to * and fix malloc
  
  # Analog to C code mad_tpsa_newd
  function TPS{T}(d::Ptr{Desc}, mo::UInt8) where {T}
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

const TPS64 = TPS{Float64}
const ComplexTPS64 = TPS{ComplexF64}

"""
    TPS(ta::Union{Number,Nothing}=nothing; use::Union{Descriptor,TPS,Nothing}=nothing)
    TPS{T}(ta::Union{Number,Nothing}=nothing; use::Union{Descriptor,TPS,Nothing}=nothing) where {T<:Union{Float64,ComplexF64}}

Constructor to create a new `TPS` equal to the real value `ta`. If `ta` is a `TPS`, this 
is equivalent to a copy constructor, with the result by default having the same `Descriptor` as `ta`.
If `ta` is not a `TPS`, then the `Descriptor` used will by default be `GTPSA.desc_current`. The `Descriptor` 
for the constructed `TPS` can be set using `use`. If a `TPS` or `ComplexTPS64` is passed to `use`, 
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
TPS(ta::TPS;          use::Union{Descriptor,TPS,Nothing}=nothing) = TPS{numtype(ta)}(ta, use=use)

function low_TPS(T, ta, use)
  if ta isa Nothing          # --- Blank TPS ---
    return TPS{T}(getdesc(use).desc, use isa TPS ? use.mo : MAD_TPSA_DEFAULT)
  elseif ta isa TPS
    if use isa Nothing       # --- Copy ctor ---
      t = TPS{T}(getdesc(ta).desc, ta.mo)
      copy!(t, ta)
    else                     # --- Change descriptor ---
      t = TPS{T}(getdesc(use).desc, ta.mo)
      setTPS!(t, ta, change=true)
      return t
    end
  else                       # --- promote number ---
    t = TPS{T}(getdesc(use).desc, use isa TPS ? use.mo : MAD_TPSA_DEFAULT)
    t[0] = ta
  end
  return t
end

# To call functions accepting tpsa*, Julia requires using Ref{TPS{T}} (single TPSA pointer).
# For arrays of mutable types, Julia's (inconsistent) syntax is Ptr{TPS{T}},
# while it probably should be Ptr{Ref{TPS{T}}} because of the mutability of TPS.
# This is a problem when trying to do basically Ref{Ref{TPS{T}}} (pointer to pointer 
# of single TPS). So the workaround is to define the following:
Base.unsafe_convert(::Type{Ptr{TPS{T}}}, r::Base.RefValue{Ptr{Nothing}}) where {T} = Base.unsafe_convert(Ptr{TPS{T}}, Base.unsafe_convert(Ptr{Cvoid}, r))
Base.cconvert(::Type{Ptr{TPS{T}}}, t::TPS{T}) where {T} = Ref(pointer_from_objref(t))
# NOTE: We need to have a GC.@preserve before the cconvert to keep t valid !!!!!
# This is only necessary for my mutable type. The other array inputs including isbits 
# types are ok and basically have the above defined for them.
# See https://github.com/JuliaLang/julia/issues/56873#event-15727452235


"""
    numtype(::Union{Type{TPS{T}},TPS{T}}) where T

Returns the type of number that the `TPS` represents.
"""
numtype

numtype(::Type{TPS{T}}) where {T} = T
numtype(::TPS{T}) where {T} = T
numtype(::Type{T}) where {T<:Number} = T
numtype(::T) where {T<:Number} = T

promote_rule(::Type{TPS{Float64}}, ::Type{T}) where {T<:Real} = TPS{Float64} 
promote_rule(::Type{TPS{Float64}}, ::Type{TPS{ComplexF64}}) = TPS{ComplexF64}
promote_rule(::Type{TPS{ComplexF64}}, ::Type{T}) where {T<:Number} = TPS{ComplexF64}
promote_rule(::Type{TPS{Float64}}, ::Type{T}) where {T<:Number} = TPS{ComplexF64}
promote_rule(::Type{T}, ::Type{TPS{Float64}}) where {T<:AbstractIrrational} = (T <: Real ? TPS{Float64} : TPS{ComplexF64})
promote_rule(::Type{T}, ::Type{TPS{ComplexF64}}) where {T<:AbstractIrrational} = TPS{ComplexF64}
promote_rule(::Type{T}, ::Type{TPS{Float64}}) where {T<:Rational} = (T <: Real ? TPS{Float64} : TPS{ComplexF64})
promote_rule(::Type{T}, ::Type{TPS{ComplexF64}}) where {T<:Rational} = TPS{ComplexF64}

complex(::Type{TPS{T}}) where{T} = TPS{complex(T)}
eps(::Type{TPS{T}}) where {T} = eps(T)
floatmin(::Type{TPS{T}}) where {T} = floatmin(T)
floatmax(::Type{TPS{T}}) where {T} = floatmax(T)

Base.broadcastable(o::TPS) = Ref(o)








