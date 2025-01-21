struct Dynamic end

#=
    `mutable struct TPS{T<:Union{Float64,ComplexF64}, D} <: Number`

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
mutable struct TPS{T, D} <: Number
  d::Ptr{Desc}                                            
  lo::UInt8                
  hi::UInt8     
  mo::UInt8  
  ao::UInt8
  uid::Cint            
  nam::NTuple{16,UInt8}  # NAMSZ = 16
  coef::Ptr{T} # CRITICAL: Flexible array members in C must NOT BE USED! 
               # In the C code: change [] to * and fix malloc
  
  function TPS{T,D}(; use::Union{Descriptor,TPS,Nothing}=nothing, _mo::UInt8=MAD_TPSA_DEFAULT) where {T<:Union{Float64,ComplexF64},D}
    if D != Dynamic
      D isa Descriptor || error("Type parameter D must be a Descriptor or GTPSA.Dynamic!")
      isnothing(use) || error("`use` kwarg is incompatible with static Descriptor `TPS` constructor.")
      d = D.desc
    else
      d = getdesc(use).desc
    end
    d != C_NULL || error("No Descriptor defined!")
    mo = min(_mo, unsafe_load(d).mo)
    sz = unsafe_load(unsafe_load(d).ord2idx, mo+2)*sizeof(T)
    coef = Base.unsafe_convert(Ptr{T}, @ccall jl_malloc(sz::Csize_t)::Ptr{Cvoid})
    ao = mo
    uid = Cint(0)
    nam = (0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
    lo = 0x1
    hi = 0x0
    unsafe_store!(coef, T(0))
    t = new{T,D}(d, lo, hi, mo, ao, uid, nam, coef)
    f(t) = @ccall jl_free(t.coef::Ptr{Cvoid})::Cvoid
    finalizer(f, t)
    return t
  end
end

const TPS64 = TPS{Float64}
const ComplexTPS64 = TPS{ComplexF64}

# Other empty ctors:
TPS{TD}(;
  use::Union{Descriptor,TPS,Nothing}=nothing,
  _mo::UInt8=MAD_TPSA_DEFAULT
) where {TD} = TD isa Descriptor ? TPS{Float64,TD}(; use=use, _mo=_mo) :
                                   TPS{TD,Dynamic}(; use=use, _mo=_mo)  
                              

TPS(;
  use::Union{Descriptor,TPS,Nothing}=nothing,
  _mo::UInt8=MAD_TPSA_DEFAULT
) = TPS{Float64,Dynamic}(; use=use, _mo=_mo)

# Now ctors including regular numbers:
function TPS{T,D}(
  ta::Number; 
  use::Union{Descriptor,TPS,Nothing}=nothing,
  _mo::UInt8=ta isa TPS ? ta.mo : MAD_TPSA_DEFAULT
) where {T<:Union{Float64,ComplexF64},D}
  t = TPS{T,D}(; use=use, _mo=_mo)
  t[0] = ta
  return t
end
 
TPS{TD}(
  ta::Number; 
  use::Union{Descriptor,TPS,Nothing}=nothing,
  _mo::UInt8=MAD_TPSA_DEFAULT
) where {TD} = TD isa Descriptor ? TPS{promote_type(typeof(ta),Float64),TD}(ta; use=use, _mo=_mo) : 
                                   TPS{TD,Dynamic}(ta; use=use, _mo=_mo)                              

TPS(
  ta::Number; 
  use::Union{Descriptor,TPS,Nothing}=nothing,
  _mo::UInt8=MAD_TPSA_DEFAULT
) = TPS{promote_type(typeof(ta),Float64),Dynamic}(ta; use=use, _mo=_mo)
                     
# Now ctors including TPSs:
function TPS{T,D}(
  ta::TPS{TA,DA}; 
  use::Union{Descriptor,TPS,Nothing}=nothing,
  _mo::UInt8=ta isa TPS ? ta.mo : MAD_TPSA_DEFAULT
) where {T<:Union{Float64,ComplexF64},TA<:Union{Float64,ComplexF64},D,DA}
  t = TPS{T,D}(; use=use, _mo=_mo);
  if getdesc(t) == getdesc(ta)
    copy!(t, ta)
  else
    setTPS!(t, ta, change=true)
  end
  return t
end

TPS{TD}(
  ta::TPS{TA,DA}; 
  use::Union{Descriptor,TPS,Nothing}=nothing,
  _mo::UInt8=MAD_TPSA_DEFAULT
) where {TD,TA<:Union{Float64,ComplexF64},DA} = TD isa Descriptor ? TPS{TA,TD}(ta; use=use, _mo=_mo) : 
                                                                    TPS{TD,DA}(ta; use=use, _mo=_mo)
                                                               
TPS(
  ta::TPS{TA,DA}; 
  use::Union{Descriptor,TPS,Nothing}=nothing,
  _mo::UInt8=MAD_TPSA_DEFAULT
) where {TA<:Union{Float64,ComplexF64},DA} = TPS{TA,DA}(ta; use=use, _mo=_mo)



"""
    TPS{D}([number]) where {D}
    TPS{T,D}([number]) where {T<:Union{Float64,ComplexF64}}

    TPS([number] [, use=(descriptor|tps)])
    TPS{T}([number] [, use=(descriptor|tps)]) where {T<:Union{Float64,ComplexF64}}
    TPS{T,GTPSA.Dynamic}([number] [, use=(descriptor|tps)]) where {T<:Union{Float64,ComplexF64}}


    
Constructor to create a new `TPS`, equal to `number` if provided. 

A constructed `TPS` must correspond to some previously defined `Descriptor`. As such, two "modes" of the `TPS` type may be 
constructed to determine how the `Descriptor` to use is resolved:

1. Dynamic `Descriptor` resolution -- The `Descriptor` is inferred at runtime, based on the passed arguments:

| Ctor Call                                | Descriptor                                                      |
| :-------------------                     | :--------------------------------------------                   |
| `TPS()`                                  | `GTPSA.desc_current`                                            |
| `TPS(use=descriptor)`                    | `descriptor`                                                    |
| `TPS(use=tps1)`                          | That of `tps1`                                                  |
| `TPS(tps)`                               | That of `tps`                                                   |
| `TPS(number)`                            | `GTPSA.desc_current`                                            |
| `TPS(number, use=(descriptor or tps1) )` | `descriptor` or that of `tps1`                                  |
| `TPS(tps, use=(descriptor or tps1) )`    | `descriptor` or that of `tps1` (copies + changes `Descriptor!`) |

The same applies for all of the above constructor calls with the constructors `TPS64(...)` and `ComplexTPS64(...)`.
The created type will be a `TPS{T,GTPSA.Dynamic} where {T<:Union{Float64,ComplexF64}}`. Dynamic `Descriptor` resolution 
will always be type stable, as the `Descriptor` for each `TPS` is not stored in the type definition. E.g., one can have 
an array with elements of the concrete type `TPS{T,GTPSA.Dynamic}` even though the individual `TPS`s in the array may 
have differing `Descriptor`s. Another example is typing the field of a struct as `TPS{T,GTPSA.Dynamic}`, so that field 
can contain `TPS`s of different `Descriptor`s in a type-stable fashion. However, with dynamic `Descriptor` resolution 
the `use` kwarg must be specified if the `Descriptor` is both not inferrable nor `GTPSA.desc_current` is the desired 
`Descriptor`. For calls such as `zeros(TPS64, N)` or `TPS64(5.0)`, only `GTPSA.desc_current` can be inferred.

2. Static `Descriptor` resolution -- The `Descriptor` is stored explicitly in the `TPS` type:

| Ctor Call                                | Descriptor                                                     |
| :-------------------                     | :--------------------------------------------                  |
| `TPS{descriptor}([number])`              | `descriptor`                                                   |
| `TPS{descriptor2}(::TPS{T,descriptor1})` | `descriptor2` (copies + changes `Descriptor!`)                 |  
| `TPS(::TPS{T,descriptor})`               | `descriptor`                                                   |

The same applies for all of the above constructor calls with the constructors `TPS64{...}(...)` and `ComplexTPS64{...}(...)`.
The created type will be a `TPS{T,descriptor} where {T<:Union{Float64,ComplexF64}}`. Care must be taken with static 
`Descriptor` resolution to ensure type-stability, and in some cases it may not be possible. However, static resolution has 
the benefit that the `Descriptor` is stored explicitly in the type. As such, `GTPSA.desc_current` is never used with this mode, 
nor is the `use` kwarg. Calls such as `zeros(TPS64{descriptor}, N)` can be made ensuring the `Descriptor` of the output is correct.
"""
TPS


# We do NOT want to specialize the low-level routines for every Descriptor.
# And, it would be nice to just keep every ::RealTPS instead of ::RealTPS{D} ... where {D}
# However, cconvert/unsafe_convert need concrete types or to convert to or 
# else there will be allocations
# So here we will override the conversion:
Base.cconvert(::Type{Ref{TPS{T}}}, t::TPS{T,D}) where {T,D} = Base.cconvert(Ref{TPS{T,D}}, t)
Base.unsafe_convert(::Type{Ptr{TPS{T}}}, r::Base.RefValue{TPS{T,D}}) where {T,D} = Base.unsafe_convert(Ptr{TPS{T,D}}, r)

# And for array types:
#Base.cconvert(::Type{Ptr{TPS{T}}}, t::AbstractArray{TPS{T,D}}) where {T,D} = Base.cconvert(Ptr{TPS{T,D}}, t)
#Base.cconvert(::Type{Ptr{TPS{T}}}, t::Array{TPS{T,D}}) where {T,D} = Base.cconvert(Ptr{TPS{T,D}}, t)
@static if VERSION >= v"1.11.0"
Base.unsafe_convert(::Type{Ptr{TPS{T}}}, mr::Base.MemoryRef{TPS{T,D}}) where {T,D} = Base.unsafe_convert(Ptr{TPS{T}}, Base.unsafe_convert(Ptr{TPS{T,D}}, mr))
end
# To call functions accepting tpsa*, Julia requires using Ref{TPS{T}} (single TPSA pointer).
# For arrays of mutable types, Julia's (inconsistent) syntax is Ptr{TPS{T}},
# while it probably should be Ptr{Ref{TPS{T}}} because of the mutability of TPS.
# This is a problem when trying to do basically Ref{Ref{TPS{T}}} (pointer to pointer 
# of single TPS). So the workaround is to define the following:
Base.unsafe_convert(::Type{Ptr{TPS{T}}}, r::Base.RefValue{Ptr{TPS{T,D}}}) where {T,D} = Base.unsafe_convert(Ptr{TPS{T}}, Base.unsafe_convert(Ptr{TPS{T,D}}, Base.unsafe_convert(Ptr{Cvoid}, r)))
Base.cconvert(::Type{Ptr{TPS{T}}}, t::TPS{T,D}) where {T,D} = Ref(Base.unsafe_convert(Ptr{TPS{T,D}}, pointer_from_objref(t)))
# NOTE: We need to have a GC.@preserve before the cconvert to keep t valid !!!!!
# This is only necessary for my mutable type. The other array inputs including isbits 
# types are ok and basically have the above defined for them.
# See https://github.com/JuliaLang/julia/issues/56873#event-15727452235



"""
    numtype(t::Number)
    numtype(::Type{T}) where {T<:Number}

If a `TPS`, then returns the type of number which the `TPS` 
represents. Else, returns that number type.
"""
numtype

numtype(::Type{<:TPS{T}}) where {T} = T
numtype(::TPS{T}) where {T} = T
numtype(::Type{T}) where {T<:Number} = T
numtype(::T) where {T<:Number} = T

promote_rule(::Type{TPS{Float64,D}}, ::Type{T}) where {T<:Real,D} = TPS{Float64,D} 

promote_rule(::Type{TPS{Float64,D}}, ::Type{TPS{Float64,D}}) where {D} = TPS{Float64,D}
promote_rule(::Type{TPS{Float64,D}}, ::Type{TPS{ComplexF64,D}}) where {D} = TPS{ComplexF64,D}
promote_rule(::Type{TPS{ComplexF64,D}}, ::Type{TPS{Float64,D}}) where {D} = TPS{ComplexF64,D}
promote_rule(::Type{TPS{ComplexF64,D}}, ::Type{TPS{ComplexF64,D}}) where {D} = TPS{ComplexF64,D}

promote_rule(::Type{TPS{Float64,D1}}, ::Type{TPS{Float64,D2}}) where {D1,D2} = error("Cannot promote `TPS`s different descriptor resolution modes (maybe you tried an out of place operation with one `TPS` dynamic and the other static descriptor resolution?)")
promote_rule(::Type{TPS{ComplexF64,D1}}, ::Type{TPS{Float64,D2}}) where {D1,D2} = error("Cannot promote `TPS`s different descriptor resolution modes (maybe you tried an out of place operation with one `TPS` dynamic and the other static descriptor resolution?)")
promote_rule(::Type{TPS{Float64,D1}}, ::Type{TPS{ComplexF64,D2}}) where {D1,D2} = error("Cannot promote `TPS`s different descriptor resolution modes (maybe you tried an out of place operation with one `TPS` dynamic and the other static descriptor resolution?)")
promote_rule(::Type{TPS{ComplexF64,D1}}, ::Type{TPS{ComplexF64,D2}}) where {D1,D2} = error("Cannot promote `TPS`s different descriptor resolution modes (maybe you tried an out of place operation with one `TPS` dynamic and the other static descriptor resolution?)")

promote_rule(::Type{TPS{ComplexF64,D}}, ::Type{T}) where {D,T<:Number} = TPS{ComplexF64,D}
promote_rule(::Type{TPS{Float64,D}}, ::Type{T}) where {D,T<:Number} = TPS{ComplexF64,D}
promote_rule(::Type{T}, ::Type{TPS{Float64,D}}) where {T<:AbstractIrrational,D} = (T <: Real ? TPS{Float64,D} : TPS{ComplexF64,D})
promote_rule(::Type{T}, ::Type{TPS{ComplexF64,D}}) where {T<:AbstractIrrational,D} = TPS{ComplexF64,D}
promote_rule(::Type{T}, ::Type{TPS{Float64,D}}) where {T<:Rational,D} = (T <: Real ? TPS{Float64,D} : TPS{ComplexF64,D})
promote_rule(::Type{T}, ::Type{TPS{ComplexF64,D}}) where {T<:Rational,D} = TPS{ComplexF64,D}

promote_rule(::Type{TPS{T}}, ::Type{G}) where {T<:Union{Float64,ComplexF64},G} = promote_rule(TPS{T,GTPSA.Dynamic}, G)
promote_rule(::Type{G}, ::Type{TPS{T}}) where {T<:Union{Float64,ComplexF64},G<:Rational} = promote_rule(G, TPS{T,GTPSA.Dynamic})
promote_rule(::Type{G}, ::Type{TPS{T}}) where {T<:Union{Float64,ComplexF64},G<:AbstractIrrational} = promote_rule(G, TPS{T,GTPSA.Dynamic})


complex(::Type{TPS{T,D}}) where {T,D} = TPS{complex(T),D}
eps(::Type{TPS{T,D}}) where {T,D} = eps(T)
floatmin(::Type{TPS{T,D}}) where {T,D} = floatmin(T)
floatmax(::Type{TPS{T,D}}) where {T,D} = floatmax(T)

Base.broadcastable(o::TPS) = Ref(o)








