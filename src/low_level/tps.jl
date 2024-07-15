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
- `nam::NTuple{NAMSZ,UInt8}` -- TPS name, max string length = GTPSA.NAMSZ = 15 chars
- `coef::Ptr{T}`             -- An array containing all of the monomial coefficients up to the TPS max order
=#
mutable struct TPS{T<:Union{Float64,ComplexF64}} <: Number
  d::Ptr{Desc}                                            
  lo::UInt8                
  hi::UInt8     
  mo::UInt8  
  ao::UInt8
  uid::Cint            
  nam::NTuple{NAMSZ,UInt8} 
  coef::Ptr{T} # CRITICAL: Flexible array members in C must NOT BE USED! Change [] to * and fix malloc
  
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