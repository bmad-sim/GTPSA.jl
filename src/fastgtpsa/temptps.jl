"""
    struct TempTPS{T<:Union{Float64,ComplexF64},D}

This is for internal use only. `TempTPS` is a temporary `TPS`, which has 
been pre-allocated in a buffer for each thread in the `Descriptor` C struct. 
When using the `@FastGTPSA` macro, all temporaries generated will be used 
from this buffer. "Constructors" of this type simply take a temporary from 
that particular thread's buffer in a stack-like manner and "Destructors" 
(which must be manually called because this is immutable) release it from 
the stack.

### Fields
- `t::Ptr{TPS{T,D}}` -- Pointer to the `TPS` in the buffer in the `Descriptor`
"""
struct TempTPS{T<:Union{Float64,ComplexF64},D}
  t::Ptr{TPS{T,D}}

  function TempTPS{T,D}(in::Union{TPS{<:Number,D},TempTPS{<:Number,D}}) where {T<:Union{Float64,ComplexF64},D}
    d = getdesc(in)
    desc = unsafe_load(d.desc)
    ti = T == Float64 ? desc.ti : desc.cti
    buf = T == Float64 ? desc.t : desc.ct
    tmpidx = unsafe_load(ti, Threads.threadid())

    if tmpidx == DESC_MAX_TMP
      # Run out of temporaries... no choice but to throw error 
      # Release this thread's temporaries and give warning to run cleartemps!()
      unsafe_store!(desc.ti, Cint(0), Threads.threadid())
      unsafe_store!(desc.cti, Cint(0), Threads.threadid())
      try
        error("Permanent temporaries buffer out of memory (max $DESC_MAX_TMP). To use @FastGTPSA, please split expression into subexpressions.")
      finally
        GTPSA.cleartemps!(d)
      end
    end
    idx = (Threads.threadid()-1)*DESC_MAX_TMP+tmpidx+1 # Julia is one based with unsafe_load! First this is 0

    t = unsafe_load(Base.unsafe_convert(Ptr{Ptr{TPS{T,D}}}, buf), idx)

    #println("threadid = ", Threads.threadid(), ", getting temp t[", idx-1,"], incrementing ti[", Threads.threadid()-1, "] = ", tmpidx, "->", tmpidx+1)
    unsafe_store!(ti, tmpidx+Cint(1), Threads.threadid())
    return new{T,D}(t)
  end

  TempTPS{T}(in::Union{TPS{<:Number,D},TempTPS{<:Number,D}}) where {T<:Union{Float64,ComplexF64},D} = TempTPS{T,D}(in)
end

# --- "destructors" ---
# These release a temporary from the stack
function rel_temp!(t::TempTPS{T,D}) where {T,D}
  desc = unsafe_load(getdesc(t).desc)
  ti = T == Float64 ? desc.ti : desc.cti
  buf = T == Float64 ? desc.t : desc.ct

  tmpidx = unsafe_load(ti, Threads.threadid())
  #println("decrementing ti[", Threads.threadid()-1, "] = ", tmpidx, "->", tmpidx-1)
  # Make sure we release this actual temporary
  @assert unsafe_load(Base.unsafe_convert(Ptr{Ptr{TPS{T,D}}}, buf), (Threads.threadid()-1)*DESC_MAX_TMP+tmpidx) == t.t "This should not have been reached! Please submit an issue to GTPSA.jl with a minimal working example"
  
  unsafe_store!(ti, tmpidx-Cint(1), Threads.threadid())
  return
end

# --- temporary sanity checks/cleaners ---
# Clears all temporaries:
"""
    GTPSA.cleartemps!(d::Descriptor=GTPSA.desc_current)

Clears the "stack" of temporaries currently in use by the `Descriptor`. This is 
necessary to run if `GTPSA.checktemps(d::Descriptor=GTPSA.desc_current)` returns 
`false`; this occurs if an error is thrown during evaluation of an expression 
using `@FastGTPSA` or `@FastGTPSA!`, and the Julia session is not terminated.
"""
function cleartemps!(d::Descriptor=GTPSA.desc_current)
  if d.desc == C_NULL
    return
  end
  desc = unsafe_load(d.desc)
  for i = 1:Threads.nthreads(:default)
    unsafe_store!(desc.ti, Cint(0), i)
    unsafe_store!(desc.cti, Cint(0), i)
  end
  return
end

"""
    GTPSA.checktemps(d::Descriptor=GTPSA.desc_current)

Sanity check of the temporary buffer in the `Descriptor` used by @FastGTPSA. 
Returns `true` if everything is OK, else `false` in which case 
`GTPSA.cleartemps!(d::Descriptor=GTPSA.desc_current)` should be run. This may 
occur if an error is thrown during evaluation of an expression using `@FastGTPSA` 
or `@FastGTPSA!`.
"""
function checktemps(d::Descriptor=GTPSA.desc_current)
  if d.desc == C_NULL
    return false
  end
  desc = unsafe_load(d.desc)
  for i=1:desc.nth
    unsafe_load(desc.ti, i) == 0 || return false
    unsafe_load(desc.cti, i) == 0 || return false
  end
  return true
end

# --- overloads for broadcasting compatibility ---
Base.broadcastable(o::TempTPS) = Ref(o)
# NOTE: for some reason, merely overloading this function causes allocations 
# in Julia 1.9. This is not the case in 1.10, so presumably this is a bug.
# Therefore, allocation tests only are performed on >=1.10
function Base.setindex!(A::(Array{<:TPS{T}} where {T<:Union{Float64, ComplexF64}}), x::TempTPS, i1::Int)
  copy!(A[i1], x)
  rel_temp!(x)
end
# These operators are *only* called when doing .+= or .-= etc
# because temporaries should otherwise never see +, -, .... Broadcasting 
# calls these guys internally.
+(t::TPS, t1::TempTPS) = (add!(t, t, t1); rel_temp!(t1); return t)
-(t::TPS, t1::TempTPS) = (sub!(t, t, t1); rel_temp!(t1); return t)
*(t::TPS, t1::TempTPS) = (mul!(t, t, t1); rel_temp!(t1); return t)
/(t::TPS, t1::TempTPS) = (div!(t, t, t1); rel_temp!(t1); return t)
^(t::TPS, t1::TempTPS) = (pow!(t, t, t1); rel_temp!(t1); return t)

# Note that this Ptr is owned by C and so is safe from GC
#Base.convert(::Type{Ref{TPS{T,D}}}, t::TempTPS{T,D}) where {T,D} = t.t # convert to Ptr instead of Ref
Base.cconvert(::Type{Ref{TPS{T}}}, t::TempTPS{T,D}) where {T,D} = t.t #Base.cconvert(Ref{TPS{T,D}}, t)
#Base.unsafe_convert(::Type{Ptr{TPS{T}}}, r::Base.RefValue{TPS{T,D}}) where {T,D} = Base.unsafe_convert(Ptr{TPS{T,D}}, r)

numtype(::Type{<:TempTPS{T}}) where {T} = T
numtype(::TempTPS{T}) where {T} = T

promote_rule(::Type{TempTPS{Float64,D}}, ::Type{T}) where {T<:Real,D} = TempTPS{Float64,D} 
promote_rule(::Type{TempTPS{Float64,D}}, ::Type{TempTPS{ComplexF64,D}}) where {D} = TempTPS{ComplexF64,D}
promote_rule(::Type{TempTPS{ComplexF64,D}}, ::Type{T}) where {T<:Number} where {D} = TempTPS{ComplexF64,D}
promote_rule(::Type{TempTPS{Float64,D}}, ::Type{T}) where {T<:Number} where {D} = TempTPS{ComplexF64,D}
promote_rule(::Type{T}, ::Type{TempTPS{Float64,D}}) where {T<:AbstractIrrational,D} = (T <: Real ? TempTPS{Float64,D} : TempTPS{ComplexF64,D})
promote_rule(::Type{T}, ::Type{TempTPS{ComplexF64,D}}) where {T<:AbstractIrrational,D} = TempTPS{ComplexF64,D}
promote_rule(::Type{T}, ::Type{TempTPS{Float64,D}}) where {T<:Rational,D} = (T <: Real ? TempTPS{Float64,D} : TempTPS{ComplexF64,D})
promote_rule(::Type{T}, ::Type{TempTPS{ComplexF64,D}}) where {T<:Rational,D} = TempTPS{ComplexF64,D}
promote_rule(::Type{TempTPS{Float64,D}}, ::Type{TPS{Float64,D}}) where {D} = TempTPS{Float64,D}
promote_rule(::Type{TempTPS{Float64,D}}, ::Type{TPS{ComplexF64,D}}) where {D} = TempTPS{ComplexF64,D}
promote_rule(::Type{TempTPS{ComplexF64,D}}, ::Type{TPS{Float64,D}}) where {D} = TempTPS{ComplexF64,D}
promote_rule(::Type{TempTPS{ComplexF64,D}}, ::Type{TPS{ComplexF64,D}}) where {D} = TempTPS{ComplexF64,D}

getmo(t::TempTPS) = unsafe_load(Base.unsafe_convert(Ptr{LowTempTPS}, t.t)).mo

# This struct just allows access to the fields of the temporaries
# because unsafe_load of mutable structs causes allocation in Julia
# instead of just reading the struct
struct LowTempTPS
  d::Ptr{Desc}                                            
  lo::UInt8                
  hi::UInt8     
  mo::UInt8  
  ao::UInt8
  uid::Cint            
  nam::NTuple{16,UInt8}  # NAMSZ = 16
  # End of compatibility
end