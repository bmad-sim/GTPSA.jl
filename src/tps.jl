# Wrapper struct for Ptr{RTPSA}
mutable struct TPS <: Real
  tpsa::Ptr{RTPSA}
  function TPS(t1::Ptr{RTPSA})::TPS
    t = new(t1)
    f(x) = mad_tpsa_del!(x.tpsa)
    finalizer(f,t)
    return t
  end
end

# Wrapper struct for Ptr{CTPSA}
mutable struct ComplexTPS <: Number
  tpsa::Ptr{CTPSA}
  function ComplexTPS(ct1::Ptr{CTPSA})::ComplexTPS
    ct = new(ct1)
    f(x) = mad_ctpsa_del!(x.tpsa)
    finalizer(f,ct)
    return ct
  end
end

"""
    TPS(ta::Union{Real,Nothing}=nothing; use::Union{Descriptor,TPS,ComplexTPS,Nothing}=nothing)::TPS

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
- `ret` -- New `TPS` equal to `ta` with removal of invalid monomials if `ta` is a `TPS` and a new `Descriptor` is specified
"""
function TPS(ta::Union{Real,Nothing}=nothing; use::Union{Descriptor,TPS,ComplexTPS,Nothing}=nothing)::TPS
  return low_TPS(ta, use)
end

# --- Blank TPS ---
function low_TPS(ta::Nothing, use::Descriptor)
  return TPS(mad_tpsa_newd(use.desc, MAD_TPSA_DEFAULT))
end

function low_TPS(ta::Nothing, use::Union{TPS,ComplexTPS})
  return TPS(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA}, use.tpsa), MAD_TPSA_SAME))
end

function low_TPS(ta::Nothing, use::Nothing)
  return TPS(mad_tpsa_newd(GTPSA.desc_current.desc, MAD_TPSA_DEFAULT))
end

# --- Copy ctor ---
function low_TPS(t1::TPS, use::Nothing)
  t = TPS(mad_tpsa_new(t1.tpsa, MAD_TPSA_SAME))
  mad_tpsa_copy!(t1.tpsa, t.tpsa)
  return t
end

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

# --- promote real to TPS ---
function low_TPS(a::Real, use::Nothing)
  t = TPS(mad_tpsa_newd(GTPSA.desc_current.desc, MAD_TPSA_DEFAULT))
  mad_tpsa_seti!(t.tpsa, Cint(0), convert(Float64, 0), convert(Float64,a))
  return t
end

function low_TPS(a::Real, use::Union{TPS,ComplexTPS})
  t = TPS(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA}, use.tpsa), MAD_TPSA_SAME))
  mad_tpsa_seti!(t.tpsa, Cint(0), 0.0, convert(Float64,a))
  return t
end

function low_TPS(a::Real, use::Descriptor)
  t = TPS(mad_tpsa_newd(use.desc, MAD_TPSA_DEFAULT))
  mad_tpsa_seti!(t.tpsa, Cint(0), 0.0, convert(Float64,a))
  return t
end

# -----------------------
"""
    ComplexTPS(cta::Union{Number,Nothing}=nothing; use::Union{Descriptor,TPS,ComplexTPS,Nothing}=nothing)::ComplexTPS 

Constructor to create a new `ComplexTPS` equal to the number `cta`. If `cta` is a `ComplexTPS` (or `TPS`), this 
is equivalent to a copy constructor, with the result by default having the same `Descriptor` as `cta`. If `cta` 
is not a `TPS` or`ComplexTPS`, then the `Descriptor` used will by default be `GTPSA.desc_current`. The `Descriptor` 
for the constructed `ComplexTPS` can be set using `use`. If a `TPS` or `ComplexTPS` is passed to `use`, 
the `Descriptor` for that TPS will be used.

The constructor can also be used to create a copy of a `ComplexTPS` under one `Descriptor` to instead 
have a different `Descriptor`. In this case, invalid monomials under the new `Descriptor` are removed.

### Input
- `cta`  -- Any `Number`
- `use` -- (Optional) specify which `Descriptor` to use, default is `nothing` which uses the `Descriptor` for `cta` if `cta <: Union{TPS,ComplexTPS}`, else uses `GTPSA.desc_current`

### Output
- `ret` -- New `ComplexTPS` equal to `cta` with removal of invalid monomials if `cta` is a `TPS`/`ComplexTPS` and a new `Descriptor` is specified
"""
function ComplexTPS(cta::Union{Number,Nothing}=nothing; use::Union{Descriptor,TPS,ComplexTPS,Nothing}=nothing)::ComplexTPS
  return low_ComplexTPS(cta, use)
end

# For some reason I need to explicitly define this:
ComplexTPS(t::ComplexTPS; use::Union{Descriptor,TPS,ComplexTPS,Nothing}=nothing) = low_ComplexTPS(t,use)

# --- Blank ComplexTPS ---
function low_ComplexTPS(cta::Nothing, use::Descriptor)
  return ComplexTPS(mad_ctpsa_newd(use.desc, MAD_TPSA_DEFAULT))
end

function low_ComplexTPS(cta::Nothing, use::Union{TPS,ComplexTPS})
  return ComplexTPS(mad_ctpsa_new(Base.unsafe_convert(Ptr{CTPSA}, use.tpsa), MAD_TPSA_SAME))
end

function low_ComplexTPS(cta::Nothing, use::Nothing)
  return ComplexTPS(mad_ctpsa_newd(GTPSA.desc_current.desc, MAD_TPSA_DEFAULT))
end

# --- Copy ctor ---
function low_ComplexTPS(ct1::ComplexTPS, use::Nothing)
  ct = ComplexTPS(mad_ctpsa_new(ct1.tpsa, MAD_TPSA_SAME))
  mad_ctpsa_copy!(ct1.tpsa, ct.tpsa)
  return ct
end

function low_ComplexTPS(t1::TPS, use::Nothing)
  ct = ComplexTPS(mad_ctpsa_new(Base.unsafe_convert(Ptr{CTPSA}, t1.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_cplx!(t1.tpsa, Base.unsafe_convert(Ptr{RTPSA}, C_NULL), ct.tpsa)
  return ct
end

# --- Change descriptor ---
function low_ComplexTPS(t1::Union{TPS,ComplexTPS}, use::Descriptor)
  t = ComplexTPS(use=use)
  setTPS!(t, t1, change=true)
  return t
end

function low_ComplexTPS(t1::Union{TPS,ComplexTPS}, use::Union{TPS,ComplexTPS})
  t = ComplexTPS(use=use)
  setTPS!(t, t1, change=true)
  return t
end

# --- promote number to ComplexTPS ---
function low_ComplexTPS(a::Union{Real,Complex}, use::Nothing)
  ct = ComplexTPS(mad_ctpsa_newd(GTPSA.desc_current.desc, MAD_TPSA_DEFAULT))
  mad_ctpsa_seti!(ct.tpsa, Cint(0), convert(ComplexF64, 0), convert(ComplexF64,a))
  return ct
end

function low_ComplexTPS(a::Union{Real,Complex}, use::Union{TPS,ComplexTPS})
  ct = ComplexTPS(mad_ctpsa_new(Base.unsafe_convert(Ptr{CTPSA}, use.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_seti!(ct.tpsa, Cint(0), convert(ComplexF64, 0), convert(ComplexF64,a))
  return ct
end

function low_ComplexTPS(a::Union{Real,Complex}, use::Descriptor)
  ct = ComplexTPS(mad_ctpsa_newd(use.desc, MAD_TPSA_DEFAULT))
  mad_ctpsa_seti!(ct.tpsa, Cint(0), convert(ComplexF64, 0), convert(ComplexF64,a))
  return ct
end

# -----------------------

# Special real argument ctors
function ComplexTPS(ta::Real, tb::Real; use::Union{Descriptor,TPS,ComplexTPS,Nothing}=nothing)::ComplexTPS
  low_ComplexTPS(ta, tb, use)
end

function low_ComplexTPS(t1::TPS, t2::TPS, use::Nothing)
  ct = ComplexTPS(mad_ctpsa_new(Base.unsafe_convert(Ptr{CTPSA}, t1.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_cplx!(t1.tpsa, t2.tpsa, ct.tpsa)
  return ct
end

function low_ComplexTPS(t1::TPS, a::Real, use::Nothing)
  ct = ComplexTPS(mad_ctpsa_new(Base.unsafe_convert(Ptr{CTPSA}, t1.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_cplx!(t1.tpsa, Base.unsafe_convert(Ptr{RTPSA}, C_NULL), ct.tpsa)
  mad_ctpsa_seti!(ct.tpsa, Cint(0), convert(ComplexF64, 1), convert(ComplexF64, complex(0,a)))
  return ct
end

function low_ComplexTPS(a::Real, t1::TPS, use::Nothing)
  ct = ComplexTPS(mad_ctpsa_new(Base.unsafe_convert(Ptr{CTPSA}, t1.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_cplx!(Base.unsafe_convert(Ptr{RTPSA}, C_NULL), t1.tpsa, ct.tpsa)
  mad_ctpsa_seti!(ct.tpsa, Cint(0), convert(ComplexF64, 1), convert(ComplexF64, a))
  return ct
end

function low_ComplexTPS(a::Real, b::Real, use::Nothing)
  ct = ComplexTPS(mad_ctpsa_newd(GTPSA.desc_current.desc, MAD_TPSA_DEFAULT))
  mad_ctpsa_seti!(ct.tpsa, Cint(0), convert(ComplexF64, 0), convert(ComplexF64, complex(a,b)))
  return ct
end

function low_ComplexTPS(a::Real, b::Real, use::Descriptor)
  ct = ComplexTPS(mad_ctpsa_newd(use.desc, MAD_TPSA_DEFAULT))
  mad_ctpsa_seti!(ct.tpsa, Cint(0), convert(ComplexF64, 0), convert(ComplexF64, complex(a,b)))
  return ct
end

function low_ComplexTPS(a::Real, b::Real, use::Union{TPS,ComplexTPS})
  ct = ComplexTPS(mad_ctpsa_new(Base.unsafe_convert(Ptr{CTPSA}, use.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_seti!(ct.tpsa, Cint(0), convert(ComplexF64, 0), convert(ComplexF64, complex(a,b)))
  return ct
end

function low_ComplexTPS(ta::TPS, tb::TPS, use::Descriptor)
  ctmp = ComplexTPS(use=use)
  ct = ComplexTPS(use=use)
  setTPS!(ct, ta, change=true)
  setTPS!(ctmp, tb, change=true)
  mul!(ctmp, ctmp, complex(0,1))
  add!(ct, ctmp, ct)
  return ct
end

function low_ComplexTPS(ta::TPS, b::Real, use::Descriptor)
  ct = ComplexTPS(use=use)
  setTPS!(ct, ta, change=true)
  add!(ct, ct, complex(0,b))
  return ct
end

function low_ComplexTPS(a::Real, tb::TPS, use::Descriptor)
  ct = ComplexTPS(use=use)
  setTPS!(ct, tb, change=true)
  mul!(ct,ct,complex(0,1))
  add!(ct, a, ct)
  return ct
end

function low_ComplexTPS(ta::TPS, tb::TPS, use::Union{TPS,ComplexTPS})
  return low_ComplexTPS(ta, tb, Descriptor(Base.unsafe_convert(Ptr{Desc}, unsafe_load(use.tpsa))))
end

function low_ComplexTPS(ta::TPS, b::Real, use::Union{TPS,ComplexTPS})
  return low_ComplexTPS(ta, b, Descriptor(Base.unsafe_convert(Ptr{Desc}, unsafe_load(use.tpsa))))
end

function low_ComplexTPS(a::Real, tb::TPS, use::Union{TPS,ComplexTPS})
  return low_ComplexTPS(a, tb, Descriptor(Base.unsafe_convert(Ptr{Desc}, unsafe_load(use.tpsa))))
end