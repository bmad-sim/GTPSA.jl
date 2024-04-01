module GTPSA


import Base:  +,
              -,
              *,
              /,
              ^,
              ∘,
              inv,
              atan,
              hypot,
              abs   ,
              sqrt  ,
              exp   ,
              log   ,
              sin   ,
              cos   ,
              tan   ,
              csc   ,
              sec   ,
              cot   ,
              sinc  ,
              sinh  ,
              cosh  ,
              tanh  ,
              csch  ,
              sech  ,
              coth  ,
              asin  ,
              acos  ,
              atan  ,
              acsc  ,
              asec  ,
              acot  ,
              asinh ,
              acosh ,
              atanh ,
              acsch ,
              asech ,
              acoth ,
              zero  ,
              one   ,
              real  ,
              imag  ,
              conj  ,
              angle ,
              complex,
              Complex,
              promote_rule,
              getindex,
              setindex!,
              ==,
              <,
              >,
              <=,
              >=,
              !=,
              isequal,
              show,
              copy!

import LinearAlgebra: norm
import SpecialFunctions: erf, erfc

using GTPSA_jll, Printf, PrettyTables

export  
  # Layer 2 structs + functions NOT in Base:
  Descriptor,
  TPS,
  ComplexTPS,
  unit  ,
  sinhc ,
  asinc ,
  asinhc,
  erf   ,
  erfc  ,
  norm,
  polar,
  rect, 

  # Monomial as TPS creators:
  vars,
  params,
  complexvars,
  complexparams,
  mono,
  complexmono,

  # Convenience getters:
  gradient,
  gradient!,
  jacobian,
  jacobian!,
  jacobiant,
  jacobiant!,
  hessian,
  hessian!,
  

  # Methods:
  evaluate,
  compose,
  integ, ∫,
  deriv, ∂,
  getord,
  cutord,
  pb,
  lb,
  getvectorfield,
  gethamiltonian,
  exppb,
  logpb,
  fgrad,
  ptinv,
  translate,
  par,
  scalar,
  setGTPSA!,
  getGTPSA,
  clear!,
  
  # Temporaries:
  @FastGTPSA,
  ±,
  ∓,
  ⨰,
  ⨱,
  ⤊,
  __t_inv, __t_atan, __t_abs, __t_sqrt, __t_exp, __t_log, __t_sin, __t_cos, __t_tan, __t_csc, __t_sec, __t_cot, __t_sinc, __t_sinh, __t_cosh,
  __t_tanh, __t_csch, __t_sech, __t_coth, __t_asin, __t_acos, __t_atan, __t_acsc, __t_asec, __t_acot, __t_asinh, __t_acosh, __t_atanh, __t_acsch, 
  __t_asech, __t_acoth, __t_real, __t_imag, __t_conj, __t_angle, __t_complex, __t_sinhc, __t_asinc, __t_asinhc, __t_erf, __t_erfc, __t_norm,
  __t_polar, __t_rect



# Low-level functions/structs and constants
const NAMSZ::Int = 16 
include("low_level/mono.jl")
include("low_level/desc.jl")
include("low_level/rtpsa.jl")
include("low_level/ctpsa.jl")

const MAD_TPSA::String = :("libgtpsa")
const MAD_TPSA_DEFAULT::Cuchar = 255
const MAD_TPSA_SAME::Cuchar = 254
const MAD_DESC_CURR::Ptr{Desc} = C_NULL
const DESC_MAX_TMP::Int = 8

# Wrapper struct for Ptr{Desc}
struct Descriptor
  desc::Ptr{Desc}
  function Descriptor(desc::Ptr{Desc})::Descriptor
    d = new(desc)
    return d
  end
end

# Global non-constants (types MUST be specified)
desc_current::Descriptor = Descriptor(MAD_DESC_CURR)   # Current Descriptor
show_eps::Float64 =  0.0                               # Print epsilon
show_sparse::Bool = false                              # Use sparse monomial print
show_header::Bool = false                              # Print a header above each TPS

"""
    setGTPSA!(a::AbstractString, val)

Function to set global variables in GTPSA. Options for `a` are:

- `desc_current::Descriptor` -- defines the `Descriptor` to use when that information is not explicitly (or implicitly in a TPS copy constructor) available, e.g. when calling `TPS(a)` where `a` is not a TPS. This is set each time a new `Descriptor` is defined
- `show_eps::Float64`        -- defines the value below which the absolute value of a monomial coefficient is NOT printed
- `show_sparse::Bool`        -- specifies whether the sparse monomial format is used for printing. This is useful for GTPSAs containing a large number of variables and parameters
- `show_header::Bool`        -- specifies whether or not to print the GTPSA `Descriptor` information above each TPS output
"""
function setGTPSA!(a::AbstractString, val)
  a == "desc_current" && (return (GTPSA.desc_current = val))
  a == "show_eps"     && (return (GTPSA.show_eps = val))
  a == "show_sparse"  && (return (GTPSA.show_sparse = val))
  a == "show_header"  && (return (GTPSA.show_header = val))
  error("Global variable \"$(a)\" does not exist!")
end

"""
    getGTPSA(a::AbstractString)

Function to get global variables in GTPSA. Options for `a` are:

- `desc_current::Descriptor` -- defines the `Descriptor` to use when that information is not explicitly (or implicitly in a TPS copy constructor) available, e.g. when calling `TPS(a)` where `a` is not a TPS. This is set each time a new `Descriptor` is defined
- `show_eps::Float64`        -- defines the value below which the absolute value of a monomial coefficient is NOT printed
- `show_sparse::Bool`        -- specifies whether the sparse monomial format is used for printing. This is useful for GTPSAs containing a large number of variables and parameters
- `show_header::Bool`        -- specifies whether or not to print the GTPSA `Descriptor` information above each TPS output
"""
function getGTPSA(a::AbstractString)
  a == "desc_current" && (return GTPSA.desc_current)
  a == "show_eps"     && (return GTPSA.show_eps)
  a == "show_sparse"  && (return GTPSA.show_sparse)
  a == "show_header"  && (return GTPSA.show_header)
  error("Global variable \"$(a)\" does not exist!")
end

# Descriptor outer constructors
"""
    Descriptor(nv::Integer, mo::Integer)::Descriptor

Creates a TPSA `Descriptor` with `nv` variables, and a maximum truncation order `mo`.

### Input
- `nv` -- Number of variables in the TPSA
- `mo` -- Maximum truncation order of the TPSA
"""
function Descriptor(nv::Integer, mo::Integer)::Descriptor
  d = Descriptor(mad_desc_newv(convert(Cint, nv), convert(Cuchar, mo)))
  GTPSA.desc_current = d
  return d
end

"""
    Descriptor(vos::Vector{<:Integer}, mo::Integer)::Descriptor

Creates a TPSA `Descriptor` with `length(vos)` variables with individual truncation 
orders specified in the Vector `vos`, and a maximum truncation order `mo` for the TPSA.

### Input
- `vos` -- `Vector` of the individual truncation orders of each variable
- `mo`  -- Maximum truncation order of the TPSA, <= sum(vos)
"""
function Descriptor(vos::Vector{<:Integer}, mo::Integer)::Descriptor
  all(vos .<= mo) == true || error("Atleast one variable truncation order is > the maximum truncation order!")
  mo <= sum(vos) || (@info "Maximum order too high: setting maximum order = sum($vos) = $(sum(vos))"; mo = sum(vos))
  nv = length(vos)
  np = 0
  po = 0
  no = vos
  d = Descriptor(mad_desc_newvpo(convert(Cint, nv), convert(Cuchar, mo), convert(Cint, np), convert(Cuchar, po), convert(Vector{Cuchar}, no)))
  GTPSA.desc_current = d
  return d
end

"""
    Descriptor(nv::Integer, mo::Integer, np::Integer, po::Integer)::Descriptor

Creates a TPSA `Descriptor` with `nv` variables and `np` parameters. The maximum 
truncation order is `mo` (including the parameters), and the truncation order for 
the parameters part of a monomial is `po`.

### Input
- `nv` -- Number of variables in the TPSA
- `mo` -- Maximum truncation order of the TPSA including variables and parameters
- `np` -- Number of parameters in the TPSA
- `po` -- Truncation order of the parameters part of a monomial
"""
function Descriptor(nv::Integer, mo::Integer, np::Integer, po::Integer)::Descriptor
  d = Descriptor(mad_desc_newvp(convert(Cint, nv), convert(Cuchar, mo), convert(Cint, np), convert(Cuchar, po)))
  GTPSA.desc_current = d
  return d
end


"""
    Descriptor(vos::Vector{<:Integer}, mo::Integer, pos::Vector{<:Integer}, po::Integer)::Descriptor

Creates a TPSA `Descriptor` with `length(vos)` variables with individual truncation 
orders specified in `vos`, and `length(pos)` parameters with individual truncation 
orders specified in `pos`. The maximum truncation order including both variables and 
parameters is `mo`, and the truncation order for just the parameters part of the is `po`.

### Input
- `vos` -- `Vector` of the individual truncation orders of each variable
- `mo`  -- Maximum truncation order of the TPSA including variables and parameters
- `pos` -- `Vector` of the individual truncation orders of each parameter
- `po` -- Truncation order of the parameters part of a monomial
"""
function Descriptor(vos::Vector{<:Integer}, mo::Integer, pos::Vector{<:Integer}, po::Integer)::Descriptor
  nv = length(vos)
  np = length(pos)
  no = vcat(vos,pos)
  d = Descriptor(mad_desc_newvpo(convert(Cint, nv), convert(Cuchar, mo), convert(Cint, np), convert(Cuchar, po), convert(Vector{Cuchar}, no)))
  GTPSA.desc_current = d
  return d
end

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
  return change(t1, use)
end

function low_TPS(t1::TPS, use::Union{TPS,ComplexTPS})
  return change(t1, Descriptor(Base.unsafe_convert(Ptr{Desc}, unsafe_load(use.tpsa).d)))
end

# --- promote real to TPS ---
function low_TPS(a::Real, use::Nothing)
  t = TPS(mad_tpsa_newd(GTPSA.desc_current.desc, MAD_TPSA_DEFAULT))
  mad_tpsa_set0!(t.tpsa, convert(Float64, 0), convert(Float64,a))
  return t
end

function low_TPS(a::Real, use::Union{TPS,ComplexTPS})
  t = TPS(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA}, use.tpsa), MAD_TPSA_SAME))
  mad_tpsa_set0!(t.tpsa, 0.0, convert(Float64,a))
  return t
end

function low_TPS(a::Real, use::Descriptor)
  t = TPS(mad_tpsa_newd(use.desc, MAD_TPSA_DEFAULT))
  mad_tpsa_set0!(t.tpsa, 0.0, convert(Float64,a))
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
  return change(t1, use)
end

function low_ComplexTPS(t1::Union{TPS,ComplexTPS}, use::Union{TPS,ComplexTPS})
  return change(t1, Descriptor(Base.unsafe_convert(Ptr{Desc}, unsafe_load(use.tpsa).d)))
end

# --- promote number to ComplexTPS ---
function low_ComplexTPS(a::Union{Real,Complex}, use::Nothing)
  ct = ComplexTPS(mad_ctpsa_newd(GTPSA.desc_current.desc, MAD_TPSA_DEFAULT))
  mad_ctpsa_set0!(ct.tpsa, convert(ComplexF64, 0), convert(ComplexF64,a))
  return ct
end

function low_ComplexTPS(a::Union{Real,Complex}, use::Union{TPS,ComplexTPS})
  ct = ComplexTPS(mad_ctpsa_new(Base.unsafe_convert(Ptr{CTPSA}, use.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_set0!(ct.tpsa, convert(ComplexF64, 0), convert(ComplexF64,a))
  return ct
end

function low_ComplexTPS(a::Union{Real,Complex}, use::Descriptor)
  ct = ComplexTPS(mad_ctpsa_newd(use.desc, MAD_TPSA_DEFAULT))
  mad_ctpsa_set0!(ct.tpsa, convert(ComplexF64, 0), convert(ComplexF64,a))
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
  mad_ctpsa_set0!(ct.tpsa, convert(ComplexF64, 1), convert(ComplexF64, complex(0,a)))
  return ct
end

function low_ComplexTPS(a::Real, t1::TPS, use::Nothing)
  ct = ComplexTPS(mad_ctpsa_new(Base.unsafe_convert(Ptr{CTPSA}, t1.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_cplx!(Base.unsafe_convert(Ptr{RTPSA}, C_NULL), t1.tpsa, ct.tpsa)
  mad_ctpsa_set0!(ct.tpsa, convert(ComplexF64, 1), convert(ComplexF64, a))
  return ct
end

function low_ComplexTPS(a::Real, b::Real, use::Nothing)
  ct = ComplexTPS(mad_ctpsa_newd(GTPSA.desc_current.desc, MAD_TPSA_DEFAULT))
  mad_ctpsa_set0!(ct.tpsa, convert(ComplexF64, 0), convert(ComplexF64, complex(a,b)))
  return ct
end

function low_ComplexTPS(a::Real, b::Real, use::Descriptor)
  ct = ComplexTPS(mad_ctpsa_newd(use.desc, MAD_TPSA_DEFAULT))
  mad_ctpsa_set0!(ct.tpsa, convert(ComplexF64, 0), convert(ComplexF64, complex(a,b)))
  return ct
end

function low_ComplexTPS(a::Real, b::Real, use::Union{TPS,ComplexTPS})
  ct = ComplexTPS(mad_ctpsa_new(Base.unsafe_convert(Ptr{CTPSA}, use.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_set0!(ct.tpsa, convert(ComplexF64, 0), convert(ComplexF64, complex(a,b)))
  return ct
end

function low_ComplexTPS(ta::TPS, tb::TPS, use::Descriptor)
  ct = change(ta, use, type=ComplexTPS)
  change!(ct, tb, 1, im)
  return ct
end

function low_ComplexTPS(ta::TPS, b::Real, use::Descriptor)
  ct = change(ta, use, type=ComplexTPS)
  mad_ctpsa_set0!(ct.tpsa, convert(ComplexF64, 1), convert(ComplexF64, complex(0,b)))
  return ct
end

function low_ComplexTPS(a::Real, tb::TPS, use::Descriptor)
  ct = change(tb, use, type=ComplexTPS, scl2=im)
  mad_ctpsa_set0!(ct.tpsa, convert(ComplexF64, 1), convert(ComplexF64, a))
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

# Useful type-generic functions:
cycle!(t::Ptr{RTPSA}, i::Cint, n::Cint, m_::Vector{Cuchar}, v_::Ref{Cdouble}) = (@inline; mad_tpsa_cycle!(t, i, n, m_, v_))
cycle!(t::Ptr{CTPSA}, i::Cint, n::Cint, m_::Vector{Cuchar}, v_::Ref{ComplexF64}) = (@inline; mad_ctpsa_cycle!(t, i, n, m_, v_))
idxm(t::Ptr{RTPSA}, n::Cint, m::Vector{Cuchar}) = (@inline; mad_tpsa_idxm(t, n, m))
idxm(t::Ptr{CTPSA}, n::Cint, m::Vector{Cuchar}) = (@inline; mad_ctpsa_idxm(t, n, m))
getdesc(t::Union{TPS,ComplexTPS}) = Descriptor(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d))
numvars(t::Union{TPS,ComplexTPS}) = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d)).nv
numparams(t::Union{TPS,ComplexTPS}) = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d)).np
numnn(t::Union{TPS,ComplexTPS}) = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d)).nn


# --- Variable/parameter generators ---

"""
    vars(use::Union{Descriptor,TPS,ComplexTPS}=GTPSA.desc_current)::Vector{TPS}

Returns `TPS`s corresponding to the variables for the `Descriptor` specified by `use`.
Default value is `GTPSA.desc_current`.

### Input
- `use` -- (Optional) Specify which TPSA `Descriptor` to use, default is `GTPSA.desc_current`

### Output
- `x`   -- `Vector` containing unit `TPS`s corresponding to each variable
"""
function vars(use::Union{Descriptor,TPS,ComplexTPS}=GTPSA.desc_current)::Vector{TPS}
  t1 = TPS(use=use)
  desc = unsafe_load(mad_tpsa_desc(t1.tpsa))
  nv = desc.nv
  if nv < 1
    return TPS[]
  end
  x = Vector{TPS}(undef, nv)
  t1[1] = 1.0
  x[1] = t1
  for i=2:nv
    t = TPS(use=use)
    mad_tpsa_seti!(t.tpsa, Cint(i), 0.0, 1.0)
    x[i] = t
  end
  return x
end

"""
    params(use::Union{Descriptor,TPS,ComplexTPS}=GTPSA.desc_current)::Vector{TPS}

Returns `TPS`s corresponding to the parameters for the `Descriptor` specified by `use`.
Default value is `GTPSA.desc_current`.

### Input
- `use` -- (Optional) Specify which TPSA `Descriptor` to use, default is `GTPSA.desc_current`

### Output
- `k`   -- `Vector` containing unit `TPS`s corresponding to each parameter
"""
function params(use::Union{Descriptor,TPS,ComplexTPS}=GTPSA.desc_current)::Vector{TPS}
  t1 = TPS(use=use)
  desc = unsafe_load(mad_tpsa_desc(t1.tpsa))
  nv = desc.nv
  np = desc.np
  if np < 1
    return TPS[]
  end
  k = Vector{TPS}(undef, np)
  mad_tpsa_seti!(t1.tpsa, Cint(nv+1), 0.0, 1.0)
  k[1] = t1
  for i=nv+2:nv+np
    t = TPS(use=use)
    mad_tpsa_seti!(t.tpsa, Cint(i), 0.0, 1.0)
    k[i-nv] = t
  end
  return k
end


"""
    complexvars(use::Union{Descriptor,TPS,ComplexTPS}=GTPSA.desc_current)::Vector{ComplexTPS}

Returns `ComplexTPS`s corresponding to the variables for the `Descriptor` specified by `use`.
Default value is `GTPSA.desc_current`.

### Input
- `use` -- (Optional) Specify which TPSA `Descriptor` to use, default is `GTPSA.desc_current`

### Output
- `x`   -- `Vector` containing unit `ComplexTPS`s corresponding to each variable
"""
function complexvars(use::Union{Descriptor,TPS,ComplexTPS}=GTPSA.desc_current)::Vector{ComplexTPS}
  ct1 = ComplexTPS(use=use)
  desc = unsafe_load(mad_ctpsa_desc(ct1.tpsa))
  nv = desc.nv
  if nv < 1
    return ComplexTPS[]
  end
  x = Vector{ComplexTPS}(undef, nv)
  ct1[1] = 1.0
  x[1] = ct1
  for i=2:nv
    ct = ComplexTPS(use=use)
    mad_ctpsa_seti!(ct.tpsa, Cint(i), ComplexF64(0.0), ComplexF64(1.0))
    x[i] = ct
  end
  return x
end

"""
    complexparams(d::Descriptor=GTPSA.desc_current)::Vector{ComplexTPS}

Returns `ComplexTPS`s corresponding to the parameters for the `Descriptor` specified by `use`.
Default value is `GTPSA.desc_current`.

### Input
- `use` -- (Optional) Specify which TPSA `Descriptor` to use, default is `GTPSA.desc_current`

### Output
- `k`   -- `Vector` containing unit `ComplexTPS`s corresponding to each parameter
"""
function complexparams(use::Union{Descriptor,TPS,ComplexTPS}=GTPSA.desc_current)::Vector{ComplexTPS}
  ct1 = ComplexTPS(use=use)
  desc = unsafe_load(mad_ctpsa_desc(ct1.tpsa))
  nv = desc.nv
  np = desc.np
  if np < 1
    return ComplexTPS[]
  end
  k = Vector{ComplexTPS}(undef, np)
  mad_ctpsa_seti!(ct1.tpsa, Cint(nv+1), ComplexF64(0.0), ComplexF64(1.0))
  k[1] = ct1
  for i=nv+2:nv+np
    ct = ComplexTPS(use=use)
    mad_ctpsa_seti!(ct.tpsa, Cint(i), ComplexF64(0.0), ComplexF64(1.0))
    k[i-nv] = ct
  end
  return k
end

"""
    mono(v::Union{Integer, Vector{<:Union{<:Pair{<:Integer,<:Integer},<:Integer}}, Nothing}=nothing; param::Union{<:Integer,Nothing}=nothing, params::Union{Vector{<:Pair{<:Integer,<:Integer}}, Nothing}=nothing, use::Descriptor=GTPSA.desc_current)::TPS

Returns a `TPS` corresponding to a specific monomial, specified using the variable/parameter index, or 
monomial indexing-by-order OR monomial indexing-by-sparse monomial. 

### Input
- `v`      -- An integer (for variable index), an array of orders for each variable (for indexing-by-order), or an array of pairs (sparse monomial)
- `param`  -- (Keyword argument, optional) An integer for the parameter index
- `params` -- (Keyword argument, optional) An array of pairs for sparse-monomial indexing
- `use`    -- (Keyword argument, optional) The descriptor to use to generate the monomial. Default is most recently-defined.

# Examples: Variable/Parameter Index:
```julia-repl
julia> d = Descriptor(3,10,2,10);

julia> mono(1)
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        1    0    0    0    0


julia> mono(2, use=d)
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        0    1    0    0    0


julia> mono(param=2)
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        0    0    0    0    1
```

# Examples: Monomial Index-by-Order
```julia-repl
julia> mono([1])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        1    0    0    0    0


julia> mono([0,1])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        0    1    0    0    0


julia> mono([0,0,0,0,1], use=d)
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        0    0    0    0    1


julia> mono([1,0,0,0,1])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    2        1    0    0    0    1
```

# Examples: Monomial Index-by-Sparse Monomial
```julia-repl
julia> mono([1=>1])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        1    0    0    0    0


julia> mono([2=>1])
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    1        0    1    0    0    0


julia> mono([1=>1], params=[2=>1], use=d)
TPS:
  Coefficient              Order     Exponent
   1.0000000000000000e+00    2        1    0    0    0    1
```
"""
function mono(v::Union{Integer, Vector{<:Union{<:Pair{<:Integer,<:Integer},<:Integer}}, Nothing}=nothing; param::Union{<:Integer,Nothing}=nothing, params::Union{Vector{<:Pair{<:Integer,<:Integer}}, Nothing}=nothing, use::Union{Descriptor,TPS,ComplexTPS}=GTPSA.desc_current)::TPS
  return low_mono(TPS, use, v, param, params)
end

function complexmono(v::Union{Integer, Vector{<:Union{<:Pair{<:Integer,<:Integer},<:Integer}}, Nothing}=nothing; param::Union{<:Integer,Nothing}=nothing, params::Union{Vector{<:Pair{<:Integer,<:Integer}}, Nothing}=nothing, use::Union{Descriptor,TPS,ComplexTPS}=GTPSA.desc_current)::ComplexTPS
  return low_mono(ComplexTPS, use, v, param, params)
end

# Variable/parameter:
function low_mono(T::Type, d::Union{Descriptor,TPS,ComplexTPS}, v::Integer, param::Nothing, params::Nothing)::T
  t = T(use=d)
  seti!(t.tpsa, Cint(v), convert(numtype(T), 0.0), convert(numtype(T), 1.0))
  return t
end

function low_mono(T::Type, d::Union{Descriptor,TPS,ComplexTPS}, v::Nothing, param::Integer, params::Nothing)::T
  t = T(use=d)
  desc = unsafe_load(mad_tpsa_desc(t.tpsa))
  nv = desc.nv # TOTAL NUMBER OF VARS!!!!
  seti!(t.tpsa, Cint(param) + nv, convert(numtype(T), 0.0), convert(numtype(T), 1.0))
  return t
end

# Default to scalar value if nothing passed
function low_mono(T::Type, d::Union{Descriptor,TPS,ComplexTPS}, v::Nothing, param::Nothing, params::Nothing)::T
  t = T(use=d)
  t[0] = 1.0
  return t
end

# Monomial by order:
function low_mono(T::Type, d::Union{Descriptor,TPS,ComplexTPS}, v::Vector{<:Integer}, param::Nothing, params::Nothing)::T
  t = T(use=d)
  t[v...] = 1.0
  return t
end

# Monomial by sparse monomial:
function low_mono(T::Type, d::Union{Descriptor,TPS,ComplexTPS}, v::Vector{<:Pair{<:Integer,<:Integer}}, param::Nothing, params::Vector{<:Pair{<:Integer,<:Integer}})::T
  t = T(use=d)
  t[v..., params=params] = 1.0
  return t
end

function low_mono(T::Type, d::Union{Descriptor,TPS,ComplexTPS}, v::Vector{<:Pair{<:Integer,<:Integer}}, param::Nothing, params::Nothing)::T
  t = T(use=d)
  # Need to create array of orders with length nv + np
  t[v...] = 1.0
  return t
end

function low_mono(T::Type, d::Union{Descriptor,TPS,ComplexTPS}, v::Nothing, param::Nothing, params::Vector{<:Pair{<:Integer,<:Integer}})::T
  t = T(use=d)
  t[params=params] = 1.0
  return t
end

# Throw error if no above use cases satisfied:
function low_mono(T::Type, d::Union{Descriptor,TPS,ComplexTPS}, v, param, params)
  error("Invalid monomial specified. Please use ONE of variable/parameter index, index by order, or index by sparse monomial.")
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
function pairs_to_m(t::Union{TPS,ComplexTPS}, vars::Union{Vector{<:Pair{<:Integer, <:Integer}},Tuple{Vararg{Pair{<:Integer,<:Integer}}}}; params::Vector{<:Pair{<:Integer,<:Integer}}=Pair{Int,Int}[],zero_mono=true)::Tuple{Vector{UInt8}, Cint}
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

include("getset.jl")
include("show.jl")
include("operators.jl")
include("methods.jl")
include("fast_gtpsa.jl")

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


promote_rule(::Type{TPS}, ::Union{Type{<:AbstractFloat}, Type{<:Integer}, Type{<:Rational}, Type{<:AbstractIrrational}}) = TPS
promote_rule(::Type{ComplexTPS}, ::Union{Type{Complex{<:Real}},Type{<:AbstractFloat}, Type{<:Integer}, Type{<:Rational}, Type{<:AbstractIrrational}}) = ComplexTPS
promote_rule(::Type{TPS}, ::Union{Type{ComplexTPS}, Type{Complex{<:Real}}}) = ComplexTPS

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
end
