module GTPSA

import Base:  +,
              -,
              *,
              /,
              ^,
              inv,
              atan,
              hypot,
              abs   ,
              # unit  ,
              sqrt  ,
              exp   ,
              log   ,
              sin   ,
              cos   ,
              tan   ,
              cot   ,
              sinc  ,
              sinh  ,
              cosh  ,
              tanh  ,
              coth  ,
              # sinhc ,
              asin  ,
              acos  ,
              atan  ,
              acot  ,
              # asinc ,
              asinh ,
              acosh ,
              atanh ,
              acoth ,
              # asinhc,
              # erf   ,
              # erfc  ,
              #show,
              print,
              zero,
              real,
              imag,
              getindex,
              firstindex,
              lastindex,
              setindex!,
              length,
              convert,
              ==
#=
import MutableArithmetics:  mutability,
                            promote_operation,
                            operate!,
                            operate_to!,
                            IsMutable,
                            mutable_copy
=#
using GTPSA_jll

export
  # Constants:
  NAMSZ,
  MAD_TPSA, 
  MAD_TPSA_DEFAULT, 
  MAD_TPSA_SAME,
  MAD_DESC_CURR,
  
  # 1-to-1 C Structs:
  RTPSA,
  CTPSA,
  Desc,

  # Descriptor:
  mad_desc_newv,
  mad_desc_newvp,
  mad_desc_newvpo,
  mad_desc_del!,
  mad_desc_getnv!,
  mad_desc_maxord,
  mad_desc_maxlen,
  mad_desc_gtrunc!,
  mad_desc_isvalids,
  mad_desc_isvalidm,
  mad_desc_isvalidsm,
  mad_desc_idxs,
  mad_desc_idxm,
  mad_desc_idxsm,
  mad_desc_nxtbyvar,
  mad_desc_nxtbyord,
  mad_desc_mono!,
  mad_desc_info,

  # Monomial:
  mad_mono_str!,
  mad_mono_prt!,
  mad_mono_fill!,
  mad_mono_copy!,
  mad_mono_min,
  mad_mono_max,
  mad_mono_ord,
  mad_mono_ordp,
  mad_mono_ordpf,
  mad_mono_eq,
  mad_mono_lt,
  mad_mono_le,
  mad_mono_cmp,
  mad_mono_rcmp,
  mad_mono_add!,
  mad_mono_sub!,
  mad_mono_cat!,
  mad_mono_rev!,
  mad_mono_print,

  # TPSA:
  mad_tpsa_newd,
  mad_tpsa_new,
  mad_tpsa_del!,
  mad_tpsa_desc,
  mad_tpsa_uid!,
  mad_tpsa_len,
  mad_tpsa_nam,
  mad_tpsa_ord,
  mad_tpsa_ordv,
  mad_tpsa_ordn,
  mad_tpsa_copy!,
  mad_tpsa_sclord!,
  mad_tpsa_getord!,
  mad_tpsa_cutord!,
  mad_tpsa_maxord!,
  mad_tpsa_convert!,
  mad_tpsa_setvar!,
  mad_tpsa_setval!,
  mad_tpsa_setnam!,
  mad_tpsa_clear!,
  mad_tpsa_isnul,
  mad_tpsa_mono!,
  mad_tpsa_idxs,
  mad_tpsa_idxm,
  mad_tpsa_idxsm,
  mad_tpsa_cycle!,
  mad_tpsa_get0,
  mad_tpsa_geti,
  mad_tpsa_gets,
  mad_tpsa_getm,
  mad_tpsa_getsm,
  mad_tpsa_set0!,
  mad_tpsa_seti!,
  mad_tpsa_sets!,
  mad_tpsa_setm!,
  mad_tpsa_setsm!,
  mad_tpsa_getv!,
  mad_tpsa_setv!,
  mad_tpsa_equ,
  mad_tpsa_dif!,
  mad_tpsa_add!,
  mad_tpsa_sub!,
  mad_tpsa_mul!,
  mad_tpsa_div!,
  mad_tpsa_pow!,
  mad_tpsa_powi!,
  mad_tpsa_pown!,
  mad_tpsa_nrm,
  mad_tpsa_abs!,
  mad_tpsa_sqrt!,
  mad_tpsa_exp!,
  mad_tpsa_log!,
  mad_tpsa_sincos!,
  mad_tpsa_sin!,
  mad_tpsa_cos!,
  mad_tpsa_tan!,
  mad_tpsa_cot!,
  mad_tpsa_sinc!,
  mad_tpsa_sincosh!,
  mad_tpsa_sinh!,
  mad_tpsa_cosh!,
  mad_tpsa_tanh!,
  mad_tpsa_coth!,
  mad_tpsa_sinhc!,
  mad_tpsa_asin!,
  mad_tpsa_acos!,
  mad_tpsa_atan!,
  mad_tpsa_acot!,
  mad_tpsa_asinc!,
  mad_tpsa_asinh!,
  mad_tpsa_acosh!,
  mad_tpsa_atanh!,
  mad_tpsa_acoth!,
  mad_tpsa_asinhc!,
  mad_tpsa_erf!,
  mad_tpsa_erfc!,
  mad_tpsa_acc!,
  mad_tpsa_scl!,
  mad_tpsa_inv!,
  mad_tpsa_invsqrt!,
  mad_tpsa_unit!,
  mad_tpsa_atan2!,
  mad_tpsa_hypot!,
  mad_tpsa_hypot3!,
  mad_tpsa_integ!,
  mad_tpsa_deriv!,
  mad_tpsa_derivm!,
  mad_tpsa_poisbra!,
  mad_tpsa_taylor!,
  mad_tpsa_axpb!,
  mad_tpsa_axpbypc!,
  mad_tpsa_axypb!,
  mad_tpsa_axypbzpc!,
  mad_tpsa_axypbvwpc!,
  mad_tpsa_ax2pby2pcz2!,
  mad_tpsa_axpsqrtbpcx2!,
  mad_tpsa_logaxpsqrtbpcx2!,
  mad_tpsa_logxdy!,
  mad_tpsa_vec2fld!,
  mad_tpsa_fld2vec!,
  mad_tpsa_fgrad!,
  mad_tpsa_liebra!,
  mad_tpsa_exppb!,
  mad_tpsa_logpb!,
  mad_tpsa_mnrm,
  mad_tpsa_minv!,
  mad_tpsa_pminv!,
  mad_tpsa_compose!,
  mad_tpsa_translate!,
  mad_tpsa_eval!,
  mad_tpsa_mconv!,
  mad_tpsa_print,
  mad_tpsa_scan,
  mad_tpsa_scan_hdr,
  mad_tpsa_scan_coef!,
  mad_tpsa_debug,
  mad_tpsa_isvalid,
  mad_tpsa_init!,

  # CTPSA:
  mad_ctpsa_newd,
  mad_ctpsa_new,
  mad_ctpsa_del!,
  mad_ctpsa_desc,
  mad_ctpsa_uid!,
  mad_ctpsa_len,
  mad_ctpsa_nam,
  mad_ctpsa_ord,
  mad_ctpsa_ordv,
  mad_ctpsa_ordn,
  mad_ctpsa_copy!,
  mad_ctpsa_sclord!,
  mad_ctpsa_getord!,
  mad_ctpsa_cutord!,
  mad_ctpsa_maxord,
  mad_ctpsa_convert!,
  mad_ctpsa_setvar!,
  mad_ctpsa_setvar_r!,
  mad_ctpsa_setval!,
  mad_ctpsa_setval_r!,
  mad_ctpsa_setnam!,
  mad_ctpsa_clear!,
  mad_ctpsa_isnul,
  mad_ctpsa_cplx!,
  mad_ctpsa_real!,
  mad_ctpsa_imag!,
  mad_ctpsa_cabs!,
  mad_ctpsa_carg!,
  mad_ctpsa_unit!,
  mad_ctpsa_rect!,
  mad_ctpsa_polar!,
  mad_ctpsa_mono!,
  mad_ctpsa_idxs,
  mad_ctpsa_idxm,
  mad_ctpsa_idxsm,
  mad_ctpsa_cycle!,
  mad_ctpsa_get0,
  mad_ctpsa_geti,
  mad_ctpsa_gets,
  mad_ctpsa_getm,
  mad_ctpsa_getsm,
  mad_ctpsa_set0!,
  mad_ctpsa_seti!,
  mad_ctpsa_sets!,
  mad_ctpsa_setm!,
  mad_ctpsa_setsm!,
  mad_ctpsa_get0_r!,
  mad_ctpsa_geti_r!,
  mad_ctpsa_gets_r!,
  mad_ctpsa_getm_r!,
  mad_ctpsa_getsm_r!,
  mad_ctpsa_set0_r!,
  mad_ctpsa_seti_r!,
  mad_ctpsa_sets_r!,
  mad_ctpsa_setm_r!,
  mad_ctpsa_setsm_r!,
  mad_ctpsa_getv!,
  mad_ctpsa_setv!,
  mad_ctpsa_equ,
  mad_ctpsa_dif!,
  mad_ctpsa_add!,
  mad_ctpsa_sub!,
  mad_ctpsa_mul!,
  mad_ctpsa_div!,
  mad_ctpsa_pow!,
  mad_ctpsa_powi!,
  mad_ctpsa_pown!,
  mad_ctpsa_pown_r!,
  mad_ctpsa_equt,
  mad_ctpsa_dift!,
  mad_ctpsa_tdif!,
  mad_ctpsa_addt!,
  mad_ctpsa_subt!,
  mad_ctpsa_tsub!,
  mad_ctpsa_mult!,
  mad_ctpsa_divt!,
  mad_ctpsa_tdiv!,
  mad_ctpsa_powt!,
  mad_ctpsa_tpow!,
  mad_ctpsa_nrm,
  mad_ctpsa_conj!,
  mad_ctpsa_sqrt!,
  mad_ctpsa_exp!,
  mad_ctpsa_log!,
  mad_ctpsa_sincos!,
  mad_ctpsa_sin!,
  mad_ctpsa_cos!,
  mad_ctpsa_tan!,
  mad_ctpsa_cot!,
  mad_ctpsa_sinc!,
  mad_ctpsa_sincosh!,
  mad_ctpsa_sinh!,
  mad_ctpsa_cosh!,
  mad_ctpsa_tanh!,
  mad_ctpsa_coth!,
  mad_ctpsa_sinhc!,
  mad_ctpsa_asin!,
  mad_ctpsa_acos!,
  mad_ctpsa_atan!,
  mad_ctpsa_acot!,
  mad_ctpsa_asinc!,
  mad_ctpsa_asinh!,
  mad_ctpsa_acosh!,
  mad_ctpsa_atanh!,
  mad_ctpsa_acoth!,
  mad_ctpsa_asinhc!,
  mad_ctpsa_erf!,
  mad_ctpsa_erfc!,
  mad_ctpsa_acc!,
  mad_ctpsa_scl!,
  mad_ctpsa_inv!,
  mad_ctpsa_invsqrt!,
  mad_ctpsa_hypot!,
  mad_ctpsa_hypot3!,
  mad_ctpsa_integ!,
  mad_ctpsa_deriv!,
  mad_ctpsa_derivm!,
  mad_ctpsa_poisbra!,
  mad_ctpsa_taylor!,
  mad_ctpsa_poisbrat!,
  mad_ctpsa_tpoisbra!,
  mad_ctpsa_acc_r!,
  mad_ctpsa_scl_r!,
  mad_ctpsa_inv_r!,
  mad_ctpsa_invsqrt_r!,
  mad_ctpsa_axpb!,
  mad_ctpsa_axpbypc!,
  mad_ctpsa_axypb!,
  mad_ctpsa_axypbzpc!,
  mad_ctpsa_axypbvwpc!,
  mad_ctpsa_ax2pby2pcz2!,
  mad_ctpsa_axpsqrtbpcx2!,
  mad_ctpsa_logaxpsqrtbpcx2!,
  mad_ctpsa_logxdy!,
  mad_ctpsa_axpb_r!,
  mad_ctpsa_axpbypc_r!,
  mad_ctpsa_axypb_r!,
  mad_ctpsa_axypbzpc_r!,
  mad_ctpsa_axypbvwpc_r!,
  mad_ctpsa_ax2pby2pcz2_r!,
  mad_ctpsa_axpsqrtbpcx2_r!,
  mad_ctpsa_logaxpsqrtbpcx2_r!,
  mad_ctpsa_vec2fld!,
  mad_ctpsa_fld2vec!,
  mad_ctpsa_fgrad!,
  mad_ctpsa_liebra!,
  mad_ctpsa_exppb!,
  mad_ctpsa_logpb!,
  mad_ctpsa_mnrm,
  mad_ctpsa_minv!,
  mad_ctpsa_pminv!,
  mad_ctpsa_compose!,
  mad_ctpsa_translate!,
  mad_ctpsa_eval!,
  mad_ctpsa_mconv!,
  mad_ctpsa_print,
  mad_ctpsa_scan,
  mad_ctpsa_scan_hdr,
  mad_ctpsa_scan_coef!,
  mad_ctpsa_debug,
  mad_ctpsa_isvalid,
  mad_ctpsa_init!,

  # Layer 2 structs + functions NOT in Base:
  Descriptor,
  TPS,
  ComplexTPS,
  print,
  unit  ,
  sinhc ,
  asinc ,
  asinhc,
  erf   ,
  erfc ,
  setname!,
  vars,
  params,
  complexvars,
  complexparams


# Low-level functions/structs and constants
const NAMSZ::Integer = 16 
include("mono.jl")
include("desc.jl")
include("rtpsa.jl")
include("ctpsa.jl")

const MAD_TPSA = :("libgtpsa")
const MAD_TPSA_DEFAULT::Cuchar = 255
const MAD_TPSA_SAME::Cuchar = 254
const MAD_DESC_CURR::Ptr{Desc} = C_NULL

# Wrapper struct for Ptr{Desc}
struct Descriptor
  desc::Ptr{Desc}
end

# Descriptor outer constructors
"""
    Descriptor(nv::Integer, mo::Integer)::Descriptor

Creates a TPSA Descriptor with `nv` variables of maximum order `mo`.

### Input
- `nv` -- Number of variables in the TPSA
- `mo` -- Maximum order of the variables in the TPSA
"""
function Descriptor(nv::Integer, mo::Integer)::Descriptor
  return Descriptor(mad_desc_newv(convert(Cint, nv), convert(Cuchar, mo)))
end

"""
    Descriptor(mos::Vector{<:Integer})::Descriptor

Creates a TPSA Descriptor with `length(mos)` variables with individual max 
orders specified in the Vector `mos`. 

### Input
- `mos` -- Vector of the individual max orders of each variable
"""
function Descriptor(mos::Vector{<:Integer})::Descriptor
  nv = length(mos)
  np = 0
  mo = maximum(mos)
  po = 0
  no = mos
  return Descriptor(mad_desc_newvpo(convert(Cint, nv), convert(Cuchar, mo), convert(Cint, np), convert(Cuchar, po), Base.unsafe_convert(Ptr{Cuchar}, convert(Vector{Cuchar}, no))))
end

"""
    Descriptor(nv::Integer, mo::Integer, np::Integer, po::Integer)::Descriptor

Creates a TPSA Descriptor with `nv` variables of maximum order `mo`, and `np` parameters
of maximum order `po` (`<= mo`).

### Input
- `nv` -- Number of variables in the TPSA
- `mo` -- Maximum order of the variables in the TPSA
- `np` -- Number of parameters in the TPSA
- `po` -- Maximum order of the parameters (`<= mo`) in the TPSA
"""
function Descriptor(nv::Integer, mo::Integer, np::Integer, po::Integer)::Descriptor
  return Descriptor(mad_desc_newvp(convert(Cint, nv), convert(Cuchar, mo), convert(Cint, np), convert(Cuchar, po)))
end


"""
    Descriptor(mos::Vector{<:Integer}, pos::Vector{<:Integer})::Descriptor

Creates a TPSA Descriptor with `length(mos)` variables with individual max 
orders specified in `mos`, and `length(pos)` parameters with individual max 
orders specified in `pos`.

### Input
- `mos` -- Vector of the individual max orders of each variable
- `pos` -- Vector of the individual max orders of each parameter
"""
function Descriptor(mos::Vector{<:Integer}, pos::Vector{<:Integer})::Descriptor
  nv = length(mos)
  np = length(pos)
  mo = maximum(mos)
  po = maximum(pos)
  no = vcat(mos,pos)
  return Descriptor(mad_desc_newvpo(convert(Cint, nv), convert(Cuchar, mo), convert(Cint, np), convert(Cuchar, po), Base.unsafe_convert(Ptr{Cuchar}, convert(Vector{Cuchar}, no))))
end

# Wrapper struct for Ptr{RTPSA}
mutable struct TPS
  tpsa::Ptr{RTPSA}
  function TPS(t1::Ptr{RTPSA})::TPS
    t = new(t1)
    f(x) = mad_tpsa_del!(x.tpsa)
    finalizer(f,t)
    return t
  end
end

# RTPSA outer constructors
"""
    TPS()::TPS

Creates a new Truncated Power Series `TPS` using the most 
recently-defined `Descriptor`
"""
function TPS()::TPS
  return TPS(mad_tpsa_newd(MAD_DESC_CURR, MAD_TPSA_DEFAULT))
end

"""
    TPS(d::Descriptor)::TPS

Creates a new Truncated Power Series `TPS` based on `d` 

### Input
- `d` -- `Descriptor`
"""
function TPS(d::Descriptor)::TPS
  return TPS(mad_tpsa_newd(d.desc, MAD_TPSA_DEFAULT))
end

"""
    TPS(t1::TPS)::TPS

Copy constructor for `TPS`

### Input
- `t1` -- `TPS` to create new copy of
"""
function TPS(t1::TPS)::TPS
  t = TPS(mad_tpsa_new(t1.tpsa, MAD_TPSA_SAME))
  mad_tpsa_copy!(t1.tpsa, t.tpsa)
  return t
end

"""
    TPS(a::Real)::TPS

Promotes the scalar `a` to a new `TPS` using the most 
recently-defined `Descriptor`.

### Input
- `a` -- Scalar to create new `TPS` with
"""
function TPS(a::Real)::TPS
  t = TPS()
  mad_tpsa_set0!(t.tpsa, 1., convert(Float64,a))
  return t
end

"""
    TPS(a::Real, t1::TPS)::TPS

Promotes the scalar `a` to a new `TPS` using the same 
`Descriptor` as `t1`


### Input
- `a`  -- Scalar to create new `TPS` with
- `t1` -- `TPS` to use same `Descriptor` as
"""
function TPS(a::Real, t1::TPS)::TPS
  t = zero(t1)
  mad_tpsa_set0!(t.tpsa, 1., convert(Float64,a))
  return t
end

#=
"""
    TPS(a::Real, ct1::ComplexTPS)::TPS

Promotes the scalar `a` to a new `TPS` using the same 
`Descriptor` as `ct1`


### Input
- `a`   -- Scalar to create new `TPS` with
- `ct1` -- `ComplexTPS` to use same `Descriptor` as
"""
function TPS(a::Real, ct1::ComplexTPS)::TPS
  t = TPS(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA}, ct1.tpsa), MAD_TPSA_SAME))
  mad_tpsa_set0!(t.tpsa, 1., convert(Float64, a))
  return t
end
=#


# Wrapper struct for Ptr{CTPSA}
mutable struct ComplexTPS
  tpsa::Ptr{CTPSA}
  function ComplexTPS(ct1::Ptr{CTPSA})::ComplexTPS
    ct = new(ct1)
    f(x) = mad_ctpsa_del!(x.tpsa)
    finalizer(f,ct)
    return ct
  end
end

# ComplexTPS outer constructors
"""
    ComplexTPS()::ComplexTPS

Creates a new Complex Truncated Power Series `ComplexTPS` using 
the most recently-defined `Descriptor`
"""
function ComplexTPS()::ComplexTPS
  return ComplexTPS(mad_ctpsa_newd(MAD_DESC_CURR, MAD_TPSA_DEFAULT))
end


"""
    ComplexTPS(d::Descriptor)::ComplexTPS

Creates a new Complex Truncated Power Series `ComplexTPS` based on `d` 

### Input
- `d` -- `Descriptor`
"""
function ComplexTPS(d::Descriptor)::ComplexTPS
  return ComplexTPS(mad_ctpsa_newd(d.desc, MAD_TPSA_DEFAULT))
end

"""
    ComplexTPS(ct1::ComplexTPS)::ComplexTPS

Copy constructor for `ComplexTPS`

### Input
- `ct1` -- `ComplexTPS` to create new copy of
"""
function ComplexTPS(ct1::ComplexTPS)::ComplexTPS
  ct = ComplexTPS(mad_ctpsa_new(ct1.tpsa, MAD_TPSA_SAME))
  mad_ctpsa_copy!(ct1.tpsa, ct.tpsa)
  return ct
end

"""
    ComplexTPS(t1::TPS)::ComplexTPS

Creates a new copy of `TPS` promoted to a `ComplexTPS`

### Input
- `t1` -- `TPS` to create new `ComplexTPS` from
"""
function ComplexTPS(t1::TPS)::ComplexTPS
  ct = ComplexTPS(mad_ctpsa_new(Base.unsafe_convert(Ptr{CTPSA}, t1.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_cplx!(ct1.tpsa, Base.unsafe_convert(Ptr{RTPSA}, C_NULL), ct.tpsa)
  return ct
end

"""
    ComplexTPS(a::Number)::ComplexTPS

Promotes the scalar `a` to a new `ComplexTPS` using the most 
recently-defined `Descriptor`

### Input
- `a` -- Scalar to create new `ComplexTPS` with
"""
function TPS(a::Number)::ComplexTPS
  ct = ComplexTPS()
  mad_ctpsa_set0!(ct.tpsa, 1., convert(ComplexF64,a))
  return ct
end

"""
    ComplexTPS(a::Number, ct1::ComplexTPS)::ComplexTPS

Promotes the scalar `a` to a new `ComplexTPS` using the same
`Descriptor` as `ct1`

### Input
- `a`    -- Scalar to create new `ComplexTPS` with
- `ct1`  -- `ComplexTPS` to use same `Descriptor` as
"""
function ComplexTPS(a::Number, ct1::ComplexTPS)::ComplexTPS
  ct = zero(ct1)
  mad_ctpsa_set0!(ct.tpsa, 1., convert(ComplexF64, a))
  return ct
end

"""
    ComplexTPS(a::Number, t1::TPS)::ComplexTPS

Promotes the scalar `a` to a new `ComplexTPS` using the same
`Descriptor` as `t1`

### Input
- `a`   -- Scalar to create new `ComplexTPS` with
- `t1`  -- `TPS` to use same `Descriptor` as
"""
function ComplexTPS(a::Number, t1::TPS)::ComplexTPS
  ct = ComplexTPS(mad_ctpsa_new(Base.unsafe_convert(Ptr{CTPSA}, t1.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_set0!(ct.tpsa, 1., convert(ComplexF64,a))
  return ct
end

# --- Variable/parameter generators ---

"""
    vars(d::Descriptor)::Vector{TPS}

Returns `TPS`s corresponding to the variables for the `Descriptor`.

### Input
- `d` -- TPSA `Descriptor`

### Output
- `x` -- Vector containing unit `TPS`s corresponding to each variable
"""
function vars(d::Descriptor)::Vector{TPS}
  desc = unsafe_load(d.desc)
  nv = desc.nv
  x = Vector{TPS}(undef, nv)
  ords = zeros(Int, nv)
  for i=1:nv
    ords[i] = 1
    t = TPS(d)
    mad_tpsa_setm!(t.tpsa, convert(Cint, length(ords)), Base.unsafe_convert(Ptr{Cuchar}, convert(Vector{Cuchar}, ords)), convert(Cdouble, 0), convert(Cdouble, 1))
    x[i] = t
    ords[i] = 0
  end
  return x
end

"""
    params(d::Descriptor)::Vector{TPS}

Returns `TPS`s corresponding to the parameters for the `Descriptor`.

### Input
- `d` -- TPSA `Descriptor`

### Output
- `k` -- Vector containing unit `TPS`s corresponding to each parameter
"""
function params(d::Descriptor)::Vector{TPS}
  desc = unsafe_load(d.desc)
  nv = desc.nv
  np = desc.np
  k = Vector{TPS}(undef, np)
  ords = zeros(Int, nv+np)
  for i=nv+1:nv+np
    ords[i] = 1
    t = TPS(d)
    mad_tpsa_setm!(t.tpsa, convert(Cint, length(ords)), Base.unsafe_convert(Ptr{Cuchar}, convert(Vector{Cuchar}, ords)), convert(Cdouble, 0), convert(Cdouble, 1))
    k[i-nv] = t
    ords[i] = 0
  end
  return k
end

#= No longer needed:
"""
    complexvars(d::Descriptor)::Vector{ComplexTPS}

Returns `ComplexTPS`s corresponding to the variables for the `Descriptor`.

### Input
- `d` -- TPSA `Descriptor`

### Output
- `x` -- Vector containing unit `ComplexTPS`s corresponding to each variable
"""
function complexvars(d::Descriptor)::Vector{ComplexTPS}
  desc = unsafe_load(d.desc)
  nv = desc.nv
  x = Vector{ComplexTPS}(undef, nv)
  ords = zeros(Int, nv)
  for i=1:nv
    ords[i] = 1
    t = ComplexTPS(d)
    mad_ctpsa_setm!(t.tpsa, convert(Cint, length(ords)), Base.unsafe_convert(Ptr{Cuchar}, convert(Vector{Cuchar}, ords)), convert(ComplexF64, 0), convert(ComplexF64, 1))
    x[i] = t
    ords[i] = 0
  end
  return x
end

"""
    complexparams(d::Descriptor)::Vector{TPS}

Returns `ComplexTPS`s corresponding to the parameters for the `Descriptor`.

### Input
- `d` -- TPSA `Descriptor`

### Output
- `k` -- Vector containing unit `ComplexTPS`s corresponding to each parameter
"""
function complexparams(d::Descriptor)::Vector{ComplexTPS}
  desc = unsafe_load(d.desc)
  nv = desc.nv
  np = desc.np
  k = Vector{ComplexTPS}(undef, np)
  ords = zeros(Int, nv+np)
  for i=nv+1:nv+np
    ords[i] = 1
    t = ComplexTPS(d)
    mad_ctpsa_setm!(t.tpsa, convert(Cint, length(ords)), Base.unsafe_convert(Ptr{Cuchar}, convert(Vector{Cuchar}, ords)), convert(ComplexF64, 0), convert(ComplexF64, 1))
    k[i-nv] = t
    ords[i] = 0
  end
  return k
end
=#



# --- Getters ---
function getindex(t::TPS, ords::Integer...)::Float64
  return mad_tpsa_getm(t.tpsa, convert(Cint, length(ords)), Base.unsafe_convert(Ptr{Cuchar}, convert(Vector{Cuchar}, [ords...])))
end

function getindex(t::TPS, vars::Pair{<:Integer, <:Integer}...; params::Tuple{Vararg{Pair{<:Integer,<:Integer}}}=())::Float64
  # Need to create array of orders with length nv + np
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d))
  nv = desc.nv
  np = desc.np
  ords = zeros(Int, nv+np)
  for var in vars
    ords[var.first] = convert(Int, var.second)
  end
  for param in params
    ords[nv + param.first] = convert(Int, param.second)
  end
  return mad_tpsa_getm(t.tpsa, convert(Cint, length(ords)), Base.unsafe_convert(Ptr{Cuchar}, convert(Vector{Cuchar}, ords)))
end


function getindex(ct::ComplexTPS, ords::Integer...)::ComplexF64
  return mad_ctpsa_getm(ct.tpsa, convert(Cint, length(ords)), Base.unsafe_convert(Ptr{Cuchar}, convert(Vector{Cuchar}, [ords...])))
end

function getindex(ct::ComplexTPS, vars::Pair{<:Integer, <:Integer}...; params::Tuple{Vararg{Pair{<:Integer,<:Integer}}}=())::ComplexF64
  # Need to create array of orders with length nv + np
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(ct.tpsa).d))
  nv = desc.nv
  np = desc.np
  ords = zeros(Int, nv+np)
  for var in vars
    ords[var.first] = convert(Int, var.second)
  end
  for param in params
    ords[nv + param.first] = convert(Int, param.second)
  end
  return mad_ctpsa_getm(ct.tpsa, convert(Cint, length(ords)), Base.unsafe_convert(Ptr{Cuchar}, convert(Vector{Cuchar}, ords)))
end


# --- Setters ---
function setindex!(t::TPS, v::Real, ords::Integer...)
  mad_tpsa_setm!(t.tpsa, convert(Cint, length(ords)), Base.unsafe_convert(Ptr{Cuchar}, convert(Vector{Cuchar}, [ords...])), convert(Cdouble, 0), convert(Cdouble, v))
end

function setindex!(t::TPS, v::Real, vars::Pair{<:Integer, <:Integer}...; params::Tuple{Vararg{Pair{<:Integer,<:Integer}}}=())
  # Need to create array of orders with length nv + np
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d))
  nv = desc.nv
  np = desc.np
  ords = zeros(Int, nv+np)
  for var in vars
    ords[var.first] = convert(Int, var.second)
  end
  for param in params
    ords[nv + param.first] = convert(Int, param.second)
  end
  mad_tpsa_setm!(t.tpsa, convert(Cint, length(ords)), Base.unsafe_convert(Ptr{Cuchar}, convert(Vector{Cuchar}, ords)), convert(Cdouble, 0), convert(Cdouble, v))
end

function setindex!(ct::ComplexTPS, v::Number, ords::Integer...)
  mad_ctpsa_setm!(ct.tpsa, convert(Cint, length(ords)), Base.unsafe_convert(Ptr{Cuchar}, convert(Vector{Cuchar}, [ords...])), convert(ComplexF64, 0), convert(ComplexF64, v))
end

function getindex(ct::ComplexTPS, v::Number, vars::Pair{<:Integer, <:Integer}...; params::Tuple{Vararg{Pair{<:Integer,<:Integer}}}=())::ComplexF64
  # Need to create array of orders with length nv + np
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(ct.tpsa).d))
  nv = desc.nv
  np = desc.np
  ords = zeros(Int, nv+np)
  for var in vars
    ords[var.first] = convert(Int, var.second)
  end
  for param in params
    ords[nv + param.first] = convert(Int, param.second)
  end
  mad_ctpsa_setm!(ct.tpsa, convert(Cint, length(ords)), Base.unsafe_convert(Ptr{Cuchar}, convert(Vector{Cuchar}, ords)), convert(ComplexF64, 0), convert(ComplexF64, v))
end

#=

function setname!(t::TPS, nam::String)
  mad_tpsa_setnam!(t.tpsa, Base.unsafe_convert(Cstring, nam))
end
=#
# --- print ---
#=
function show(io::IO, t::TPS)
  # Get nn (length of monomial)
  tpsa = unsafe_load(t.tpsa)
  name = Base.unsafe_convert(String, tpsa.nam)
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, tpsa))
  nv = desc.nv
  np = desc.np
  nn = desc.nn

  m = @ccall malloc(sizeof(Cuchar)*desc.n)
  v = @ccall malloc(sizeof(Cdouble))
  i = Cint(-2)
  while i != Cint(-1)
    i += Cint(1)
    i = mad_tpsa_cycle!(tpsa, i, desc.nn, m, v)

  end



end=#


function print(t::TPS)
  mad_tpsa_print(t.tpsa, Base.unsafe_convert(Cstring, ""), 0.,Int32(0),C_NULL)
end

function print(t::ComplexTPS)
  mad_ctpsa_print(t.tpsa, Base.unsafe_convert(Cstring, ""), 0.,Int32(0),C_NULL)
end

# --- convert ---
function convert(::Type{TPS}, v::Real)::TPS
  t = TPS()
  mad_tpsa_setval!(t.tpsa, convert(Cdouble, v))
  return t
end

function convert(::Type{ComplexTPS}, v::Number)::TPS
  t = TPS()
  mad_ctpsa_setval!(t.tpsa, convert(ComplexF64, v))
  return t
end

# -- zero -- 
@inline function zero(t::TPS)::TPS
  return TPS(mad_tpsa_new(t.tpsa, MAD_TPSA_SAME))
end

@inline function zero(::Type{TPS})::TPS
  return TPS()
end

@inline function zero(ct::ComplexTPS)::ComplexTPS
  return ComplexTPS(mad_ctpsa_new(ct.tpsa, MAD_TPSA_SAME))
end

@inline function zero(::Type{ComplexTPS})::ComplexTPS
  return ComplexTPS()
end

include("operators.jl")

end
