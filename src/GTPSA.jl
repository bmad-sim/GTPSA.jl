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
              real  ,
              imag  ,
              conj  ,
              angle ,
              complex, 
              getindex,
              setindex!,
              ==,
              <,
              >,
              <=,
              >=,
              !=,
              isequal,
              show

using GTPSA_jll, Printf, PrettyTables

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
  unit  ,
  sinhc ,
  asinc ,
  asinhc,
  erf   ,
  erfc ,
  norm,
  vars,
  params,
  complexvars,
  complexparams,
  polar,
  rect, 

  # Convenience getters:
  gradient,
  gradient!,
  jacobian,
  jacobian!,
  hessian,
  hessian!,

  # Methods:
  evaluate,
  integrate,
  differentiate,
  getord,
  poisbra,
  liebra,
  


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
const NAMSZ = 16 
include("low_level/mono.jl")
include("low_level/desc.jl")
include("low_level/rtpsa.jl")
include("low_level/ctpsa.jl")

const MAD_TPSA = :("libgtpsa")
const MAD_TPSA_DEFAULT::Cuchar = 255
const MAD_TPSA_SAME::Cuchar = 254
const MAD_DESC_CURR::Ptr{Desc} = C_NULL
const DESC_MAX_TMP = 8

# Wrapper struct for Ptr{Desc}
struct Descriptor
  desc::Ptr{Desc}
end

# Descriptor outer constructors
"""
    Descriptor(nv::Integer, vo::Integer)::Descriptor

Creates a TPSA `Descriptor` with `nv` variables of maximum order `vo` for each.

### Input
- `nv` -- Number of variables in the TPSA
- `vo` -- Maximum order of the variables in the TPSA
"""
function Descriptor(nv::Integer, vo::Integer)::Descriptor
  return Descriptor(mad_desc_newv(convert(Cint, nv), convert(Cuchar, vo)))
end

"""
    Descriptor(vos::Vector{<:Integer})::Descriptor

Creates a TPSA `Descriptor` with `length(mos)` variables with individual truncation 
orders specified in the Vector `vos`. 

### Input
- `vos` -- `Vector` of the individual truncation orders of each variable
"""
function Descriptor(vos::Vector{<:Integer})::Descriptor
  nv = length(vos)
  np = 0
  mo = maximum(vos)
  po = 0
  no = vos
  return Descriptor(mad_desc_newvpo(convert(Cint, nv), convert(Cuchar, mo), convert(Cint, np), convert(Cuchar, po), convert(Vector{Cuchar}, no)))
end

"""
    Descriptor(nv::Integer, vo::Integer, np::Integer, po::Integer)::Descriptor

Creates a TPSA `Descriptor` with `nv` variables each with truncation order `vo`, and `np` 
parameters each with truncation order `po`

### Input
- `nv` -- Number of variables in the TPSA
- `vo` -- Truncation order of the variables in the TPSA
- `np` -- Number of parameters in the TPSA
- `po` -- Truncation order of the parameters
"""
function Descriptor(nv::Integer, vo::Integer, np::Integer, po::Integer)::Descriptor
  mo = max(vo,po)
  no = vcat(Cuchar(vo)*ones(Cuchar,nv), Cuchar(po)*ones(Cuchar,np))
  return Descriptor(mad_desc_newvpo(convert(Cint, nv), convert(Cuchar, mo), convert(Cint, np), convert(Cuchar, po), no))
end


"""
    Descriptor(vos::Vector{<:Integer}, pos::Vector{<:Integer})::Descriptor

Creates a TPSA `Descriptor` with `length(vos)` variables with individual truncation 
orders specified in `vos`, and `length(pos)` parameters with individual truncation 
orders specified in `pos`.

### Input
- `vos` -- `Vector` of the individual truncation orders of each variable
- `pos` -- `Vector` of the individual truncation orders of each parameter
"""
function Descriptor(vos::Vector{<:Integer}, pos::Vector{<:Integer})::Descriptor
  nv = length(vos)
  np = length(pos)
  po = maximum(pos)
  mo = max(maximum(vos),po)
  no = vcat(vos,pos)
  return Descriptor(mad_desc_newvpo(convert(Cint, nv), convert(Cuchar, mo), convert(Cint, np), convert(Cuchar, po), convert(Vector{Cuchar}, no)))
end

# Wrapper struct for Ptr{RTPSA}
mutable struct TPS <: Number
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

# Unsafe constructors using most recently-defined Descriptor:
"""
    unsafe_TPS()::TPS

Creates a new Truncated Power Series `TPS`, however only 
uses the most recently-defined `Descriptor`. Therefore, this can 
be unsafe if more than one `Descriptor` is defined.
"""
function unsafe_TPS()::TPS
  return TPS(mad_tpsa_newd(MAD_DESC_CURR, MAD_TPSA_DEFAULT))
end

"""
    unsafe_TPS(a::Real)::TPS

Promotes the scalar `a` to a new `TPS`, however only 
uses the most recently-defined `Descriptor`. Therefore, this can 
be unsafe if more than one `Descriptor` is defined.

### Input
- `a` -- Scalar to create new `TPS` with
"""
function unsafe_TPS(a::Real)::TPS
  t = unsafe_TPS()
  mad_tpsa_set0!(t.tpsa, 1., convert(Float64,a))
  return t
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

# ComplexTPS outer constructors
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
  mad_ctpsa_cplx!(t1.tpsa, Base.unsafe_convert(Ptr{RTPSA}, C_NULL), ct.tpsa)
  return ct
end

"""
    ComplexTPS(t1::TPS, t2::TPS)::ComplexTPS

Creates a new `ComplexTPS` equal to `t1 + im*t2`

### Input
- `t1` -- Real part of `ComplexTPS` as a `TPS` 
- `t2` -- Imaginary part of `ComplexTPS` as a `TPS` 
"""
function ComplexTPS(t1::TPS, t2::TPS)::ComplexTPS
  ct = ComplexTPS(mad_ctpsa_new(Base.unsafe_convert(Ptr{CTPSA}, t1.tpsa), MAD_TPSA_SAME))
  mad_ctpsa_cplx!(t1.tpsa, t2.tpsa, ct.tpsa)
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
  mad_ctpsa_set0!(ct.tpsa, convert(ComplexF64, 1), convert(ComplexF64, a))
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

# Unsafe constructors using most recently-defined Descriptor:
"""
    unsafe_ComplexTPS()::ComplexTPS

Creates a new Complex Truncated Power Series `ComplexTPS`, however only 
uses the most recently-defined `Descriptor`. Therefore, this can 
be unsafe if more than one `Descriptor` is defined.
"""
function unsafe_ComplexTPS()::ComplexTPS
  return ComplexTPS(mad_ctpsa_newd(MAD_DESC_CURR, MAD_TPSA_DEFAULT))
end

"""
    unsafe_ComplexTPS(a::Number)::ComplexTPS

Promotes the scalar `a` to a new `ComplexTPS`, however only 
uses the most recently-defined `Descriptor`. Therefore, this can 
be unsafe if more than one `Descriptor` is defined.

### Input
- `a` -- Scalar to create new `ComplexTPS` with
"""
function unsafe_ComplexTPS(a::Number)::ComplexTPS
  ct = unsafe_ComplexTPS()
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
- `x` -- `Vector` containing unit `TPS`s corresponding to each variable
"""
function vars(d::Descriptor)::Vector{TPS}
  desc = unsafe_load(d.desc)
  nv = desc.nv
  x = Vector{TPS}(undef, nv)
  ords = zeros(Cuchar, nv)
  for i=1:nv
    ords[i] = 0x1
    t = TPS(d)
    mad_tpsa_setm!(t.tpsa, nv, ords, 0.0, 1.0)
    x[i] = t
    ords[i] = 0x0
  end
  return x
end

"""
    params(d::Descriptor)::Vector{TPS}

Returns `TPS`s corresponding to the parameters for the `Descriptor`.

### Input
- `d` -- TPSA `Descriptor`

### Output
- `k` -- `Vector` containing unit `TPS`s corresponding to each parameter
"""
function params(d::Descriptor)::Vector{TPS}
  desc = unsafe_load(d.desc)
  nv = desc.nv
  np = desc.np
  k = Vector{TPS}(undef, np)
  ords = zeros(Cuchar, nv+np)
  for i=nv+1:nv+np
    ords[i] = 0x1
    t = TPS(d)
    mad_tpsa_setm!(t.tpsa, nv+np, ords, 0.0, 1.0)
    k[i-nv] = t
    ords[i] = 0x0
  end
  return k
end


"""
    complexvars(d::Descriptor)::Vector{ComplexTPS}

Returns `ComplexTPS`s corresponding to the variables for the `Descriptor`.

### Input
- `d` -- TPSA `Descriptor`

### Output
- `x` -- `Vector` containing unit `ComplexTPS`s corresponding to each variable
"""
function complexvars(d::Descriptor)::Vector{ComplexTPS}
  desc = unsafe_load(d.desc)
  nv = desc.nv
  x = Vector{ComplexTPS}(undef, nv)
  ords = zeros(Cuchar, nv)
  for i=1:nv
    ords[i] = 0x1
    t = ComplexTPS(d)
    mad_ctpsa_setm!(t.tpsa, nv, ords, convert(ComplexF64, 0), convert(ComplexF64, 1))
    x[i] = t
    ords[i] = 0x0
  end
  return x
end

"""
    complexparams(d::Descriptor)::Vector{TPS}

Returns `ComplexTPS`s corresponding to the parameters for the `Descriptor`.

### Input
- `d` -- TPSA `Descriptor`

### Output
- `k` -- `Vector` containing unit `ComplexTPS`s corresponding to each parameter
"""
function complexparams(d::Descriptor)::Vector{ComplexTPS}
  desc = unsafe_load(d.desc)
  nv = desc.nv
  np = desc.np
  k = Vector{ComplexTPS}(undef, np)
  ords = zeros(Cuchar, nv+np)
  for i=nv+1:nv+np
    ords[i] = 0x1
    t = ComplexTPS(d)
    mad_ctpsa_setm!(t.tpsa, nv+np, ords, convert(ComplexF64, 0), convert(ComplexF64, 1))
    k[i-nv] = t
    ords[i] = 0x0
  end
  return k
end

# Function to convert var=>ord, params=(param=>ord,) to sparse monomial format (varidx1, ord1, varidx2, ord2, paramidx, ordp1,...)
function pairs_to_sm(t::Union{TPS,ComplexTPS}, vars::Pair{<:Integer, <:Integer}...; params::Tuple{Vararg{Pair{<:Integer,<:Integer}}}=())::Tuple{Vector{Cint}, Cint}
  # WE MUST Order THE VARIABLES !!!
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d))
  nv = desc.nv # TOTAL NUMBER OF VARS!!!!!!
  numv = Cint(length(vars))
  nump = Cint(length(params))
  imin = min(minimum(x->x.first, vars,init=typemax(Int)), minimum(x->x.first+nv, params,init=typemax(Int)))
  imax = max(maximum(x->x.first, vars,init=0), maximum(x->x.first+nv, params,init=0))
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
function pairs_to_m(t::Union{TPS,ComplexTPS}, vars::Pair{<:Integer, <:Integer}...; params::Tuple{Vararg{Pair{<:Integer,<:Integer}}}=())::Tuple{Vector{UInt8}, Cint}
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d))
  nv = desc.nv
  n = Cint(0)
  if isempty(params)
    n = Cint(maximum(map(x->x.first, vars)))
  else
    n = Cint(maximum(map(x->x.first, params))) + nv
  end
  ords = zeros(Cuchar, n)
  for var in vars
    ords[var.first] = convert(Cuchar, var.second)
  end
  for param in params
    ords[nv + param.first] = convert(Cuchar, param.second)
  end
  return ords, n
end

include("getset.jl")
include("show.jl")
include("operators.jl")
include("methods.jl")
include("fast_gtpsa.jl")

end
