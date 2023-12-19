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
              length

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
  TPSA,
  ComplexTPSA,
  print,
  unit  ,
  sinhc ,
  asinc ,
  asinhc,
  erf   ,
  erfc ,
  setname!


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
    Descriptor(nv::Integer, mo::Integer)

Creates a TPSA Descriptor with `nv` variables of maximum order `mo`.

### Input
- `nv` -- Number of variables in the TPSA
- `mo` -- Maximum order of the variables in the TPSA
"""
function Descriptor(nv::Integer, mo::Integer)::Descriptor
  return Descriptor(mad_desc_newv(convert(Cint, nv), convert(Cuchar, mo)))
end


"""
    Descriptor(nv::Integer, mo::Integer, np::Integer, po::Integer)

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
    Descriptor(nv::Integer, np::Integer,no::Vector{<:Integer})

Creates a TPSA Descriptor with `nv` variables and `np` parameters of with individual 
variable/parameter orders specified in the Vector `no`. 

### Input
- `nv` -- Number of variables in the TPSA
- `np` -- Number of parameters in the TPSA
- `no` -- Vector of the individual orders of each variable and parameter, with variables first `nv` entries and parameters last `np` entries
"""
function Descriptor(nv::Integer, np::Integer, no::Vector{<:Integer})::Descriptor
  if length(no) != nv+np
    println("Invalid length for no (must be nv + np). Descriptor not constructed.")
    return
  end
  mo = maximum(no[1:nv])
  po = maximum(no[end-np+1:end])
  return Descriptor(mad_desc_newvpo(convert(Cint, nv), convert(Cuchar, mo), convert(Cint, np), convert(Cuchar, po), Base.unsafe_convert(Ptr{Cuchar}, convert(Vector{Cuchar}, no))))
end


# Wrapper struct for Ptr{RTPSA}
abstract type AbstractTPSA end

mutable struct TPSA <: AbstractTPSA
  tpsa::Ptr{RTPSA}
  function TPSA(t1::Ptr{RTPSA})
    #t = new(mad_tpsa_new(t1, MAD_TPSA_DEFAULT))
    #mad_tpsa_copy!(t1, t.tpsa)
    #mad_tpsa_clear!(t1)
    t = new(t1)
    f(x) = mad_tpsa_del!(x.tpsa)
    finalizer(f,t)
    return t
  end
end

# RTPSA outer constructors
"""
    TPSA()

Creates a `TPSA` using the most recently-defined `Descriptor`
"""
function TPSA()
  return TPSA(mad_tpsa_newd(MAD_DESC_CURR, MAD_TPSA_DEFAULT))
end

"""
    TPSA(d::Descriptor)

Creates a `TPSA` based on `d` 

### Input
- `d` -- `Descriptor`
"""
function TPSA(d::Descriptor)
  return TPSA(mad_tpsa_newd(d.desc, MAD_TPSA_DEFAULT))
end

"""
    TPSA(t1::TPSA)

Creates a `TPSA` with the same `Descriptor` as `t1`

### Input
- `t1` -- TPSA to create new TPSA from with same `Descriptor`
"""
function TPSA(t1::TPSA)
  return TPSA(mad_tpsa_new(t1.tpsa, MAD_TPSA_DEFAULT))
end

"""
    TPSA(t1::AbstractTPSA)

Creates a `TPSA` with the same `Descriptor` as `t1`

### Input
- `t1` -- TPSA to create new TPSA from with same `Descriptor`
"""
function TPSA(t1::AbstractTPSA)
  return TPSA(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA}, t1.tpsa), MAD_TPSA_DEFAULT))
end


# Wrapper struct for Ptr{CTPSA}
mutable struct ComplexTPSA <: AbstractTPSA
  tpsa::Ptr{CTPSA}
  function ComplexTPSA(t1::Ptr{CTPSA})
    t = new(t1)
    f(x) = mad_ctpsa_del!(x.tpsa)
    finalizer(f,t)
    return t
  end
end

# ComplexTPSA outer constructors
"""
    ComplexTPSA()

Creates a `ComplexTPSA` using the most recently-defined `Descriptor`
"""
function ComplexTPSA()
  return ComplexTPSA(mad_ctpsa_newd(MAD_DESC_CURR, MAD_TPSA_DEFAULT))
end


"""
    ComplexTPSA(d::Descriptor)

Creates a `ComplexTPSA` based on `d` 

### Input
- `d` -- `Descriptor`
"""
function ComplexTPSA(d::Descriptor)
  return ComplexTPSA(mad_ctpsa_newd(d.desc, MAD_TPSA_DEFAULT))
end

"""
    ComplexTPSA(t1::ComplexTPSA)

Creates a `ComplexTPSA` with the same `Descriptor` as `t1`

### Input
- `t1` -- TPSA to create new TPSA from with same `Descriptor`
"""
function ComplexTPSA(t1::ComplexTPSA)
  return ComplexTPSA(mad_ctpsa_new(t1.tpsa, MAD_TPSA_DEFAULT))
end

"""
    ComplexTPSA(t1::AbstractTPSA)

Creates a `ComplexTPSA` with the same `Descriptor` as `t1`

### Input
- `t1` -- TPSA to create new TPSA from with same `Descriptor`
"""
function ComplexTPSA(t1::AbstractTPSA)
  return ComplexTPSA(mad_ctpsa_new(Base.unsafe_convert(Ptr{CTPSA}, t1.tpsa), MAD_TPSA_DEFAULT))
end



# --- Getters ---
function getindex(t::TPSA, ords::Integer...)::Float64
  return mad_tpsa_getm(t.tpsa, convert(Cint, length(ords)), Base.unsafe_convert(Ptr{Cuchar}, convert(Vector{Cuchar}, [ords...])))
end

function getindex(t::ComplexTPSA, ords::Integer...)::ComplexF64
  return mad_ctpsa_getm(t.tpsa, convert(Cint, length(ords)), Base.unsafe_convert(Ptr{Cuchar}, convert(Vector{Cuchar}, [ords...])))
end

# --- Setters ---
function setindex!(t::TPSA, v::Real, ords::Integer...)
  mad_tpsa_setm!(t.tpsa, convert(Cint, length(ords)), Base.unsafe_convert(Ptr{Cuchar}, convert(Vector{Cuchar}, [ords...])), convert(Cdouble, 0), convert(Cdouble, v))
end

function setindex!(t::ComplexTPSA, v::ComplexF64, ords::Integer...)
  mad_ctpsa_setm!(t.tpsa, convert(Cint, length(ords)), Base.unsafe_convert(Ptr{Cuchar}, convert(Vector{Cuchar}, [ords...])), convert(ComplexF64, 0), convert(ComplexF64, v))
end

function setname!(t::TPSA, nam::String)
  mad_tpsa_setnam!(t.tpsa, Base.unsafe_convert(Cstring, nam))
end

# --- print ---
#=
function show(io::IO, t::TPSA)
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


function print(t::TPSA)
  mad_tpsa_print(t.tpsa, Base.unsafe_convert(Cstring, ""), 0.,Int32(0),C_NULL)
end

function print(t::ComplexTPSA)
  mad_ctpsa_print(t.tpsa, Base.unsafe_convert(Cstring, ""), 0.,Int32(0),C_NULL)
end



# -- zero -- (For LinearAlgebra overloading)
function zero(a::TPSA)
  return TPSA()
end

function zero(a::ComplexTPSA)
  return ComplexTPSA()
end

include("operators.jl")

end
