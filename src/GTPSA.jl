module GTPSA

import Base:  *, 
              +,
              -,
              *,
              /,
              ^,
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
              print,
              zero,
              real,
              imag,
              getindex,
              firstindex,
              lastindex,
              setindex!

using GTPSA_jll

export
  # Constants:
  NAMSZ,
  MAD_TPSA, 
  MAD_TPSA_DEFAULT, 
  MAD_TPSA_SAME,
  MAD_DESC_CURR,
  
  # Julia Structs:
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

  Descriptor,
  TPSA,
  ComplexTPSA,
  print,
  unit  ,
  sinhc ,
  asinc ,
  asinhc,
  erf   ,
  erfc  ,
  getproperty

const NAMSZ::Integer = 16

struct Desc{T,C}
  id::Cint                        # index in list of registered descriptors
  nn::Cint                        # nn = nv+np <= 100000
  nv::Cint                        # nv = number of variables
  np::Cint                        # np = number of parameters
  mo::Cuchar                      # max orders of vars
  po::Cuchar                      # max orders of params
  to::Cuchar                      # global order of truncation. Note ord_t in mad_tpsa is typedef for unsigned char (Cuchar)
  no::Ptr{Cuchar}                 # orders of each vars and params, no[nn]. In C this is const

  uno::Cint                       # user provided no
  nth::Cint                       # max #threads or 1
  nc::Cuint                       # number of coefs (max length of TPSA)

  monos::Ptr{Cuchar}              # 'matrix' storing the monomials (sorted by var)
  ords::Ptr{Cuchar}               # Order of each mono of To
  prms::Ptr{Cuchar}               # order of parameters in each mono of To (zero = no prms)
  To::Ptr{Ptr{Cuchar}}            # Table by orders -- pointers to monos, sorted by order
  Tv::Ptr{Ptr{Cuchar}}            # Table by vars   -- pointers to monos, sorted by vars
  ocs::Ptr{Ptr{Cuchar}}           # ocs[t,i] -> o in mul, compute o on thread t 3 <= o <= mo terminated with 0

  ord2idx::Ptr{Cint}              # order to polynomial start index in To (i.e. in TPSA coef[])
  tv2to::Ptr{Cint}                # lookup tv->to
  to2tv::Ptr{Cint}                # lookup to->tv
  H::Ptr{Cint}                    # indexing matrix in Tv
  L::Ptr{Ptr{Cint}}               # multiplication indexes L[oa,ob]->L_ord L_ord[ia,ib]->ic
  L_idx::Ptr{Ptr{Ptr{Cint}}}      # L_idx[oa,ob]->[start] [split] [end] idxs in L

  size::Culonglong                # bytes used by desc. Unsigned Long Int, ikn 32 bit system is int32 but 64 bit int64. Using Culonglong assuming 64 bit

  t::Ptr{Ptr{T}}                  # tmp for tpsa
  ct::Ptr{Ptr{C}}                 # tmp for ctpsa
  ti::Ptr{Cint}                   # idx of tmp ised
  cti::Ptr{Cint}                  # idx of tmp used
end

struct RTPSA{T}
  d::Ptr{T}                       # Ptr to tpsa descriptor
  uid::Cint                       # Special user field for external use (and padding)
  mo::Cuchar                      # max ord (allocated)
  lo::Cuchar                      # lowest used ord
  hi::Cuchar                      # highest used ord
  nz::Culonglong                  # zero/nonzero homogenous polynomials. Int64 if 64 bit else 32 bit
  nam::NTuple{NAMSZ,Cuchar}       # tpsa name max string length 16 NAMSZ
  coef::Ptr{Cdouble}              # warning: must be identical to ctpsa up to coef excluded
end

struct CTPSA{T}
  d::Ptr{T}                       # Ptr to ctpsa descriptor
  uid::Cint                       # Special user field for external use (and padding)
  mo::Cuchar                      # max ord (allocated)
  lo::Cuchar                      # lowest used ord
  hi::Cuchar                      # highest used ord
  nz::Culonglong                  # zero/nonzero homogenous polynomials. Int64 if 64 bit else 32 bit
  nam::NTuple{NAMSZ,Cuchar}       # tpsa name
  coef::Ptr{ComplexF64}           # warning: must be identical to ctpsa up to coef excluded
end

const MAD_TPSA = :("libgtpsa")
const MAD_TPSA_DEFAULT::Cuchar = 255
const MAD_TPSA_SAME::Cuchar = 254
const MAD_DESC_CURR::Ptr{Desc{RTPSA,CTPSA}} = C_NULL

# High-Level Wrapper Structs
struct Descriptor
  desc::Ptr{Desc{RTPSA,CTPSA}}

  """
    Descriptor(nv::Integer, mo::Integer)

  Creates a TPSA Descriptor with nv variables of maximum order mo.
  """
  function Descriptor(nv::Integer, mo::Integer)
    d = new(mad_desc_newv(convert(Cint, nv), convert(Cuchar, mo)))
    #f(x) = mad_desc_del!(x.desc)
    #finalizer(f,d)
    return d
  end


  """
    Descriptor(nv::Integer, mo::Integer, np::Integer, po::Integer)

  Creates a TPSA Descriptor with nv variables of maximum order mo, and np parameters
  of maximum order po.
  """
  function Descriptor(nv::Integer, mo::Integer, np::Integer, po::Integer)
    d = new(mad_desc_newvp(convert(Cint, nv), convert(Cuchar, mo), convert(Cint, np), convert(Cuchar, po)))
    #f(x) = mad_desc_del!(x.desc)
    #finalizer(f,d)
    return d
  end


  """
    Descriptor(nv::Integer, mo::Integer, np::Integer, po::Integer, no::Vector{Int})
  
  Creates a TPSA Descriptor with nv variables of maximum order mo, np parameters of 
  maximum order po (<= mo), and the individual variable/parameter orders specified 
  in no. 
  """
  function Descriptor(nv::Integer, mo::Integer, np::Integer, po::Integer, no::Vector{Int})
    d = new(mad_desc_newvpo(convert(Cint, nv), convert(Cuchar, mo), convert(Cint, np), convert(Cuchar, po), convert(Vector{Cuchar}, no)))
    #f(x) = mad_desc_del!(x.desc)
    #finalizer(f,d)
    return d
  end
end

abstract type AbstractTPSA end

mutable struct TPSA <: AbstractTPSA
  tpsa::Ptr{RTPSA{Desc}}

  function TPSA()
    t = new(mad_tpsa_newd(MAD_DESC_CURR, MAD_TPSA_DEFAULT))
    f(x) = mad_tpsa_del!(x.tpsa)
    finalizer(f,t)
    return t
  end

  function TPSA(d::Descriptor)
    t = new(mad_tpsa_newd(d.desc, MAD_TPSA_DEFAULT))
    f(x) = mad_tpsa_del!(x.tpsa)
    finalizer(f,t)
    return t
  end

  
  function TPSA(t1::TPSA)
    t = new(mad_tpsa_new(t1.tpsa, MAD_TPSA_DEFAULT))
    f(x) = mad_tpsa_del!(x.tpsa)
    finalizer(f,t)
    return t
  end

  function TPSA(t1::AbstractTPSA)
    t = new(mad_tpsa_new(Base.unsafe_convert(Ptr{RTPSA{Desc}}, t1.tpsa), MAD_TPSA_DEFAULT))
    f(x) = mad_tpsa_del!(x.tpsa)
    finalizer(f,t)
    return t
  end
end


mutable struct ComplexTPSA <: AbstractTPSA
  tpsa::Ptr{CTPSA{Desc}}

  function ComplexTPSA()
    t = new(mad_ctpsa_newd(MAD_DESC_CURR, MAD_TPSA_DEFAULT))
    f(x) = mad_ctpsa_del!(x.tpsa)
    finalizer(f,t)
    return t
  end

  function ComplexTPSA(d::Descriptor)
    t = new(mad_ctpsa_newd(d.desc, MAD_TPSA_DEFAULT))
    f(x) = mad_ctpsa_del!(x.tpsa)
    finalizer(f,t)
    return t
  end

  function ComplexTPSA(t1::ComplexTPSA)
    t = new(mad_ctpsa_new(t1.tpsa, MAD_TPSA_DEFAULT))
    f(x) = mad_ctpsa_del!(x.tpsa)
    finalizer(f,t)
    return t
  end

  function ComplexTPSA(t1::AbstractTPSA)
    t = new(mad_ctpsa_new(Base.unsafe_convert(Ptr{CTPSA{Desc}}, t1.tpsa), MAD_TPSA_DEFAULT))
    f(x) = mad_ctpsa_del!(x.tpsa)
    finalizer(f,t)
    return t
  end
end

# --- Getters ---
function getindex(t::TPSA, i::Integer)::Float64
  return i == 0 ? mad_tpsa_get0(t.tpsa) : mad_tpsa_geti(t.tpsa, convert(Cint, i))
end

function getindex(t::TPSA, I)
  return [t[i] for i in I]
end

firstindex(t::TPSA) = 1
lastindex(t::TPSA) = mad_tpsa_len(t.tpsa)-1

function getindex(t::TPSA, s::AbstractString)::Float64
  return mad_tpsa_gets(t.tpsa, convert(Cint, 0), Base.unsafe_convert(Cstring, Base.cconvert(Cstring, s)))
end
#=
num_t operator[](const std::string& s) const {
  return mad_tpsa_gets(ptr(), s.size(), s.c_str());
}
num_t operator[](const std::vector<ord_t>& m) const {
  return mad_tpsa_getm(ptr(), m.size(), m.data());
}
num_t operator[](const std::vector<idx_t>& m) const {
  return mad_tpsa_getsm(ptr(), m.size(), m.data());
}
=#
#=
# Setters
@inline function setindex!(t::TPSA, a::Float64, i::Int64)

end
=#


# --- print ---
function print(t::TPSA)
  mad_tpsa_print(t.tpsa, Base.unsafe_convert(Cstring, ""), 0.,Int32(0),C_NULL)
end

function print(t::ComplexTPSA)
  mad_ctpsa_print(t.tpsa, Base.unsafe_convert(Cstring, ""), 0.,Int32(0),C_NULL)
end


# --- unary ---
@inline function +(a::TPSA)::TPSA
  c = TPSA(a)
  return c
end

@inline function -(a::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_scl!(a.tpsa, -1., c.tpsa)
  return c
end


# --- add ---
@inline function +(a::TPSA, b::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_add!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function +(a::Real, b::TPSA)::TPSA
  c = TPSA(b)
  mad_tpsa_copy!(b.tpsa, c.tpsa)
  mad_tpsa_set0!(c.tpsa, 1., convert(Float64,a))
  return c
end

@inline function +(a::TPSA, b::Real)::TPSA
  return b+a
end


# --- sub ---
@inline function -(a::TPSA, b::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_sub!(a.tpsa, b.tpsa, c.tpsa)
  return c
end


@inline function -(a::TPSA, b::Real)::TPSA
  c = TPSA(a)
  mad_tpsa_copy!(a.tpsa, c.tpsa)
  mad_tpsa_set0!(c.tpsa, 1., convert(Float64, -b))
  return c
end

@inline function -(a::Real, b::TPSA)::TPSA
  c = TPSA(b)
  mad_tpsa_scl!(b.tpsa, -1., c.tpsa)
  mad_tpsa_set0!(c.tpsa, 1., convert(Float64, a))
  return c
end


# --- mul ---
@inline function *(a::TPSA, b::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_mul!(a.tpsa, b.tpsa, c.tpsa) 
  return c
end

@inline function *(a::Real, b::TPSA)::TPSA
  c = TPSA(b)
  mad_tpsa_scl!(b.tpsa, convert(Float64, a), c.tpsa)
  return c
end

@inline function *(a::TPSA, b::Real)::TPSA
  return b*a
end


# --- div ---
@inline function /(a::TPSA, b::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_div!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function /(a::TPSA, b::Real)::TPSA
  c = TPSA(a)
  mad_tpsa_scl!(a.tpsa, convert(Float64, 1/b), c.tpsa)
  return c
end

@inline function /(a::Real, b::TPSA)::TPSA
  c = TPSA(b)
  mad_tpsa_inv!(b.tpsa, convert(Float64,a), c.tpsa)
  return c
end


# --- pow ---
@inline function ^(a::TPSA, b::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_pow!(a.tpsa, b.tpsa, c.tpsa)
  return c
end


@inline function ^(a::TPSA, b::Integer)::TPSA
  c = TPSA(a) 
  mad_tpsa_powi!(a.tpsa, convert(Cint, b), c.tpsa)
  return c
end

@inline function ^(a::TPSA, b::Real)::TPSA
  c = TPSA(a)
  mad_tpsa_pown!(a.tpsa, convert(Float64,b), c.tpsa)
  return c
end

@inline function ^(a::Real, b::TPSA)::TPSA
  c = TPSA(b)
  mad_tpsa_scl!(b.tpsa, convert(Float64,log(a)), c.tpsa)
  mad_tpsa_exp!(c.tpsa, c.tpsa)
  return c
end


# --- atan2, hypot ---
@inline function atan(a::TPSA, b::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_atan2!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function hypot(a::TPSA, b::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_hypot!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

#=
# --- I/O ---
inline std::FILE* operator<< (std::FILE *out, a::TPSA) {
  mad_tpsa_print(a.tpsa, 0,0,0, out) return out
end

inline std::FILE* operator<< (std::FILE *out, const tpsa_t &a) {
  mad_tpsa_print(&a, 0,0,0, out) return out
end

inline std::FILE* operator>> (std::FILE *in, tpsa &a) {
  T c(mad_tpsa_scan(in)) a = c return in
end

# --- functions ---
#inline num_t fval   (num_t a           ) { TRC("num") return a end
#inline num_t nrm    (num_t a           ) { TRC("num") return abs(a) end
#inline num_t sqr    (num_t a           ) { TRC("num") return a*a end
#inline num_t inv    (num_t a, num_t v=1) { TRC("num") return v/a end
#inline num_t invsqrt(num_t a, num_t v=1) { TRC("num") return v/sqrt(a) end
#inline num_t sinc   (num_t a           ) { TRC("num") return mad_num_sinc (a) end
#inline num_t sinhc  (num_t a           ) { TRC("num") return mad_num_sinhc(a) end
#inline num_t asinc  (num_t a           ) { TRC("num") return mad_num_asinc(a) end

inline num_t fval(const tpsa_t *a) { TRC("tspa")
  return mad_tpsa_get0(a)
end


inline num_t fval (a::TPSA) { TRC("baz")
  return a[0]
end

inline num_t fabs (const tpsa_t *a) { TRC("tspa")
  return abs(mad_tpsa_get0(a))
end


inline num_t fabs (a::TPSA) { TRC("baz")
  return abs(a[0])
end
=#

@inline function norm(a::TPSA)::Float64
  return mad_tpsa_nrm(a.tpsa)
end

# --- unary ---
macro FUN(F)
  fn = Symbol("mad_tpsa_" * F * "!")
  quote
      @inline function $(esc(Symbol(F)))(a::TPSA)::TPSA
        c = TPSA(a)
        $(esc(fn))(a.tpsa, c.tpsa)
        return c
      end
  end
end

@FUN("abs"  )
@FUN("unit"  )
@FUN("sqrt"  )
@FUN("exp"  )
@FUN("log"  )
@FUN("sin"  )
@FUN("cos"  )
@FUN("tan"  )
@FUN("cot"  )
@FUN("sinc"  )
@FUN("sinh"  )
@FUN("cosh"  )
@FUN("tanh"  )
@FUN("coth"  )
@FUN("sinhc" )
@FUN("asin"  )
@FUN("acos"  )
@FUN("atan"  )
@FUN("acot"  )
@FUN("asinc" )
@FUN("asinh")
@FUN("acosh" )
@FUN("atanh" )
@FUN("acoth" )
@FUN("asinhc")
@FUN("erf"  )
@FUN("erfc"  )




# CTPSA Operators:
@inline function real(a::ComplexTPSA)::TPSA
  c = TPSA(a)
  mad_ctpsa_real!(a.tpsa,c.tpsa)
  return c
end

@inline function imag(a::ComplexTPSA)::TPSA
  c = TPSA(a)
  mad_ctpsa_imag!(a.tpsa,c.tpsa)
  return c
end

# --- unary ---
@inline function +(a::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(a) 
  return c
end

@inline function -(a::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_scl!(a.tpsa, -1., c.tpsa)
  return c
end


#--- add ---
@inline function +(a::ComplexTPSA, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_add!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function +(a::Number, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(b)
  mad_ctpsa_copy!(b.tpsa, c.tpsa)
  mad_ctpsa_set0!(c.tpsa, 1.0+0.0*im, convert(ComplexF64, a))
  return c
end

@inline function +(a::ComplexTPSA, b::Number)::ComplexTPSA
  return b+a
end


# --- sub ---
@inline function -(a::ComplexTPSA, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_sub!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function -(a::ComplexTPSA, b::Number)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_copy!(a.tpsa, c.tpsa)
  mad_ctpsa_set0!(c.tpsa, 1.0+0.0*im, convert(ComplexF64, -b))
  return c
end

@inline function -(a::Number, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(b)
  mad_ctpsa_scl!(b.tpsa,-1., c.tpsa)
  mad_ctpsa_set0!(c.tpsa, 1.0+0.0*im, convert(ComplexF64,a))
  return c
end


# --- mul ---
@inline function *(a::ComplexTPSA, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_mul!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function *(a::Number, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(b)
  mad_ctpsa_scl!(b.tpsa, convert(ComplexF64,a), c.tpsa)
  return c
end

@inline function *(a::ComplexTPSA, b::Number)::ComplexTPSA
  return b*a
end


# --- div ---

@inline function /(a::ComplexTPSA, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_div!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function /(a::ComplexTPSA, b::Number)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_scl!(a.tpsa, convert(ComplexF64, 1.0/b), c.tpsa)
  return c
end

@inline function /(a::Number, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(b)
  mad_ctpsa_inv!(b.tpsa, convert(ComplexF64, a), c.tpsa)
  return c
end


# --- pow ---
@inline function ^(a::ComplexTPSA, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_pow!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function ^(a::ComplexTPSA, b::Integer)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_powi!(a.tpsa, convert(Cint, b), c.tpsa)
  return c
end

@inline function ^(a::ComplexTPSA, b::Number)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_pown!(a.tpsa, convert(ComplexF64, b), c.tpsa)
  return c
end

@inline function ^(a::Number, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(b)
  mad_ctpsa_scl!(b.tpsa, convert(ComplexF64, log(a)), c.tpsa)
  mad_ctpsa_exp!(c.tpsa, c.tpsa)
  return c
end

# --- hypot ---
@inline function hypot(a::ComplexTPSA, b::ComplexTPSA)::ComplexTPSA
  c = ComplexTPSA(a)
  mad_ctpsa_hypot!(a.tpsa, b.tpsa, c.tpsa)
  return c
end


#= --- I/O ---

template <class A>
inline std::FILE* operator<< (std::FILE *out, a::ComplexTPSA) {
  mad_ctpsa_print(a.tpsa, 0,0,0, out) return out
end

inline std::FILE* operator<< (std::FILE *out, const ctpsa_t &a) {
  mad_ctpsa_print(&a, 0,0,0, out) return out
end

inline std::FILE* operator>> (std::FILE *in, ctpsa &a) {
  T c(mad_ctpsa_scan(in)) a = c return in
end

// --- functions ---

inline CPX fval   (CPX a         ) { TRC("cpx") return a end
inline CPX nrm    (CPX a         ) { TRC("cpx") return std::abs(a) end
inline CPX sqr    (CPX a         ) { TRC("cpx") return a*a end
inline CPX inv    (CPX a, CPX v=1) { TRC("cpx") return v/a end
inline CPX invsqrt(CPX a, CPX v=1) { TRC("cpx") return v/std::sqrt(a) end
inline CPX sinc   (CPX a         ) { TRC("cpx") cpx_t r = mad_cpx_sinc (C(a)) return CPX(RE(r),IM(r)) end
inline CPX sinhc  (CPX a         ) { TRC("cpx") cpx_t r = mad_cpx_sinhc(C(a)) return CPX(RE(r),IM(r)) end
inline CPX asinc  (CPX a         ) { TRC("cpx") cpx_t r = mad_cpx_asinc(C(a)) return CPX(RE(r),IM(r)) end

inline CPX fval(const ctpsa_t *a) { TRC("tspa")
  return mad_ctpsa_get0(a)
end

template <class A>
inline CPX fval (a::ComplexTPSA) { TRC("baz")
  return a[0]
end
=#

@inline function norm(a::ComplexTPSA)::Float64
  return mad_ctpsa_nrm(a.tpsa)
end
#=

template <class A>
inline T sqr (a::ComplexTPSA) { TRC("baz")
  T c(a) mad_ctpsa_mul(a.tpsa, a.tpsa, c.tpsa) return c
end

template <class A>
inline T inv (a::ComplexTPSA, CPX v=1) { TRC("baz")
  T c(a) mad_ctpsa_inv(a.tpsa, C(v), c.tpsa) return c
end

template <class A>
inline T invsqrt (a::ComplexTPSA, CPX v=1) { TRC("baz")
  T c(a) mad_ctpsa_invsqrt(a.tpsa, C(v), c.tpsa) return c
end
=#



# --- unary ---
macro FUNC(F)
  fn = Symbol("mad_ctpsa_" * F * "!")
  quote
      @inline function $(esc(Symbol(F)))(a::ComplexTPSA)::ComplexTPSA
        c = ComplexTPSA(a)
        $(esc(fn))(a.tpsa, c.tpsa)
        return c
      end
  end
end

@FUNC("unit"  )
@FUNC("sqrt"  )
@FUNC("exp"  )
@FUNC("log"  )
@FUNC("sin"  )
@FUNC("cos"  )
@FUNC("tan"  )
@FUNC("cot"  )
@FUNC("sinc"  )
@FUNC("sinh"  )
@FUNC("cosh"  )
@FUNC("tanh"  )
@FUNC("coth"  )
@FUNC("sinhc" )
@FUNC("asin"  )
@FUNC("acos"  )
@FUNC("atan"  )
@FUNC("acot"  )
@FUNC("asinc" )
@FUNC("asinh" )
@FUNC("acosh" )
@FUNC("atanh" )
@FUNC("acoth" )
@FUNC("asinhc")
@FUNC("erf"  )
@FUNC("erfc"  )

# -- zero -- (For LinearAlgebra overloading)
function zero(a::TPSA)
  return TPSA()
end

function zero(a::ComplexTPSA)
  return ComplexTPSA()
end

include("mono.jl")
include("desc.jl")
include("rtpsa.jl")
include("ctpsa.jl")
end
