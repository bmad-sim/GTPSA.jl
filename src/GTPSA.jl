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
              print

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
  *,
  print,
  getproperty
  const NAMSZ::Int = 16

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

const MAD_TPSA = :("libmad_tpsa")
const MAD_TPSA_DEFAULT::Cuchar = 255
const MAD_TPSA_SAME::Cuchar = 254
const MAD_DESC_CURR::Ptr{Desc{RTPSA,CTPSA}} = C_NULL

# High-Level Wrapper Structs
mutable struct Descriptor
  desc::Ptr{Desc{RTPSA,CTPSA}}

  """
    Descriptor(nv::Int, mo::Int)

  Creates a TPSA Descriptor with nv variables of maximum order mo.
  """
  function Descriptor(nv::Int, mo::Int)
    d = new(mad_desc_newv(convert(Cint, nv), convert(Cuchar, mo)))
    f(x) = mad_desc_del!(x.desc)
    finalizer(f,d)
  end


  """
    Descriptor(nv::Int, mo::Int, np::Int, po::Int)

  Creates a TPSA Descriptor with nv variables of maximum order mo, and np parameters
  of maximum order po.
  """
  function Descriptor(nv::Int, mo::Int, np::Int, po::Int)
    d = new(mad_desc_newvp(convert(Cint, nv), convert(Cuchar, mo), convert(Cint, np), convert(Cuchar, po)))
    f(x) = mad_desc_del!(x.desc)
    finalizer(f,d)
  end


  """
    Descriptor(nv::Int, mo::Int, np::Int, po::Int, no::Vector{Int})
  
  Creates a TPSA Descriptor with nv variables of maximum order mo, np parameters of 
  maximum order po (<= mo), and the individual variable/parameter orders specified 
  in no. 
  """
  function Descriptor(nv::Int, mo::Int, np::Int, po::Int, no::Vector{Int})
    d = new(mad_desc_newvpo(convert(Cint, nv), convert(Cuchar, mo), convert(Cint, np), convert(Cuchar, po), convert(Vector{Cuchar}, no)))
    f(x) = mad_desc_del!(x.desc)
    finalizer(f,d)
  end
end


mutable struct TPSA
  tpsa::Ptr{RTPSA{Desc}}

  function TPSA()
    t = new(mad_tpsa_newd(MAD_DESC_CURR, MAD_TPSA_DEFAULT))
    f(x) = mad_tpsa_del!(x.tpsa)
    finalizer(f,t)
  end

  function TPSA(d::Descriptor)
    t = new(mad_tpsa_newd(d.desc, MAD_TPSA_DEFAULT))
    f(x) = mad_tpsa_del!(x.tpsa)
    finalizer(f,t)
  end

  function TPSA(t1::TPSA)
    t = new(mad_tpsa_new(t1.tpsa, MAD_TPSA_DEFAULT))
    f(x) = mad_tpsa_del!(x.tpsa)
    finalizer(f,t)
  end
end

function print(t::TPSA)
  mad_tpsa_print(t.tpsa, Base.unsafe_convert(Cstring, ""), 0.,Int32(0),C_NULL)
end

#=
# Allows one to access low level stuff in the TPSA
function getproperty(t::TPSA, p::Symbol)
  if p == :d
    return unsafe_load(t.tpsa).d
  elseif p == :uid
    return unsafe_load(t.tpsa).uid
  elseif p == :mo
    return unsafe_load(t.tpsa).mo
  elseif p == :lo
    return unsafe_load(t.tpsa).lo
  elseif p == :hi
    return unsafe_load(t.tpsa).hi
  elseif p == :nz
    return unsafe_load(t.tpsa).nz
  elseif p == :nam
    return unsafe_load(t.tpsa).nam
  elseif p == :coef
    return unsafe_load(t.tpsa).coef    # Needs modification
  else
    return getfield(t, p)
  end
end
=#

# Unary
@inline function +(a::TPSA)::TPSA
  c = TPSA(a)
  return c
end

@inline function -(a::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_scl!(a.tpsa, -1., c.tpsa)
  return c
end

#=
#if TPSA_USE_TMP

@inline function + (const T &a) {  TRC("+tmp")
  T c(a) return c
}

@inline function - (const T &a) {  TRC("-tmp")
  T c(a) mad_tpsa_scl(c.tpsa, -1, c.tpsa) return c
}

#endif // TPSA_USE_TMP
=#

# --- add ---
@inline function +(a::TPSA, b::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_add!(a.tpsa, b.tpsa, c.tpsa)
  return c
end

@inline function +(a::Float64, b::TPSA)::TPSA
  c = TPSA(b)
  mad_tpsa_copy!(b.tpsa, c.tpsa)
  mad_tpsa_set0!(c.tpsa, 1., a)
  return c
end

@inline function +(a::TPSA, b::Float64)::TPSA
  return b+a
end

#=
#if TPSA_USE_TMP


@inline function + (a::TPSA, const T &b) {  TRC("baz+tmp")
  T c(b) mad_tpsa_add(a.tpsa, c.tpsa, c.tpsa) return c
}


@inline function + (const T &a, b::TPSA) {  TRC("tmp+baz")
  T c(a) mad_tpsa_add(c.tpsa, b.tpsa, c.tpsa) return c
}

@inline function + (const T &a, const T &b) {  TRC("tmp+tmp")
  T c(a) mad_tpsa_add(c.tpsa, b.tpsa, c.tpsa) return c
}

@inline function + (num_t a, const T &b) {  TRC("num+tmp")
  T c(b) mad_tpsa_set0(c.tpsa, 1, a) return c
}

@inline function + (const T &a, num_t b) {  TRC("tmp+num")
  return b+a
}

#endif // TPSA_USE_TMP
=#

# --- sub ---
@inline function -(a::TPSA, b::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_sub!(a.tpsa, b.tpsa, c.tpsa)
  return c
end


@inline function -(a::TPSA, b::Float64)::TPSA
  c = TPSA(a)
  mad_tpsa_copy!(a.tpsa, c.tpsa)
  mad_tpsa_set0!(c.tpsa, 1., -b)
  return c
end


@inline function -(a::Float64, b::TPSA)::TPSA
  c = TPSA(b)
  mad_tpsa_scl!(b.tpsa, -1., c.tpsa)
  mad_tpsa_set0!(c.tpsa, 1., a)
  return c
end

#=
#if TPSA_USE_TMP


@inline function - (a::TPSA, const T &b) {  TRC("baz-tmp")
  T c(b) mad_tpsa_sub(a.tpsa, c.tpsa, c.tpsa) return c
}


@inline function - (const T &a, b::TPSA) {  TRC("tmp-baz")
  T c(a) mad_tpsa_sub(c.tpsa, b.tpsa, c.tpsa) return c
}

@inline function - (const T &a, const T &b) {  TRC("tmp-tmp")
  T c(a) mad_tpsa_sub(c.tpsa, b.tpsa, c.tpsa) return c
}

@inline function - (const T &a, num_t b) {  TRC("tmp-num")
  T c(a) mad_tpsa_set0(c.tpsa, 1, -b) return c
}

@inline function - (num_t a, const T &b) {  TRC("num-tmp")
  T c(b)
  mad_tpsa_scl (c.tpsa,-1, c.tpsa)
  mad_tpsa_set0(c.tpsa, 1, a)
  return c
}

#endif // TPSA_USE_TMP
=#

# --- mul ---
@inline function *(a::TPSA, b::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_mul!(a.tpsa, b.tpsa, c.tpsa) 
  return c
end


@inline function *(a::Float64, b::TPSA)::TPSA
  c = TPSA(b)
  mad_tpsa_scl!(b.tpsa, a, c.tpsa)
  return c
end


@inline function *(a::TPSA, b::Float64)::TPSA
  return b*a
end

#=

#if TPSA_USE_TMP


@inline function * (a::TPSA, const T &b) {  TRC("baz*tmp")
  T c(b) mad_tpsa_mul(a.tpsa, c.tpsa, c.tpsa) return c
}


@inline function * (const T &a, b::TPSA) {  TRC("tmp*baz")
  T c(a) mad_tpsa_mul(c.tpsa, b.tpsa, c.tpsa) return c
}

@inline function * (const T &a, const T &b) {  TRC("tmp*tmp")
  T c(a) mad_tpsa_mul(c.tpsa, b.tpsa, c.tpsa) return c
}

@inline function *(num_t a, const T &b) { TRC("num*tmp")
  T c(b) mad_tpsa_scl(c.tpsa, a, c.tpsa) return c
}

inline T
operator* (const T &a, num_t b) {  TRC("tmp*num")
  return b*a
}

#endif // TPSA_USE_TMP
=#

# --- div ---
@inline function /(a::TPSA, b::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_div!(a.tpsa, b.tpsa, c.tpsa)
  return c
end


@inline function /(a::TPSA, b::Float64)::TPSA
  c = TPSA(a)
  mad_tpsa_scl!(a.tpsa, 1/b, c.tpsa)
  return c
end


@inline function /(a::Float64, b::TPSA)::TPSA
  c = TPSA(b)
  mad_tpsa_inv!(b.tpsa, a, c.tpsa)
  return c
end

#=
#if TPSA_USE_TMP


@inline function / (a::TPSA, const T &b) {  TRC("baz/tmp")
  T c(b) mad_tpsa_div(a.tpsa, c.tpsa, c.tpsa) return c
}


@inline function / (const T &a, b::TPSA) {  TRC("tmp/baz")
  T c(a) mad_tpsa_div(c.tpsa, b.tpsa, c.tpsa) return c
}

@inline function / (const T &a, const T &b) {  TRC("tmp/tmp")
  T c(a) mad_tpsa_div(c.tpsa, b.tpsa, c.tpsa) return c
}

@inline function / (const T &a, num_t b) {  TRC("tmp/num")
  T c(a) mad_tpsa_scl(c.tpsa, 1/b, c.tpsa) return c
}

@inline function / (num_t a, const T &b) {  TRC("num/tmp")
  T c(b) mad_tpsa_inv(c.tpsa, a, c.tpsa) return c
}

#endif // TPSA_USE_TMP
=#

# --- pow ---
@inline function ^(a::TPSA, b::TPSA)::TPSA
  c = TPSA(a)
  mad_tpsa_pow!(a.tpsa, b.tpsa, c.tpsa)
  return c
end


@inline function ^(a::TPSA, b::Int64)::TPSA
  c = TPSA(a) 
  mad_tpsa_powi!(a.tpsa, convert(Cint, b), c.tpsa)
  return c
end


@inline function ^(a::TPSA, b::Float64)::TPSA
  c = TPSA(a)
  mad_tpsa_pown!(a.tpsa, b, c.tpsa)
  return c
end


@inline function ^(a::Float64, b::TPSA)::TPSA
  c = TPSA(b)
  mad_tpsa_scl!(b.tpsa, log(a), c.tpsa)
  mad_tpsa_exp!(c.tpsa, c.tpsa)
  return c
end


#=
#if TPSA_USE_TMP


inline T pow (a::TPSA, const T &b) {  TRC("baz^tmp")
  T c(b) mad_tpsa_pow(a.tpsa, c.tpsa, c.tpsa) return c
}


inline T pow (const T &a, b::TPSA) {  TRC("tmp^baz")
  T c(a) mad_tpsa_pow(c.tpsa, b.tpsa, c.tpsa) return c
}

inline T pow (const T &a, const T &b) {  TRC("tmp^tmp")
  T c(a) mad_tpsa_pow(c.tpsa, b.tpsa, c.tpsa) return c
}

inline T pow (const T &a, int b) {  TRC("tmp^int")
  T c(a) mad_tpsa_powi(c.tpsa, b, c.tpsa) return c
}

inline T pow (const T &a, num_t b) {  TRC("tmp^num")
  T c(a) mad_tpsa_pown(c.tpsa, b, c.tpsa) return c
}

inline T pow (num_t a, const T &b) {  TRC("num^tmp")
  T c(b)
  mad_tpsa_scl(c.tpsa, log(a), c.tpsa)
  mad_tpsa_exp(c.tpsa, c.tpsa)
  return c
}

#endif // TPSA_USE_TMP

=#

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
#if TPSA_USE_TMP


inline T atan2 (a::TPSA, const T &b) {  TRC("baz,tmp")
  T c(b) mad_tpsa_atan2(a.tpsa, c.tpsa, c.tpsa) return c
}


inline T atan2 (const T &a, b::TPSA) {  TRC("tmp,baz")
  T c(a) mad_tpsa_atan2(c.tpsa, b.tpsa, c.tpsa) return c
}

inline T atan2 (const T &a, const T &b) {  TRC("tmp,tmp")
  T c(a) mad_tpsa_atan2(c.tpsa, b.tpsa, c.tpsa) return c
}


inline T hypot (a::TPSA, const T &b) {  TRC("baz,tmp")
  T c(b) mad_tpsa_hypot(a.tpsa, c.tpsa, c.tpsa) return c
}


inline T hypot (const T &a, b::TPSA) {  TRC("tmp,baz")
  T c(a) mad_tpsa_hypot(c.tpsa, b.tpsa, c.tpsa) return c
}

inline T hypot (const T &a, const T &b) {  TRC("tmp,tmp")
  T c(a) mad_tpsa_hypot(c.tpsa, b.tpsa, c.tpsa) return c
}

#endif // TPSA_USE_TMP
=#

#=
# --- I/O ---
inline std::FILE* operator<< (std::FILE *out, a::TPSA) {
  mad_tpsa_print(a.tpsa, 0,0,0, out) return out
}

inline std::FILE* operator<< (std::FILE *out, const tpsa_t &a) {
  mad_tpsa_print(&a, 0,0,0, out) return out
}

inline std::FILE* operator>> (std::FILE *in, tpsa &a) {
  T c(mad_tpsa_scan(in)) a = c return in
}
=#

# --- swap --- (In Julia just do a, b = b, a)
#=
inline void swap (tpsa_ref &a, tpsa_ref &b) { TRC("ref") a.swp(b) }
inline void swap (tpsa     &a, tpsa     &b) { TRC("tpa") a.swp(b) }
inline void swap (num_t    &a, num_t    &b) { TRC("num") std::swap(a,b) }
=#

# --- functions ---
#inline num_t fval   (num_t a           ) { TRC("num") return a }
#inline num_t nrm    (num_t a           ) { TRC("num") return abs(a) }
#inline num_t sqr    (num_t a           ) { TRC("num") return a*a }
#inline num_t inv    (num_t a, num_t v=1) { TRC("num") return v/a }
#inline num_t invsqrt(num_t a, num_t v=1) { TRC("num") return v/sqrt(a) }
#inline num_t sinc   (num_t a           ) { TRC("num") return mad_num_sinc (a) }
#inline num_t sinhc  (num_t a           ) { TRC("num") return mad_num_sinhc(a) }
#inline num_t asinc  (num_t a           ) { TRC("num") return mad_num_asinc(a) }

#=
inline num_t fval(const tpsa_t *a) { TRC("tspa")
  return mad_tpsa_get0(a)
}


inline num_t fval (a::TPSA) { TRC("baz")
  return a[0]
}

inline num_t fabs (const tpsa_t *a) { TRC("tspa")
  return abs(mad_tpsa_get0(a))
}


inline num_t fabs (a::TPSA) { TRC("baz")
  return abs(a[0])
}
=#

@inline function norm(a::TPSA)::Float64
  return mad_tpsa_nrm(a.tpsa)
end

#=
inline T sqr (a::TPSA) { TRC("baz")
  T c(a) mad_tpsa_mul(a.tpsa, a.tpsa, c.tpsa) return c
}


inline T inv (a::TPSA, num_t v=1) { TRC("baz")
  T c(a) mad_tpsa_inv(a.tpsa, v, c.tpsa) return c
}


inline T invsqrt (a::TPSA, num_t v=1) { TRC("baz")
  T c(a) mad_tpsa_invsqrt(a.tpsa, v, c.tpsa) return c
}



#if TPSA_USE_TMP

inline T sqr (const T &a) { TRC("tmp")
  T c(a) mad_tpsa_mul(c.tpsa, c.tpsa, c.tpsa) return c
}

inline T inv (const T &a, num_t v=1) { TRC("tmp")
  T c(a) mad_tpsa_inv(c.tpsa, v, c.tpsa) return c
}

inline T invsqrt (const T &a, num_t v=1) { TRC("tmp")
  T c(a) mad_tpsa_invsqrt(c.tpsa, v, c.tpsa) return c
}

#endif // TPSA_USE_TMP
=#

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

#=
#if TPSA_USE_TMP

#define FUN_TMP(F) \
inline T F (const T &a) { TRC("tmp") \
  T c(a) mad_tpsa_ ## F (c.tpsa, c.tpsa) return c \
}

#else
#define FUN_TMP(F)
#endif // TPSA_USE_TMP
=#

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


include("mono.jl")
include("desc.jl")
include("rtpsa.jl")
include("ctpsa.jl")
end
