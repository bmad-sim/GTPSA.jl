# TPSA Structures Module implementation
module Descriptor
export Desc, new

struct Desc{T,C}
  id::Cint                  # index in list of registered descriptors
  nn::Cint                  # nn = nv+np <= 100000
  nv::Cint                  # nv = number of variables
  np::Cint                  # np = number of parameters
  mo::Cuchar                # max orders of vars
  po::Cuchar                # max orders of params
  to::Cuchar                # global order of truncation. Note ord_t in mad_tpsa is typedef for unsigned char (Cuchar)
  no::Ptr{Cuchar}           # orders of each vars and params, no[nn]. In C this is const

  uno::Cint                 # user provided no
  nth::Cint                 # max #threads or 1
  nc::Cuint                 # number of coefs (max length of TPSA)

  monos::Ptr{Cuchar}        # 'matrix' storing the monomials (sorted by var)
  ords::Ptr{Cuchar}         # Order of each mono of To
  To::Ptr{Ptr{Cuchar}}      # Table by orders -- pointers to monos, sorted by order
  Tv::Ptr{Ptr{Cuchar}}      # Table by vars   -- pointers to monos, sorted by vars
  ocs::Ptr{Ptr{Cuchar}}     # ocs[t,i] -> o; in mul, compute o on thread t; 3 <= o <= mo; terminated with 0

  order2idx::Ptr{Cint}      # order to polynomial start index in To (i.e. in TPSA coef[])
  tv2to::Ptr{Cint}          # lookup tv->to
  to2tv::Ptr{Cint}          # lookup to->tv
  H::Ptr{Cint}              # indexing matrix in Tv
  L::Ptr{Ptr{Cint}}         # multiplication indexes L[oa,ob]->L_ord; L_ord[ia,ib]->ic
  L_idx::Ptr{Ptr{Ptr{Cint}}}  # L_idx[oa,ob]->[start] [split] [end] idxs in L

  size::Culonglong          # bytes used by desc. Unsigned Long Int, ikn 32 bit system is int32 but 64 bit int64. Using Culonglong assuming 64 bit

  t::Ptr{Ptr{T}}              # tmp for tpsa
  ct::Ptr{Ptr{C}}             # tmp for ctpsa
  ti::Ptr{Cint}          # idx of tmp ised
  cti::Ptr{Cint}         # idx of tmp used
end
end