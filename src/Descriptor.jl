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

  ord2idx::Ptr{Cint}      # order to polynomial start index in To (i.e. in TPSA coef[])
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

"""
    mad_desc_newv(nv::Cint, mo::Cuchar)::Ptr{Desc{RTPSA,CTPSA}}

Creates a TPSA descriptor with the specified number of variables and maximum order. 
The number of parameters is set to 0. 

### Input
- `nv`  -- Number of variables in th
- `mo`  -- Maximum order of TPSA, mo = max(1, mo)

### Output
- `ret` -- Descriptor with the specified number of variables and maximum order
"""
function mad_desc_newv(nv::Cint, mo::Cuchar)::Ptr{Desc{RTPSA,CTPSA}}
  ret = @ccall MAD_TPSA.mad_desc_newv(nv::Cint, mo::Cuchar)::Ptr{Desc{RTPSA,CTPSA}}
  return ret
end


"""
    mad_desc_newvp(nv::Cint, mo::Cuchar, np_::Cint, po_::Cuchar)::Ptr{Desc{RTPSA,CTPSA}}

Creates a TPSA descriptor with the specifed number of variables, maximum order, number of 
parameters, and parameter order.

### Input
- `nv`  -- Number of variables
- `mo`  -- Maximum order of TPSA, mo = max(1, mo)
- `np_` -- Number of parameters
- `po_` -- Order of parameters, po = max(1, po_)

### Output
- `ret` -- Descriptor with the specified nv, mo, np, and po.
"""
function mad_desc_newvp(nv::Cint, mo::Cuchar, np_::Cint, po_::Cuchar)::Ptr{Desc{RTPSA,CTPSA}}
  ret = @ccall MAD_TPSA.mad_desc_newvp(nv::Cint, mo::Cuchar, np_::Cint, po_::Cuchar)::Ptr{Desc{RTPSA,CTPSA}}
  return ret
end


"""
    mad_desc_newvpo(nv::Cint, mo::Cuchar, np_::Cint, po_::Cuchar, no_::Ptr{Cuchar})::Ptr{Desc{RTPSA,CTPSA}}

Creates a TPSA descriptor with the specifed number of variables, maximum order, number of parameters, 
parameter order, and individual variable/parameter orders specified in no. The first nv entries in no 
correspond to the variables' orders and the next np entries correspond the parameters' orders.

### Input
- `nv`   -- Number of variables
- `mo`   -- Maximum order of TPSA (mo = max(mo , no[0 :nn-1]), nn = nv+np)
- `np_`  -- Number of parameters
- `po_`  -- Order of parameters (po = max(po_, no[nv:nn-1]), po <= mo)
- `no_`  -- Array of orders of variables and parameters

### Output
- `ret` -- Descriptor with the specified nv, mo, np, po, no.
"""
function mad_desc_newvpo(nv::Cint, mo::Cuchar, np_::Cint, po_::Cuchar, no_::Ptr{Cuchar})::Ptr{Desc{RTPSA,CTPSA}}
  ret = @ccall MAD_TPSA.mad_desc_newvpo(nv::Cint, mo::Cuchar, np_::Cint, po_::Cuchar, no_::Ptr{Cuchar})::Ptr{Desc{RTPSA,CTPSA}}
  return ret
end


"""
    mad_desc_del!(d_::Ptr{Desc{RTPSA,CTPSA}})

Calls the destructor for the descriptor, or all descriptors if null pointer is passed.

### Input
- `d_` -- Descriptor to destruct. If null, all registered descriptors will be deleted
"""
function mad_desc_del!(d_::Ptr{Desc{RTPSA,CTPSA}})
  @ccall MAD_TPSA.mad_desc_del(d_::Ptr{Desc{RTPSA,CTPSA}})::Cvoid
end


"""
    mad_desc_getnv!(d::Ptr{Desc{RTPSA,CTPSA}}, mo_::Ptr{Cuchar}, np_::Ptr{Cint}, po_::Ptr{Cuchar)::Cint

Returns the number of variables in the descriptor, and sets the passed mo_, np_, and po_ to the maximum 
order, number of parameters, and parameter order respectively. ???

### Input
- `d` -- Descriptor
- `mo_`  -- Maximum order to be set to that of the descriptor
- `np_`  -- Number of parameters to be set to that of the descriptor
- `po_`  -- Parameter order to be set to that of the descriptor

### Output
- `ret`   -- Number of variables in TPSA
"""
function mad_desc_getnv!(desc::Ptr{Desc{RTPSA,CTPSA}}, mo_::Ptr{Cuchar}, np_::Ptr{Cint}, po_::Ptr{Cuchar})::Cint
  ret = @ccall MAD_TPSA.mad_desc_getnv(desc::Ptr{Desc{RTPSA,CTPSA}}, mo_::Cuchar, np_::Cint, po_::Cuchar)::Cint
  return ret
end


"""
    mad_desc_maxord(d::Ptr{Desc{RTPSA,CTPSA}}, nn::Cint, no_::Ptr{Cuchar})::Cuchar

Sets the order of the variables and parameters of the TPSA to those specified in no_ and 
returns the maximum order of the TPSA.

### Input
- `d`   -- Descriptor
- `nn`  -- Number of variables + number of parameters, no_[1..nn]
- `no_` -- Orders of parameters to be filled if provided

### Output
- `ret`  -- Maximum order of TPSA
"""
function mad_desc_maxord(d::Ptr{Desc{RTPSA,CTPSA}}, nn::Cint, no_::Ptr{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_desc_maxord(d::Ptr{Desc{RTPSA,CTPSA}}, nn::Cint, no_::Ptr{Cuchar})::Cuchar
  return ret
end


"""
    mad_desc_maxlen(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Cint

???

### Input
- `d`   -- Descriptor
- `mo`  -- ordlen(maxord) == maxlen

### Output
- `ret` -- monomials in 0..order
"""
function mad_desc_maxlen(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Cint
  ret = @ccall MAD_TPSA.mad_desc_maxlen(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Cint
  return ret
end


"""
    mad_desc_gtrunc!(d::Ptr{Desc{RTPSA,CTPSA}}, to::Cuchar)::Cuchar

Sets the global truncation order (to) of the TPSA, and returns the old global truncation order.

### Input
- `d`     -- Descriptor
- `to`    -- New global truncation order

### Output
- `oldto` -- Old global truncation order
"""
function mad_desc_gtrunc!(d::Ptr{Desc{RTPSA,CTPSA}}, to::Cuchar)::Cuchar
  oldto = @ccall MAD_TPSA.mad_desc_gtrunc(d::Ptr{Desc{RTPSA,CTPSA}}, to::Cuchar)::Cuchar
  return oldto
end


"""
    mad_desc_isvalids(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, s::Cstring)::Cuchar

???

### Input
- `d`  -- Descriptor
- `n`  -- String length of 0 (unknown)
- `s`  -- Monomial as string "[0-9]*"

### Output
- `ret` -- True or false
"""
function mad_desc_isvalids(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, s::Cstring)::Cuchar
  ret = @ccall MAD_TPSA.mad_desc_isvalids(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, s::Cstring)::Cuchar
  return ret
end


"""
    mad_desc_isvalidm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cuchar

???

### Input
- `d`  -- Descriptor
- `n`  -- Length of monomial
- `m`  -- Monomial

### Output
- `ret` -- True or false
"""
function mad_desc_isvalidm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_desc_isvalidm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cuchar
  return ret
end


"""
    mad_desc_isvalidsm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cint})::Cuchar

??? why int isntead uint8

### Input
- `d`   -- Descriptor
- `n`   -- Length of monomial
- `m`   -- Sparse monomial (idx, ord)

### Output
- `ret` -- True or false.
"""
function mad_desc_isvalidsm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cint})::Cuchar
  ret = @ccall MAD_TPSA.mad_desc_isvalidsm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cint})::Cuchar
  return ret
end


"""
    mad_desc_idxs(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, s::Cstring)::Cint

???

### Input
- `d`   -- Descriptor
- `n`   -- String length or 0 (unknown)
- `s`   -- Monomial as string "[0-9]*"

### Output
- `ret` -- Monomial index or -1
"""
function mad_desc_idxs(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, s::Cstring)::Cint
  ret = @ccall MAD_TPSA.mad_desc_idxs(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, s::Cstring)::Cint
  return ret
end


"""
    mad_desc_idxm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint

???

### Input
- `d` -- Descriptor
- `n`    -- Monomial length
- `m`    -- Monomial

### Output
- `ret`  -- Monomial index or -1
"""
function mad_desc_idxm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint
  ret = @ccall MAD_TPSA.mad_desc_idxm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint
  return ret
end


"""
    mad_desc_idxsm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cint})::Cint

??? Why int instead Uint8

### Input
- `d`   -- Descriptor
- `n`   -- Monomial length
- `m`   -- Sparse monomial (idx,ord)

### Output
- `ret` -- Monomial index or -1
"""
function mad_desc_idxsm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cint})::Cint
  ret = @ccall MAD_TPSA.mad_desc_idxsm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cint})::Cint
  return ret
end


"""
    mad_desc_nxtbyvar(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint

???

### Input
- `d`   -- Descriptor
- `n`   -- Monomial length
- `m`   -- Monomial

### Output
- `idx` -- Monomial index or -1
"""
function mad_desc_nxtbyvar(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint
  idx = @ccall MAD_TPSA.mad_desc_nxtbyvar(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint
  return idx
end


"""
    mad_desc_nxtbyord(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint

???

### Input
- `d`   -- Descriptor
- `n`   -- Monomial length
- `m`   -- Monomial

### Output
- `idx` -- Monomial index or -1
"""
function mad_desc_nxtbyord(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint
  idx = @ccall MAD_TPSA.mad_desc_nxtbyord(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint
  return idx
end


"""
    mad_desc_mono(d::Ptr{Desc{RTPSA,CTPSA}}, i::Cint, n::Cint, m_::Ptr{Cuchar})::Cuchar

???

### Input
- `d`   -- Descriptor
- `i`   -- Slot index (must be valid)
- `n`   -- Monomial length
- `m_`  -- Monomial to fill (if provided)

### Output
- `ret` -- Monomial order
"""
function mad_desc_mono(d::Ptr{Desc{RTPSA,CTPSA}}, i::Cint, n::Cint, m_::Ptr{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_desc_mono(d::Ptr{Desc{RTPSA,CTPSA}}, i::Cint, n::Cint, m_::Ptr{Cuchar})::Cuchar
  return ret
end

"""
  mad_desc_info(d::Ptr{Desc{RTPSA,CTPSA}}, fp::Ptr{Cvoid})

For debugging.

### Input
- `d`  -- Descriptor to debug
- `fp` -- File to write to. If null, will write to stdout
"""
function mad_desc_info(d::Ptr{Desc{RTPSA,CTPSA}}, fp::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_desc_info(d::Ptr{Desc{RTPSA,CTPSA}}, fp::Ptr{Cvoid})::Cvoid
end


end