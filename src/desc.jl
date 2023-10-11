"""
    mad_desc_newv(nv::Cint, mo::Cuchar)::Ptr{Desc{RTPSA,CTPSA}}

Creates a TPSA descriptor with the specified number of variables and maximum order. 
The number of parameters is set to 0. Order

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
- `np_` -- (Optional) Number of parameters, default is 0
- `po_` -- (Optional) Order of parameters, po = max(1, po_)

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
- `np_`  -- (Optional) Number of parameters, default is 0
- `po_`  -- (Optional) Order of parameters (po = max(po_, no[nv:nn-1]), po <= mo)
- `no_`  -- (Optional) Array of orders of variables and parameters

### Output
- `ret` -- Descriptor with the specified nv, mo, np, po, no.
"""
function mad_desc_newvpo(nv::Cint, mo::Cuchar, np_::Cint, po_::Cuchar, no_::Ptr{Cuchar})::Ptr{Desc{RTPSA,CTPSA}}
  ret = @ccall MAD_TPSA.mad_desc_newvpo(nv::Cint, mo::Cuchar, np_::Cint, po_::Cuchar, no_::Ptr{Cuchar})::Ptr{Desc{RTPSA,CTPSA}}
  return ret
end


"""
    mad_desc_del!(d_::Ptr{Desc{RTPSA,CTPSA}})

Calls the destructor for the passed descriptor.

"""
function mad_desc_del!(d_::Ptr{Desc{RTPSA,CTPSA}})
  @ccall MAD_TPSA.mad_desc_del(d_::Ptr{Desc{RTPSA,CTPSA}})::Cvoid
end

"""
    mad_desc_del!(d_::Ptr{Cvoid})

Calls the destructor for all existing descriptors. Defined to allow C_NULL pointers

"""
function mad_desc_del!(d_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_desc_del(d_::Ptr{Cvoid})::Cvoid
end


"""
    mad_desc_getnv!(d::Ptr{Desc{RTPSA,CTPSA}}, mo_::Ptr{Cuchar}, np_::Ptr{Cint}, po_::Ptr{Cuchar)::Cint

Returns the number of variables in the descriptor, and sets the passed mo_, np_, and po_ to the maximum 
order, number of parameters, and parameter order respectively.

### Input
- `d` -- Descriptor

### Output
- `mo_` -- (Optional) Maximum order of the descriptor
- `np_` -- (Optional) Number of parameters of the descriptor
- `po_` -- (Optional) Parameter order of the descriptor
- `ret` -- Number of variables in TPSA
"""
function mad_desc_getnv!(d::Ptr{Desc{RTPSA,CTPSA}}, mo_::Ptr{Cuchar}, np_::Ptr{Cint}, po_::Ptr{Cuchar})::Cint
  ret = @ccall MAD_TPSA.mad_desc_getnv(d::Ptr{Desc{RTPSA,CTPSA}}, mo_::Cuchar, np_::Cint, po_::Cuchar)::Cint
  return ret
end


"""
    mad_desc_maxord(d::Ptr{Desc{RTPSA,CTPSA}}, nn::Cint, no_::Ptr{Cuchar})::Cuchar

Sets the order of the variables and parameters of the TPSA to those specified in no_ and 
returns the maximum order of the TPSA.

### Input
- `d`   -- Descriptor
- `nn`  -- Number of variables + number of parameters, no_[1..nn]
- `no_` -- (Optional) Orders of parameters to be filled if provided

### Output
- `ret`  -- Maximum order of TPSA
"""
function mad_desc_maxord(d::Ptr{Desc{RTPSA,CTPSA}}, nn::Cint, no_::Ptr{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_desc_maxord(d::Ptr{Desc{RTPSA,CTPSA}}, nn::Cint, no_::Ptr{Cuchar})::Cuchar
  return ret
end


"""
    mad_desc_maxlen(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Cint

Gets the maximum length of the TPSA given an order. 

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

Checks if monomial as string s is valid given maximum order of descriptor.

### Input
- `d`  -- Descriptor
- `n`  -- Monomial string length
- `s`  -- Monomial as string "[0-9]*"

### Output
- `ret` -- True if valid, false if invalid
"""
function mad_desc_isvalids(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, s::Cstring)::Cuchar
  ret = @ccall MAD_TPSA.mad_desc_isvalids(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, s::Cstring)::Cuchar
  return ret
end


"""
    mad_desc_isvalidm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cuchar

Checks if monomial as byte array m is valid given maximum order of descriptor.

### Input
- `d`  -- Descriptor
- `n`  -- Length of monomial
- `m`  -- Monomial as byte array

### Output
- `ret` -- True if valid, false if invalid
"""
function mad_desc_isvalidm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_desc_isvalidm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cuchar
  return ret
end


"""
    mad_desc_isvalidsm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cint})::Cuchar

Checks the monomial as sparse monomial m (monomial stored as sequence of integers with each pair 
[(i,o)] such that i = index, o = order) is valid given the maximum order of the descriptor.

### Input
- `d`   -- Descriptor
- `n`   -- Length of monomial
- `m`   -- Sparse monomial [(idx, ord)]

### Output
- `ret` -- True if valid, false if invalid
"""
function mad_desc_isvalidsm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cint})::Cuchar
  ret = @ccall MAD_TPSA.mad_desc_isvalidsm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cint})::Cuchar
  return ret
end


"""
    mad_desc_idxs(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, s::Cstring)::Cint

Returns the index of the monomial as string s in the descriptor, or -1 if the monomial is invalid.

### Input
- `d`   -- Descriptor
- `n`   -- String length or 0 if unknown
- `s`   -- Monomial as string "[0-9]*"

### Output
- `ret` -- Monomial index or -1 if invalid monomial
"""
function mad_desc_idxs(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, s::Cstring)::Cint
  ret = @ccall MAD_TPSA.mad_desc_idxs(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, s::Cstring)::Cint
  return ret
end


"""
    mad_desc_idxm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint

Returns the index of the monomial as byte array m in the descriptor, or -1 if the monomial is invalid.

### Input
- `d` -- Descriptor
- `n`    -- Monomial length
- `m`    -- Monomial as byte array

### Output
- `ret`  -- Monomial index or -1 if invalid
"""
function mad_desc_idxm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint
  ret = @ccall MAD_TPSA.mad_desc_idxm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint
  return ret
end


"""
    mad_desc_idxsm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cint})::Cint

Returns the index of the monomial as sparse monomial [(i,o)] m in the descriptor, or -1 if the monomial is invalid.

### Input
- `d`   -- Descriptor
- `n`   -- Monomial length
- `m`   -- Sparse monomial [idx,ord)]

### Output
- `ret` -- Monomial index or -1 if invalid
"""
function mad_desc_idxsm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cint})::Cint
  ret = @ccall MAD_TPSA.mad_desc_idxsm(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cint})::Cint
  return ret
end


"""
    mad_desc_nxtbyvar(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint

Returns the next monomial after monomial m in the TPSA when sorted by variable.

### Input
- `d`   -- Descriptor
- `n`   -- Monomial length
- `m`   -- Monomial as byte array

### Output
- `idx` -- Monomial index or -1 if no valid next monomial
"""
function mad_desc_nxtbyvar(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint
  idx = @ccall MAD_TPSA.mad_desc_nxtbyvar(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint
  return idx
end


"""
    mad_desc_nxtbyord(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint

Returns the next monomial after monomial m in the TPSA when sorted by order.

### Input
- `d`   -- Descriptor
- `n`   -- Monomial length
- `m`   -- Monomial as byte array

### Output
- `idx` -- Monomial index or -1 if no valid next monomial
"""
function mad_desc_nxtbyord(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint
  idx = @ccall MAD_TPSA.mad_desc_nxtbyord(d::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint
  return idx
end


"""
    mad_desc_mono!(d::Ptr{Desc{RTPSA,CTPSA}}, i::Cint, n::Cint, m_::Ptr{Cuchar})::Cuchar

Returns the order of the monomial at index i, and if n and m_ are provided, then will also fill m_ 
with the monomial at this index.

### Input
- `d`   -- Descriptor
- `i`   -- Slot index (must be valid)
- `n`   -- Monomial length (must be provided if m_ is to be filled)

### Output
- `ret` -- Monomial order at slot index
- `m_`  -- (Optional) Monomial to fill if provided
"""
function mad_desc_mono!(d::Ptr{Desc{RTPSA,CTPSA}}, i::Cint, n::Cint, m_::Ptr{Cuchar})::Cuchar
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
function mad_desc_info(d::Ptr{Desc{RTPSA,CTPSA}}, fp_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_desc_info(d::Ptr{Desc{RTPSA,CTPSA}}, fp_::Ptr{Cvoid})::Cvoid
end
