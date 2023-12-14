"""
    mad_tpsa_newd(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{RTPSA{Desc}}

Creates a TPSA defined by the specified descriptor and maximum order. If `MAD_TPSA_DEFAULT` 
is passed for `mo`, the `mo` defined in the descriptor is used. If `mo > d_mo`, then `mo = d_mo`.

### Input
- `d`  -- Descriptor
- `mo` -- Maximum order

### Output
- `t`  -- New TPSA defined by the descriptor
"""
function mad_tpsa_newd(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
  t = @ccall MAD_TPSA.mad_tpsa_newd(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
  return t
end


"""
    mad_tpsa_new(t::Ptr{RTPSA{Desc}}, mo::Cuchar)::Ptr{RTPSA{Desc}}

Creates a TPSA `c`opy of the inputted TPSA, with maximum order specified by `mo`.
If `MAD_TPSA_SAME` is passed for `mo`, the `mo` currently in `t` is used for the created TPSA.
Ok with `t=(tpsa_t*)ctpsa`

### Input
- `t`   -- TPSA to copy
- `mo`  -- Maximum order of new TPSA

### Output
- `ret` -- New TPSA with maximum order `mo`
"""
function mad_tpsa_new(t::Ptr{RTPSA{Desc}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
  ret = @ccall MAD_TPSA.mad_tpsa_new(t::Ptr{RTPSA{Desc}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
  return ret
end


"""
    mad_tpsa_del!(t::Ptr{RTPSA{Desc}})

Calls the destructor for the TPSA.

### Input
- `t` -- TPSA to destruct
"""
function mad_tpsa_del!(t::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_del(t::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_desc(t::Ptr{RTPSA{Desc}})::Ptr{Desc{RTPSA,CTPSA}}

Gets the descriptor for the TPSA.

### Input
- `t`   -- TPSA

### Output
- `ret` -- Descriptor for the RTPSA
"""
function mad_tpsa_desc(t::Ptr{RTPSA{Desc}})::Ptr{Desc{RTPSA,CTPSA}}
  ret = @ccall MAD_TPSA.mad_tpsa_desc(t::Ptr{RTPSA{Desc}})::Ptr{Desc{RTPSA,CTPSA}}
  return ret
end


"""
    mad_tpsa_uid!(t::Ptr{RTPSA{Desc}}, uid_::Cint)::Cint

Sets the TPSA uid if `uid_ != 0`, and returns the current (previous if set) TPSA `uid`. 

### Input
- `t`    -- TPSA
- `uid_` -- `uid` to set in the TPSA if `uid_ != 0`

### Output
- `ret`  -- Current (previous if set) TPSA uid
"""
function mad_tpsa_uid!(t::Ptr{RTPSA{Desc}}, uid_::Cint)::Cint
  ret = @ccall MAD_TPSA.mad_tpsa_uid(t::Ptr{RTPSA{Desc}}, uid_::Cint)::Cint
  return ret
end


"""
    mad_tpsa_len(t::Ptr{RTPSA{Desc}})::Cint

Gets the length of the TPSA itself (e.g. the descriptor may be order 10 but TPSA may only be order 2)

### Input
- `t`   -- TPSA

### Output
- `ret` -- Length of RTPSA
"""
function mad_tpsa_len(t::Ptr{RTPSA{Desc}})::Cint
  ret = @ccall MAD_TPSA.mad_tpsa_len(t::Ptr{RTPSA{Desc}})::Cint
  return ret
end


"""
    mad_tpsa_nam(t::Ptr{RTPSA{Desc}})::Cstring

Get the name of the TPSA.

### Input
- `t`    -- TPSA

### Output
- `ret`  -- Name of RTPSA (null terminated in C)
"""
function mad_tpsa_nam(t::Ptr{RTPSA{Desc}})::Cstring
  ret = @ccall MAD_TPSA.mad_tpsa_nam(t::Ptr{RTPSA{Desc}})::Cstring
  return ret
end


"""
    mad_tpsa_ord(t::Ptr{RTPSA{Desc}})::Cuchar

Gets the TPSA order.

### Input
- `t`   -- TPSA

### Output
- `ret` -- Order of TPSA
"""
function mad_tpsa_ord(t::Ptr{RTPSA{Desc}})::Cuchar
  ret = @ccall MAD_TPSA.mad_tpsa_ord(t::Ptr{RTPSA{Desc}})::Cuchar
  return ret
end

"""
  mad_tpsa_ordv(t::Ptr{RTPSA{Desc}}, ts::Ptr{RTPSA{Desc}}...)::Cuchar

Returns maximum order of all TPSAs provided.

### Input
- `t`  -- TPSA
- `ts` -- Variable number of TPSAs passed as parameters

### Output
- `mo` -- Maximum order of all TPSAs provided
"""
function mad_tpsa_ordv(t::Ptr{RTPSA{Desc}}, ts::Ptr{RTPSA{Desc}}...)::Cuchar
  #mo = @ccall MAD_TPSA.mad_tpsa_ordv(t::Ptr{RTPSA{Desc}}, ts::Ptr{RTPSA{Desc}}..., 0::Cint)::Cuchar # null pointer after args for safe use
  mo = ccall((:mad_tpsa_ordv, MAD_TPSA), Cuchar, (Ptr{RTPSA{Desc}}, Ptr{RTPSA{Desc}}...), t, ts...)
  return mo
end


"""
    mad_tpsa_ordn(n::Cint, t::Ptr{Ptr{RTPSA{Desc}}})::Cuchar

Returns the max order of all TPSAs in `t`.

### Input
- `n`  -- Number of TPSAs
- `t`  -- Array of TPSAs 

### Output
- `mo` -- Maximum order of all TPSAs
"""
function mad_tpsa_ordn(n::Cint, t::Ptr{Ptr{RTPSA{Desc}}})::Cuchar
  mo = @ccall MAD_TPSA.mad_tpsa_ordn(n::Cint, t::Ptr{Ptr{RTPSA{Desc}}})::Cuchar
  return mo
end


"""
    mad_tpsa_copy!(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

Makes a copy of the TPSA `t` to `r`.

### Input
- `t` -- Source TPSA

### Output
- `r` -- Destination TPSA
"""
function mad_tpsa_copy!(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_copy(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_sclord!(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, inv::Cuchar)

Scales all coefficients by order. If `inv == 0`, scales coefficients by order (derivation), else 
scales coefficients by 1/order (integration).

### Input
- `t`  -- Source TPSA
- `inv`-- Put order up, divide, scale by `inv` of value of order

### Output
- `r`  -- Destination TPSA
"""
function mad_tpsa_sclord!(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, inv::Cuchar)
  @ccall MAD_TPSA.mad_tpsa_sclord(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, inv::Cuchar)::Cvoid
end


"""
    mad_tpsa_getord!(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, ord::Cuchar)

Extract one homogeneous polynomial of the given order

### Input
- `t``  -- Source TPSA
- `ord` -- Order to retrieve

### Output
- `r`   -- Destination TPSA
"""
function mad_tpsa_getord!(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, ord::Cuchar)
  @ccall MAD_TPSA.mad_tpsa_getord(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, ord::Cuchar)::Cvoid
end


"""
    mad_tpsa_cutord!(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, ord::Cint)

Cuts the TPSA off at the given order and above, or if `ord` is negative, will cut orders below 
`abs(ord)` (e.g. if ord = -3, then orders 0-3 are cut off).

### Input
- `t`   -- Source TPSA
- `ord` -- Cut order: `0..-ord` or `ord..mo`

### Output
- `r`   -- Destination TPSA
"""
function mad_tpsa_cutord!(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, ord::Cint)
  @ccall MAD_TPSA.mad_tpsa_cutord(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, ord::Cint)::Cvoid
end

"""
  mad_tpsa_maxord!(t::Ptr{RTPSA{Desc}}, n::Cint, idx_::Ptr{Cint})::Cint

Returns the index to the monomial with maximum abs(coefficient) in the TPSA for all orders 0 to `n. If `idx_` 
is provided, it is filled with the indices for the maximum abs(coefficient) monomial for each order up to `n`. 

### Input
- `t`    -- TPSA
- `n`    -- Highest order to include in finding the maximum abs(coefficient) in the TPSA, length of `idx_` if provided

### Output
- `idx_` -- (Optional) If provided, is filled with indices to the monomial for each order up to `n` with maximum abs(coefficient)
- `mi`   -- Index to the monomial in the TPSA with maximum abs(coefficient)
"""
function mad_tpsa_maxord!(t::Ptr{RTPSA{Desc}}, n::Cint, idx_::Ptr{Cint})::Cint
  mi = @ccall MAD_TPSA.mad_tpsa_maxord(t::Ptr{RTPSA{Desc}}, n::Cint, idx_::Ptr{Cint})::Cint
  return mi
end

"""
    mad_tpsa_convert!(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)

General function to convert TPSAs to different orders and reshuffle canonical coordinates. The destination TPSA will 
be of order `n`, and optionally have the variable reshuffling defined by `t2r_` and poisson bracket sign. e.g. if 
`t2r_ = {1,2,3,4,6,5}` and `pb = -1`, canonical coordinates 6 and 5 are swapped and the new 5th canonical coordinate 
will be negated. Useful for comparing with different differential algebra packages.

### Input
- `t`    -- Source TPSA
- `n`    -- Length of vector
- `t2r_` -- (Optional) Vector of index lookup
- `pb`   -- Poisson bracket, 0, 1:fwd, -1:bwd

### Output
- `r`    -- Destination TPSA with specified order and canonical coordinate reshuffling.
"""
function mad_tpsa_convert!(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)
  @ccall MAD_TPSA.mad_tpsa_convert(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)::Cvoid
end


"""
    mad_tpsa_setvar!(t::Ptr{RTPSA{Desc}}, v::Cdouble, iv_::Cint, scl_::Cdouble)

Sets the 0th and 1st order values for the variables.

### Input
- `t`    -- TPSA
- `v`    -- 0th order value (coefficient)
- `iv_`  -- Variable index, optional if order of TPSA is 0 (behaves like `mad_tpsa_setval` then)
- `scl_` -- 1st order variable value (typically will be 1)
"""
function mad_tpsa_setvar!(t::Ptr{RTPSA{Desc}}, v::Cdouble, iv_::Cint, scl_::Cdouble)
  @ccall MAD_TPSA.mad_tpsa_setvar(t::Ptr{RTPSA{Desc}}, v::Cdouble, iv_::Cint, scl_::Cdouble)::Cvoid
end

"""
    mad_tpsa_setval!(t::Ptr{RTPSA{Desc}}, v::Cdouble)

Sets the scalar part of the TPSA to `v` and all other values to 0 (sets the TPSA order to 0).

### Input
- `t` -- TPSA to set to scalar
- `v` -- Scalar value to set TPSA
"""
function mad_tpsa_setval!(t::Ptr{RTPSA{Desc}}, v::Cdouble)
  @ccall MAD_TPSA.mad_tpsa_setval(t::Ptr{RTPSA{Desc}}, v::Cdouble)::Cvoid
end


"""
    mad_tpsa_setnam!(t::Ptr{RTPSA{Desc}}, nam::Cstring)

Sets the name of the RTPSA.

### Input
- `t`   -- TPSA
- `nam` -- Name to set for RTPSA
"""
function mad_tpsa_setnam!(t::Ptr{RTPSA{Desc}}, nam::Cstring)
  @ccall MAD_TPSA.mad_tpsa_setnam(t::Ptr{RTPSA{Desc}}, nam::Cstring)::Cvoid
end


"""
    mad_tpsa_clear!(t::Ptr{RTPSA{Desc}})

Clears the TPSA (reset to 0)

### Input
- `t` -- TPSA
"""
function mad_tpsa_clear!(t::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_clear(t::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_isnul(t::Ptr{RTPSA{Desc}})::Cuchar

Checks if TPSA is 0 or not

### Input
- `t`    -- TPSA to check

### Output
- `ret`  -- True or false
"""
function mad_tpsa_isnul(t::Ptr{RTPSA{Desc}})::Cuchar
  ret = @ccall MAD_TPSA.mad_tpsa_isnul(t::Ptr{RTPSA{Desc}})::Cuchar
  return ret
end


"""
    mad_tpsa_mono!(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar})::Cuchar

Returns the order of the monomial at index `i` in the TPSA `a`nd optionally the monomial at that index is returned in `m_`

### Input
- `t`   -- TPSA
- `i`   -- Index valid in TPSA
- `n`   -- Length of monomial

### Output
- `m_`  -- (Optional) Monomial at index `i` in TPSA
- `ret` -- Order of monomial in TPSA `a`t index `i`
"""
function mad_tpsa_mono!(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_tpsa_mono(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar})::Cuchar
  return ret
end


"""
    mad_tpsa_idxs(t::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring)::Cint

Returns index of monomial in the TPSA given the monomial as string. This generally should not be used, as there 
are no assumptions about which monomial is attached to which index.

### Input
- `t`   -- TPSA
- `n`   -- Length of monomial
- `s`   -- Monomial as string

### Output
- `ret` -- Index of monomial in TPSA
"""
function mad_tpsa_idxs(t::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring)::Cint
  ret = @ccall MAD_TPSA.mad_tpsa_idxs(t::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring)::Cint
  return ret
end



"""
    mad_tpsa_idxm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint

Returns index of monomial in the TPSA given the monomial as a byte array

### Input
- `t`   -- TPSA
- `n`   -- Length of monomial
- `s`   -- Monomial as byte array

### Output
- `ret` -- Index of monomial in TPSA
"""
function mad_tpsa_idxm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint
  ret = @ccall MAD_TPSA.mad_tpsa_idxm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint
  return ret
end


"""
    mad_tpsa_idxsm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cint})::Cint

Returns index of monomial in the TPSA given the monomial as a sparse monomial. This generally should not be used, as there 
are no assumptions about which monomial is attached to which index.

### Input
- `t`   -- TPSA
- `n`   -- Length of monomial
- `s`   -- Monomial as sparse monomial

### Output
- `ret` -- Index of monomial in TPSA
"""
function mad_tpsa_idxsm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cint})::Cint
  ret = @ccall MAD_TPSA.mad_tpsa_idxsm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cint})::Cint
  return ret
end


"""
    mad_tpsa_cycle!(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, v_::Ptr{Cdouble})::Cint

Used for scanning through each nonzero monomial in the TPSA. Given a starting index (-1 if starting at 0), will 
optionally fill monomial `m_` with the monomial at index `i` and the value at `v_` with the monomials coefficient, and 
return the next NONZERO monomial index in the TPSA. This is useful for building an iterator through the TPSA.

### Input
- `t`  -- TPSA to scan
- `i`  -- Index to start from (-1 to start at 0)
- `n`  -- Length of monomial
- `m_` -- (Optional) Monomial to be filled if provided
- `v_` -- (Optional) Pointer to value of coefficient

### Output
- `i`  -- Index of next nonzero monomial in the TPSA, or -1 if reached the end
"""
function mad_tpsa_cycle!(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, v_::Ptr{Cdouble})::Cint
  i = @ccall MAD_TPSA.mad_tpsa_cycle(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, v_::Ptr{Cdouble})::Cint
  return i
end


"""
    mad_tpsa_get0(t::Ptr{RTPSA{Desc}})::Cdouble

Gets the 0th order (scalar) value of the TPSA

### Input
- `t`   -- TPSA

### Output
- `ret` -- Scalar value of TPSA
"""
function mad_tpsa_get0(t::Ptr{RTPSA{Desc}})::Cdouble
  ret = @ccall MAD_TPSA.mad_tpsa_get0(t::Ptr{RTPSA{Desc}})::Cdouble
  return ret
end


"""
    mad_tpsa_geti(t::Ptr{RTPSA{Desc}}, i::Cint)::Cdouble

Gets the coefficient of the monomial at index `i`. Generally should use `mad_tpsa_cycle` instead of this.

### Input
- `t`   -- TPSA
- `i`   -- Monomial index

### Output
- `ret` -- Coefficient of monomial at index `i`
"""
function mad_tpsa_geti(t::Ptr{RTPSA{Desc}}, i::Cint)::Cdouble
  ret = @ccall MAD_TPSA.mad_tpsa_geti(t::Ptr{RTPSA{Desc}}, i::Cint)::Cdouble
  return ret
end


"""
    mad_tpsa_gets(t::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring)::Cdouble

Gets the coefficient of the monomial `s` defined as a string. Generally should use `mad_tpsa_cycle` instead of this.

### Input
- `t`   -- TPSA
- `n`   -- Length of monomial
- `s`   -- Monomial as string

### Output
- `ret` -- Coefficient of monomial `s` in TPSA
"""
function mad_tpsa_gets(t::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring)::Cdouble
  ret = @ccall MAD_TPSA.mad_tpsa_gets(t::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring)::Cdouble
  return ret
end


"""
    mad_tpsa_getm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cdouble

Gets the coefficient of the monomial `m` defined as a byte array. Generally should use `mad_tpsa_cycle` instead of this.

### Input
- `t`   -- TPSA
- `n`   -- Length of monomial
- `m`   -- Monomial as byte array

### Output
- `ret` -- Coefficient of monomial `m` in TPSA
"""
function mad_tpsa_getm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cdouble
  val = @ccall MAD_TPSA.mad_tpsa_getm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cdouble
  return ret
end


"""
    mad_tpsa_getsm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cint})::Cdouble

Gets the coefficient of the monomial `m` defined as a sparse monomial. Generally should use `mad_tpsa_cycle` instead of this.

### Input
- `t`   -- TPSA
- `n`   -- Length of monomial
- `m`   -- Monomial as sparse monomial

### Output
- `ret` -- Coefficient of monomial `m` in TPSA
"""
function mad_tpsa_getsm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cint})::Cdouble
  ret = @ccall MAD_TPSA.mad_tpsa_getsm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cint})::Cdouble
  return ret
end


"""
    mad_tpsa_set0!(t::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble)

Sets the 0th order coefficient (scalar part of TPSA) according to `coef[0] = a*coef[0] + b`. Does not modify other values in TPSA.

### Input
- `t` -- TPSA
- `a` -- Scaling of current 0th order value
- `b` -- Constant added to current 0th order value
"""
function mad_tpsa_set0!(t::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble)
  @ccall MAD_TPSA.mad_tpsa_set0(t::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble)::Cvoid
end


"""
    mad_tpsa_seti!(t::Ptr{RTPSA{Desc}}, i::Cint, a::Cdouble, b::Cdouble)

Sets the coefficient of monomial at index `i` to `coef[i] = a*coef[i] + b`. Does not modify other values in TPSA.

### Input
- `t` -- TPSA
- `i` -- Index of monomial
- `a` -- Scaling of current coefficient
- `b` -- Constant added to current coefficient
"""
function mad_tpsa_seti!(t::Ptr{RTPSA{Desc}}, i::Cint, a::Cdouble, b::Cdouble)
  @ccall MAD_TPSA.mad_tpsa_seti(t::Ptr{RTPSA{Desc}}, i::Cint, a::Cdouble, b::Cdouble)::Cvoid
end


"""
    mad_tpsa_sets!(t::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring, a::Cdouble, b::Cdouble)

Sets the coefficient of monomial defined by string `s` to `coef = a*coef + b`. Does not modify other values in TPSA.

### Input
- `t` -- TPSA
- `n` -- Length of monomial
- `s` -- Monomial as string
- `a` -- Scaling of current coefficient
- `b` -- Constant added to current coefficient
"""
function mad_tpsa_sets!(t::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring, a::Cdouble, b::Cdouble)
  @ccall MAD_TPSA.mad_tpsa_sets(t::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring, a::Cdouble, b::Cdouble)::Cvoid
end


"""
    mad_tpsa_setm!(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::Cdouble, b::Cdouble)

Sets the coefficient of monomial defined by byte array `m` to `coef = a*coef + b`. Does not modify other values in TPSA.

### Input
- `t` -- TPSA
- `n` -- Length of monomial
- `m` -- Monomial as byte array
- `a` -- Scaling of current coefficient
- `b` -- Constant added to current coefficient
"""
function mad_tpsa_setm!(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::Cdouble, b::Cdouble)
  @ccall MAD_TPSA.mad_tpsa_setm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::Cdouble, b::Cdouble)::Cvoid
end


"""
    mad_tpsa_setsm!(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cint}, a::Cdouble, b::Cdouble)

Sets the coefficient of monomial defined by sparse monomial `m` to `coef = a*coef + b`. Does not modify other values in TPSA.

### Input
- `t` -- TPSA
- `n` -- Length of monomial
- `m` -- Monomial as sparse monomial
- `a` -- Scaling of current coefficient
- `b` -- Constant added to current coefficient
"""
function mad_tpsa_setsm!(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cint}, a::Cdouble, b::Cdouble)
  @ccall MAD_TPSA.mad_tpsa_setsm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cint}, a::Cdouble, b::Cdouble)::Cvoid
end


"""
    mad_tpsa_getv!(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{Cdouble})

Vectorized getter of the coefficients for monomials with indices `i..i+n`. Useful for extracting the 1st order parts of 
a TPSA to construct a matrix (`i = 1`, `n = nv+np = nn`). 

### Input
- `t` -- TPSA
- `i` -- Starting index of monomials to get coefficients
- `n` -- Number of monomials to get coefficients of starting at `i`

### Output
- `v` -- Array of coefficients for monomials `i..i+n`
"""
function mad_tpsa_getv!(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{Cdouble})
  @ccall MAD_TPSA.mad_tpsa_getv(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{Cdouble})::Cvoid
end



"""
    mad_tpsa_setv!(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{Cdouble})

Vectorized setter of the coefficients for monomials with indices `i..i+n`. Useful for putting a matrix into a map.

### Input
- `t` -- TPSA
- `i` -- Starting index of monomials to set coefficients
- `n` -- Number of monomials to set coefficients of starting at `i`
- `v` -- Array of coefficients for monomials `i..i+n`
"""
function mad_tpsa_setv!(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{Cdouble})
  @ccall MAD_TPSA.mad_tpsa_setv(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{Cdouble})::Cvoid
end


"""
    mad_tpsa_equ(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, tol_::Cdouble)::Cuchar

Checks if the TPSAs `a` and `b` are equal within the specified tolerance `tol_`. If `tol_` is not specified, `DBL_EPSILON` is used.

### Input
- `a`    -- TPSA `a`
- `b`    -- TPSA `b`
- `tol_` -- (Optional) Difference below which the TPSAs are considered equal

### Output
- `ret`   - True if `a == b` within `tol_`
"""
function mad_tpsa_equ(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, tol_::Cdouble)::Cuchar
  ret = @ccall MAD_TPSA.mad_tpsa_equ(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, tol_::Cdouble)::Cuchar
  return ret
end


"""
    mad_tpsa_dif!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

For each homogeneous polynomial in TPSAs `a` and `b`, calculates either the relative error or absolute error for each order.
If the maximum coefficient for a given order in `a` is > 1, the relative error is computed for that order. Else, the absolute 
error is computed. This is very useful for comparing maps between codes or doing unit tests. In Julia, essentially:

`c_i = (a_i.-b_i)/maximum([abs.(a_i)...,1])` where `a_i` and `b_i` are vectors of the monomials for an order `i`

### Input
- `a` -- Source TPSA `a`
- `b` -- Source TPSA `b`

### Output
- `c` -- Destination TPSA `c` 
"""
function mad_tpsa_dif!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_dif(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_add!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets the destination TPSA `c = a + b`

### Input
- `a` -- Source TPSA `a`
- `b` -- Source TPSA `b`

### Output
- `c` -- Destination TPSA `c = a + b`
"""
function mad_tpsa_add!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_add(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_sub!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets the destination TPSA `c = a - b`

### Input
- `a` -- Source TPSA `a`
- `b` -- Source TPSA `b`

### Output
- `c` -- Destination TPSA `c = a - b`
"""
function mad_tpsa_sub!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sub(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_mul!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets the destination TPSA `c = a * b`

### Input
- `a` -- Source TPSA `a`
- `b` -- Source TPSA `b`

### Output
- `c` -- Destination TPSA `c = a * b`
"""
function mad_tpsa_mul!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_mul(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_div!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets the destination TPSA `c = a / b`

### Input
- `a` -- Source TPSA `a`
- `b` -- Source TPSA `b`

### Output
- `c` -- Destination TPSA `c = a / b`
"""
function mad_tpsa_div!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_div(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_pow!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets the destination TPSA `c = a ^ b`

### Input
- `a` -- Source TPSA `a`
- `b` -- Source TPSA `b`

### Output
- `c` -- Destination TPSA `c = a ^ b`
"""
function mad_tpsa_pow!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_pow(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_powi!(a::Ptr{RTPSA{Desc}}, n::Cint, c::Ptr{RTPSA{Desc}})

Sets the destination TPSA `c = a ^ n` where `n` is an integer.

### Input
- `a` -- Source TPSA `a`
- `n` -- Integer power

### Output
- `c` -- Destination TPSA `c = a ^ n`
"""
function mad_tpsa_powi!(a::Ptr{RTPSA{Desc}}, n::Cint, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_powi(a::Ptr{RTPSA{Desc}}, n::Cint, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_pown!(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})

Sets the destination TPSA `c = a ^ v` where `v` is of double precision.

### Input
- `a` -- Source TPSA `a`
- `v` -- "double" precision power

### Output
- `c` -- Destination TPSA `c = a ^ v`
"""
function mad_tpsa_pown!(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_pown(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_nrm(a::Ptr{RTPSA{Desc}})::Cdouble

Calculates the norm of TPSA `a`.

### Input
- `a`   -- TPSA

### Output
- `nrm` -- Norm of TPSA
"""
function mad_tpsa_nrm(a::Ptr{RTPSA{Desc}})::Cdouble
  nrm = @ccall MAD_TPSA.mad_tpsa_nrm(a::Ptr{RTPSA{Desc}}, tpsa_b_::Ptr{RTPSA{Desc}})::Cdouble
  return nrm
end


"""
    mad_tpsa_abs!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the absolute value of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = |a|`
"""
function mad_tpsa_abs!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_abs(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_sqrt!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the sqrt of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = sqrt(a)`
"""
function mad_tpsa_sqrt!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sqrt(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_exp!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the exponential of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = exp(a)`
"""
function mad_tpsa_exp!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_exp(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end



"""
    mad_tpsa_log!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the log of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = log(a)`
"""
function mad_tpsa_log!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_log(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_sincos!(a::Ptr{RTPSA{Desc}}, s::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `s = sin(a)` and TPSA `c = cos(a)`

### Input
- `a` -- Source TPSA `a`

### Output
- `s` -- Destination TPSA `s = sin(a)`
- `c` -- Destination TPSA `c = cos(a)`
"""
function mad_tpsa_sincos!(a::Ptr{RTPSA{Desc}}, s::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sincos(a::Ptr{RTPSA{Desc}}, s::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_sin!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the `sin` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = sin(a)`
"""
function mad_tpsa_sin!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sin(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_cos!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the `cos` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = cos(a)`
"""
function mad_tpsa_cos!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_cos(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_tan!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the `tan` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = tan(a)`
"""
function mad_tpsa_tan!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_tan(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_cot!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the `cot` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = cot(a)`
"""
function mad_tpsa_cot!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_cot(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_sinc!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the `sinc` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = sinc(a)`
"""
function mad_tpsa_sinc!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sinc(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_sincosh!(a::Ptr{RTPSA{Desc}}, s::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `s = sinh(a)` and TPSA `c = cosh(a)`

### Input
- `a` -- Source TPSA `a`

### Output
- `s` -- Destination TPSA `s = sinh(a)`
- `c` -- Destination TPSA `c = cosh(a)`
"""
function mad_tpsa_sincosh!(a::Ptr{RTPSA{Desc}}, s::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sincosh(a::Ptr{RTPSA{Desc}}, s::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_sinh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

  Sets TPSA `c` to the `sinh` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = sinh(a)`
"""
function mad_tpsa_sinh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sinh(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_cosh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the `cosh` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = cosh(a)`
"""
function mad_tpsa_cosh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_cosh(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_tanh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the `tanh` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = tanh(a)`
"""
function mad_tpsa_tanh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_tanh(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_coth!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the `coth` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = coth(a)`
"""
function mad_tpsa_coth!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_coth(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_sinhc!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the `sinhc` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = sinhc(a)`
"""
function mad_tpsa_sinhc!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sinhc(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_asin!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the `asin` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = asin(a)`
"""
function mad_tpsa_asin!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_asin(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_acos!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the `acos` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = acos(a)`
"""
function mad_tpsa_acos!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_acos(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_atan!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the `atan` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = atan(a)`
"""
function mad_tpsa_atan!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_atan(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_acot!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the `acot` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = acot(a)`
"""
function mad_tpsa_acot!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_acot(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end

"""
    mad_tpsa_asinc!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the `asinc` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = asinc(a)'
"""
function mad_tpsa_asinc!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_asinc(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_asinh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the `asinh` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = asinh(a)'
"""
function mad_tpsa_asinh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_asinh(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_acosh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the `acosh` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = acosh(a)'
"""
function mad_tpsa_acosh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_acosh(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_atanh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the `atanh` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = atanh(a)'
"""
function mad_tpsa_atanh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_atanh(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_acoth!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the acoth of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = acoth(a)'
"""
function mad_tpsa_acoth!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_acoth(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_asinhc!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the `asinhc` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = asinhc(a)'
"""
function mad_tpsa_asinhc!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_asinhc(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_erf!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the `erf` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = erf(a)'
"""
function mad_tpsa_erf!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_erf(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_erfc!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to the `erfc` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = erfc(a)'
"""
function mad_tpsa_erfc!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_erfc(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_acc!(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})

Adds `a*v` to TPSA `c`. Aliasing OK.

### Input
- `a` -- Source TPSA `a`
- `v` -- Scalar with double precision

### Output
- `c` -- Destination TPSA `c += v*a`
"""
function mad_tpsa_acc!(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_acc(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_scl!(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to `v*a`. 

### Input
- `a` -- Source TPSA `a`
- `v` -- Scalar with double precision

### Output
- `c` -- Destination TPSA `c = v*a`
"""
function mad_tpsa_scl!(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_scl(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_inv!(a::Ptr{RTPSA{Desc}},  v::Cdouble, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to `v/a`. 

### Input
- `a` -- Source TPSA `a`
- `v` -- Scalar with double precision

### Output
- `c` -- Destination TPSA `c = v/a`
"""
function mad_tpsa_inv!(a::Ptr{RTPSA{Desc}},  v::Cdouble, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_inv(a::Ptr{RTPSA{Desc}},  v::Cdouble, c::Ptr{RTPSA{Desc}})::Cvoid
end

"""
    mad_tpsa_invsqrt!(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})

Sets TPSA `c` to `v/sqrt(a)`. 

### Input
- `a` -- Source TPSA `a`
- `v` -- Scalar with double precision

### Output
- `c` -- Destination TPSA `c = v/sqrt(a)``
"""
function mad_tpsa_invsqrt!(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_invsqrt(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_unit!(x::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

Interpreting TPSA as a vector, gets the "unit vector", e.g. `r = x/norm(x)`. 
May be useful for checking for convergence.

### Input
- `x` -- Source TPSA `x`

### Output
- `r` -- Destination TPSA `r`
"""
function  mad_tpsa_unit!(x::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_unit(x::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_atan2!(y::Ptr{RTPSA{Desc}}, x::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

Sets TPSA `r` to `atan2(y,x)`

### Input
- `y` -- Source TPSA `y`
- `x` -- Source TPSA `x`

### Output
- `r` -- Destination TPSA r = atan2(y,x)
"""
function  mad_tpsa_atan2!(y::Ptr{RTPSA{Desc}}, x::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_atan2(y::Ptr{RTPSA{Desc}}, x::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end

"""
    mad_tpsa_hypot!(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

Sets TPSA `r` to `sqrt(x^2+y^2)`. Used to oversimplify polymorphism in code but not optimized

### Input
- `x` -- Source TPSA `x`
- `y` -- Source TPSA `y`

### Output
- `r` -- Destination TPSA r = sqrt(x^2+y^2)
"""
function  mad_tpsa_hypot!(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_hypot(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end

"""
    mad_tpsa_hypot3!(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, z::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

Sets TPSA `r` to `sqrt(x^2+y^2+z^2)`

### Input
- `x` -- Source TPSA `x`
- `y` -- Source TPSA `y`
- `z` -- Source TPSA `z`

### Output
- `r` -- Destination TPSA `r = sqrt(x^2+y^2+z^2)`
"""
function  mad_tpsa_hypot3!(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, z::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_hypot3(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, z::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end



"""
    mad_tpsa_integ!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, iv::Cint)

Integrates TPSA with respect to the variable with index `iv`.

### Input
- `a`  -- Source TPSA to integrate
- `iv` -- Index of variable to integrate over (e.g. integrate over `x`, `iv = 1`). 

### Output
- `c`  -- Destination TPSA
"""
function mad_tpsa_integ!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, iv::Cint)
  @ccall MAD_TPSA.mad_tpsa_integ(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, iv::Cint)::Cvoid
end


"""
    mad_tpsa_deriv!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, iv::Cint)

Differentiates TPSA with respect to the variable with index `iv`.

### Input
- `a`  -- Source TPSA to differentiate
- `iv` -- Index of variable to take derivative wrt to (e.g. derivative wrt `x`, `iv = 1`). 

### Output
- `c`  -- Destination TPSA
"""
function mad_tpsa_deriv!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, iv::Cint)
  @ccall MAD_TPSA.mad_tpsa_deriv(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, iv::Cint)::Cvoid
end


"""
    mad_tpsa_derivm!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})

Differentiates TPSA with respect to the monomial defined by byte array `m`.

### Input
- `a` -- Source TPSA to differentiate
- `n` -- Length of monomial to differentiate wrt
- `m` -- Monomial to take derivative wrt

### Output
- `c` -- Destination TPSA
"""
function mad_tpsa_derivm!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})
  @ccall MAD_TPSA.mad_tpsa_derivm(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cvoid
end


"""
    mad_tpsa_poisbra!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, nv::Cint)

Sets TPSA `c` to the poisson bracket of TPSAs `a` and `b`.

### Input
- `a`  -- Source TPSA `a`
- `b`  -- Source TPSA `b`
- `nv` -- Number of variables in the TPSA

### Output
- `c`  -- Destination TPSA `c`
"""
function mad_tpsa_poisbra!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, nv::Cint)
  @ccall MAD_TPSA.mad_tpsa_poisbra(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, nv::Cint)::Cvoid
end


"""
    mad_tpsa_taylor!(a::Ptr{RTPSA{Desc}}, n::Cint, coef::Ptr{Cdouble}, c::Ptr{RTPSA{Desc}})

Computes the result of the Taylor series up to order `n-1` with Taylor coefficients coef for the scalar value in `a`. That is,
`c = coef[0] + coef[1]*a_0 + coef[2]*a_0^2 + ...` where `a_0` is the scalar part of TPSA `a`.

### Input
- `a`    -- TPSA `a`
- `n`    -- `Order-1` of Taylor expansion, size of `coef` array
- `coef` -- Array of coefficients in Taylor `s`
- `c`    -- Result
"""
function mad_tpsa_taylor!(a::Ptr{RTPSA{Desc}}, n::Cint, coef::Ptr{Cdouble}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_taylor(a::Ptr{RTPSA{Desc}}, n::Cint, coef::Ptr{Cdouble}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_axpb!(a::Cdouble, x::Ptr{RTPSA{Desc}}, b::Cdouble, r::Ptr{RTPSA{Desc}})

`r = a*x + b`

### Input
- `a` -- Scalar `a`
- `x` -- TPSA `x`
- `b` -- Scalar `b`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_tpsa_axpb!(a::Cdouble, x::Ptr{RTPSA{Desc}}, b::Cdouble, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_axpb(a::Cdouble, x::Ptr{RTPSA{Desc}}, b::Cdouble, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_axpbypc!(a::Cdouble, x::Ptr{RTPSA{Desc}}, b::Cdouble, y::Ptr{RTPSA{Desc}}, c::Cdouble, r::Ptr{RTPSA{Desc}})

`r = a*x + b*y + c`

### Input
- `a` -- Scalar `a`
- `x` -- TPSA `x`
- `b` -- Scalar `b`
- `y` -- TPSA `y`
- `c` -- Scalar `c`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_tpsa_axpbypc!(a::Cdouble, x::Ptr{RTPSA{Desc}}, b::Cdouble, y::Ptr{RTPSA{Desc}}, c::Cdouble, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_axpbypc(a::Cdouble, x::Ptr{RTPSA{Desc}}, b::Cdouble, y::Ptr{RTPSA{Desc}}, c::Cdouble, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_axypb!(a::Cdouble, x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, b::Cdouble, r::Ptr{RTPSA{Desc}})

`r = a*x*y + b`

### Input
- `a` -- Scalar `a`
- `x` -- TPSA `x`
- `y` -- TPSA `y`
- `b` -- Scalar `b`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_tpsa_axypb!(a::Cdouble, x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, b::Cdouble, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_axypb(a::Cdouble, x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, b::Cdouble, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_axypbzpc!(a::Cdouble, x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, b::Cdouble, z::Ptr{RTPSA{Desc}}, c::Cdouble, r::Ptr{RTPSA{Desc}})

`r = a*x*y + b*z + c`

### Input
- `a` -- Scalar `a`
- `x` -- TPSA `x`
- `y` -- TPSA `y`
- `b` -- Scalar `b`
- `z` -- TPSA `z`
- `c` -- Scalar `c`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_tpsa_axypbzpc!(a::Cdouble, x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, b::Cdouble, z::Ptr{RTPSA{Desc}}, c::Cdouble, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_axypbzpc(a::Cdouble, x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, b::Cdouble, z::Ptr{RTPSA{Desc}}, c::Cdouble, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_axypbvwpc!(a::Cdouble, x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, b::Cdouble, v::Ptr{RTPSA{Desc}}, w::Ptr{RTPSA{Desc}}, c::Cdouble, r::Ptr{RTPSA{Desc}})

`r = a*x*y + b*v*w + c`

### Input
- `a` -- Scalar `a`
- `x` -- TPSA `x`
- `y` -- TPSA `y`
- `b` -- Scalar `b`
- `v` -- TPSA v
- `w` -- TPSA w
- `c` -- Scalar `c`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_tpsa_axypbvwpc!(a::Cdouble, x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, b::Cdouble, v::Ptr{RTPSA{Desc}}, w::Ptr{RTPSA{Desc}}, c::Cdouble, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_axypbvwpc(a::Cdouble, x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, b::Cdouble, v::Ptr{RTPSA{Desc}}, w::Ptr{RTPSA{Desc}}, c::Cdouble, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_ax2pby2pcz2!(a::Cdouble, x::Ptr{RTPSA{Desc}}, b::Cdouble, y::Ptr{RTPSA{Desc}}, c::Cdouble, z::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

`r = a*x^2 + b*y^2 + c*z^2`

### Input
- `a` -- Scalar `a`
- `x` -- TPSA `x`
- `b` -- Scalar `b`
- `y` -- TPSA `y`
- `c` -- Scalar `c`
- `z` -- TPSA `z`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_tpsa_ax2pby2pcz2!(a::Cdouble, x::Ptr{RTPSA{Desc}}, b::Cdouble, y::Ptr{RTPSA{Desc}}, c::Cdouble, z::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_ax2pby2pcz2(a::Cdouble, x::Ptr{RTPSA{Desc}}, b::Cdouble, y::Ptr{RTPSA{Desc}}, c::Cdouble, z::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_axpsqrtbpcx2!(x::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Cdouble, r::Ptr{RTPSA{Desc}})

`r = a*x + sqrt(b + c*x^2)`

### Input
- `x` -- TPSA `x`
- `a` -- Scalar `a`
- `b` -- Scalar `b`
- `c` -- Scalar `c`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_tpsa_axpsqrtbpcx2!(x::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Cdouble, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_axpsqrtbpcx2(x::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Cdouble, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_logaxpsqrtbpcx2!(x::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Cdouble, r::Ptr{RTPSA{Desc}})

`r = log(a*x + sqrt(b + c*x^2))`

### Input
- `x` -- TPSA `x`
- `a` -- Scalar `a`
- `b` -- Scalar `b`
- `c` -- Scalar `c`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_tpsa_logaxpsqrtbpcx2!(x::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Cdouble, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_logaxpsqrtbpcx2(x::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Cdouble, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_logxdy!(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

`r = log(x / y)`

### Input
- `x` -- TPSA `x`
- `y` -- TPSA `y`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_tpsa_logxdy!(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_logxdy(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_vec2fld!(na::Cint, a::Ptr{RTPSA{Desc}}, mc::Ptr{Ptr{RTPSA{Desc}}})

Writes the vector a in terms 
mc is a map (m is map)
Take vector a, write in terms of all variables in mc 
scalar potential described as TPSA -> vector field
???
Map to hamiltonian

### Input
- `na`  -- Number of TPSA in mc consistent with number of variables in a
- `a`   
- `mc`
"""
function mad_tpsa_vec2fld!(na::Cint, a::Ptr{RTPSA{Desc}}, mc::Ptr{Ptr{RTPSA{Desc}}})
  @ccall MAD_TPSA.mad_tpsa_vec2fld(na::Cint, a::Ptr{RTPSA{Desc}}, mc::Ptr{Ptr{RTPSA{Desc}}})::Cvoid
end


"""
    mad_tpsa_fld2vec!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, c::Ptr{RTPSA{Desc}})

???

### Input
- `na`
- `ma`
- `c`
"""
function mad_tpsa_fld2vec!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_fld2vec(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_fgrad!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Derivating vs a variable,. multiply and add sum
Deriving a map vs each variable by itself TPSA
???
"Incomplete operator" used by the incomplete poisson bracket- computing PB with only 1 
only positive part of PB
Taking 1 TPSA, derive TPSA vs all variables, multiply result by variable in map and then sum

### Input
- `na`
- `ma`
- `b`
- `c`
"""
function mad_tpsa_fgrad!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_fgrad(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_liebra!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})

Computes the Lie bracket of the maps `ma` and `mb`.

### Input
- `na` -- Number of TPSAs in map `ma` and map `mb`
- `ma` -- Map `ma`
- `mb` -- Map `mb`

### Output
- `mc` -- Destination map `mc`
"""
function mad_tpsa_liebra!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})
  @ccall MAD_TPSA.mad_tpsa_liebra(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})::Cvoid
end


"""
    mad_tpsa_exppb!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})

Computes the exponential of the Poisson bracket of the maps `ma` and `mb`.

### Input
- `na` -- Number of TPSAs in map `ma` and map `mb`
- `ma` -- Map `ma`
- `mb` -- Map `mb`

### Output
- `mc` -- Destination map `mc`
"""
function mad_tpsa_exppb!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})
  @ccall MAD_TPSA.mad_tpsa_exppb(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})::Cvoid
end


"""
    mad_tpsa_logpb!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})

Computes the log of the Poisson bracket of the maps `ma` and `mb`.

### Input
- `na` -- Number of TPSAs in map `ma` and map `mb`
- `ma` -- map `ma`
- `mb` -- map `mb`

### Output
- `mc` -- Destination map `mc`
"""
function mad_tpsa_logpb!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})
  @ccall MAD_TPSA.mad_tpsa_logpb(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})::Cvoid
end



"""
    mad_tpsa_mnrm(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}})::Cdouble

Computes the norm of the map (sum of absolute value of coefficients of all TPSAs in the map).

### Input
- `na`  -- Number of TPSAs in the map
- `ma`  -- map `ma`

### Output
- `nrm` -- Norm of map (sum of absolute value of coefficients of all TPSAs in the map)
"""
function mad_tpsa_mnrm(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}})::Cdouble
  nrm = @ccall MAD_TPSA.mad_tpsa_mnrm(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}})::Cdouble
  return nrm
end


"""
    mad_tpsa_minv!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})

Inverts the map.

### Input
- `na` -- Number of TPSAs in the map
- `ma` -- map `ma`

### Output
- `mc` -- Inversion of map `ma`
"""
function mad_tpsa_minv!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})
  @ccall MAD_TPSA.mad_tpsa_minv(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})::Cvoid
end


"""
    mad_tpsa_pminv!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}}, select::Ptr{Cint})

Computes the partial inverse of the map with only the selected variables, specified by 0s or 1s in select.

### Input
- `na`     -- Number of TPSAs in ma
- `ma`     -- map `ma`
- `select` -- Array of 0s or 1s defining which variables to do inverse on (atleast same size as na)'

### Output
- `mc`     -- Partially inverted map using variables specified as 1 in the select array
"""
function mad_tpsa_pminv!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}}, select::Ptr{Cint})
  @ccall MAD_TPSA.mad_tpsa_pminv(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}}, select::Ptr{Cint})::Cvoid
end


"""
    mad_tpsa_compose!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nb::Cint, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})

Composes two maps.

### Input
- `na` -- Number of TPSAs in map `ma`
- `ma` -- map `ma`
- `nb` -- Number of TPSAs in map `mb`
- `mb` -- map `mb`

### Output
- `mc` -- Composition of maps `ma` and `mb`
"""
function mad_tpsa_compose!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nb::Cint, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})
  @ccall MAD_TPSA.mad_tpsa_compose(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nb::Cint, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})::Cvoid
end


"""
    mad_tpsa_translate!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nb::Cint, tb::Ptr{Cdouble}, mc::Ptr{Ptr{RTPSA{Desc}}})

Translates the expansion point of the map by the amount `tb`.

### Input
- `na` -- Number of TPSAS in the map
- `ma` -- map `ma`
- `nb` -- Length of `tb`
- `tb` -- Vector of amount to translate for each varaible

### Output
- `mc` -- Map evaluated at the new point translated `tb` from the original evaluation point
"""
function mad_tpsa_translate!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nb::Cint, tb::Ptr{Cdouble}, mc::Ptr{Ptr{RTPSA{Desc}}})
  @ccall MAD_TPSA.mad_tpsa_translate(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nb::Cint, tb::Ptr{Cdouble}, mc::Ptr{Ptr{RTPSA{Desc}}})::Cvoid
end


"""
    mad_tpsa_eval!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nb::Cint, tb::Ptr{Cdouble}, tc::Ptr{Cdouble})

Evaluates the map at the point `tb`

### Input
- `na` -- Number of TPSAs in the map
- `ma` -- map `ma`
- `nb` -- Length of `tb`
- `tb` -- Point at which to evaluate the map

### Output
- `tc` -- Values for each TPSA in the map evaluated at the point `tb`
"""
function mad_tpsa_eval!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nb::Cint, tb::Ptr{Cdouble}, tc::Ptr{Cdouble})
  @ccall MAD_TPSA.mad_tpsa_eval(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nb::Cint, tb::Ptr{Cdouble}, tc::Ptr{Cdouble})::Cvoid
end


"""
    mad_tpsa_mconv!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nc::Cint, mc::Ptr{Ptr{RTPSA{Desc}}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)

Equivalent to mad_tpsa_convert, but applies the conversion to all TPSAs in the map `ma`.

### Input
- `na`   -- Number of TPSAs in the map
- `ma`   -- map `ma`
- `nc`   -- Number of TPSAs in the output map `mc`
- `n`    -- Length of vector (size of `t2r_`)
- `t2r_` -- (Optional) Vector of index lookup
- `pb`   -- Poisson bracket, 0, 1:fwd, -1:bwd

### Output
- `mc`   -- map `mc` with specified conversions 
"""
function mad_tpsa_mconv!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nc::Cint, mc::Ptr{Ptr{RTPSA{Desc}}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)
  @ccall MAD_TPSA.mad_tpsa_mconv(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nc::Cint, mc::Ptr{Ptr{RTPSA{Desc}}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)::Cvoid
end


"""
    mad_tpsa_print(t::Ptr{RTPSA{Desc}}, name_::Cstring, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid})

Prints the TPSA coefficients with precision `eps_`. If `nohdr_` is not zero, 
the header is not printed. 

### Input
- `t`       -- TPSA to print
- `name_`   -- (Optional) Name of TPSA
- `eps_`    -- (Optional) Precision to output
- `nohdr_`  -- (Optional) If True, no header is printed
- `stream_` -- (Optional) `FILE` pointer of output stream. Default is `stdout`
"""
function mad_tpsa_print(t::Ptr{RTPSA{Desc}}, name_::Cstring, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_tpsa_print(t::Ptr{RTPSA{Desc}}, name_::Cstring, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid})::Cvoid
end


"""
    mad_tpsa_scan(stream_::Ptr{Cvoid})::Ptr{RTPSA{Desc}}

Scans in a TPSA from the `stream_`.

### Input
- `stream_` -- (Optional) I/O stream from which to read the TPSA, default is `stdin`

### Output
- `t`       -- TPSA scanned from I/O `stream_`
"""
function mad_tpsa_scan(stream_::Ptr{Cvoid})::Ptr{RTPSA{Desc}}
  t = @ccall MAD_TPSA.mad_tpsa_scan(stream_::Ptr{Cvoid})::Ptr{RTPSA{Desc}}
  return t
end


"""
    mad_tpsa_scan_hdr(kind_::Ptr{Cint}, name_::Ptr{Cuchar}, stream_::Ptr{Cvoid})::Ptr{Desc{RTPSA,CTPSA}}

Read TPSA header. Returns descriptor for TPSA given the header. This is useful for external languages using 
this library where the memory is managed NOT on the C side.

### Input
- `kind_`   -- (Optional) Real or complex TPSA, or detect automatically if not provided.
- `name_`   -- (Optional) Name of TPSA
- `stream_` -- (Optional) I/O stream to read TPSA from,  default is `stdin`

### Output
- `ret`     -- Descriptor for the TPSA 
"""
function mad_tpsa_scan_hdr(kind_::Ptr{Cint}, name_::Ptr{Cuchar}, stream_::Ptr{Cvoid})::Ptr{Desc{RTPSA,CTPSA}}
  desc = @ccall MAD_TPSA.mad_tpsa_scan_hdr(kind_::Ptr{Cint}, name_::Ptr{Cuchar}, stream_::Ptr{Cvoid})::Ptr{Desc{RTPSA,CTPSA}}
  return ret
end


"""
    mad_tpsa_scan_coef!(t::Ptr{RTPSA{Desc}}, stream_::Ptr{Cvoid})

Read TPSA coefficients into TPSA `t`. This should be used with `mad_tpsa_scan_hdr` for external languages using 
this library where the memory is managed NOT on the C side.

### Input
- `stream_` -- (Optional) I/O stream to read TPSA from,  default is `stdin`

### Output
- `t`       -- TPSA with coefficients scanned from `stream_`
"""
function mad_tpsa_scan_coef!(t::Ptr{RTPSA{Desc}}, stream_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_tpsa_scan_coef(t::Ptr{RTPSA{Desc}}, stream_::Ptr{Cvoid})::Cvoid
end


"""
    mad_tpsa_debug(t::Ptr{RTPSA{Desc}}, name_::Cstring, fnam_::Cstring, line_::Cint, stream_::Ptr{Cvoid})

Prints TPSA with all information of data structure.

### Input
- `t`       -- TPSA
- `name_`   -- (Optional) Name of TPSA
- `fnam_`   -- (Optional) File name to print to
- `line_`   -- (Optional) Line number in file to start at
- `stream_` -- (Optional) I/O stream to print to, default is `stdout`
"""
function mad_tpsa_debug(t::Ptr{RTPSA{Desc}}, name_::Cstring, fnam_::Cstring, line_::Cint, stream_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_tpsa_debug(t::Ptr{RTPSA{Desc}}, name_::Cstring, fnam_::Cstring, line_::Cint, stream_::Ptr{Cvoid})::Cvoid
end

"""
    mad_tpsa_isvalid(t::Ptr{RTPSA{Desc}})::Cuchar

Sanity check of the TPSA integrity.

### Input
- `t` -- TPSA to check if valid

### Output
- `ret`  -- True if valid TPSA, false otherwise
"""
function mad_tpsa_isvalid(t::Ptr{RTPSA{Desc}})::Cuchar
  ret = @ccall MAD_TPSA.mad_tpsa_isvalid(t::Ptr{RTPSA{Desc}})::Cuchar
  return ret
end


"""
    mad_tpsa_init(t::Ptr{RTPSA{Desc}}, d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{RTPSA{Desc}}

Unsafe initialization of an already existing TPSA `t` with maximum order `mo` to the descriptor `d`. `mo` must be less than 
the maximum order of the descriptor. `t` is modified in place and also returned.

### Input
- `t`  -- TPSA to initialize to descriptor `d`
- `d`  -- Descriptor
- `mo` -- Maximum order of the TPSA (must be less than maximum order of the descriptor)

### Output
- `t`  -- TPSA initialized to descriptor `d` with maximum order `mo`
"""
function mad_tpsa_init!(t::Ptr{RTPSA{Desc}}, d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
  t = @ccall MAD_TPSA.mad_tpsa_init(t::Ptr{RTPSA{Desc}}, d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
  return t
end