"""
    mad_ctpsa_newd(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{CTPSA{Desc}}

Creates a complex TPSA defined by the specified descriptor and maximum order. If MAD_CTPSA_DEFAULT 
is passed for `mo`, the `mo` defined in the descriptor is used. If `mo > d_mo`, then `mo = d_mo`.

### Input
- `d`  -- Descriptor
- `mo` -- Maximum order

### Output
- `t`  -- New complex TPSA defined by the descriptor
"""
function mad_ctpsa_newd(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{CTPSA{Desc}}
  t = @ccall MAD_TPSA.mad_ctpsa_newd(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{CTPSA{Desc}}
  return t
end


"""
    mad_ctpsa_new(t::Ptr{CTPSA{Desc}}, mo::Cuchar)::Ptr{CTPSA{Desc}}

Creates a complex TPSA copy of the inputted TPSA, with maximum order specified by mo.
If `MAD_TPSA_SAME` is passed for `mo`, the `mo` currently in `t` is used for the created TPSA.
Ok with `t=(tpsa_t*)ctpsa`

### Input
- `t`   -- Complex TPSA to copy
- `mo`  -- Maximum order of new TPSA

### Output
- `ret` -- New complex TPSA with maximum order `mo`
"""
function mad_ctpsa_new(t::Ptr{CTPSA{Desc}}, mo::Cuchar)::Ptr{CTPSA{Desc}}
  ret = @ccall MAD_TPSA.mad_ctpsa_new(t::Ptr{CTPSA{Desc}}, mo::Cuchar)::Ptr{CTPSA{Desc}}
  return ret
end


"""
    mad_ctpsa_del!(t::Ptr{CTPSA{Desc}})

Calls the destructor for the complex TPSA.

### Input
- `t` -- Complex TPSA to destruct
"""
function mad_ctpsa_del!(t::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_del(t::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_desc(t::Ptr{CTPSA{Desc}})::Ptr{Desc{RTPSA,CTPSA}}

Gets the descriptor for the complex TPSA.

### Input
- `t`   -- Complex TPSA

### Output
- `ret` -- Descriptor for the TPSA
"""
function mad_ctpsa_desc(t::Ptr{CTPSA{Desc}})::Ptr{Desc{RTPSA,CTPSA}}
  ret = @ccall MAD_TPSA.mad_ctpsa_desc(t::Ptr{CTPSA{Desc}})::Ptr{Desc{RTPSA,CTPSA}}
  return ret
end


"""
    mad_ctpsa_uid!(t::Ptr{CTPSA{Desc}}, uid_::Cint)::Cint

Sets the TPSA `uid` if `uid_ != 0`, and returns the current (previous if set) TPSA `uid`. 

### Input
- `t`    -- Complex TPSA
- `uid_` -- `uid` to set in the TPSA if `uid_ != 0`

### Output
- `ret`  -- Current (previous if set) TPSA `uid`
"""
function mad_ctpsa_uid!(t::Ptr{CTPSA{Desc}}, uid_::Cint)::Cint
  ret = @ccall MAD_TPSA.mad_ctpsa_uid(t::Ptr{CTPSA{Desc}}, uid_::Cint)::Cint
  return ret
end


"""
    mad_ctpsa_len(t::Ptr{CTPSA{Desc}})::Cint

Gets the length of the TPSA itself (e.g. the descriptor may be order 10 but TPSA may only be order 2)

### Input
- `t`   -- Complex TPSA

### Output
- `ret` -- Length of CTPSA
"""
function mad_ctpsa_len(t::Ptr{CTPSA{Desc}})::Cint
  ret = @ccall MAD_TPSA.mad_ctpsa_len(t::Ptr{CTPSA{Desc}})::Cint
  return ret
end


"""
    mad_ctpsa_nam(t::Ptr{CTPSA{Desc}})::Cstring

Get the name of the TPSA.

### Input
- `t`    -- Complex TPSA

### Output
- `ret`  -- Name of CTPSA (Null terminated in C)
"""
function mad_ctpsa_nam(t::Ptr{CTPSA{Desc}})::Cstring
  ret = @ccall MAD_TPSA.mad_ctpsa_nam(t::Ptr{CTPSA{Desc}})::Cstring
  return ret
end


"""
    mad_ctpsa_ord(t::Ptr{CTPSA{Desc}})::Cuchar

Gets the TPSA order.

### Input
- `t`   -- Complex TPSA

### Output
- `ret` -- Order of TPSA
"""
function mad_ctpsa_ord(t::Ptr{CTPSA{Desc}})::Cuchar
  ret = @ccall MAD_TPSA.mad_ctpsa_ord(t::Ptr{CTPSA{Desc}})::Cuchar
  return ret
end

"""
    mad_ctpsa_ordv(t::Ptr{CTPSA{Desc}}, ts::Ptr{CTPSA{Desc}}...)::Cuchar

Returns maximum order of all TPSAs provided.

### Input
- `t`  -- TPSA
- `ts` -- Variable number of TPSAs passed as parameters

### Output
- `mo` -- Maximum order of all TPSAs provided
"""
function mad_ctpsa_ordv(t::Ptr{CTPSA{Desc}}, ts::Ptr{CTPSA{Desc}}...)::Cuchar
  # mo = @ccall MAD_TPSA.mad_ctpsa_ordv(t::Ptr{CTPSA{Desc}}, ts::Ptr{CTPSA{Desc}}..., 0::Cint)::Cuchar # null pointer after args for safe use
  ccall((:mad_tpsa_ordv, MAD_TPSA), Cuchar, (Ptr{CTPSA{Desc}}, Ptr{CTPSA{Desc}}...), t, ts...)
  return mo
end


"""
    mad_ctpsa_ordn(n::Cint, t::Ptr{Ptr{CTPSA{Desc}}})::Cuchar

Returns the max order of all TPSAs in `t`.

### Input
- `n`  -- Number of TPSAs
- `t`  -- Array of TPSAs 

### Output
- `mo` -- Maximum order of all TPSAs
"""
function mad_ctpsa_ordn(n::Cint, t::Ptr{Ptr{CTPSA{Desc}}})::Cuchar
  mo = @ccall MAD_TPSA.mad_ctpsa_ordn(n::Cint, t::Ptr{Ptr{CTPSA{Desc}}})::Cuchar
  return mo
end


"""
    mad_ctpsa_copy!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})

Makes a copy of the complex TPSA `t` to `r`.

### Input
- `t` -- Source complex TPSA

### Output
- `r` -- Destination complex TPSA
"""
function mad_ctpsa_copy!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_copy(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_sclord!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, inv::Cuchar, prm::Cuchar)

Scales all coefficients by order. If `inv == 0`, scales coefficients by order (derivation), else scales coefficients 
by 1/order (integration).

### Input
- `t`   -- Source complex TPSA
- `inv` -- Put order up, divide, scale by `inv` of value of order
- `prm` -- Parameters flag. If set to 0x0, the scaling excludes the order of the parameters in the monomials. Else, scaling is with total order of monomial

### Output
- `r`   -- Destination complex TPSA
"""
function mad_ctpsa_sclord!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, inv::Cuchar, prm::Cuchar)
  @ccall MAD_TPSA.mad_ctpsa_sclord(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, inv::Cuchar, prm::Cuchar)::Cvoid
end


"""
    mad_ctpsa_getord!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, ord::Cuchar)

Extract one homogeneous polynomial of the given order

### Input
- `t``  -- Sourcecomplex TPSA
- `ord` -- Order to retrieve

### Output
- `r`   -- Destination complex TPSA
"""
function mad_ctpsa_getord!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, ord::Cuchar)
  @ccall MAD_TPSA.mad_ctpsa_getord(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, ord::Cuchar)::Cvoid
end


"""
    mad_ctpsa_cutord!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, ord::Cint)

Cuts the TPSA off at the given order and above, or if `ord` is negative, will cut orders below 
`abs(ord)` (e.g. if `ord` = -3, then orders 0-3 are cut off).

### Input
- `t`   -- Source complex TPSA
- `ord` -- Cut order: `0..-ord` or `ord..mo`

### Output
- `r`   -- Destination complex TPSA
"""
function mad_ctpsa_cutord!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, ord::Cint)
  @ccall MAD_TPSA.mad_ctpsa_cutord(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, ord::Cint)::Cvoid
end

"""
    mad_ctpsa_maxord(t::Ptr{CTPSA{Desc}}, n::Cint, idx_::Ptr{Cint})::Cint

Returns the index to the monomial with maximum abs(coefficient) in the TPSA for all orders 0 to `n`. If `idx_` 
is provided, it is filled with the indices for the maximum abs(coefficient) monomial for each order up to `n`. 

### Input
- `t`    -- Complex TPSA
- `n`    -- Highest order to include in finding the maximum abs(coefficient) in the TPSA, length of `idx_` if provided

### Output
- `idx_` -- (Optional) If provided, is filled with indices to the monomial for each order up to `n` with maximum abs(coefficient)
- `mi`   -- Index to the monomial in the TPSA with maximum abs(coefficient)
"""
function mad_ctpsa_maxord(t::Ptr{CTPSA{Desc}}, n::Cint, idx_::Ptr{Cint})::Cint
  mi = @ccall MAD_TPSA.mad_ctpsa_maxord(t::Ptr{CTPSA{Desc}}, n::Cint, idx_::Ptr{Cint})::Cint
  return mi
end

"""
    mad_ctpsa_convert!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)

General function to convert TPSAs to different orders and reshuffle canonical coordinates. The destination TPSA will 
be of order `n`, and optionally have the variable reshuffling defined by `t2r_` and poisson bracket sign. e.g. if 
`t2r_ = {1,2,3,4,6,5}` and `pb = -1`, canonical coordinates 6 and 5 are swapped and the new 5th canonical coordinate 
will be negated. Useful for comparing with different differential algebra packages.

### Input
- `t`    -- Source complex TPSA
- `n`    -- Length of vector
- `t2r_` -- (Optional) Vector of index lookup
- `pb`   -- Poisson bracket, 0, 1:fwd, -1:bwd

### Output
- `r`    -- Destination complex TPSA with specified order and canonical coordinate reshuffling.
"""
function mad_ctpsa_convert!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)
  @ccall MAD_TPSA.mad_ctpsa_convert(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)::Cvoid
end


"""
    mad_ctpsa_setvar!(t::Ptr{CTPSA{Desc}}, v::ComplexF64, iv_::Cint, scl_::ComplexF64)

Sets the 0th and 1st order values for the variables.

  ### Input
  - `t`    -- Real TPSA
  - `v`    -- 0th order value (coefficient)
  - `iv_`  -- (Optional) Variable index, optional if order of TPSA is 0 (behaves like `mad_ctpsa_setval` then)
  - `scl_` -- 1st order variable value (typically will be 1)
  """
function mad_ctpsa_setvar!(t::Ptr{CTPSA{Desc}}, v::ComplexF64, iv_::Cint, scl_::ComplexF64)
  @ccall MAD_TPSA.mad_ctpsa_setvar(t::Ptr{CTPSA{Desc}}, v::ComplexF64, iv_::Cint, scl_::ComplexF64)::Cvoid
end

"""
    mad_ctpsa_setvar_r!(t::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, iv_::Cint, scl_re_::Cdouble, scl_im_::Cdouble)

Sets the 0th and 1st order values for the variables. Equivalent to `mad_ctpsa_setvar` but without complex-by-value arguments.

### Input
- `t`       -- Complex TPSA
- `v_re`    -- Real part of 0th order value
- `v_im`    -- Imaginary part of 0th order value
- `iv_`     -- (Optional) Variable index, optional if order of TPSA is 0 (behaves `like mad_ctpsa_setval` then)
- `scl_re_` -- (Optional) Real part of 1st order variable value
- `scl_im_` -- (Optional)Imaginary part of 1st order variable value
"""
function mad_ctpsa_setvar_r!(t::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, iv_::Cint, scl_re_::Cdouble, scl_im_::Cdouble)
  @ccall MAD_TPSA.mad_ctpsa_setvar_r(t::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, iv_::Cint, scl_re_::Cdouble, scl_im_::Cdouble)::Cvoid
end

"""
    mad_ctpsa_setval!(t::Ptr{CTPSA{Desc}}, v::ComplexF64)

Sets the scalar part of the TPSA to `v` and all other values to 0 (sets the TPSA order to 0).

### Input
- `t` -- TPSA to set to scalar
- `v` -- Scalar value to set TPSA
"""
function mad_ctpsa_setval!(t::Ptr{CTPSA{Desc}}, v::ComplexF64)
  @ccall MAD_TPSA.mad_ctpsa_setval(t::Ptr{CTPSA{Desc}}, v::ComplexF64)::Cvoid
end

"""
    mad_ctpsa_setval_r!(t::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble)

Sets the scalar part of the TPSA to `v` and all other values to 0 (sets the TPSA order to 0).
Equivalent to `mad_ctpsa_setval` but without complex-by-value arguments.

### Input
- `t`    -- TPSA to set to scalar
- `v_re` -- Real part of scalar value to set TPSA
- `v_im` -- Imaginary part of scalar value to set TPSA
"""
function mad_ctpsa_setval_r!(t::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble)
  @ccall MAD_TPSA.mad_ctpsa_setval_r(t::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble)::Cvoid
end

"""
    mad_ctpsa_setnam!(t::Ptr{CTPSA{Desc}}, nam::Cstring)

Sets the name of the CTPSA.

### Input
- `t`   -- Complex TPSA
- `nam` -- Name to set for CTPSA
"""
function mad_ctpsa_setnam!(t::Ptr{CTPSA{Desc}}, nam::Cstring)
  @ccall MAD_TPSA.mad_ctpsa_setnam(t::Ptr{CTPSA{Desc}}, nam::Cstring)::Cvoid
end


"""
    mad_ctpsa_clear!(t::Ptr{CTPSA{Desc}})

Clears the TPSA (reset to 0)

### Input
- `t` -- Complex TPSA
"""
function mad_ctpsa_clear!(t::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_clear(t::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_isnul(t::Ptr{CTPSA{Desc}})::Cuchar

Checks if TPSA is 0 or not

### Input
- `t`    -- Complex TPSA to check

### Output
- `ret`  -- True or false
"""
function mad_ctpsa_isnul(t::Ptr{CTPSA{Desc}})::Cuchar
  ret = @ccall MAD_TPSA.mad_ctpsa_isnul(t::Ptr{CTPSA{Desc}})::Cuchar
  return ret
end



""" 
    mad_ctpsa_cplx!(re_::Ptr{RTPSA{Desc}}, im_::Ptr{RTPSA{Desc}}, r::Ptr{CTPSA{Desc}})

Creates a CTPSA with real and imaginary parts from the RTPSAs `re_` and `im_` respectively.

### Input
- `re_` -- Real part of CTPSA to make
- `im_` -- Imaginary part of CTPSA to make

### Output
- `r`   -- Destination CTPSA with `r = re_ + im*im_`
"""
function mad_ctpsa_cplx!(re_::Ptr{RTPSA{Desc}}, im_::Ptr{RTPSA{Desc}}, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_cplx(re_::Ptr{RTPSA{Desc}}, im_::Ptr{RTPSA{Desc}}, r::Ptr{CTPSA{Desc}})::Cvoid
end


""" 
    mad_ctpsa_real!(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

Sets the RTPSA `r` equal to the real part of CTPSA `t`.

### Input
- `t` -- Source CTPSA

### Output
- `r` -- Destination RTPSA with `r = Re(t)`
"""
function mad_ctpsa_real!(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_real(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end


""" 
    mad_ctpsa_imag!(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

Sets the RTPSA `r` equal to the imaginary part of CTPSA `t`.

### Input
- `t` -- Source CTPSA

### Output
- `r` -- Destination RTPSA with `r = Im(t)`
"""
function mad_ctpsa_imag!(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_imag(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end

""" 
    mad_ctpsa_cabs!(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

Sets the RTPSA `r` equal to the aboslute value of CTPSA `t`

### Input
- `t` -- Source CTPSA

### Output
- `r` -- Destination RTPSA with `r = |t|`
"""
function mad_ctpsa_cabs!(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_cabs(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end


""" 
    mad_ctpsa_carg!(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

Sets the RTPSA `r` equal to the argument (phase) of CTPSA `t`

### Input
- `t` -- Source CTPSA

### Output
- `r` -- Destination RTPSA with `r = carg(t)`
"""
function mad_ctpsa_carg!(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_carg(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end


""" 
    mad_ctpsa_unit!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})

Interpreting TPSA `a` vector, gets the "unit vector", e.g. `r = t/norm(t)`. May be useful for checking for convergence.

### Input
- `t` -- Source TPSA `x`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_ctpsa_unit!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_unit(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})::Cvoid
end


""" 
    mad_ctpsa_rect!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})

Sets `r = Re(t)*cos(Im(t)) + im*Re(t)*sin(Im(t))`

### Input
- `t` -- Source CTPSA
- `r` -- Destination CTPSA
"""
function mad_ctpsa_rect!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_rect(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})::Cvoid
end


""" 
    mad_ctpsa_polar!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})

Sets `r = |t| + im*atan2(Im(t), Re(t))`

### Input
- `t` -- Source CTPSA
- `r` -- Destination CTPSA
"""
function mad_ctpsa_polar!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_polar(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_mono!(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, p_::Ptr{Cuchar})::Cuchar

Returns the order of the monomial at index `i` in the TPSA and optionally the monomial at that index is returned in `m_`
and the order of parameters in the monomial in `p_`

### Input
- `t`   -- TPSA
- `i`   -- Index valid in TPSA
- `n`   -- Length of monomial

### Output
- `m_`  -- (Optional) Monomial at index `i` in TPSA
- `p_`  -- (Optional) Order of parameters in monomial
- `ret` -- Order of monomial in TPSA `a` index `i`
"""
function mad_ctpsa_mono!(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, p_::Ptr{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_ctpsa_mono(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, p_::Ptr{Cuchar})::Cuchar
  return ret
end


"""
    mad_ctpsa_idxs(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring)::Cint

Returns index of monomial in the TPSA given the monomial as string. This generally should not be used, as there 
are no assumptions about which monomial is attached to which index.

### Input
- `t`   -- TPSA
- `n`   -- Length of monomial
- `s`   -- Monomial as string

### Output
- `ret` -- Index of monomial in TPSA
"""
function mad_ctpsa_idxs(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring)::Cint
  ret = @ccall MAD_TPSA.mad_ctpsa_idxs(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring)::Cint
  return ret
end



"""
    mad_ctpsa_idxm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint


Returns index of monomial in the TPSA given the monomial as a byte array. This generally should not be used, as there 
are no assumptions about which monomial is attached to which index.

### Input
- `t`   -- TPSA
- `n`   -- Length of monomial
- `s`   -- Monomial as byte array

### Output
- `ret` -- Index of monomial in TPSA
"""
function mad_ctpsa_idxm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint
  ret = @ccall MAD_TPSA.mad_ctpsa_idxm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint
  return ret
end


"""
    mad_ctpsa_idxsm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint})::Cint

Returns index of monomial in the TPSA given the monomial as a sparse monomial. This generally should not be used, as there 
are no assumptions about which monomial is attached to which index.

### Input
- `t`   -- TPSA
- `n`   -- Length of monomial
- `s`   -- Monomial as sparse monomial

### Output
- `ret` -- Index of monomial in TPSA
"""
function mad_ctpsa_idxsm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint})::Cint
  ret = @ccall MAD_TPSA.mad_ctpsa_idxsm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint})::Cint
  return ret
end


"""
    mad_ctpsa_cycle!(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, v_::Ptr{Cdouble})::Cint

Used for scanning through each nonzero monomial in the TPSA. Given a starting index (-1 if starting at 0), will 
optionally fill monomial `m_` with the monomial at index `i` and the value at `v_` with the monomials coefficient, and 
return the next NONZERO monomial index in the TPSA. This is useful for building an iterator through the TPSA.

### Input
- `t`  -- TPSA to scan
- `i`  -- Index to start from (-1 to start at 0)
- `n`  -- Size of monomial
- `m_` -- (Optional) Monomial to be filled if provided
- `v_` -- (Optional) Pointer to value of coefficient

### Output
- `i`  -- Index of next nonzero monomial in the TPSA, or -1 if reached the end
"""
function mad_ctpsa_cycle!(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, v_::Ptr{ComplexF64})::Cint
  i = @ccall MAD_TPSA.mad_ctpsa_cycle(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, v_::Ptr{ComplexF64})::Cint
  return i
end


"""
    mad_ctpsa_get0(t::Ptr{CTPSA{Desc}})::ComplexF64

Gets the 0th order (scalar) value of the TPSA

### Input
- `t`   -- TPSA

### Output
- `ret` -- Scalar value of TPSA
"""
function mad_ctpsa_get0(t::Ptr{CTPSA{Desc}})::ComplexF64
  ret = @ccall MAD_TPSA.mad_ctpsa_get0(t::Ptr{CTPSA{Desc}})::ComplexF64
  return ret
end


"""
    mad_ctpsa_geti(t::Ptr{CTPSA{Desc}}, i::Cint)::ComplexF64

Gets the coefficient of the monomial at index `i`.  Generally should use `mad_tpsa_cycle` instead of this.

### Input
- `t`   -- TPSA
- `i`   -- Monomial index

### Output
- `ret` -- Coefficient of monomial at index `i`
"""
function mad_ctpsa_geti(t::Ptr{CTPSA{Desc}}, i::Cint)::ComplexF64
  ret = @ccall MAD_TPSA.mad_ctpsa_geti(t::Ptr{CTPSA{Desc}}, i::Cint)::ComplexF64
  return ret
end


"""
    mad_ctpsa_gets(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring)::ComplexF64

Gets the coefficient of the monomial `s` defined as a string. Generally should use `mad_tpsa_cycle` instead of this.

### Input
- `t`   -- TPSA
- `n`   -- Size of monomial
- `s`   -- Monomial as string

### Output
- `ret` -- Coefficient of monomial `s` in TPSA 
"""
function mad_ctpsa_gets(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring)::ComplexF64
  ret = @ccall MAD_TPSA.mad_ctpsa_gets(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring)::ComplexF64
  return ret
end


"""
    mad_ctpsa_getm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::ComplexF64

Gets the coefficient of the monomial `m` defined as a byte array. Generally should use `mad_tpsa_cycle` instead of this.

### Input
- `t`   -- TPSA
- `n`   -- Length of monomial
- `m`   -- Monomial as byte array

### Output
- `ret` -- Coefficient of monomial `m` in TPSA
"""
function mad_ctpsa_getm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::ComplexF64
  val = @ccall MAD_TPSA.mad_ctpsa_getm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::ComplexF64
  return ret
end


"""
    mad_ctpsa_getsm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint})::ComplexF64

Gets the coefficient of the monomial `m` defined as a sparse monomial. Generally should use `mad_tpsa_cycle` instead of this.

### Input
- `t`   -- TPSA
- `n`   -- Length of monomial
- `m`   -- Monomial as sparse monomial

### Output
- `ret` -- Coefficient of monomial `m` in TPSA
"""
function mad_ctpsa_getsm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint})::ComplexF64
  ret = @ccall MAD_TPSA.mad_ctpsa_getsm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint})::ComplexF64
  return ret
end


"""
    mad_ctpsa_set0!(t::Ptr{CTPSA{Desc}}, a::ComplexF64, b::ComplexF64)

Sets the 0th order coefficient (scalar part of TPSA) according to `coef[0] = a*coef[0] + b`. Does not modify other values in TPSA.

### Input
- `t` -- TPSA
- `a` -- Scaling of current 0th order value
- `b` -- Constant added to current 0th order value
"""
function mad_ctpsa_set0!(t::Ptr{CTPSA{Desc}}, a::ComplexF64, b::ComplexF64)
  @ccall MAD_TPSA.mad_ctpsa_set0(t::Ptr{CTPSA{Desc}}, a::ComplexF64, b::ComplexF64)::Cvoid
end


"""
    mad_ctpsa_seti!(t::Ptr{CTPSA{Desc}}, i::Cint, a::ComplexF64, b::ComplexF64)

Sets the coefficient of monomial at index `i` to `coef[i] = a*coef[i] + b`. Does not modify other values in TPSA.

### Input
- `t` -- TPSA
- `i` -- Index of monomial
- `a` -- Scaling of current coefficient
- `b` -- Constant added to current coefficient
"""
function mad_ctpsa_seti!(t::Ptr{CTPSA{Desc}}, i::Cint, a::ComplexF64, b::ComplexF64)
  @ccall MAD_TPSA.mad_ctpsa_seti(t::Ptr{CTPSA{Desc}}, i::Cint, a::ComplexF64, b::ComplexF64)::Cvoid
end


"""
    mad_ctpsa_sets!(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, a::ComplexF64, b::ComplexF64)

Sets the coefficient of monomial defined by string `s` to `coef = a*coef + b`. Does not modify other values in TPSA.

### Input
- `t` -- TPSA
- `n` -- Length of monomial
- `s` -- Monomial as string
- `a` -- Scaling of current coefficient
- `b` -- Constant added to current coefficient
"""
function mad_ctpsa_sets!(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, a::ComplexF64, b::ComplexF64)
  @ccall MAD_TPSA.mad_ctpsa_sets(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, a::ComplexF64, b::ComplexF64)::Cvoid
end


"""
    mad_ctpsa_setm!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::ComplexF64, b::ComplexF64)

Sets the coefficient of monomial defined by byte array `m` to `coef = a*coef + b`. Does not modify other values in TPSA.

### Input
- `t` -- TPSA
- `n` -- Length of monomial
- `m` -- Monomial as byte array
- `a` -- Scaling of current coefficient
- `b` -- Constant added to current coefficient
"""
function mad_ctpsa_setm!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::ComplexF64, b::ComplexF64)
  @ccall MAD_TPSA.mad_ctpsa_setm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::ComplexF64, b::ComplexF64)::Cvoid
end


"""
    mad_ctpsa_setsm!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint}, a::ComplexF64, b::ComplexF64)

Sets the coefficient of monomial defined by sparse monomial `m` to `coef = a*coef + b`. Does not modify other values in TPSA.

### Input
- `t` -- TPSA
- `n` -- Length of monomial
- `m` -- Monomial as sparse monomial
- `a` -- Scaling of current coefficient
- `b` -- Constant added to current coefficient
"""
function mad_ctpsa_setsm!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint}, a::ComplexF64, b::ComplexF64)
  @ccall MAD_TPSA.mad_ctpsa_setsm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint}, a::ComplexF64, b::ComplexF64)::Cvoid
end


# Accessors without complex-by-value
"""
    mad_ctpsa_get0_r!(t::Ptr{CTPSA{Desc}}, r::Ptr{ComplexF64})

Gets the 0th order (scalar) value of the TPSA in place.

### Input
- `t` -- TPSA

### Output
- `r` -- Scalar value of TPSA
"""
function mad_ctpsa_get0_r!(t::Ptr{CTPSA{Desc}}, r::Ptr{ComplexF64})
  ret = @ccall MAD_TPSA.mad_ctpsa_get0_r(t::Ptr{CTPSA{Desc}}, r::Ptr{ComplexF64})::Cvoid
  return ret
end


"""
    mad_ctpsa_geti_r!(t::Ptr{CTPSA{Desc}}, i::Cint,  r::Ptr{ComplexF64})

Gets the coefficient of the monomial at index `i` in place. Generally should use `mad_tpsa_cycle` instead of this.

### Input
- `t` -- TPSA
- `i` -- Monomial index

### Output
- `r` -- Coefficient of monomial at index `i`
"""
function mad_ctpsa_geti_r!(t::Ptr{CTPSA{Desc}}, i::Cint, r::Ptr{ComplexF64})
  ret = @ccall MAD_TPSA.mad_ctpsa_geti_r(t::Ptr{CTPSA{Desc}}, i::Cint, r::Ptr{ComplexF64})::Cvoid
  return ret
end


"""
    mad_ctpsa_gets_r!(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, r::Ptr{ComplexF64})

Gets the coefficient of the monomial `s` defined as a string in place. Generally should use `mad_tpsa_cycle` instead of this.

### Input
- `t` -- TPSA
- `n` -- Length of monomial
- `s` -- Monomial as string

### Output
- `r` -- Coefficient of monomial `s` in TPSA
"""
function mad_ctpsa_gets_r!(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, r::Ptr{ComplexF64})
  ret = @ccall MAD_TPSA.mad_ctpsa_gets_r(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, r::Ptr{ComplexF64})::Cvoid
  return ret
end


"""
    mad_ctpsa_getm_r!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, r::Ptr{ComplexF64})

Gets the coefficient of the monomial `m` defined as a byte array in place. Generally should use `mad_tpsa_cycle` instead of this.

### Input
- `t` -- TPSA
- `n` -- Length of monomial
- `m` -- Monomial as byte array

### Output
- `r` -- Coefficient of monomial `m` in TPSA
"""
function mad_ctpsa_getm_r!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, r::Ptr{ComplexF64})
  val = @ccall MAD_TPSA.mad_ctpsa_getm_r(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, r::Ptr{ComplexF64})::Cvoid
  return ret
end


"""
    mad_ctpsa_getsm_r!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint}, r::Ptr{ComplexF64})

Gets the coefficient of the monomial `m` defined as a sparse monomial in place. Generally should use `mad_tpsa_cycle` instead of this.

### Input
- `t` -- TPSA
- `n` -- Length of monomial
- `m` -- Monomial as sparse monomial

### Output
- `r` -- Coefficient of monomial `m` in TPSA
"""
function mad_ctpsa_getsm_r!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint}, r::Ptr{ComplexF64})
  ret = @ccall MAD_TPSA.mad_ctpsa_getsm_r(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint}, r::Ptr{ComplexF64})::Cvoid
  return ret
end


"""
    mad_ctpsa_set0_r!(t::Ptr{CTPSA{Desc}}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)

Sets the 0th order coefficient (scalar part of TPSA) according to `coef[0] = a*coef[0] + b`. Does not modify other values in TPSA.
Equivalent to `mad_ctpsa_set0` but without complex-by-value arguments.

### Input
- `t`    -- TPSA
- `a_re` -- Real part of `a`
- `a_im` -- Imaginary part of `a`
- `b_re` -- Real part of `b`
- `b_im` -- Imaginary part of `b`
"""
function mad_ctpsa_set0_r!(t::Ptr{CTPSA{Desc}}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)
  @ccall MAD_TPSA.mad_ctpsa_set0_r(t::Ptr{CTPSA{Desc}}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)::Cvoid
end


"""
    mad_ctpsa_seti_r!(t::Ptr{CTPSA{Desc}}, i::Cint, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)

Sets the coefficient of monomial at index `i` to `coef[i] = a*coef[i] + b`. Does not modify other values in TPSA.
Equivalent to `mad_ctpsa_seti` but without complex-by-value arguments.

### Input
- `t` -- TPSA
- `i` -- Index of monomial
- `a_re` -- Real part of `a`
- `a_im` -- Imaginary part of `a`
- `b_re` -- Real part of `b`
- `b_im` -- Imaginary part of `b`
"""
function mad_ctpsa_seti_r!(t::Ptr{CTPSA{Desc}}, i::Cint, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)
  @ccall MAD_TPSA.mad_ctpsa_seti_r(t::Ptr{CTPSA{Desc}}, i::Cint, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)::Cvoid
end


"""
    mad_ctpsa_sets_r!(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)

Sets the coefficient of monomial defined by string `s` to `coef = a*coef + b`. Does not modify other values in TPSA.
Equivalent to `mad_ctpsa_set` but without complex-by-value arguments.

### Input
- `t` -- TPSA
- `n` -- Length of monomial
- `s` -- Monomial as string
- `a_re` -- Real part of `a`
- `a_im` -- Imaginary part of `a`
- `b_re` -- Real part of `b`
- `b_im` -- Imaginary part of `b`
"""
function mad_ctpsa_sets_r!(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)
  @ccall MAD_TPSA.mad_ctpsa_sets_r(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)::Cvoid
end


"""
    mad_ctpsa_setm_r!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)

Sets the coefficient of monomial defined by byte array `m` to `coef = a*coef + b`. Does not modify other values in TPSA.
Equivalent to `mad_ctpsa_setm` but without complex-by-value arguments.

### Input
- `t` -- TPSA
- `n` -- Length of monomial
- `m` -- Monomial as byte array
- `a_re` -- Real part of `a`
- `a_im` -- Imaginary part of `a`
- `b_re` -- Real part of `b`
- `b_im` -- Imaginary part of `b`
"""
function mad_ctpsa_setm_r!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)
  @ccall MAD_TPSA.mad_ctpsa_setm_r(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)::Cvoid
end


"""
    mad_ctpsa_setsm_r!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)

Sets the coefficient of monomial defined by sparse monomial m to `coef = a*coef + b`. Does not modify other values in TPSA.
Equivalent to `mad_ctpsa_setsm` but without complex-by-value arguments.

### Input
- `t` -- TPSA
- `n` -- Length of monomial
- `m` -- Monomial as sparse monomial
- `a_re` -- Real part of `a`
- `a_im` -- Imaginary part of `a`
- `b_re` -- Real part of `b`
- `b_im` -- Imaginary part of `b`
"""
function mad_ctpsa_setsm_r!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)
  @ccall MAD_TPSA.mad_ctpsa_setsm_r(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)::Cvoid
end


"""
    mad_ctpsa_getv!(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{ComplexF64})

Vectorized getter of the coefficients for monomials with indices `i..i+n`. Useful for extracting the 1st order parts of 
a TPSA to construct a matrix (`i = 1`, `n = nv+np = nn`). 

### Input
- `t` -- TPSA
- `i` -- Starting index of monomials to get coefficients
- `n` -- Number of monomials to get coefficients of starting at `i`

### Output
- `v` -- Array of coefficients for monomials `i..i+n`
"""
function mad_ctpsa_getv!(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{ComplexF64})
  @ccall MAD_TPSA.mad_ctpsa_getv(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{ComplexF64})::Cvoid
end



"""
    mad_ctpsa_setv!(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{ComplexF64})

Vectorized setter of the coefficients for monomials with indices `i..i+n`. Useful for putting a matrix into a map.

### Input
- `t` -- TPSA
- `i` -- Starting index of monomials to set coefficients
- `n` -- Number of monomials to set coefficients of starting at `i`
- `v` -- Array of coefficients for monomials `i..i+n`
"""
function mad_ctpsa_setv!(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{ComplexF64})
  @ccall MAD_TPSA.mad_ctpsa_setv(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{ComplexF64})::Cvoid
end


"""
    mad_ctpsa_equ(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, tol_::Cdouble)::Cuchar

Checks if the TPSAs `a` and `b` are equal within the specified tolerance `tol_`. If `tol_` is not specified, `DBL_EPSILON` is used.

### Input
- `a`    -- TPSA `a`
- `b`    -- TPSA `b`
- `tol_` -- (Optional) Difference below which the TPSAs are considered equal

### Output
- `ret`   - True if `a == b` within `tol_`
"""
function mad_ctpsa_equ(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, tol_::Cdouble)::Cuchar
  ret = @ccall MAD_TPSA.mad_ctpsa_equ(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, tol_::Cdouble)::Cuchar
  return ret
end


"""
    mad_ctpsa_dif!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

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
function mad_ctpsa_dif!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_dif(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_add!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination TPSA `c = a + b`

### Input
- `a` -- Source TPSA `a`
- `b` -- Source TPSA `b`

### Output
- `c` -- Destination TPSA `c = a + b`
"""
function mad_ctpsa_add!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_add(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_sub!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination TPSA `c = a - b`

### Input
- `a` -- Source TPSA `a`
- `b` -- Source TPSA `b`

### Output
- `c` -- Destination TPSA `c = a - b`
"""
function mad_ctpsa_sub!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sub(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_mul!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination TPSA `c = a * b`

### Input
- `a` -- Source TPSA `a`
- `b` -- Source TPSA `b`

### Output
- `c` -- Destination TPSA `c = a * b`
"""
function mad_ctpsa_mul!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_mul(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_div!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination TPSA `c = a / b`

### Input
- `a` -- Source TPSA `a`
- `b` -- Source TPSA `b`

### Output
- `c` -- Destination TPSA `c = a / b`
"""
function mad_ctpsa_div!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_div(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_pow!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination TPSA `c = a ^ b`

### Input
- `a` -- Source TPSA `a`
- `b` -- Source TPSA `b`

### Output
- `c` -- Destination TPSA `c = a ^ b`
"""
function mad_ctpsa_pow!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_pow(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_powi!(a::Ptr{CTPSA{Desc}}, n::Cint, c::Ptr{CTPSA{Desc}})

Sets the destination TPSA `c = a ^ n` where `n` is an integer.

### Input
- `a` -- Source TPSA `a`
- `n` -- Integer power

### Output
- `c` -- Destination TPSA `c = a ^ n`
"""
function mad_ctpsa_powi!(a::Ptr{CTPSA{Desc}}, n::Cint, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_powi(a::Ptr{CTPSA{Desc}}, n::Cint, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_pown!(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})

Sets the destination TPSA `c = a ^ v` where `v` is of double precision.

### Input
- `a` -- Source TPSA `a`
- `v` -- Power, ComplexF64

### Output
- `c` -- Destination TPSA `c = a ^ v`
"""
function mad_ctpsa_pown!(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_pown(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_pown_r!(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})

Sets the destination TPSA `c = a ^ v` where `v` is of double precision. Without complex-by-value arguments.

### Input
- `a`    -- Source TPSA `a`
- `v_re` -- Real part of power
- `v_im` -- Imaginary part of power

### Output
- `c`    -- Destination TPSA `c = a ^ v`
"""
function mad_ctpsa_pown_r!(a::Ptr{CTPSA{Desc}},  v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_pown_r(a::Ptr{CTPSA{Desc}},  v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_equt(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, tol_::Cdouble)::Cuchar

Checks if the CTPSA `a` is equal to the RTPSA `b` within the specified tolerance `tol_` 
(internal real-to-complex conversion).

### Input
- `a`    -- CTPSA `a`
- `b`    -- RTPSA `b`
- `tol_` -- (Optional) Difference below which the TPSAs are considered equal

### Output
- `ret`   - True if `a == b` within `tol_`
"""
function mad_ctpsa_equt(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, tol_::Cdouble)::Cuchar
  ret = @ccall MAD_TPSA.mad_ctpsa_equt(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, tol_::Cdouble)::Cuchar
  return ret
end


"""
    mad_ctpsa_dift!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

For each homogeneous polynomial in CTPSA `a` and RTPSA `b`, calculates either the relative error or absolute error for each order.
If the maximum coefficient for a given order in `a` is > 1, the relative error is computed for that order. Else, the absolute 
error is computed. This is very useful for comparing maps between codes or doing unit tests. In Julia, essentially:

`c_i = (a_i.-b_i)/maximum([abs.(a_i)...,1])` where `a_i` and `b_i` are vectors of the monomials for an order `i`

### Input
- `a` -- Source CTPSA `a`
- `b` -- Source RTPSA `b`

### Output
- `c` -- Destination CTPSA `c`
"""
function mad_ctpsa_dift!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_dift(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_tdif!(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

For each homogeneous polynomial in RTPSA `a` and CTPSA `b`, calculates either the relative error or absolute error for each order.
If the maximum coefficient for a given order in `a` is > 1, the relative error is computed for that order. Else, the absolute 
error is computed. This is very useful for comparing maps between codes or doing unit tests. In Julia, essentially:

`c_i = (a_i.-b_i)/maximum([abs.(a_i)...,1])` where `a_i` and `b_i` are vectors of the monomials for an order `i`

### Input
- `a` -- Source RTPSA `a`
- `b` -- Source CTPSA `b`

### Output
- `c` -- Destination CTPSA `c`
"""
function mad_ctpsa_tdif!(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tdif(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end

"""
    mad_ctpsa_addt!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination CTPSA `c = a + b` (internal real-to-complex conversion).

### Input
- `a` -- Source CTPSA `a`
- `b` -- Source RTPSA `b`

### Output
- `c` -- Destination CTPSA `c = a + b`
"""
function mad_ctpsa_addt!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_addt(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_subt!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination CTPSA `c = a - b` (internal real-to-complex conversion).

### Input
- `a` -- Source CTPSA `a`
- `b` -- Source RTPSA `b`

### Output
- `c` -- Destination CTPSA `c = a - b`
"""
function mad_ctpsa_subt!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_subt(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_tsub!(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination CTPSA `c = a - b` (internal real-to-complex conversion).

### Input
- `a` -- Source RTPSA `a`
- `b` -- Source CTPSA `b`

### Output
- `c` -- Destination CTPSA `c = a - b`
"""
function mad_ctpsa_tsub!(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tsub(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_mult!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination CTPSA `c = a * b` (internal real-to-complex conversion).

### Input
- `a` -- Source CTPSA `a`
- `b` -- Source RTPSA `b`

### Output
- `c` -- Destination CTPSA `c = a * b`
"""
function mad_ctpsa_mult!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_mult(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_divt!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination CTPSA `c = a / b` (internal real-to-complex conversion).

### Input
- `a` -- Source CTPSA `a`
- `b` -- Source RTPSA `b`

### Output
- `c` -- Destination CTPSA `c = a / b`
"""
function mad_ctpsa_divt!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_divt(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_tdiv!(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination CTPSA `c = a / b` (internal real-to-complex conversion).

### Input
- `a` -- Source RTPSA `a`
- `b` -- Source CTPSA `b`

### Output
- `c` -- Destination CTPSA `c = a / b`
"""
function mad_ctpsa_tdiv!(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tdiv(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end

"""
    mad_ctpsa_powt!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination CTPSA `c = a ^ b` (internal real-to-complex conversion).

### Input
- `a` -- Source CTPSA `a`
- `b` -- Source RTPSA `b`

### Output
- `c` -- Destination CTPSA `c = a ^ b`
"""
function mad_ctpsa_powt!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_powt(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_tpow!(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination CTPSA `c = a ^ b` (internal real-to-complex conversion).

### Input
- `a` -- Source RTPSA `a`
- `b` -- Source CTPSA `b`

### Output
- `c` -- Destination TPSA `c = a ^ b`
"""
function mad_ctpsa_tpow!(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tpow(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_nrm(a::Ptr{CTPSA{Desc}})::Cdouble

Calculates the norm of TPSA `a`.

### Input
- `a`   -- TPSA

### Output
- `nrm` -- Norm of TPSA `a`
"""
function mad_ctpsa_nrm(a::Ptr{CTPSA{Desc}})::Cdouble
  nrm = @ccall MAD_TPSA.mad_ctpsa_nrm(a::Ptr{CTPSA{Desc}}, tpsa_b_::Ptr{CTPSA{Desc}})::Cdouble
  return nrm
end

"""
    mad_ctpsa_conj(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Calculates the complex conjugate of of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = conj(a)`
"""
function mad_ctpsa_conj!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_conj(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end

"""
    mad_ctpsa_sqrt!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `sqrt` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = sqrt(a)`
"""
function mad_ctpsa_sqrt!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sqrt(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_exp!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `exp` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = exp(a)`
"""
function mad_ctpsa_exp!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_exp(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end



"""
    mad_ctpsa_log!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `log` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = log(a)`
"""
function mad_ctpsa_log!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_log(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_sincos!(a::Ptr{CTPSA{Desc}}, s::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `s = sin(a)` and TPSA `c = cos(a)`

### Input
- `a` -- Source TPSA `a`

### Output
- `s` -- Destination TPSA `s = sin(a)`
- `c` -- Destination TPSA `c = cos(a)`
"""
function mad_ctpsa_sincos!(a::Ptr{CTPSA{Desc}}, s::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sincos(a::Ptr{CTPSA{Desc}}, s::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_sin!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `sin` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = sin(a)`
"""
function mad_ctpsa_sin!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sin(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_cos!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `cos` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = cos(a)`
"""
function mad_ctpsa_cos!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_cos(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_tan!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `tan` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = tan(a)`
"""
function mad_ctpsa_tan!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tan(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_cot!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `cot` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = cot(a)`
"""
function mad_ctpsa_cot!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_cot(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_sinc!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `sinc` of TPSA `a`

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = sinc(a)`
"""
function mad_ctpsa_sinc!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sinc(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_sincosh!(a::Ptr{CTPSA{Desc}}, s::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `s = sinh(a)` and TPSA `c = cosh(a)`

### Input
- `a` -- Source TPSA `a`

### Output
- `s` -- Destination TPSA `s = sinh(a)`
- `c` -- Destination TPSA `c = cosh(a)`
"""
function mad_ctpsa_sincosh!(a::Ptr{CTPSA{Desc}}, s::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sincosh(a::Ptr{CTPSA{Desc}}, s::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_sinh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

  Sets TPSA `c` to the `sinh` of TPSA `a`

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = sinh(a)`
"""
function mad_ctpsa_sinh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sinh(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_cosh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `cosh` of TPSA `a`

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = cosh(a)`
"""
function mad_ctpsa_cosh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_cosh(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_tanh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `tanh` of TPSA `a`

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = tanh(a)`
"""
function mad_ctpsa_tanh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tanh(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_coth!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `coth` of TPSA `a`

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = coth(a)`
"""
function mad_ctpsa_coth!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_coth(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_sinhc!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `sinhc` of TPSA `a`

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = sinhc(a)`
"""
function mad_ctpsa_sinhc!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sinhc(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_asin!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `asin` of TPSA `a`

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = asin(a)`
"""
function mad_ctpsa_asin!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_asin(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_acos!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `acos` of TPSA `a`

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = acos(a)`
"""
function mad_ctpsa_acos!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_acos(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_atan!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `atan` of TPSA `a`

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = atan(a)`
"""
function mad_ctpsa_atan!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_atan(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_acot!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `acot` of TPSA `a`

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = acot(a)`
"""
function mad_ctpsa_acot!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_acot(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end

"""
    mad_ctpsa_asinc!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `asinc` of TPSA `a`

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = asinc(a)`
"""
function mad_ctpsa_asinc!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_asinc(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_asinh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `asinh` of TPSA `a`

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = asinh(a)`
"""
function mad_ctpsa_asinh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_asinh(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_acosh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `acosh` of TPSA `a`

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = acosh(a)`
"""
function mad_ctpsa_acosh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_acosh(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_atanh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `atanh` of TPSA `a`

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = atanh(a)`
"""
function mad_ctpsa_atanh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_atanh(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_acoth!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `acoth` of TPSA `a`

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = acoth(a)`
"""
function mad_ctpsa_acoth!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_acoth(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_asinhc!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `asinhc` of TPSA `a`

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = asinhc(a)`
"""
function mad_ctpsa_asinhc!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_asinhc(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_erf!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `erf` of TPSA `a`

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = erf(a)`
"""
function mad_ctpsa_erf!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_erf(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_erfc!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to the `erfc` of TPSA `a`

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = erfc(a)`
"""
function mad_ctpsa_erfc!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_erfc(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_acc!(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})

Adds `a*v` to TPSA `c`. Aliasing OK.

### Input
- `a` -- Source TPSA `a`
- `v` -- Scalar with double precision

### Output
- `c` -- Destination TPSA `c += v*a`
"""
function mad_ctpsa_acc!(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_acc(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_scl!(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to `v*a`. 

### Input
- `a` -- Source TPSA `a`
- `v` -- Scalar with double precision

### Output
- `c` -- Destination TPSA `c = v*a`
"""
function mad_ctpsa_scl!(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_scl(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_inv!(a::Ptr{CTPSA{Desc}},  v::ComplexF64, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to `v/a`. 

### Input
- `a` -- Source TPSA `a`
- `v` -- Scalar with double precision

### Output
- `c` -- Destination TPSA `c = v/a`
"""
function mad_ctpsa_inv!(a::Ptr{CTPSA{Desc}},  v::ComplexF64, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_inv(a::Ptr{CTPSA{Desc}},  v::ComplexF64, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_invsqrt!(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to `v/sqrt(a)`. 

### Input
- `a` -- Source TPSA `a`
- `v` -- Scalar with double precision

### Output
- `c` -- Destination TPSA `c = v/sqrt(a)`
"""
function mad_ctpsa_invsqrt!(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_invsqrt(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_hypot!(x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})

Sets TPSA `r` to `sqrt(x^2+y^2)`

### Input
- `x` -- Source TPSA `x`
- `y` -- Source TPSA `y`

### Output
- `r` -- Destination TPSA `r = sqrt(x^2+y^2)`
"""
function  mad_ctpsa_hypot!(x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_hypot(x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})::Cvoid
end

"""
    mad_ctpsa_hypot3!(x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, z::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})

Sets TPSA `r` to `sqrt(x^2+y^2+z^2)`

### Input
- `x` -- Source TPSA `x`
- `y` -- Source TPSA `y`
- `z` -- Source TPSA `z`

### Output
- `r` -- Destination TPSA `r = sqrt(x^2+y^2+z^2)`
"""
function  mad_ctpsa_hypot3!(x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, z::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_hypot3(x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, z::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_integ!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, iv::Cint)

Integrates TPSA with respect to the variable with index `iv`.

### Input
- `a`  -- Source TPSA to integrate
- `iv` -- Index of variable to integrate over (e.g. integrate over `x`, `iv = 1`). 

### Output
- `c`  -- Destination TPSA
"""
function mad_ctpsa_integ!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, iv::Cint)
  @ccall MAD_TPSA.mad_ctpsa_integ(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, iv::Cint)::Cvoid
end


"""
    mad_ctpsa_deriv!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, iv::Cint)

Differentiates TPSA with respect to the variable with index `iv`.

### Input
- `a`  -- Source TPSA to differentiate
- `iv` -- Index of variable to take derivative wrt to (e.g. derivative wrt `x`, `iv = 1`). 

### Output
- `c`  -- Destination TPSA
"""
function mad_ctpsa_deriv!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, iv::Cint)
  @ccall MAD_TPSA.mad_ctpsa_deriv(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, iv::Cint)::Cvoid
end


"""
    mad_ctpsa_derivm!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})

Differentiates TPSA with respect to the monomial defined by byte array `m`.

### Input
- `a` -- Source TPSA to differentiate
- `n` -- Length of monomial to differentiate wrt
- `m` -- Monomial to take derivative wrt

### Output
- `c` -- Destination TPSA
"""
function mad_ctpsa_derivm!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})
  @ccall MAD_TPSA.mad_ctpsa_derivm(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cvoid
end


"""
    mad_ctpsa_poisbra!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, nv::Cint)

Sets TPSA `c` to the poisson bracket of TPSAs `a` and `b`.

### Input
- `a`  -- Source TPSA `a`
- `b`  -- Source TPSA `b`
- `nv` -- Number of variables in the TPSA

### Output
- `c`  -- Destination TPSA `c`
"""
function mad_ctpsa_poisbra!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, nv::Cint)
  @ccall MAD_TPSA.mad_ctpsa_poisbra(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, nv::Cint)::Cvoid
end


"""
    mad_ctpsa_taylor!(a::Ptr{CTPSA{Desc}}, n::Cint, coef::Ptr{ComplexF64}, c::Ptr{CTPSA{Desc}})

Computes the result of the Taylor series up to order `n-1` with Taylor coefficients `coef` for the scalar value in `a`. That is,
`c = coef[0] + coef[1]*a_0 + coef[2]*a_0^2 + ...` where `a_0` is the scalar part of TPSA `a`

### Input
- `a`    -- TPSA `a`
- `n`    -- `Order-1` of Taylor expansion, size of `coef` array
- `coef` -- Array of coefficients in Taylor `s`
- `c`    -- Result
"""
function mad_ctpsa_taylor!(a::Ptr{CTPSA{Desc}}, n::Cint, coef::Ptr{ComplexF64}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_taylor(a::Ptr{CTPSA{Desc}}, n::Cint, coef::Ptr{ComplexF64}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_poisbrat!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, nv::Cint)

Sets TPSA `c` to the poisson bracket of CTPSA `a`and RTPSA `b` (internal real-to-complex conversion).

### Input
- `a`  -- Source CTPSA `a`
- `b`  -- Source RTPSA `b`
- `nv` -- Number of variables in the TPSA

### Output
- `c`  -- Destination CTPSA `c`
"""
function mad_ctpsa_poisbrat!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, nv::Cint)
  @ccall MAD_TPSA.mad_ctpsa_poisbrat(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, nv::Cint)::Cvoid
end


"""
    mad_ctpsa_tpoisbra!(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, nv::Cint)

Sets TPSA `c` to the poisson bracket of RTPSA `a` and CTPSA `b` (internal real-to-complex conversion).

### Input
- `a`  -- Source RTPSA `a`
- `b`  -- Source CTPSA `b`
- `nv` -- Number of variables in the TPSA

### Output
- `c`  -- Destination CTPSA `c`
"""
function mad_ctpsa_tpoisbra!(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, nv::Cint)
  @ccall MAD_TPSA.mad_ctpsa_tpoisbra(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, nv::Cint)::Cvoid
end


"""
    mad_ctpsa_acc_r!(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})

Adds `a*v` to TPSA `c`. Aliasing OK. Without complex-by-value arguments.

### Input
- `a`    -- Source TPSA `a`
- `v_re` -- Real part of scalar with double precision
- `v_im` -- Imaginary part of scalar with double precision

### Output
- `c`    -- Destination TPSA `c += v*a`
"""
function mad_ctpsa_acc_r!(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_acc_r(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_scl_r!(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble,, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to `v*a`.  Without complex-by-value arguments.

### Input
- `a`    -- Source TPSA `a`
- `v_re` -- Real part of scalar with double precision
- `v_im` -- Imaginary part of scalar with double precision

### Output
- `c`    -- Destination TPSA `c = v*a`
"""
function mad_ctpsa_scl_r!(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_scl_r(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_inv_r!(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to `v/a`.  Without complex-by-value arguments.

### Input
- `a`    -- Source TPSA `a`
- `v_re` -- Real part of scalar with double precision
- `v_im` -- Imaginary part of scalar with double precision

### Output
- `c`    -- Destination TPSA `c = v*a`
"""
function mad_ctpsa_inv_r!(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_inv_r(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})::Cvoid
end

"""
    mad_ctpsa_invsqrt_r!(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})

Sets TPSA `c` to `v/sqrt(a)`. Without complex-by-value arguments.

### Input
- `a`    -- Source TPSA `a`
- `v_re` -- Real part of scalar with double precision
- `v_im` -- Imaginary part of scalar with double precision

### Output
- `c`    -- Destination TPSA `c = v*a`
"""
function mad_ctpsa_invsqrt_r!(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_invsqrt_r(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axpb!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, b::ComplexF64, r::Ptr{CTPSA{Desc}})

`r = a*x + b`

### Input
- `a` -- Scalar `a`
- `x` -- TPSA `x`
- `b` -- Scalar `b`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_ctpsa_axpb!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, b::ComplexF64, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axpb(a::ComplexF64, x::Ptr{CTPSA{Desc}}, b::ComplexF64, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axpbypc!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, b::ComplexF64, y::Ptr{CTPSA{Desc}}, c::ComplexF64, r::Ptr{CTPSA{Desc}})

`r = a*x+b*y+c`

### Input
- `a` -- Scalar `a`
- `x` -- TPSA `x`
- `b` -- Scalar `b`
- `y` -- TPSA `y`
- `c` -- Scalar `c`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_ctpsa_axpbypc!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, b::ComplexF64, y::Ptr{CTPSA{Desc}}, c::ComplexF64, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axpbypc(a::ComplexF64, x::Ptr{CTPSA{Desc}}, b::ComplexF64, y::Ptr{CTPSA{Desc}}, c::ComplexF64, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axypb!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b::ComplexF64, r::Ptr{CTPSA{Desc}})

`r = a*x*y + b`

### Input
- `a` -- Scalar `a`
- `x` -- TPSA `x`
- `y` -- TPSA `y`
- `b` -- Scalar `b`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_ctpsa_axypb!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b::ComplexF64, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axypb(a::ComplexF64, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b::ComplexF64, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axypbzpc!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b::ComplexF64, z::Ptr{CTPSA{Desc}}, c::ComplexF64, r::Ptr{CTPSA{Desc}})

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
function mad_ctpsa_axypbzpc!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b::ComplexF64, z::Ptr{CTPSA{Desc}}, c::ComplexF64, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axypbzpc(a::ComplexF64, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b::ComplexF64, z::Ptr{CTPSA{Desc}}, c::ComplexF64, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axypbvwpc!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b::ComplexF64, v::Ptr{CTPSA{Desc}}, w::Ptr{CTPSA{Desc}}, c::ComplexF64, r::Ptr{CTPSA{Desc}})

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
function mad_ctpsa_axypbvwpc!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b::ComplexF64, v::Ptr{CTPSA{Desc}}, w::Ptr{CTPSA{Desc}}, c::ComplexF64, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axypbvwpc(a::ComplexF64, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b::ComplexF64, v::Ptr{CTPSA{Desc}}, w::Ptr{CTPSA{Desc}}, c::ComplexF64, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_ax2pby2pcz2!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, b::ComplexF64, y::Ptr{CTPSA{Desc}}, c::ComplexF64, z::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})

`r = a*x^2 + b*y^2 + c*z^2 `

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
function mad_ctpsa_ax2pby2pcz2!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, b::ComplexF64, y::Ptr{CTPSA{Desc}}, c::ComplexF64, z::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_ax2pby2pcz2(a::ComplexF64, x::Ptr{CTPSA{Desc}}, b::ComplexF64, y::Ptr{CTPSA{Desc}}, c::ComplexF64, z::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axpsqrtbpcx2!(x::Ptr{CTPSA{Desc}}, a::ComplexF64, b::ComplexF64, c::ComplexF64, r::Ptr{CTPSA{Desc}})

`r = a*x + sqrt(b + c*x^2)`

### Input
- `x` -- TPSA `x`
- `a` -- Scalar `a`
- `b` -- Scalar `b`
- `c` -- Scalar `c`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_ctpsa_axpsqrtbpcx2!(x::Ptr{CTPSA{Desc}}, a::ComplexF64, b::ComplexF64, c::ComplexF64, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axpsqrtbpcx2(x::Ptr{CTPSA{Desc}}, a::ComplexF64, b::ComplexF64, c::ComplexF64, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_logaxpsqrtbpcx2!(x::Ptr{CTPSA{Desc}}, a::ComplexF64, b::ComplexF64, c::ComplexF64, r::Ptr{CTPSA{Desc}})

`r = log(a*x + sqrt(b + c*x^2))`

### Input
- `x` -- TPSA `x`
- `a` -- Scalar `a`
- `b` -- Scalar `b`
- `c` -- Scalar `c`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_ctpsa_logaxpsqrtbpcx2!(x::Ptr{CTPSA{Desc}}, a::ComplexF64, b::ComplexF64, c::ComplexF64, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_logaxpsqrtbpcx2(x::Ptr{CTPSA{Desc}}, a::ComplexF64, b::ComplexF64, c::ComplexF64, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_logxdy!(x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})

`r = log(x / y)`

### Input
- `x` -- TPSA `x`
- `y` -- TPSA `y`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_ctpsa_logxdy!(x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_logxdy(x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axpb_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, r::Ptr{CTPSA{Desc}})

`r = a*x + b`. Same as `mad_ctpsa_axpb` without complex-by-value arguments.

### Input
- `a_re` -- Real part of Scalar `a`
- `a_im` -- Imag part of Scalar `a`
- `x`    -- TPSA `x`
- `b_re` -- Real part of Scalar `b`
- `b_im` -- Imag part of Scalar `b`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_ctpsa_axpb_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axpb_r(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axpbypc_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, y::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})

`r = a*x + b*y + c`. Same as `mad_ctpsa_axpbypc` without complex-by-value arguments.

### Input
- `a_re` -- Real part of Scalar `a`
- `a_im` -- Imag part of Scalar `a`
- `x`    -- TPSA `x`
- `b_re` -- Real part of Scalar `b`
- `b_im` -- Imag part of Scalar `b`
- `y`    -- TPSA `y`
- `c_re` -- Real part of Scalar `c`
- `c_im` -- Imag part of Scalar `c`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_ctpsa_axpbypc_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, y::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axpbypc_r(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, y::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axypb_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, r::Ptr{CTPSA{Desc}})

`r = a*x*y + b`. Same as `mad_ctpsa_axypb` without complex-by-value arguments.

### Input
- `a_re` -- Real part of Scalar `a`
- `a_im` -- Imag part of Scalar `a`
- `x`    -- TPSA `x`
- `y`    -- TPSA `y`
- `b_re` -- Real part of Scalar `b`
- `b_im` -- Imag part of Scalar `b`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_ctpsa_axypb_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axypb_r(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axypbzpc_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, z::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})

`r = a*x*y + b*z + c`. Same as `mad_ctpsa_axypbzpc` without complex-by-value arguments.

### Input
- `a_re` -- Real part of Scalar `a`
- `a_im` -- Imag part of Scalar `a`
- `x`    -- TPSA `x`
- `y`    -- TPSA `y`
- `b_re` -- Real part of Scalar `b`
- `b_im` -- Imag part of Scalar `b`
- `z`    -- TPSA `z`
- `c_re` -- Real part of Scalar `c`
- `c_im` -- Imag part of Scalar `c`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_ctpsa_axypbzpc_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, z::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axypbzpc_r(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, z::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axypbvwpc_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, v::Ptr{CTPSA{Desc}}, w::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})

`r = a*x*y + b*v*w + c`. Same as `mad_ctpsa_axypbvwpc` without complex-by-value arguments.

### Input
- `a_re` -- Real part of Scalar `a`
- `a_im` -- Imag part of Scalar `a`
- `x`    -- TPSA `x`
- `y`    -- TPSA `y`
- `b_re` -- Real part of Scalar `b`
- `b_im` -- Imag part of Scalar `b`
- `v`    -- TPSA v
- `w`    -- TPSA w
- `c_re` -- Real part of Scalar `c`
- `c_im` -- Imag part of Scalar `c`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_ctpsa_axypbvwpc_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, v::Ptr{CTPSA{Desc}}, w::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axypbvwpc_r(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, v::Ptr{CTPSA{Desc}}, w::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_ax2pby2pcz2_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, y::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, z::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})

`r = a*x^2 + b*y^2 + c*z^2`. Same as `mad_ctpsa_ax2pby2pcz2` without complex-by-value arguments.

### Input
- `a_re` -- Real part of Scalar `a`
- `a_im` -- Imag part of Scalar `a`
- `x`    -- TPSA `x`
- `b_re` -- Real part of Scalar `b`
- `b_im` -- Imag part of Scalar `b`
- `y`    -- TPSA `y`
- `c_re` -- Real part of Scalar `c`
- `c_im` -- Imag part of Scalar `c`
- `z`    -- TPSA `z`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_ctpsa_ax2pby2pcz2_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, y::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, z::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_ax2pby2pcz2_r(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, y::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, z::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axpsqrtbpcx2_r!(x::Ptr{CTPSA{Desc}}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})

`r = a*x + sqrt(b + c*x^2)`. Same as `mad_ctpsa_axpsqrtbpcx2` without complex-by-value arguments.

### Input
- `a_re` -- Real part of Scalar `a`
- `a_im` -- Imag part of Scalar `a`
- `b_re` -- Real part of Scalar `b`
- `b_im` -- Imag part of Scalar `b`
- `c_re` -- Real part of Scalar `c`
- `c_im` -- Imag part of Scalar `c`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_ctpsa_axpsqrtbpcx2_r!(x::Ptr{CTPSA{Desc}}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axpsqrtbpcx2_r(x::Ptr{CTPSA{Desc}}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_logaxpsqrtbpcx2_r!(x::Ptr{CTPSA{Desc}}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})

`r = log(a*x + sqrt(b + c*x^2))`. Same as `mad_ctpsa_logaxpsqrtbpcx2` without complex-by-value arguments.

### Input
- `a_re` -- Real part of Scalar `a`
- `a_im` -- Imag part of Scalar `a`
- `b_re` -- Real part of Scalar `b`
- `b_im` -- Imag part of Scalar `b`
- `c_re` -- Real part of Scalar `c`
- `c_im` -- Imag part of Scalar `c`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_ctpsa_logaxpsqrtbpcx2_r!(x::Ptr{CTPSA{Desc}}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_logaxpsqrtbpcx2_r(x::Ptr{CTPSA{Desc}}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_vec2fld!(na::Cint, a::Ptr{CTPSA{Desc}}, mc::Ptr{Ptr{CTPSA{Desc}}})

???

### Input
- `na`
- `a`
- `mc`
"""
function mad_ctpsa_vec2fld!(na::Cint, a::Ptr{CTPSA{Desc}}, mc::Ptr{Ptr{CTPSA{Desc}}})
  @ccall MAD_TPSA.mad_ctpsa_vec2fld(na::Cint, a::Ptr{CTPSA{Desc}}, mc::Ptr{Ptr{CTPSA{Desc}}})::Cvoid
end


"""
    mad_ctpsa_fld2vec!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, c::Ptr{CTPSA{Desc}})

???

### Input
- `na`
- `ma`
- `c`
"""
function mad_ctpsa_fld2vec!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_fld2vec(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_fgrad!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

???

### Input
- `na`
- `ma`
- `b`
- `c`
"""
function mad_ctpsa_fgrad!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_fgrad(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_liebra!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})

Computes the Lie bracket of the maps `ma` and `mb`.

### Input
- `na` -- Number of TPSAs in map `ma` and map `mb`
- `ma` -- Map `ma`
- `mb` -- Map `mb`

### Output
- `mc` -- Destination map `mc`
"""
function mad_ctpsa_liebra!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})
  @ccall MAD_TPSA.mad_ctpsa_liebra(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})::Cvoid
end


"""
    mad_ctpsa_exppb!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})

Computes the exponential of the Poisson bracket of the maps `ma` and `mb`.

### Input
- `na` -- Number of TPSAs in Map `ma` and map `mb`
- `ma` -- Map `ma`
- `mb` -- Map `mb`

### Output
- `mc` -- Destination map `mc`
"""
function mad_ctpsa_exppb!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})
  @ccall MAD_TPSA.mad_ctpsa_exppb(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})::Cvoid
end


"""
    mad_ctpsa_logpb!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})

Computes the log of the Poisson bracket of the maps `ma` and `mb`.

### Input
- `na` -- Number of TPSAs in Map `ma` and map `mb`
- `ma` -- Map `ma`
- `mb` -- Map `mb`

### Output
- `mc` -- Destination map `mc`
"""
function mad_ctpsa_logpb!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})
  @ccall MAD_TPSA.mad_ctpsa_logpb(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})::Cvoid
end


"""
    mad_ctpsa_mnrm(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}})::Cdouble

Computes the norm of the map (sum of absolute value of coefficients of all TPSAs in the map).

### Input
- `na`  -- Number of TPSAs in the map
- `ma`  -- Map `ma`

### Output
- `nrm` -- Norm of map (sum of absolute value of coefficients of all TPSAs in the map)
"""
function mad_ctpsa_mnrm(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}})::Cdouble
  nrm = @ccall MAD_TPSA.mad_ctpsa_mnrm(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}})::Cdouble
  return nrm
end


"""
    mad_ctpsa_minv!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})

Inverts the map.

### Input
- `na` -- Number of TPSAs in the map
- `ma` -- Map `ma`

### Output
- `mc` -- Inversion of Map `ma`
"""
function mad_ctpsa_minv!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})
  @ccall MAD_TPSA.mad_ctpsa_minv(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})::Cvoid
end


"""
    mad_ctpsa_pminv!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}}, select::Ptr{Cint})

Computes the partial inverse of the map with only the selected variables, specified by 0s or 1s in select.

### Input
- `na`     -- Number of TPSAs in `ma`
- `ma`     -- Map `ma`
- `select` -- Array of 0s or 1s defining which variables to do inverse on (atleast same size as na)

### Output
- `mc`     -- Partially inverted map using variables specified as 1 in the select array
"""
function mad_ctpsa_pminv!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}}, select::Ptr{Cint})
  @ccall MAD_TPSA.mad_ctpsa_pminv(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}}, select::Ptr{Cint})::Cvoid
end


"""
    mad_ctpsa_compose!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})

Composes two maps.

### Input
- `na` -- Number of TPSAs in Map `ma`
- `ma` -- Map `ma`
- `nb` -- Number of TPSAs in Map `mb`
- `mb` -- Map `mb`

### Output
- `mc` -- Composition of maps `ma` and `mb`
"""
function mad_ctpsa_compose!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})
  @ccall MAD_TPSA.mad_ctpsa_compose(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})::Cvoid
end


"""
    mad_ctpsa_translate!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, tb::Ptr{ComplexF64}, mc::Ptr{Ptr{CTPSA{Desc}}})

Translates the expansion point of the map by the amount `tb`.

### Input
- `na` -- Number of TPSAS in the map
- `ma` -- Map `ma`
- `nb` -- Length of `tb`
- `tb` -- Vector of amount to translate for each varaible

### Output
- `mc` -- Map evaluated at the new point translated `tb` from the original evaluation point
"""
function mad_ctpsa_translate!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, tb::Ptr{ComplexF64}, mc::Ptr{Ptr{CTPSA{Desc}}})
  @ccall MAD_TPSA.mad_ctpsa_translate(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, tb::Ptr{ComplexF64}, mc::Ptr{Ptr{CTPSA{Desc}}})::Cvoid
end


"""
    mad_ctpsa_eval!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, tb::Ptr{ComplexF64}, tc::Ptr{ComplexF64})

Evaluates the map at the point `tb`

### Input
- `na` -- Number of TPSAs in the map
- `ma` -- Map `ma`
- `nb` -- Length of `tb`
- `tb` -- Point at which to evaluate the map

### Output
- `tc` -- Values for each TPSA in the map evaluated at the point `tb`
"""
function mad_ctpsa_eval!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, tb::Ptr{ComplexF64}, tc::Ptr{ComplexF64})
  @ccall MAD_TPSA.mad_ctpsa_eval(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, tb::Ptr{ComplexF64}, tc::Ptr{ComplexF64})::Cvoid
end


"""
    mad_ctpsa_mconv!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nc::Cint, mc::Ptr{Ptr{CTPSA{Desc}}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)

Equivalent to `mad_tpsa_convert`, but applies the conversion to all TPSAs in the map `ma`.

### Input
- `na`   -- Number of TPSAs in the map
- `ma`   -- Map `ma`
- `nc`   -- Number of TPSAs in the output map `mc`
- `n`    -- Length of vector (size of `t2r_`)
- `t2r_` -- (Optional) Vector of index lookup
- `pb`   -- Poisson bracket, 0, 1:fwd, -1:bwd

### Output
- `mc`   -- Map `mc` with specified conversions 
"""
function mad_ctpsa_mconv!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nc::Cint, mc::Ptr{Ptr{CTPSA{Desc}}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)
  @ccall MAD_TPSA.mad_ctpsa_mconv(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nc::Cint, mc::Ptr{Ptr{CTPSA{Desc}}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)::Cvoid
end


"""
    mad_ctpsa_print(t::Ptr{CTPSA{Desc}}, name_::Cstring, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid})

Prints the TPSA coefficients with precision `eps_`. If `nohdr_` is not zero, 
the header is not printed. 

### Input
- `t`       -- TPSA to print
- `name_`   -- (Optional) Name of TPSA
- `eps_`    -- (Optional) Precision to output
- `nohdr_`  -- (Optional) If True, no header is printed
- `stream_` -- (Optional) `FILE` pointer of output stream. Default is `stdout`
"""
function mad_ctpsa_print(t::Ptr{CTPSA{Desc}}, name_::Cstring, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_ctpsa_print(t::Ptr{CTPSA{Desc}}, name_::Cstring, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid})::Cvoid
end


"""
    mad_ctpsa_scan(stream_::Ptr{Cvoid})::Ptr{CTPSA{Desc}}

Scans in a TPSA from the `stream_`.

### Input
- `stream_` -- (Optional) I/O stream from which to read the TPSA, default is `stdin`

### Output
- `t`       -- TPSA scanned from I/O `stream_`
"""
function mad_ctpsa_scan(stream_::Ptr{Cvoid})::Ptr{CTPSA{Desc}}
  t = @ccall MAD_TPSA.mad_ctpsa_scan(stream_::Ptr{Cvoid})::Ptr{CTPSA{Desc}}
  return t
end


"""
    mad_ctpsa_scan_hdr(kind_::Ptr{Cint}, name_::Ptr{Cuchar}, stream_::Ptr{Cvoid})::Ptr{Desc{RTPSA,CTPSA}}

Read TPSA header. Returns descriptor for TPSA given the header. This is useful for external languages using 
this library where the memory is managed NOT on the C side.

### Input
- `kind_`   -- (Optional) Real or complex TPSA, or detect automatically if not provided.
- `name_`   -- (Optional) Name of TPSA
- `stream_` -- (Optional) I/O stream to read TPSA from,  default is `stdin`

### Output
- `ret`     -- Descriptor for the TPSA 
"""
function mad_ctpsa_scan_hdr(kind_::Ptr{Cint}, name_::Ptr{Cuchar}, stream_::Ptr{Cvoid})::Ptr{Desc{RTPSA,CTPSA}}
  desc = @ccall MAD_TPSA.mad_ctpsa_scan_hdr(kind_::Ptr{Cint}, name_::Ptr{Cuchar}, stream_::Ptr{Cvoid})::Ptr{Desc{RTPSA,CTPSA}}
  return ret
end


"""
    mad_ctpsa_scan_coef!(t::Ptr{CTPSA{Desc}}, stream_::Ptr{Cvoid})

Read TPSA coefficients into TPSA `t`. This should be used with `mad_tpsa_scan_hdr` for external languages using 
this library where the memory is managed NOT on the C side.

### Input
- `stream_` -- (Optional) I/O stream to read TPSA from, default is `stdin`

### Output
- `t`       -- TPSA with coefficients scanned from `stream_`
"""
function mad_ctpsa_scan_coef!(t::Ptr{CTPSA{Desc}}, stream_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_ctpsa_scan_coef(t::Ptr{CTPSA{Desc}}, stream_::Ptr{Cvoid})::Cvoid
end


"""
    mad_ctpsa_debug(t::Ptr{CTPSA{Desc}}, name_::Cstring, fnam_::Cstring, line_::Cint, stream_::Ptr{Cvoid})

Prints TPSA with all information of data structure.

### Input
- `t`       -- TPSA
- `name_`   -- (Optional) Name of TPSA
- `fnam_`   -- (Optional) File name to print to
- `line_`   -- (Optional) Line number in file to start at
- `stream_` -- (Optional) I/O stream to print to, default is `stdout`
"""
function mad_ctpsa_debug(t::Ptr{CTPSA{Desc}}, name_::Cstring, fnam_::Cstring, line_::Cint, stream_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_ctpsa_debug(t::Ptr{CTPSA{Desc}}, name_::Cstring, fnam_::Cstring, line_::Cint, stream_::Ptr{Cvoid})::Cvoid
end

"""
    mad_ctpsa_isvalid(t::Ptr{CTPSA{Desc}})::Cuchar

Sanity check of the TPSA integrity.

### Input
- `t` -- Complex TPSA to check if valid

### Output
- `ret`  -- True if valid TPSA, false otherwise
"""
function mad_ctpsa_isvalid(t::Ptr{CTPSA{Desc}})::Cuchar
  ret = @ccall MAD_TPSA.mad_ctpsa_isvalid(t::Ptr{CTPSA{Desc}})::Cuchar
  return ret
end


"""
    mad_ctpsa_init(t::Ptr{CTPSA{Desc}}, d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{CTPSA{Desc}}

Unsafe initialization of an already existing TPSA `t` with maximum order `mo` to the descriptor `d`. `mo` must be less than 
the maximum order of the descriptor. `t` is modified in place and also returned.

### Input
- `t`  -- TPSA to initialize to descriptor `d`
- `d`  -- Descriptor
- `mo` -- Maximum order of the TPSA (must be less than maximum order of the descriptor)

### Output
- `t`  -- TPSA initialized to descriptor `d` with maximum order `mo`
"""
function mad_ctpsa_init!(t::Ptr{CTPSA{Desc}}, d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{CTPSA{Desc}}
  t = @ccall MAD_TPSA.mad_ctpsa_init(t::Ptr{CTPSA{Desc}}, d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{CTPSA{Desc}}
  return t
end
