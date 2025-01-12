# Internal constant to aid multiple dispatch including temporaries 
const RealTPS{D} = Union{TempTPS{Float64,D}, TPS{Float64,D}}

"""
    mad_tpsa_newd(d::Ptr{Desc}, mo::Cuchar)

Creates a TPSA defined by the specified descriptor and maximum order. If `MAD_TPSA_DEFAULT` 
is passed for `mo`, the `mo` defined in the descriptor is used. If `mo > d_mo`, then `mo = d_mo`.

### Input
- `d`  -- Descriptor
- `mo` -- Maximum order

### Output
- `t`  -- New TPSA defined by the descriptor
"""
function mad_tpsa_newd(d::Ptr{Desc}, mo::Cuchar)
  t = @ccall MAD_TPSA.mad_tpsa_newd(d::Ptr{Desc}, mo::Cuchar)::Ptr{TPS{Float64,Nothing}}
  return t
end


"""
    mad_tpsa_new(t::Ref{TPS{Float64,D}}, mo::Cuchar)

Creates a blank TPSA with same number of variables/parameters of the inputted TPSA, 
with maximum order specified by `mo`. If `MAD_TPSA_SAME` is passed for `mo`, the `mo` 
currently in `t` is used for the created TPSA. Ok with `t=(tpsa_t*)ctpsa`

### Input
- `t`   -- TPSA
- `mo`  -- Maximum order of new TPSA

### Output
- `ret` -- New blank TPSA with maximum order `mo`
"""
function mad_tpsa_new(t::Ref{TPS{Float64,D}}, mo::Cuchar) where {D}
  ret = @ccall MAD_TPSA.mad_tpsa_new(t::Ref{TPS{Float64,D}}, mo::Cuchar)::Ptr{TPS{Float64,D}}
  return ret
end


"""
    mad_tpsa_del!(t::Ref{TPS{Float64,D}})

Calls the destructor for the TPSA.

### Input
- `t` -- TPSA to destruct
"""
function mad_tpsa_del!(t::Ref{TPS{Float64,D}}) where {D}
  @ccall MAD_TPSA.mad_tpsa_del(t::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_desc(t::RealTPS{D})::Ptr{Desc} where {D}

Gets the descriptor for the TPSA.

### Input
- `t`   -- TPSA

### Outputs
- `ret` -- Descriptor for the TPS{Float64,D}
"""
function mad_tpsa_desc(t::RealTPS{D})::Ptr{Desc} where {D}
  ret = @ccall MAD_TPSA.mad_tpsa_desc(t::Ref{TPS{Float64,D}})::Ptr{Desc}
  return ret
end


"""
    mad_tpsa_uid!(t::RealTPS{D}, uid_::Cint)::Cint where {D}

Sets the TPSA uid if `uid_ != 0`, and returns the current (previous if set) TPSA `uid`. 

### Input
- `t`    -- TPSA
- `uid_` -- `uid` to set in the TPSA if `uid_ != 0`

### Output
- `ret`  -- Current (previous if set) TPSA uid
"""
function mad_tpsa_uid!(t::RealTPS{D}, uid_::Cint)::Cint where {D}
  ret = @ccall MAD_TPSA.mad_tpsa_uid(t::Ref{TPS{Float64,D}}, uid_::Cint)::Cint
  return ret
end


"""
    mad_tpsa_len(t::RealTPS{D}, hi_::Bool)::Cint where {D}

Gets the length of the TPSA itself (e.g. the descriptor may be order 10 but TPSA may only be order 2)

### Input
- `t`   -- TPSA
- `hi_` -- If `true`, returns the length up to the `hi` order in the TPSA, else up to `mo`. Default is false
### Output
- `ret` -- Length of TPS{Float64,D}
"""
function mad_tpsa_len(t::RealTPS{D}, hi_::Bool)::Cint where {D}
  ret = @ccall MAD_TPSA.mad_tpsa_len(t::Ref{TPS{Float64,D}}, hi_::Bool)::Cint
  return ret
end


"""
    mad_tpsa_mo!(t::RealTPS{D}, mo::Cuchar)::Cuchar where {D}

Sets the maximum order `mo` of the TPSA `t`, and returns the original `mo`.
`mo` should be less than or equal to the allocated order `ao`.

### Input
- `t`   -- TPSA
- `mo` -- Maximum order to set the TPSA

### Output
- `ret` -- Original `mo` of the TPSA
"""
function mad_tpsa_mo!(t::RealTPS{D}, mo::Cuchar)::Cuchar where {D}
  ret = @ccall MAD_TPSA.mad_tpsa_mo(t::Ref{TPS{Float64,D}}, mo::Cuchar)::Cuchar
  return ret
end


"""
    mad_tpsa_nam(t::RealTPS{D}, nam_)::Cstring where {D}

Get the name of the TPSA, and will optionally set if `nam_ != null`

### Input
- `t`    -- TPSA
- `nam_` -- Name to set the TPSA

### Output
- `ret`  -- Name of TPS{Float64,D} (null terminated in C)
"""
function mad_tpsa_nam(t::RealTPS{D}, nam_)::Cstring where {D}
  ret = @ccall MAD_TPSA.mad_tpsa_nam(t::Ref{TPS{Float64,D}}, nam_::Cstring)::Cstring
  return ret
end


"""
    mad_tpsa_ord(t::RealTPS{D}, hi_::Bool)::Cuchar where {D}

Gets the TPSA maximum order, or `hi` if `hi_` is true.

### Input
- `t`   -- TPSA
- `hi_` -- Set `true` if `hi` is returned, else `mo` is returned

### Output
- `ret` -- Order of TPSA
"""
function mad_tpsa_ord(t::RealTPS{D}, hi_::Bool)::Cuchar where {D}
  ret = @ccall MAD_TPSA.mad_tpsa_ord(t::Ref{TPS{Float64,D}}, hi_::Bool)::Cuchar
  return ret
end

"""
    mad_tpsa_ordv(t::RealTPS{D}, ts::RealTPS{D}...)::Cuchar where {D}

Returns maximum order of all TPSAs provided.

### Input
- `t`  -- TPSA
- `ts` -- Variable number of TPSAs passed as parameters

### Output
- `mo` -- Maximum order of all TPSAs provided
"""
function mad_tpsa_ordv(t::RealTPS{D}, ts::RealTPS{D}...)::Cuchar where {D}
  #mo = @ccall MAD_TPSA.mad_tpsa_ordv(t::Ref{TPS{Float64,D}}, ts::Ref{TPS{Float64,D}}..., 0::Cint)::Cuchar # null pointer after args for safe use
  #mo = ccall((:mad_tpsa_ordv, MAD_TPSA), Cuchar, (TPS{Float64,D}, TPS{Float64,D}...), (t, ts...))
  return mo
end


"""
    mad_tpsa_copy!(t::RealTPS{D}, r::RealTPS{D}) where {D}

Makes a copy of the TPSA `t` to `r`.

### Input
- `t` -- Source TPSA

### Output
- `r` -- Destination TPSA
"""
function mad_tpsa_copy!(t::RealTPS{D}, r::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_copy(t::Ref{TPS{Float64,D}}, r::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_sclord!(t::RealTPS{D}, r::RealTPS{D}, inv::Bool, prm::Bool) where {D}

Scales all coefficients by order. If `inv == 0`, scales coefficients by order (derivation), else 
scales coefficients by 1/order (integration).

### Input
- `t`   -- Source TPSA
- `inv` -- Put order up, divide, scale by `inv` of value of order
- `prm` -- Parameters flag. If set to 0x0, the scaling excludes the order of the parameters in the monomials. Else, scaling is with total order of monomial

### Output
- `r`   -- Destination TPSA
"""
function mad_tpsa_sclord!(t::RealTPS{D}, r::RealTPS{D}, inv::Bool, prm::Bool) where {D}
  @ccall MAD_TPSA.mad_tpsa_sclord(t::Ref{TPS{Float64,D}}, r::Ref{TPS{Float64,D}}, inv::Bool, prm::Bool)::Cvoid
end


"""
    mad_tpsa_getord!(t::RealTPS{D}, r::RealTPS{D}, ord::Cuchar) where {D}

Extract one homogeneous polynomial of the given order

### Input
- `t`  -- Source TPSA
- `ord` -- Order to retrieve

### Output
- `r`   -- Destination TPSA
"""
function mad_tpsa_getord!(t::RealTPS{D}, r::RealTPS{D}, ord::Cuchar) where {D}
  @ccall MAD_TPSA.mad_tpsa_getord(t::Ref{TPS{Float64,D}}, r::Ref{TPS{Float64,D}}, ord::Cuchar)::Cvoid
end


"""
    mad_tpsa_cutord!(t::RealTPS{D}, r::RealTPS{D}, ord::Cint) where {D}

Cuts the TPSA off at the given order and above, or if `ord` is negative, will cut orders below 
`abs(ord)` (e.g. if ord = -3, then orders 0-3 are cut off).

### Input
- `t`   -- Source TPSA
- `ord` -- Cut order: `0..-ord` or `ord..mo`

### Output
- `r`   -- Destination TPSA
"""
function mad_tpsa_cutord!(t::RealTPS{D}, r::RealTPS{D}, ord::Cint) where {D}
  @ccall MAD_TPSA.mad_tpsa_cutord(t::Ref{TPS{Float64,D}}, r::Ref{TPS{Float64,D}}, ord::Cint)::Cvoid
end

"""
    mad_tpsa_clrord!(t::RealTPS{D}, ord::Cuchar) where {D}

Clears all monomial coefficients of the TPSA at order `ord`

### Input
- `t` -- TPSA
- `ord` -- Order to clear monomial coefficients
"""
function mad_tpsa_clrord!(t::RealTPS{D}, ord::Cuchar) where {D}
  @ccall MAD_TPSA.mad_tpsa_clrord(t::Ref{TPS{Float64,D}}, ord::Cuchar)::Cvoid
end

"""
    mad_tpsa_maxord!(t::RealTPS{D}, n::Cint, idx_)::Cint where {D}

Returns the index to the monomial with maximum abs(coefficient) in the TPSA for all orders 0 to `n`. If `idx_` 
is provided, it is filled with the indices for the maximum abs(coefficient) monomial for each order up to `n`. 

### Input
- `t`    -- TPSA
- `n`    -- Highest order to include in finding the maximum abs(coefficient) in the TPSA, length of `idx_` if provided

### Output
- `idx_` -- (Optional) If provided, is filled with indices to the monomial for each order up to `n` with maximum abs(coefficient)
- `mi`   -- Index to the monomial in the TPSA with maximum abs(coefficient)
"""
function mad_tpsa_maxord!(t::RealTPS{D}, n::Cint, idx_)::Cint where {D}
  typeof(idx_) == Ptr{Nothing} || eltype(idx_) == Cint || error("idx_ must have eltype Cint if provided!")
  mi = @ccall MAD_TPSA.mad_tpsa_maxord(t::Ref{TPS{Float64,D}}, n::Cint, idx_::Ptr{Cint})::Cint
  return mi
end

"""
    mad_tpsa_convert!(t::RealTPS{DT}, r::RealTPS{DR}, n::Cint, t2r_, pb::Cint)

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
function mad_tpsa_convert!(t::RealTPS{DT}, r::RealTPS{DR}, n::Cint, t2r_, pb::Cint) where {DT,DR}
  typeof(t2r_) == Ptr{Nothing} || eltype(t2r_) == Cint || error("t2r_ must have eltype Cint if provided!")
  @ccall MAD_TPSA.mad_tpsa_convert(t::Ref{TPS{Float64,DT}}, r::Ref{TPS{Float64,DR}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)::Cvoid
end


"""
    mad_tpsa_setvar!(t::RealTPS{D}, v::Cdouble, iv::Cint, scl_::Cdouble) where {D}

Sets the 0th and 1st order values for the specified variable, and sets the rest of the variables/parameters to 0

### Input
- `t`    -- TPSA
- `v`    -- 0th order value (coefficient)
- `iv`   -- Variable index
- `scl_` -- 1st order variable value (typically will be 1)
"""
function mad_tpsa_setvar!(t::RealTPS{D}, v::Cdouble, iv::Cint, scl_::Cdouble) where {D}
  @ccall MAD_TPSA.mad_tpsa_setvar(t::Ref{TPS{Float64,D}}, v::Cdouble, iv::Cint, scl_::Cdouble)::Cvoid
end

"""
    mad_tpsa_setprm!(t::RealTPS{D}, v::Cdouble, ip::Cint) where {D}

Sets the 0th and 1st order values for the specified parameter, and sets the rest of the variables/parameters to 0. 
The 1st order value `scl_` of a parameter is always 1.

### Input
- `t`    -- TPSA
- `v`    -- 0th order value (coefficient)
- `ip`   -- Parameter index (e.g. iv = 1 is nn-nv+1)
"""
function mad_tpsa_setprm!(t::RealTPS{D}, v::Cdouble, ip::Cint) where {D}
  @ccall MAD_TPSA.mad_tpsa_setprm(t::Ref{TPS{Float64,D}}, v::Cdouble, ip::Cint)::Cvoid
end

"""
    mad_tpsa_setval!(t::RealTPS{D}, v::Cdouble) where {D}

Sets the scalar part of the TPSA to `v` and all other values to 0 (sets the TPSA order to 0).

### Input
- `t` -- TPSA to set to scalar
- `v` -- Scalar value to set TPSA
"""
function mad_tpsa_setval!(t::RealTPS{D}, v::Cdouble) where {D}
  @ccall MAD_TPSA.mad_tpsa_setval(t::Ref{TPS{Float64,D}}, v::Cdouble)::Cvoid
end

"""
    mad_tpsa_update!(t::RealTPS{D}) where {D}

Updates the `lo` and `hi` fields of the TPSA to reflect the current state 
given the lowest/highest nonzero monomial coefficients.
"""
function mad_tpsa_update!(t::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_update(t::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_clear!(t::RealTPS{D}) where {D}

Clears the TPSA (reset to 0)

### Input
- `t` -- TPSA
"""
function mad_tpsa_clear!(t::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_clear(t::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_isnul(t::RealTPS{D})::Bool where {D}

Checks if TPSA is 0 or not

### Input
- `t`    -- TPSA to check

### Output
- `ret`  -- True or false
"""
function mad_tpsa_isnul(t::RealTPS{D})::Bool where {D}
  ret = @ccall MAD_TPSA.mad_tpsa_isnul(t::Ref{TPS{Float64,D}})::Bool
  return ret
end


"""
    mad_tpsa_mono!(t::RealTPS{D}, i::Cint, n::Cint, m_, p_)::Cuchar where {D}

Returns the order of the monomial at index `i` in the TPSA and optionally the monomial at that index is returned in `m_` 
and the order of parameters in the monomial in `p_`

### Input
- `t`   -- TPSA
- `i`   -- Index valid in TPSA
- `n`   -- Length of monomial

### Output
- `m_`  -- (Optional) Monomial at index `i` in TPSA
- `p_`  -- (Optional) Order of parameters in monomial
- `ret` -- Order of monomial in TPSA `a`t index `i`
"""
function mad_tpsa_mono!(t::RealTPS{D}, i::Cint, n::Cint, m_, p_)::Cuchar where {D}
  typeof(m_) == Ptr{Nothing} || eltype(m_) == Cuchar || error("m_ must have eltype Cuchar if provided!")
  typeof(p_) == Ptr{Nothing} || eltype(p_) == Cuchar || error("p_ must have eltype Cuchar if provided!")
  ret = @ccall MAD_TPSA.mad_tpsa_mono(t::Ref{TPS{Float64,D}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, p_::Ptr{Cuchar})::Cuchar
  return ret
end


"""
    mad_tpsa_idxs(t::RealTPS{D}, n::Cint, s::Cstring)::Cint where {D}

Returns index of monomial in the TPSA given the monomial as string. This generally should not be used, as there 
are no assumptions about which monomial is attached to which index.

### Input
- `t`   -- TPSA
- `n`   -- Length of monomial
- `s`   -- Monomial as string

### Output
- `ret` -- Index of monomial in TPSA
"""
function mad_tpsa_idxs(t::RealTPS{D}, n::Cint, s::Cstring)::Cint where {D}
  ret = @ccall MAD_TPSA.mad_tpsa_idxs(t::Ref{TPS{Float64,D}}, n::Cint, s::Cstring)::Cint
  return ret
end



"""
    mad_tpsa_idxm(t::RealTPS{D}, n::Cint, m)::Cint where {D}

Returns index of monomial in the TPSA given the monomial as a byte array

### Input
- `t`   -- TPSA
- `n`   -- Length of monomial
- `s`   -- Monomial as byte array

### Output
- `ret` -- Index of monomial in TPSA
"""
function mad_tpsa_idxm(t::RealTPS{D}, n::Cint, m)::Cint where {D}
  eltype(m) == Cuchar || error("m must have eltype Cuchar !")
  ret = @ccall MAD_TPSA.mad_tpsa_idxm(t::Ref{TPS{Float64,D}}, n::Cint, m::Ptr{Cuchar})::Cint
  return ret
end


"""
    mad_tpsa_idxsm(t::RealTPS{D}, n::Cint, m)::Cint where {D}

Returns index of monomial in the TPSA given the monomial as a sparse monomial. This generally should not be used, as there 
are no assumptions about which monomial is attached to which index.

### Input
- `t`   -- TPSA
- `n`   -- Length of monomial
- `s`   -- Monomial as sparse monomial

### Output
- `ret` -- Index of monomial in TPSA
"""
function mad_tpsa_idxsm(t::RealTPS{D}, n::Cint, m)::Cint where {D}
  eltype(m) == Cint || error("m must have eltype Cint !")
  ret = @ccall MAD_TPSA.mad_tpsa_idxsm(t::Ref{TPS{Float64,D}}, n::Cint, m::Ptr{Cint})::Cint
  return ret
end


"""
    mad_tpsa_cycle!(t::RealTPS{D}, i::Cint, n::Cint, m_, v_)::Cint where {D}

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
function mad_tpsa_cycle!(t::RealTPS{D}, i::Cint, n::Cint, m_, v_)::Cint where {D}
  typeof(m_) == Ptr{Nothing} || eltype(m_) == Cuchar || error("m_ must have eltype Cuchar if provided!")
  typeof(v_) == Ptr{Nothing} || eltype(v_) == Float64 || error("v_ must have eltype Float64 if provided!")
  i = @ccall MAD_TPSA.mad_tpsa_cycle(t::Ref{TPS{Float64,D}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, v_::Ptr{Cdouble})::Cint
  return i
end


"""
    mad_tpsa_geti(t::RealTPS{D}, i::Cint)::Cdouble where {D}

Gets the coefficient of the monomial at index `i`. Generally should use `mad_tpsa_cycle` instead of this.

### Input
- `t`   -- TPSA
- `i`   -- Monomial index

### Output
- `ret` -- Coefficient of monomial at index `i`
"""
function mad_tpsa_geti(t::RealTPS{D}, i::Cint)::Cdouble where {D}
  ret = @ccall MAD_TPSA.mad_tpsa_geti(t::Ref{TPS{Float64,D}}, i::Cint)::Cdouble
  return ret
end


"""
    mad_tpsa_gets(t::RealTPS{D}, n::Cint, s::Cstring)::Cdouble where {D}

Gets the coefficient of the monomial `s` defined as a string. Generally should use `mad_tpsa_cycle` instead of this.

### Input
- `t`   -- TPSA
- `n`   -- Length of monomial
- `s`   -- Monomial as string

### Output
- `ret` -- Coefficient of monomial `s` in TPSA
"""
function mad_tpsa_gets(t::RealTPS{D}, n::Cint, s::Cstring)::Cdouble where {D}
  ret = @ccall MAD_TPSA.mad_tpsa_gets(t::Ref{TPS{Float64,D}}, n::Cint, s::Cstring)::Cdouble
  return ret
end


"""
    mad_tpsa_getm(t::RealTPS{D}, n::Cint, m)::Cdouble where {D}

Gets the coefficient of the monomial `m` defined as a byte array. Generally should use `mad_tpsa_cycle` instead of this.

### Input
- `t`   -- TPSA
- `n`   -- Length of monomial
- `m`   -- Monomial as byte array

### Output
- `ret` -- Coefficient of monomial `m` in TPSA
"""
function mad_tpsa_getm(t::RealTPS{D}, n::Cint, m)::Cdouble where {D}
  eltype(m) == Cuchar || error("m must have eltype Cuchar")
  ret = @ccall MAD_TPSA.mad_tpsa_getm(t::Ref{TPS{Float64,D}}, n::Cint, m::Ptr{Cuchar})::Cdouble
  return ret
end


"""
    mad_tpsa_getsm(t::RealTPS{D}, n::Cint, m)::Cdouble where {D}

Gets the coefficient of the monomial `m` defined as a sparse monomial. Generally should use `mad_tpsa_cycle` instead of this.

### Input
- `t`   -- TPSA
- `n`   -- Length of monomial
- `m`   -- Monomial as sparse monomial

### Output
- `ret` -- Coefficient of monomial `m` in TPSA
"""
function mad_tpsa_getsm(t::RealTPS{D}, n::Cint, m)::Cdouble where {D}
  eltype(m) == Cint || error("m must have eltype Cint !")
  ret = @ccall MAD_TPSA.mad_tpsa_getsm(t::Ref{TPS{Float64,D}}, n::Cint, m::Ptr{Cint})::Cdouble
  return ret
end



"""
    mad_tpsa_seti!(t::RealTPS{D}, i::Cint, a::Cdouble, b::Cdouble) where {D}

Sets the coefficient of monomial at index `i` to `coef[i] = a*coef[i] + b`. Does not modify other values in TPSA.

### Input
- `t` -- TPSA
- `i` -- Index of monomial
- `a` -- Scaling of current coefficient
- `b` -- Constant added to current coefficient
"""
function mad_tpsa_seti!(t::RealTPS{D}, i::Cint, a::Cdouble, b::Cdouble) where {D}
  @ccall MAD_TPSA.mad_tpsa_seti(t::Ref{TPS{Float64,D}}, i::Cint, a::Cdouble, b::Cdouble)::Cvoid
end


"""
    mad_tpsa_sets!(t::RealTPS{D}, n::Cint, s::Cstring, a::Cdouble, b::Cdouble) where {D}

Sets the coefficient of monomial defined by string `s` to `coef = a*coef + b`. Does not modify other values in TPSA.

### Input
- `t` -- TPSA
- `n` -- Length of monomial
- `s` -- Monomial as string
- `a` -- Scaling of current coefficient
- `b` -- Constant added to current coefficient
"""
function mad_tpsa_sets!(t::RealTPS{D}, n::Cint, s::Cstring, a::Cdouble, b::Cdouble) where {D}
  @ccall MAD_TPSA.mad_tpsa_sets(t::Ref{TPS{Float64,D}}, n::Cint, s::Cstring, a::Cdouble, b::Cdouble)::Cvoid
end


"""
    mad_tpsa_setm!(t::RealTPS{D}, n::Cint, m, a::Cdouble, b::Cdouble) where {D}

Sets the coefficient of monomial defined by byte array `m` to `coef = a*coef + b`. Does not modify other values in TPSA.

### Input
- `t` -- TPSA
- `n` -- Length of monomial
- `m` -- Monomial as byte array
- `a` -- Scaling of current coefficient
- `b` -- Constant added to current coefficient
"""
function mad_tpsa_setm!(t::RealTPS{D}, n::Cint, m, a::Cdouble, b::Cdouble) where {D}
  eltype(m) == Cuchar || error("m must have eltype Cuchar !")
  @ccall MAD_TPSA.mad_tpsa_setm(t::Ref{TPS{Float64,D}}, n::Cint, m::Ptr{Cuchar}, a::Cdouble, b::Cdouble)::Cvoid
end


"""
    mad_tpsa_setsm!(t::RealTPS{D}, n::Cint, m, a::Cdouble, b::Cdouble) where {D}

Sets the coefficient of monomial defined by sparse monomial `m` to `coef = a*coef + b`. Does not modify other values in TPSA.

### Input
- `t` -- TPSA
- `n` -- Length of monomial
- `m` -- Monomial as sparse monomial
- `a` -- Scaling of current coefficient
- `b` -- Constant added to current coefficient
"""
function mad_tpsa_setsm!(t::RealTPS{D}, n::Cint, m, a::Cdouble, b::Cdouble) where {D}
  eltype(m) == Cint || error("m must have eltype Cint !")
  @ccall MAD_TPSA.mad_tpsa_setsm(t::Ref{TPS{Float64,D}}, n::Cint, m::Ptr{Cint}, a::Cdouble, b::Cdouble)::Cvoid
end


"""
    mad_tpsa_cpyi!(t::RealTPS{D}, r::RealTPS{D}, i::Cint) where {D}

Copies the monomial coefficient at index `i` in `t` into the 
same monomial coefficient in `r`

### Input
- `t` -- Source TPSA
- `r` -- Destination TPSA 
- `i` -- Index of monomial
"""
function mad_tpsa_cpyi!(t::RealTPS{D}, r::RealTPS{D}, i::Cint) where {D}
  @ccall MAD_TPSA.mad_tpsa_cpyi(t::Ref{TPS{Float64,D}}, r::Ref{TPS{Float64,D}}, i::Cint)::Cvoid
end

"""
    mad_tpsa_cpys!(t::RealTPS{D}, r::RealTPS{D}, n::Cint, s::Cstring) where {D}

Copies the monomial coefficient at the monomial-as-string-of-order
`s` in `t` into the same monomial coefficient in `r`

### Input
- `t` -- Source TPSA
- `r` -- Destination TPSA 
- `n` -- Length of string
- `s` -- Monomial as string
"""
function mad_tpsa_cpys!(t::RealTPS{D}, r::RealTPS{D}, n::Cint, s::Cstring) where {D}
  @ccall MAD_TPSA.mad_tpsa_cpys(t::Ref{TPS{Float64,D}}, r::Ref{TPS{Float64,D}}, n::Cint, s::Cstring)::Cvoid
end

"""
    mad_tpsa_cpym!(t::RealTPS{D}, r::RealTPS{D}, n::Cint, m) where {D}

Copies the monomial coefficient at the monomial-as-vector-of-orders
`m` in `t` into the same monomial coefficient in `r`

### Input
- `t` -- Source TPSA
- `r` -- Destination TPSA 
- `n` -- Length of monomial `m`
- `m` -- Monomial as vector of orders
"""
function mad_tpsa_cpym!(t::RealTPS{D}, r::RealTPS{D}, n::Cint, m) where {D}
  eltype(m) == Cuchar || error("m must have eltype Cuchar !")
  @ccall MAD_TPSA.mad_tpsa_cpym(t::Ref{TPS{Float64,D}}, r::Ref{TPS{Float64,D}}, n::Cint, m::Ptr{Cuchar})::Cvoid
end

"""
    mad_tpsa_cpysm!(t::RealTPS{D}, r::RealTPS{D}, n::Cint, m) where {D}

Copies the monomial coefficient at the monomial-as-sparse-monomial
`m` in `t` into the same monomial coefficient in `r`

### Input
- `t` -- Source TPSA
- `r` -- Destination TPSA 
- `n` -- Length of monomial `m`
- `m` -- Monomial as sparse-monomial
"""
function mad_tpsa_cpysm!(t::RealTPS{D}, r::RealTPS{D}, n::Cint, m) where {D}
  eltype(m) == Cint || error("m must have eltype Cint !")
  @ccall MAD_TPSA.mad_tpsa_cpysm(t::Ref{TPS{Float64,D}}, r::Ref{TPS{Float64,D}}, n::Cint, m::Ptr{Cint})::Cvoid
end


"""
    mad_tpsa_getv!(t::RealTPS{D}, i::Cint, n::Cint, v) where {D}


Vectorized getter of the coefficients for monomials with indices `i..i+n`. Useful for extracting the 1st order parts of 
a TPSA to construct a matrix (`i = 1`, `n = nv+np = nn`). 

### Input
- `t` -- TPSA
- `i` -- Starting index of monomials to get coefficients
- `n` -- Number of monomials to get coefficients of starting at `i`

### Output
- `v` -- Array of coefficients for monomials `i..i+n`
"""
function mad_tpsa_getv!(t::RealTPS{D}, i::Cint, n::Cint, v) where {D}
  @ccall MAD_TPSA.mad_tpsa_getv(t::Ref{TPS{Float64,D}}, i::Cint, n::Cint, v::Ptr{Cdouble})::Cvoid
end


"""
    mad_tpsa_setv!(t::RealTPS{D}, i::Cint, n::Cint, v) where {D}

Vectorized setter of the coefficients for monomials with indices `i..i+n`. Useful for putting a matrix into a map.

### Input
- `t` -- TPSA
- `i` -- Starting index of monomials to set coefficients
- `n` -- Number of monomials to set coefficients of starting at `i`
- `v` -- Array of coefficients for monomials `i..i+n`
"""
function mad_tpsa_setv!(t::RealTPS{D}, i::Cint, n::Cint, v) where {D}
  eltype(v) == Cdouble || error("v must have eltype Cdouble !")
  @ccall MAD_TPSA.mad_tpsa_setv(t::Ref{TPS{Float64,D}}, i::Cint, n::Cint, v::Ptr{Cdouble})::Cvoid
end


"""
    mad_tpsa_equ(a::RealTPS{D}, b::RealTPS{D}, tol_::Cdouble)::Bool where {D}

Checks if the TPSAs `a` and `b` are equal within the specified tolerance `tol_`. If `tol_` is not specified, `DBL_GTPSA.show_epsILON` is used.

### Input
- `a`    -- TPSA `a`
- `b`    -- TPSA `b`
- `tol_` -- (Optional) Difference below which the TPSAs are considered equal

### Output
- `ret`   - True if `a == b` within `tol_`
"""
function mad_tpsa_equ(a::RealTPS{D}, b::RealTPS{D}, tol_::Cdouble)::Bool where {D}
  ret = @ccall MAD_TPSA.mad_tpsa_equ(a::Ref{TPS{Float64,D}}, b::Ref{TPS{Float64,D}}, tol_::Cdouble)::Bool
  return ret
end


"""
    mad_tpsa_dif!(a::RealTPS{D}, b::RealTPS{D}, c::RealTPS{D}) where {D}

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
function mad_tpsa_dif!(a::RealTPS{D}, b::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_dif(a::Ref{TPS{Float64,D}}, b::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_add!(a::RealTPS{D}, b::RealTPS{D}, c::RealTPS{D}) where {D}

Sets the destination TPSA `c = a + b`

### Input
- `a` -- Source TPSA `a`
- `b` -- Source TPSA `b`

### Output
- `c` -- Destination TPSA `c = a + b`
"""
function mad_tpsa_add!(a::RealTPS{D}, b::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_add(a::Ref{TPS{Float64,D}}, b::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_sub!(a::RealTPS{D}, b::RealTPS{D}, c::RealTPS{D}) where {D}

Sets the destination TPSA `c = a - b`

### Input
- `a` -- Source TPSA `a`
- `b` -- Source TPSA `b`

### Output
- `c` -- Destination TPSA `c = a - b`
"""
function mad_tpsa_sub!(a::RealTPS{D}, b::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_sub(a::Ref{TPS{Float64,D}}, b::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_mul!(a::RealTPS{D}, b::RealTPS{D}, c::RealTPS{D}) where {D}

Sets the destination TPSA `c = a * b`

### Input
- `a` -- Source TPSA `a`
- `b` -- Source TPSA `b`

### Output
- `c` -- Destination TPSA `c = a * b`
"""
function mad_tpsa_mul!(a::RealTPS{D}, b::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_mul(a::Ref{TPS{Float64,D}}, b::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_div!(a::RealTPS{D}, b::RealTPS{D}, c::RealTPS{D}) where {D}

Sets the destination TPSA `c = a / b`

### Input
- `a` -- Source TPSA `a`
- `b` -- Source TPSA `b`

### Output
- `c` -- Destination TPSA `c = a / b`
"""
function mad_tpsa_div!(a::RealTPS{D}, b::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_div(a::Ref{TPS{Float64,D}}, b::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_pow!(a::RealTPS{D}, b::RealTPS{D}, c::RealTPS{D}) where {D}

Sets the destination TPSA `c = a ^ b`

### Input
- `a` -- Source TPSA `a`
- `b` -- Source TPSA `b`

### Output
- `c` -- Destination TPSA `c = a ^ b`
"""
function mad_tpsa_pow!(a::RealTPS{D}, b::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_pow(a::Ref{TPS{Float64,D}}, b::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_powi!(a::RealTPS{D}, n::Cint, c::RealTPS{D}) where {D}

Sets the destination TPSA `c = a ^ n` where `n` is an integer.

### Input
- `a` -- Source TPSA `a`
- `n` -- Integer power

### Output
- `c` -- Destination TPSA `c = a ^ n`
"""
function mad_tpsa_powi!(a::RealTPS{D}, n::Cint, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_powi(a::Ref{TPS{Float64,D}}, n::Cint, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_pown!(a::RealTPS{D}, v::Cdouble, c::RealTPS{D}) where {D}

Sets the destination TPSA `c = a ^ v` where `v` is of double precision.

### Input
- `a` -- Source TPSA `a`
- `v` -- "double" precision power

### Output
- `c` -- Destination TPSA `c = a ^ v`
"""
function mad_tpsa_pown!(a::RealTPS{D}, v::Cdouble, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_pown(a::Ref{TPS{Float64,D}}, v::Cdouble, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_nrm(a::RealTPS{D})::Cdouble where {D}

Calculates the 1-norm of TPSA `a` (sum of `abs` of all coefficients)

### Input
- `a`   -- TPSA

### Output
- `nrm` -- 1-Norm of TPSA
"""
function mad_tpsa_nrm(a::RealTPS{D})::Cdouble where {D}
  nrm = @ccall MAD_TPSA.mad_tpsa_nrm(a::Ref{TPS{Float64,D}})::Cdouble
  return nrm
end


"""
    mad_tpsa_abs!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the absolute value of TPSA `a`. Specifically, the 
result contains a TPSA with the `abs` of all coefficients.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = |a|`
"""
function mad_tpsa_abs!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_abs(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_sqrt!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the sqrt of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = sqrt(a)`
"""
function mad_tpsa_sqrt!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_sqrt(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_exp!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the exponential of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = exp(a)`
"""
function mad_tpsa_exp!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_exp(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end



"""
    mad_tpsa_log!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the log of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = log(a)`
"""
function mad_tpsa_log!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_log(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_sincos!(a::RealTPS{D}, s::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `s = sin(a)` and TPSA `c = cos(a)`

### Input
- `a` -- Source TPSA `a`

### Output
- `s` -- Destination TPSA `s = sin(a)`
- `c` -- Destination TPSA `c = cos(a)`
"""
function mad_tpsa_sincos!(a::RealTPS{D}, s::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_sincos(a::Ref{TPS{Float64,D}}, s::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_sin!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `sin` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = sin(a)`
"""
function mad_tpsa_sin!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_sin(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_cos!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `cos` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = cos(a)`
"""
function mad_tpsa_cos!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_cos(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_tan!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `tan` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = tan(a)`
"""
function mad_tpsa_tan!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_tan(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_cot!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `cot` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = cot(a)`
"""
function mad_tpsa_cot!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_cot(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_sinc!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `sinc` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = sinc(a)`
"""
function mad_tpsa_sinc!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_sinc(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_sincosh!(a::RealTPS{D}, s::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `s = sinh(a)` and TPSA `c = cosh(a)`

### Input
- `a` -- Source TPSA `a`

### Output
- `s` -- Destination TPSA `s = sinh(a)`
- `c` -- Destination TPSA `c = cosh(a)`
"""
function mad_tpsa_sincosh!(a::RealTPS{D}, s::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_sincosh(a::Ref{TPS{Float64,D}}, s::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_sinh!(a::RealTPS{D}, c::RealTPS{D}) where {D}

  Sets TPSA `c` to the `sinh` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = sinh(a)`
"""
function mad_tpsa_sinh!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_sinh(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_cosh!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `cosh` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = cosh(a)`
"""
function mad_tpsa_cosh!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_cosh(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_tanh!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `tanh` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = tanh(a)`
"""
function mad_tpsa_tanh!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_tanh(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_coth!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `coth` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = coth(a)`
"""
function mad_tpsa_coth!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_coth(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_sinhc!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `sinhc` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = sinhc(a)`
"""
function mad_tpsa_sinhc!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_sinhc(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_asin!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `asin` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = asin(a)`
"""
function mad_tpsa_asin!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_asin(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_acos!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `acos` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = acos(a)`
"""
function mad_tpsa_acos!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_acos(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_atan!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `atan` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = atan(a)`
"""
function mad_tpsa_atan!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_atan(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_acot!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `acot` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = acot(a)`
"""
function mad_tpsa_acot!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_acot(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end

"""
    mad_tpsa_asinc!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `asinc(a) = asin(a)/a`

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = asinc(a) = asin(a)/a`
"""
function mad_tpsa_asinc!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_asinc(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_asinh!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `asinh` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = asinh(a)'
"""
function mad_tpsa_asinh!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_asinh(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_acosh!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `acosh` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = acosh(a)'
"""
function mad_tpsa_acosh!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_acosh(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_atanh!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `atanh` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = atanh(a)'
"""
function mad_tpsa_atanh!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_atanh(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_acoth!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the acoth of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = acoth(a)'
"""
function mad_tpsa_acoth!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_acoth(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_asinhc!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `asinhc` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = asinhc(a)'
"""
function mad_tpsa_asinhc!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_asinhc(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_erf!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `erf` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = erf(a)'
"""
function mad_tpsa_erf!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_erf(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_erfc!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `erfc` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = erfc(a)'
"""
function mad_tpsa_erfc!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_erfc(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_erfcx!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `erfcx` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = erfcx(a)'
"""
function mad_tpsa_erfcx!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_erfcx(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_erfi!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the `erfi` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = erfi(a)'
"""
function mad_tpsa_erfi!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_erfi(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_wf!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Sets TPSA `c` to the Fadeeva function `wf` of TPSA `a`.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c = wf(a)'
"""
function mad_tpsa_wf!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_wf(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_acc!(a::RealTPS{D}, v::Cdouble, c::RealTPS{D}) where {D}

Adds `a*v` to TPSA `c`. Aliasing OK.

### Input
- `a` -- Source TPSA `a`
- `v` -- Scalar with double precision

### Output
- `c` -- Destination TPSA `c += v*a`
"""
function mad_tpsa_acc!(a::RealTPS{D}, v::Cdouble, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_acc(a::Ref{TPS{Float64,D}}, v::Cdouble, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_scl!(a::RealTPS{D}, v::Cdouble, c::RealTPS{D}) where {D}

Sets TPSA `c` to `v*a`. 

### Input
- `a` -- Source TPSA `a`
- `v` -- Scalar with double precision

### Output
- `c` -- Destination TPSA `c = v*a`
"""
function mad_tpsa_scl!(a::RealTPS{D}, v::Cdouble, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_scl(a::Ref{TPS{Float64,D}}, v::Cdouble, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_inv!(a::RealTPS{D},  v::Cdouble, c::RealTPS{D}) where {D}

Sets TPSA `c` to `v/a`. 

### Input
- `a` -- Source TPSA `a`
- `v` -- Scalar with double precision

### Output
- `c` -- Destination TPSA `c = v/a`
"""
function mad_tpsa_inv!(a::RealTPS{D},  v::Cdouble, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_inv(a::Ref{TPS{Float64,D}},  v::Cdouble, c::Ref{TPS{Float64,D}})::Cvoid
end

"""
    mad_tpsa_invsqrt!(a::RealTPS{D}, v::Cdouble, c::RealTPS{D}) where {D}

Sets TPSA `c` to `v/sqrt(a)`. 

### Input
- `a` -- Source TPSA `a`
- `v` -- Scalar with double precision

### Output
- `c` -- Destination TPSA `c = v/sqrt(a)`
"""
function mad_tpsa_invsqrt!(a::RealTPS{D}, v::Cdouble, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_invsqrt(a::Ref{TPS{Float64,D}}, v::Cdouble, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_unit!(a::RealTPS{D}, c::RealTPS{D}) where {D}

Interpreting TPSA as a vector, gets the "unit vector", e.g. `c = a/norm(a)`. 
May be useful for checking for convergence.

### Input
- `a` -- Source TPSA `a`

### Output
- `c` -- Destination TPSA `c`
"""
function  mad_tpsa_unit!(a::RealTPS{D}, c::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_unit(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_atan2!(y::RealTPS{D}, x::RealTPS{D}, r::RealTPS{D}) where {D}

Sets TPSA `r` to `atan2(y,x)`

### Input
- `y` -- Source TPSA `y`
- `x` -- Source TPSA `x`

### Output
- `r` -- Destination TPSA r = atan2(y,x)
"""
function  mad_tpsa_atan2!(y::RealTPS{D}, x::RealTPS{D}, r::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_atan2(y::Ref{TPS{Float64,D}}, x::Ref{TPS{Float64,D}}, r::Ref{TPS{Float64,D}})::Cvoid
end

"""
    mad_tpsa_hypot!(x::RealTPS{D}, y::RealTPS{D}, r::RealTPS{D}) where {D}

Sets TPSA `r` to `sqrt(x^2+y^2)`. Used to oversimplify polymorphism in code but not optimized

### Input
- `x` -- Source TPSA `x`
- `y` -- Source TPSA `y`

### Output
- `r` -- Destination TPSA r = sqrt(x^2+y^2)
"""
function  mad_tpsa_hypot!(x::RealTPS{D}, y::RealTPS{D}, r::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_hypot(x::Ref{TPS{Float64,D}}, y::Ref{TPS{Float64,D}}, r::Ref{TPS{Float64,D}})::Cvoid
end

"""
    mad_tpsa_hypot3!(x::RealTPS{D}, y::RealTPS{D}, z::RealTPS{D}, r::RealTPS{D}) where {D}

Sets TPSA `r` to `sqrt(x^2+y^2+z^2)`. Does NOT allow for r = x, y, z !!!

### Input
- `x` -- Source TPSA `x`
- `y` -- Source TPSA `y`
- `z` -- Source TPSA `z`

### Output
- `r` -- Destination TPSA `r = sqrt(x^2+y^2+z^2)`
"""
function  mad_tpsa_hypot3!(x::RealTPS{D}, y::RealTPS{D}, z::RealTPS{D}, r::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_hypot3(x::Ref{TPS{Float64,D}}, y::Ref{TPS{Float64,D}}, z::Ref{TPS{Float64,D}}, r::Ref{TPS{Float64,D}})::Cvoid
end



"""
    mad_tpsa_integ!(a::RealTPS{D}, c::RealTPS{D}, iv::Cint) where {D}

Integrates TPSA with respect to the variable with index `iv`.

### Input
- `a`  -- Source TPSA to integrate
- `iv` -- Index of variable to integrate over (e.g. integrate over `x`, `iv = 1`). 

### Output
- `c`  -- Destination TPSA
"""
function mad_tpsa_integ!(a::RealTPS{D}, c::RealTPS{D}, iv::Cint) where {D}
  @ccall MAD_TPSA.mad_tpsa_integ(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}}, iv::Cint)::Cvoid
end


"""
    mad_tpsa_deriv!(a::RealTPS{D}, c::RealTPS{D}, iv::Cint) where {D}

Differentiates TPSA with respect to the variable with index `iv`.

### Input
- `a`  -- Source TPSA to differentiate
- `iv` -- Index of variable to take derivative wrt to (e.g. derivative wrt `x`, `iv = 1`). 

### Output
- `c`  -- Destination TPSA
"""
function mad_tpsa_deriv!(a::RealTPS{D}, c::RealTPS{D}, iv::Cint) where {D}
  @ccall MAD_TPSA.mad_tpsa_deriv(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}}, iv::Cint)::Cvoid
end


"""
    mad_tpsa_derivm!(a::RealTPS{D}, c::RealTPS{D}, n::Cint, m) where {D}

Differentiates TPSA with respect to the monomial defined by byte array `m`.

### Input
- `a` -- Source TPSA to differentiate
- `n` -- Length of monomial to differentiate wrt
- `m` -- Monomial to take derivative wrt

### Output
- `c` -- Destination TPSA
"""
function mad_tpsa_derivm!(a::RealTPS{D}, c::RealTPS{D}, n::Cint, m) where {D}
  eltype(m) == Cuchar || error("m must have eltype Cuchar !")
  @ccall MAD_TPSA.mad_tpsa_derivm(a::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}}, n::Cint, m::Ptr{Cuchar})::Cvoid
end


"""
    mad_tpsa_poisbra!(a::RealTPS{D}, b::RealTPS{D}, c::RealTPS{D}, nv::Cint) where {D}

Sets TPSA `c` to the poisson bracket of TPSAs `a` and `b`.

### Input
- `a`  -- Source TPSA `a`
- `b`  -- Source TPSA `b`
- `nv` -- Number of variables in the TPSA

### Output
- `c`  -- Destination TPSA `c`
"""
function mad_tpsa_poisbra!(a::RealTPS{D}, b::RealTPS{D}, c::RealTPS{D}, nv::Cint) where {D}
  @ccall MAD_TPSA.mad_tpsa_poisbra(a::Ref{TPS{Float64,D}}, b::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}}, nv::Cint)::Cvoid
end


"""
    mad_tpsa_taylor!(a::RealTPS{D}, n::Cint, coef, c::RealTPS{D}) where {D}

Computes the result of the Taylor series up to order `n-1` with Taylor coefficients coef for the scalar value in `a`. That is,
`c = coef[0] + coef[1]*a_0 + coef[2]*a_0^2 + ...` where `a_0` is the scalar part of TPSA `a`.

### Input
- `a`    -- TPSA `a`
- `n`    -- `Order-1` of Taylor expansion, size of `coef` array
- `coef` -- Array of coefficients in Taylor `s`
- `c`    -- Result
"""
function mad_tpsa_taylor!(a::RealTPS{D}, n::Cint, coef, c::RealTPS{D}) where {D}
  eltype(coef) == Cdouble || error("coef must have eltype Cdouble")
  @ccall MAD_TPSA.mad_tpsa_taylor(a::Ref{TPS{Float64,D}}, n::Cint, coef::Ptr{Cdouble}, c::Ref{TPS{Float64,D}})::Cvoid
end

"""
    mad_tpsa_taylor_h!(a::RealTPS{D}, n::Cint, coef, c::RealTPS{D}) where {D}

Computes the result of the Taylor series up to order `n-1` with Taylor coefficients coef for 
the scalar value in `a`. That is, `c = coef[0] + coef[1]*a_0 + coef[2]*a_0^2 + ...` where `a_0` 
is the scalar part of TPSA `a`.

Same as `mad_tpsa_taylor`, but uses Horner's method (which is 50%-100% slower because mul is 
always full order).

### Input
- `a`    -- TPSA `a`
- `n`    -- `Order-1` of Taylor expansion, size of `coef` array
- `coef` -- Array of coefficients in Taylor `s`
- `c`    -- Result
"""
function mad_tpsa_taylor_h!(a::RealTPS{D}, n::Cint, coef, c::RealTPS{D}) where {D}
  eltype(coef) == Cdouble || error("coef must have eltype Cdouble")
  @ccall MAD_TPSA.mad_tpsa_taylor_h(a::Ref{TPS{Float64,D}}, n::Cint, coef::Ptr{Cdouble}, c::Ref{TPS{Float64,D}})::Cvoid
end

"""
    mad_tpsa_axpb!(a::Cdouble, x::RealTPS{D}, b::Cdouble, r::RealTPS{D}) where {D}

`r = a*x + b`

### Input
- `a` -- Scalar `a`
- `x` -- TPSA `x`
- `b` -- Scalar `b`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_tpsa_axpb!(a::Cdouble, x::RealTPS{D}, b::Cdouble, r::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_axpb(a::Cdouble, x::Ref{TPS{Float64,D}}, b::Cdouble, r::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_axpbypc!(a::Cdouble, x::RealTPS{D}, b::Cdouble, y::RealTPS{D}, c::Cdouble, r::RealTPS{D}) where {D}

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
function mad_tpsa_axpbypc!(a::Cdouble, x::RealTPS{D}, b::Cdouble, y::RealTPS{D}, c::Cdouble, r::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_axpbypc(a::Cdouble, x::Ref{TPS{Float64,D}}, b::Cdouble, y::Ref{TPS{Float64,D}}, c::Cdouble, r::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_axypb!(a::Cdouble, x::RealTPS{D}, y::RealTPS{D}, b::Cdouble, r::RealTPS{D}) where {D}

`r = a*x*y + b`

### Input
- `a` -- Scalar `a`
- `x` -- TPSA `x`
- `y` -- TPSA `y`
- `b` -- Scalar `b`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_tpsa_axypb!(a::Cdouble, x::RealTPS{D}, y::RealTPS{D}, b::Cdouble, r::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_axypb(a::Cdouble, x::Ref{TPS{Float64,D}}, y::Ref{TPS{Float64,D}}, b::Cdouble, r::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_axypbzpc!(a::Cdouble, x::RealTPS{D}, y::RealTPS{D}, b::Cdouble, z::RealTPS{D}, c::Cdouble, r::RealTPS{D}) where {D}

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
function mad_tpsa_axypbzpc!(a::Cdouble, x::RealTPS{D}, y::RealTPS{D}, b::Cdouble, z::RealTPS{D}, c::Cdouble, r::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_axypbzpc(a::Cdouble, x::Ref{TPS{Float64,D}}, y::Ref{TPS{Float64,D}}, b::Cdouble, z::Ref{TPS{Float64,D}}, c::Cdouble, r::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_axypbvwpc!(a::Cdouble, x::RealTPS{D}, y::RealTPS{D}, b::Cdouble, v::RealTPS{D}, w::RealTPS{D}, c::Cdouble, r::RealTPS{D}) where {D}

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
function mad_tpsa_axypbvwpc!(a::Cdouble, x::RealTPS{D}, y::RealTPS{D}, b::Cdouble, v::RealTPS{D}, w::RealTPS{D}, c::Cdouble, r::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_axypbvwpc(a::Cdouble, x::Ref{TPS{Float64,D}}, y::Ref{TPS{Float64,D}}, b::Cdouble, v::Ref{TPS{Float64,D}}, w::Ref{TPS{Float64,D}}, c::Cdouble, r::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_ax2pby2pcz2!(a::Cdouble, x::RealTPS{D}, b::Cdouble, y::RealTPS{D}, c::Cdouble, z::RealTPS{D}, r::RealTPS{D}) where {D}

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
function mad_tpsa_ax2pby2pcz2!(a::Cdouble, x::RealTPS{D}, b::Cdouble, y::RealTPS{D}, c::Cdouble, z::RealTPS{D}, r::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_ax2pby2pcz2(a::Cdouble, x::Ref{TPS{Float64,D}}, b::Cdouble, y::Ref{TPS{Float64,D}}, c::Cdouble, z::Ref{TPS{Float64,D}}, r::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_axpsqrtbpcx2!(x::RealTPS{D}, a::Cdouble, b::Cdouble, c::Cdouble, r::RealTPS{D}) where {D}

`r = a*x + sqrt(b + c*x^2)`

### Input
- `x` -- TPSA `x`
- `a` -- Scalar `a`
- `b` -- Scalar `b`
- `c` -- Scalar `c`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_tpsa_axpsqrtbpcx2!(x::RealTPS{D}, a::Cdouble, b::Cdouble, c::Cdouble, r::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_axpsqrtbpcx2(x::Ref{TPS{Float64,D}}, a::Cdouble, b::Cdouble, c::Cdouble, r::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_logaxpsqrtbpcx2!(x::RealTPS{D}, a::Cdouble, b::Cdouble, c::Cdouble, r::RealTPS{D}) where {D}

`r = log(a*x + sqrt(b + c*x^2))`

### Input
- `x` -- TPSA `x`
- `a` -- Scalar `a`
- `b` -- Scalar `b`
- `c` -- Scalar `c`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_tpsa_logaxpsqrtbpcx2!(x::RealTPS{D}, a::Cdouble, b::Cdouble, c::Cdouble, r::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_logaxpsqrtbpcx2(x::Ref{TPS{Float64,D}}, a::Cdouble, b::Cdouble, c::Cdouble, r::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_logxdy!(x::RealTPS{D}, y::RealTPS{D}, r::RealTPS{D}) where {D}

`r = log(x / y)`

### Input
- `x` -- TPSA `x`
- `y` -- TPSA `y`

### Output
- `r` -- Destination TPSA `r`
"""
function mad_tpsa_logxdy!(x::RealTPS{D}, y::RealTPS{D}, r::RealTPS{D}) where {D}
  @ccall MAD_TPSA.mad_tpsa_logxdy(x::Ref{TPS{Float64,D}}, y::Ref{TPS{Float64,D}}, r::Ref{TPS{Float64,D}})::Cvoid
end


"""
    mad_tpsa_vec2fld!(na::Cint, a::RealTPS{D}, mc) where {D}

Assuming the variables in the TPSA are canonically-conjugate, and ordered so that the canonically-
conjugate variables are consecutive (q1, p1, q2, p2, ...), calculates the vector field (Hamilton's 
equations) from the passed Hamiltonian, defined as `[da/dp1, -da/dq1, ...]`

### Input
- `na`  -- Number of TPSA in `mc` consistent with number of variables in `a`
- `a`   -- Hamiltonian as a TPSA

### Output
- `mc`  -- Vector field derived from `a` using Hamilton's equations 
"""
function mad_tpsa_vec2fld!(na::Cint, a::RealTPS{D}, mc) where {D}
  eltype(mc) <: RealTPS{D} || error("mc must have eltype <: RealTPS{D} !")
  GC.@preserve mc begin
  @ccall MAD_TPSA.mad_tpsa_vec2fld(na::Cint, a::Ref{TPS{Float64,D}}, mc::Ptr{TPS{Float64,D}})::Cvoid
  end
end


"""
    mad_tpsa_fld2vec!(na::Cint, ma, c::RealTPS{D}) where {D}

Assuming the variables in the TPSA are canonically-conjugate, and ordered so that the canonically-
conjugate variables are consecutive (q1, p1, q2, p2, ...), calculates the Hamiltonian one obtains 
from ther vector field (in the form `[da/dp1, -da/dq1, ...]`)

### Input
- `na`  -- Number of TPSA in `ma` consistent with number of variables in `c`
- `ma`  -- Vector field 

### Output
- `c`   -- Hamiltonian as a TPSA derived from the vector field `ma`
"""
function mad_tpsa_fld2vec!(na::Cint, ma, c::RealTPS{D}) where {D}
  eltype(ma) <: RealTPS{D} || error("ma must have eltype <: RealTPS{D} !")
  GC.@preserve ma begin
  @ccall MAD_TPSA.mad_tpsa_fld2vec(na::Cint, ma::Ptr{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
  end
end


"""
    mad_tpsa_fgrad!(na::Cint, ma, b::RealTPS{D}, c::RealTPS{D}) where {D}

Calculates `dot(ma, grad(b))`

### Input
- `na` -- Length of `ma` consistent with number of variables in `b`
- `ma` -- Vector of TPSA
- `b`  -- TPSA

### Output
- `c`  -- `dot(ma, grad(b))`
"""
function mad_tpsa_fgrad!(na::Cint, ma, b::RealTPS{D}, c::RealTPS{D}) where {D}
  eltype(ma) <: RealTPS{D} || error("ma must have eltype <: RealTPS{D} !")
  GC.@preserve ma begin
  @ccall MAD_TPSA.mad_tpsa_fgrad(na::Cint, ma::Ptr{TPS{Float64,D}}, b::Ref{TPS{Float64,D}}, c::Ref{TPS{Float64,D}})::Cvoid
  end
end


"""
    mad_tpsa_liebra!(na::Cint, ma, mb, mc)

Computes the Lie bracket of the vector fields `ma` and `mb`, defined as 
sum_i ma_i (dmb/dx_i) - mb_i (dma/dx_i).

### Input
- `na` -- Length of `ma` and `mb`
- `ma` -- Vector of TPSA `ma`
- `mb` -- Vector of TPSA `mb`

### Output
- `mc` -- Destination vector of TPSA `mc`
"""
function mad_tpsa_liebra!(na::Cint, ma, mb, mc)
  eltype(ma) <: RealTPS{D} || error("ma must have eltype <: RealTPS{D} !")
  eltype(mb) <: RealTPS{D} || error("mb must have eltype <: RealTPS{D} !")
  eltype(mc) <: RealTPS{D} || error("mc must have eltype <: RealTPS{D} !")
  GC.@preserve ma mb mc begin
  @ccall MAD_TPSA.mad_tpsa_liebra(na::Cint, ma::Ptr{TPS{Float64,D}}, mb::Ptr{TPS{Float64,D}}, mc::Ptr{TPS{Float64,D}})::Cvoid
  end
end


"""
    mad_tpsa_exppb!(na::Cint, ma, mb, mc)

Computes the exponential of fgrad of the vector fields `ma` and `mb`,
literally `exppb(ma, mb) = mb + fgrad(ma, mb) + fgrad(ma, fgrad(ma, mb))/2! + ...`

### Input
- `na` -- Length of `ma` and `mb`
- `ma` -- Vector of TPSA `ma`
- `mb` -- Vector of TPSA `mb`

### Output
- `mc` -- Destination vector of TPSA `mc`
"""
function mad_tpsa_exppb!(na::Cint, ma, mb, mc)
  eltype(ma) <: RealTPS{D} || error("ma must have eltype <: RealTPS{D} !")
  eltype(mb) <: RealTPS{D} || error("mb must have eltype <: RealTPS{D} !")
  eltype(mc) <: RealTPS{D} || error("mc must have eltype <: RealTPS{D} !")
  GC.@preserve ma mb mc begin
  @ccall MAD_TPSA.mad_tpsa_exppb(na::Cint, ma::Ptr{TPS{Float64,D}}, mb::Ptr{TPS{Float64,D}}, mc::Ptr{TPS{Float64,D}})::Cvoid
  end
end


"""
    mad_tpsa_logpb!(na::Cint, ma, mb, mc)

Computes the log of the Poisson bracket of the vector of TPSA `ma` and `mb`; the result 
is the vector field `F` used to evolve to `ma` from `mb`.

### Input
- `na` -- Length of `ma` and `mb`
- `ma` -- Vector of TPSA `ma`
- `mb` -- Vector of TPSA `mb`

### Output
- `mc` -- Destination vector of TPSA `mc`
"""
function mad_tpsa_logpb!(na::Cint, ma, mb, mc)
  eltype(ma) <: RealTPS{D} || error("ma must have eltype <: RealTPS{D} !")
  eltype(mb) <: RealTPS{D} || error("mb must have eltype <: RealTPS{D} !")
  eltype(mc) <: RealTPS{D} || error("mc must have eltype <: RealTPS{D} !")
  GC.@preserve ma mb mc begin
  @ccall MAD_TPSA.mad_tpsa_logpb(na::Cint, ma::Ptr{TPS{Float64,D}}, mb::Ptr{TPS{Float64,D}}, mc::Ptr{TPS{Float64,D}})::Cvoid
  end
end


"""
    mad_tpsa_mord(na::Cint, ma, hi::Bool)::Cuchar

If `hi` is false, getting the maximum `mo` among all TPSAs in `ma`. 
If `hi` is `true`, gets the maximum `hi` of the map instead of `mo`

### Input
- `na` -- Length of map `ma`
- `ma` -- Map (vector of TPSAs)
- `hi` -- If `true`, returns maximum `hi`, else returns maximum `mo` of the map

### Output
- `ret` -- Maximum `hi` of the map if `hi` is `true`, else returns maximum `mo` of the map
"""
function mad_tpsa_mord(na::Cint, ma, hi::Bool)::Cuchar
  eltype(ma) <: RealTPS{D} || error("ma must have eltype <: RealTPS{D} !")
  GC.@preserve ma begin
  ret = @ccall MAD_TPSA.mad_tpsa_mord(na::Cint, ma::Ptr{TPS{Float64,D}}, hi::Bool)::Cuchar
  end
  return ret
end


"""
    mad_tpsa_mnrm(na::Cint, ma)::Cdouble

Computes the norm of the map (sum of absolute value of coefficients of all TPSAs in the map).

### Input
- `na`  -- Number of TPSAs in the map
- `ma`  -- map `ma`

### Output
- `nrm` -- Norm of map (sum of absolute value of coefficients of all TPSAs in the map)
"""
function mad_tpsa_mnrm(na::Cint, ma)::Cdouble
  eltype(ma) <: RealTPS{D} || error("ma must have eltype <: RealTPS{D} !")
  GC.@preserve ma begin
  nrm = @ccall MAD_TPSA.mad_tpsa_mnrm(na::Cint, ma::Ptr{TPS{Float64,D}})::Cdouble
  end
  return nrm
end


"""
    mad_tpsa_minv!(na::Cint, ma, nb::Cint, mc)

Inverts the map. To include the parameters in the inversion, `na` = `nn` and the output map 
length only need be `nb` = `nv`.

### Input
- `na` -- Input map length (should be `nn` to include parameters)
- `ma` -- Map `ma`
- `nb` -- Output map length (generally = `nv`)

### Output
- `mc` -- Inversion of map `ma`
"""
function mad_tpsa_minv!(na::Cint, ma, nb::Cint, mc)
  eltype(ma) <: RealTPS{D} || error("ma must have eltype <: RealTPS{D} !")
  eltype(mc) <: RealTPS{D} || error("mc must have eltype <: RealTPS{D} !")
  GC.@preserve ma mc begin
  @ccall MAD_TPSA.mad_tpsa_minv(na::Cint, ma::Ptr{TPS{Float64,D}}, nb::Cint, mc::Ptr{TPS{Float64,D}})::Cvoid
  end
end


"""
    mad_tpsa_pminv!(na::Cint, ma, nb::Cint, mc, select)

Computes the partial inverse of the map with only the selected variables, specified by 0s or 1s in select.
To include the parameters in the inversion, `na` = `nn` and the output map length only need be `nb` = `nv`.

### Input
- `na` -- Input map length (should be `nn` to include parameters)
- `ma` -- Map `ma`
- `nb` -- Output map length (generally = `nv`)
- `select` -- Array of 0s or 1s defining which variables to do inverse on (atleast same size as na)'

### Output
- `mc`     -- Partially inverted map using variables specified as 1 in the select array
"""
function mad_tpsa_pminv!(na::Cint, ma, nb::Cint, mc, select)
  eltype(ma) <: RealTPS{D} || error("ma must have eltype <: RealTPS{D} !")
  eltype(mc) <: RealTPS{D} || error("mc must have eltype <: RealTPS{D} !")
  eltype(select) == Cint || error("select must have eltype Cint !")
  GC.@preserve ma mc begin
  @ccall MAD_TPSA.mad_tpsa_pminv(na::Cint, ma::Ptr{TPS{Float64,D}}, nb::Cint, mc::Ptr{TPS{Float64,D}}, select::Ptr{Cint})::Cvoid
  end
end


"""
    mad_tpsa_compose!(na::Cint, ma, nb::Cint, mb, mc)

Composes two maps.

### Input
- `na` -- Number of TPSAs in map `ma`
- `ma` -- map `ma`
- `nb` -- Number of TPSAs in map `mb`
- `mb` -- map `mb`

### Output
- `mc` -- Composition of maps `ma` and `mb`
"""
function mad_tpsa_compose!(na::Cint, ma, nb::Cint, mb, mc)
  eltype(ma) <: RealTPS{D} || error("ma must have eltype <: RealTPS{D} !")
  eltype(mb) <: RealTPS{D} || error("mb must have eltype <: RealTPS{D} !")
  eltype(mc) <: RealTPS{D} || error("mc must have eltype <: RealTPS{D} !")
  GC.@preserve ma mb mc begin
  @ccall MAD_TPSA.mad_tpsa_compose(na::Cint, ma::Ptr{TPS{Float64,D}}, nb::Cint, mb::Ptr{TPS{Float64,D}}, mc::Ptr{TPS{Float64,D}})::Cvoid
  end
end


"""
    mad_tpsa_translate!(na::Cint, ma, nb::Cint, tb, mc)

Translates the expansion point of the map by the amount `tb`.

### Input
- `na` -- Number of TPSAS in the map
- `ma` -- map `ma`
- `nb` -- Length of `tb`
- `tb` -- Vector of amount to translate for each variable

### Output
- `mc` -- Map evaluated at the new point translated `tb` from the original evaluation point
"""
function mad_tpsa_translate!(na::Cint, ma, nb::Cint, tb, mc)
  eltype(ma) <: RealTPS{D} || error("ma must have eltype <: RealTPS{D} !")
  eltype(mc) <: RealTPS{D} || error("mc must have eltype <: RealTPS{D} !")
  eltype(tb) == Cdouble || error("tb must have eltype Cdouble !")
  GC.@preserve ma mc begin
  @ccall MAD_TPSA.mad_tpsa_translate(na::Cint, ma::Ptr{TPS{Float64,D}}, nb::Cint, tb::Ptr{Cdouble}, mc::Ptr{TPS{Float64,D}})::Cvoid
  end
end


"""
    mad_tpsa_eval!(na::Cint, ma, nb::Cint, tb, tc)

Evaluates the map at the point `tb`

### Input
- `na` -- Number of TPSAs in the map
- `ma` -- map `ma`
- `nb` -- Length of `tb`
- `tb` -- Point at which to evaluate the map

### Output
- `tc` -- Values for each TPSA in the map evaluated at the point `tb`
"""
function mad_tpsa_eval!(na::Cint, ma, nb::Cint, tb, tc)
  eltype(ma) <: RealTPS{D} || error("ma must have eltype <: RealTPS{D} !")
  eltype(tb) == Cdouble || error("tb must have eltype Cdouble !")
  eltype(tc) == Cdouble || error("tc must have eltype Cdouble !")
  GC.@preserve ma begin
  @ccall MAD_TPSA.mad_tpsa_eval(na::Cint, ma::Ptr{TPS{Float64,D}}, nb::Cint, tb::Ptr{Cdouble}, tc::Ptr{Cdouble})::Cvoid
  end
end


"""
    mad_tpsa_mconv!(na::Cint, ma, nc::Cint, mc, n::Cint, t2r_, pb::Cint)

Equivalent to `mad_tpsa_convert`, but applies the conversion to all TPSAs in the map `ma`.

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
function mad_tpsa_mconv!(na::Cint, ma, nc::Cint, mc, n::Cint, t2r_, pb::Cint)
  eltype(ma) <: RealTPS{D} || error("ma must have eltype <: RealTPS{D} !")
  eltype(mc) <: RealTPS{D} || error("mc must have eltype <: RealTPS{D} !")
  typeof(t2r_) == Ptr{Nothing} || eltype(t2r_) == Cint || error("t2r_ must have eltype Cint if provided!")
  GC.@preserve ma mc begin
  @ccall MAD_TPSA.mad_tpsa_mconv(na::Cint, ma::Ptr{TPS{Float64,D}}, nc::Cint, mc::Ptr{TPS{Float64,D}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)::Cvoid
  end
end


"""
    mad_tpsa_print(t::RealTPS{D}, name_::Cstring, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid}) where {D}

Prints the TPSA coefficients with precision `eps_`. If `nohdr_` is not zero, 
the header is not printed. 

### Input
- `t`       -- TPSA to print
- `name_`   -- (Optional) Name of TPSA
- `eps_`    -- (Optional) Precision to output
- `nohdr_`  -- (Optional) If True, no header is printed
- `stream_` -- (Optional) `FILE` pointer of output stream. Default is `stdout`
"""
function mad_tpsa_print(t::RealTPS{D}, name_, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid}) where {D}
  @ccall MAD_TPSA.mad_tpsa_print(t::Ref{TPS{Float64,D}}, name_::Cstring, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid})::Cvoid
end


"""
    mad_tpsa_scan(stream_::Ptr{Cvoid})::RealTPS{D} where {D}

Scans in a TPSA from the `stream_`.

### Input
- `stream_` -- (Optional) I/O stream from which to read the TPSA, default is `stdin`

### Output
- `t`       -- TPSA scanned from I/O `stream_`
"""
function mad_tpsa_scan(stream_::Ptr{Cvoid})::RealTPS{Nothing}
  t = @ccall MAD_TPSA.mad_tpsa_scan(stream_::Ptr{Cvoid})::Ptr{TPS{Float64,Nothing}}
  return t
end


"""
    mad_tpsa_scan_hdr(kind_::Ref{Cint}, name_::Ptr{Cuchar}, stream_::Ptr{Cvoid})::Ptr{Desc}

Read TPSA header. Returns descriptor for TPSA given the header. This is useful for external languages using 
this library where the memory is managed NOT on the C side.

### Input
- `kind_`   -- (Optional) Real or complex TPSA, or detect automatically if not provided.
- `name_`   -- (Optional) Name of TPSA
- `stream_` -- (Optional) I/O stream to read TPSA from,  default is `stdin`

### Output
- `ret`     -- Descriptor for the TPSA 
"""
function mad_tpsa_scan_hdr(kind_::Ref{Cint}, name_::Ptr{Cuchar}, stream_::Ptr{Cvoid})::Ptr{Desc}
  desc = @ccall MAD_TPSA.mad_tpsa_scan_hdr(kind_::Ptr{Cint}, name_::Ptr{Cuchar}, stream_::Ptr{Cvoid})::Ptr{Desc}
  return ret
end


"""
    mad_tpsa_scan_coef!(t::RealTPS{D}, stream_::Ptr{Cvoid}) where {D}

Read TPSA coefficients into TPSA `t`. This should be used with `mad_tpsa_scan_hdr` for external languages using 
this library where the memory is managed NOT on the C side.

### Input
- `stream_` -- (Optional) I/O stream to read TPSA from,  default is `stdin`

### Output
- `t`       -- TPSA with coefficients scanned from `stream_`
"""
function mad_tpsa_scan_coef!(t::RealTPS{D}, stream_::Ptr{Cvoid}) where {D}
  @ccall MAD_TPSA.mad_tpsa_scan_coef(t::Ref{TPS{Float64,D}}, stream_::Ptr{Cvoid})::Cvoid
end


"""
    mad_tpsa_debug(t::RealTPS{D}, name_::Cstring, fnam_::Cstring, line_::Cint, stream_::Ptr{Cvoid})::Cint where {D}

Prints TPSA with all information of data structure.

### Input
- `t`       -- TPSA
- `name_`   -- (Optional) Name of TPSA
- `fnam_`   -- (Optional) File name to print to
- `line_`   -- (Optional) Line number in file to start at
- `stream_` -- (Optional) I/O stream to print to, default is `stdout`

### Output
- `ret` -- `Cint` reflecting internal state of TPSA
"""
function mad_tpsa_debug(t::RealTPS{D}, name_::Cstring, fnam_::Cstring, line_::Cint, stream_::Ptr{Cvoid})::Cint where {D}
  ret = @ccall MAD_TPSA.mad_tpsa_debug(t::Ref{TPS{Float64,D}}, name_::Cstring, fnam_::Cstring, line_::Cint, stream_::Ptr{Cvoid})::Cint
  return ret
end

"""
    mad_tpsa_isval(t::RealTPS{D})::Bool where {D}

Sanity check of the TPSA integrity.

### Input
- `t` -- TPSA to check if valid

### Output
- `ret`  -- True if valid TPSA, false otherwise
"""
function mad_tpsa_isval(t::RealTPS{D})::Bool where {D}
  ret = @ccall MAD_TPSA.mad_tpsa_isval(t::Ref{TPS{Float64,D}})::Bool
  return ret
end

"""
    mad_tpsa_isvalid(t::RealTPS{D})::Bool where {D}

Sanity check of the TPSA integrity.

### Input
- `t` -- TPSA to check if valid

### Output
- `ret`  -- True if valid TPSA, false otherwise
"""
function mad_tpsa_isvalid(t::RealTPS{D})::Bool where {D}
  ret = @ccall MAD_TPSA.mad_tpsa_isvalid(t::Ref{TPS{Float64,D}})::Bool
  return ret
end


"""
    mad_tpsa_density(t::RealTPS{D}, stat_, reset::Bool)::Cdouble where {D}

Computes the ratio of `nz`/`nc` in `[0] U [lo,hi]` or `stat_`
"""
function mad_tpsa_density(t::RealTPS{D}, stat_, reset::Bool)::Cdouble where {D}
  ret = @ccall MAD_TPSA.mad_tpsa_density(t::Ref{TPS{Float64,D}}, stat_::Ptr{Cdouble}, reset::Bool)::Cdouble
  return ret
end


"""
    mad_tpsa_init(t::RealTPS{D}, d::Ptr{Desc}, mo::Cuchar)::RealTPS{D} where {D}

Unsafe initialization of an already existing TPSA `t` with maximum order `mo` to the descriptor `d`. `mo` must be less than 
the maximum order of the descriptor. `t` is modified in place and also returned.

### Input
- `t`  -- TPSA to initialize to descriptor `d`
- `d`  -- Descriptor
- `mo` -- Maximum order of the TPSA (must be less than maximum order of the descriptor)

### Output
- `t`  -- TPSA initialized to descriptor `d` with maximum order `mo`
"""
function mad_tpsa_init!(t::RealTPS{D}, d::Ptr{Desc}, mo::Cuchar)::RealTPS where {D}
  t = @ccall MAD_TPSA.mad_tpsa_init(t::Ref{TPS{Float64,D}}, d::Ptr{Desc}, mo::Cuchar)::Ptr{TPS{Float64,Nothing}}
  return t
end

