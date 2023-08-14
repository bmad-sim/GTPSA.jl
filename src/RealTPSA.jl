module RealTPSA
include("Structs.jl")
using .Structs


"""
    mad_tpsa_newd(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{RTPSA{Desc}}

Creates a real TPSA defined by the specified descriptor and maximum order. If MAD_TPSA_DEFAULT 
is passed for mo, the mo defined in the descriptor is used. If mo > d_mo, mo = d_mo.

### Input
- `d`  -- Descriptor
- `mo` -- Maximum order

### Output
- `t`  -- New real TPSA defined by the descriptor
"""
function mad_tpsa_newd(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
  t = @ccall MAD_TPSA.mad_tpsa_newd(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
  return t
end


"""
    mad_tpsa_new(t::Ptr{RTPSA{Desc}}, mo::Cuchar)::Ptr{RTPSA{Desc}}

Creates a real TPSA copy of the inputted TPSA, with maximum order specified by mo.
If MAD_TPSA_SAME is passed for mo, the mo currently in t is used for the created TPSA.
  ok with t=(tpsa_t*)ctpsa

### Input
- `t`   -- Real TPSA to copy
- `mo`  -- Maximum order of new TPSA

### Output
- `ret` -- New real TPSA with maximum order mo
"""
function mad_tpsa_new(t::Ptr{RTPSA{Desc}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
  ret = @ccall MAD_TPSA.mad_tpsa_new(t::Ptr{RTPSA{Desc}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
  return ret
end


"""
    mad_tpsa_del!(t::Ptr{RTPSA{Desc}})

Calls the destructor for the real TPSA.

### Input
- `t` -- Real TPSA to destruct
"""
function mad_tpsa_del!(t::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_del(t::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_desc(t::Ptr{RTPSA{Desc}})::Ptr{Desc{RTPSA,CTPSA}}

Gets the descriptor for the real TPSA.

### Input
- `t`   -- Real TPSA

### Output
- `ret` -- Descriptor for the RTPSA
"""
function mad_tpsa_desc(t::Ptr{RTPSA{Desc}})::Ptr{Desc{RTPSA,CTPSA}}
  ret = @ccall MAD_TPSA.mad_tpsa_desc(t::Ptr{RTPSA{Desc}})::Ptr{Desc{RTPSA,CTPSA}}
  return ret
end


"""
    mad_tpsa_uid!(t::Ptr{RTPSA{Desc}}, uid_::Cint)::Cint

Sets the TPSA uid if uid_ != 0, and returns the current (previous if set) TPSA uid. 

### Input
- `t`    -- Real TPSA
- `uid_` -- uid to set in the TPSA if uid_ != 0

### Output
- `ret`  -- Current (previous if set) TPSA uid
"""
function mad_tpsa_uid!(t::Ptr{RTPSA{Desc}}, uid_::Cint)::Cint
  ret = @ccall MAD_TPSA.mad_tpsa_uid(t::Ptr{RTPSA{Desc}}, uid_::Cint)::Cint
  return ret
end


"""
    mad_tpsa_len(t::Ptr{RTPSA{Desc}})::Cint

???

### Input
- `t`   -- Real TPSA

### Output
- `ret` -- Monomials in RTPSA
"""
function mad_tpsa_len(t::Ptr{RTPSA{Desc}})::Cint
  ret = @ccall MAD_TPSA.mad_tpsa_len(t::Ptr{RTPSA{Desc}})::Cint
  return ret
end


"""
    mad_tpsa_nam(t::Ptr{RTPSA{Desc}})::Cstring

Get the name of the TPSA.

### Input
- `t` -- Real TPSA

### Output
- `ret`  -- Name of RTPSA (nul term in C)
"""
function mad_tpsa_nam(t::Ptr{RTPSA{Desc}})::Cstring
  ret = @ccall MAD_TPSA.mad_tpsa_nam(t::Ptr{RTPSA{Desc}})::Cstring
  return ret
end


"""
    mad_tpsa_ord(t::Ptr{RTPSA{Desc}})::Cuchar

Gets the TPSA order.

### Input
- `t` -- Real TPSA

### Output
- `ret`  -- Order of TPSA
"""
function mad_tpsa_ord(t::Ptr{RTPSA{Desc}})::Cuchar
  ret = @ccall MAD_TPSA.mad_tpsa_ord(t::Ptr{RTPSA{Desc}})::Cuchar
  return ret
end

"""
  mad_tpsa_ordv(t::Ptr{RTPSA{Desc}}, ts::Vector{Ptr{RTPSA{Desc}}}...)::Cuchar

???

### Input
- `t` -- TPSA
- `ts`

### Output
- `mo` -- Order
"""
#function mad_tpsa_ordv(t::Ptr{RTPSA{Desc}}, ts::Vector{Ptr{RTPSA{Desc}}}...)::Cuchar
#  mo = @ccall MAD_TPSA.mad_tpsa_ordv(t::Ptr{RTPSA{Desc}}, ts::Vector{Ptr{RTPSA{Desc}}}..., 0::Cint)::Cuchar # null pointer after args for safe use
#  return mo
#end


"""
    mad_tpsa_ordn(n::Cint, t::Ptr{Ptr{RTPSA{Desc}}})::Cuchar

Gets the max order of all TPSAs in t.

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

Makes a copy of the real TPSA t to r.

### Input
- `t` -- Source real TPSA
- `r` -- Destination real TPSA
"""
function mad_tpsa_copy!(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_copy(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_sclord!(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, inv::Cuchar)

???

### Input
- `t`  -- Source real TPSA
- `r`  -- Destination real TPSA
- `inv`-- scl by inverse
"""
function mad_tpsa_sclord!(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, inv::Cuchar)
  @ccall MAD_TPSA.mad_tpsa_sclord(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, inv::Cuchar)::Cvoid
end


"""
    mad_tpsa_getord!(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, ord::Cuchar)

???
is ! ?

### Input
- `t``  -- Source real TPSA
- `r`   -- Destination real TPSA
- `ord` -- Order to retrieve
"""
function mad_tpsa_getord!(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, ord::Cuchar)
  @ccall MAD_TPSA.mad_tpsa_getord(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, ord::Cuchar)::Cvoid
end


"""
    mad_tpsa_cutord!(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, ord::Cint)

???

### Input
- `t`   -- Source real TPSA
- `r`   -- Destination real TPSA
- `ord` -- Cut order: 0..-ord or ord..mo
"""
function mad_tpsa_cutord!(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, ord::Cint)
  @ccall MAD_TPSA.mad_tpsa_cutord(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, ord::Cint)::Cvoid
end

"""
  mad_tpsa_maxord(t::Ptr{RTPSA{Desc}}, n::Cint, idx_::Ptr{Cint})::Cint

???

### Input
- `t`    -- Real TPSA
- `n`    -- Length of idx_
- `idx_`

### Output
- `mi` -- ?
"""
function mad_tpsa_maxord(t::Ptr{RTPSA{Desc}}, n::Cint, idx_::Ptr{Cint})::Cint
  mi = @ccall MAD_TPSA.mad_tpsa_maxord(t::Ptr{RTPSA{Desc}}, n::Cint, idx_::Ptr{Cint})::Cint
  return mi
end

"""
    mad_tpsa_convert!(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)

???

### Input
- `t`    -- Source real TPSA
- `r`    -- Destination real TPSA
- `n`    -- Length of vector
- `t2r_` -- Vector of index lookup
- `pb`   -- Poisson bracket, 0,1:fwd,-1:bwd
"""
function mad_tpsa_convert!(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)
  @ccall MAD_TPSA.mad_tpsa_convert(t::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)::Cvoid
end


"""
    mad_tpsa_setvar!(t::Ptr{RTPSA{Desc}}, v::Cdouble, iv_::Cint, scl_::Cdouble)

???

### Input
- `t`    -- Real TPSA
- `v`    -- 0th order value
- `iv_`  -- Variable index
- `scl_` -- 1st order variable value
"""
function mad_tpsa_setvar!(t::Ptr{RTPSA{Desc}}, v::Cdouble, iv_::Cint, scl_::Cdouble)
  @ccall MAD_TPSA.mad_tpsa_setvar(t::Ptr{RTPSA{Desc}}, v::Cdouble, iv_::Cint, scl_::Cdouble)::Cvoid
end


"""
    mad_tpsa_setnam!(t::Ptr{RTPSA{Desc}}, nam::Cstring)

Sets the name of the RTPSA.

### Input
- `t`   -- Real TPSA
- `nam` -- Name to set for RTPSA
"""
function mad_tpsa_setnam!(t::Ptr{RTPSA{Desc}}, nam::Cstring)
  @ccall MAD_TPSA.mad_tpsa_setnam(t::Ptr{RTPSA{Desc}}, nam::Cstring)::Cvoid
end


"""
    mad_tpsa_clear!(t::Ptr{RTPSA{Desc}})

Clears the TPSA (reset to 0)

### Input
- `t` -- Real TPSA
"""
function mad_tpsa_clear!(t::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_clear(t::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_isnul(t::Ptr{RTPSA{Desc}})::Cuchar

??? checks if null c2i?

### Input
- `t` -- Real TPSA to check

### Output
- `ret`  -- True or false
"""
function mad_tpsa_isnul(t::Ptr{RTPSA{Desc}})::Cuchar
  ret = @ccall MAD_TPSA.mad_tpsa_isnul(t::Ptr{RTPSA{Desc}})::Cuchar
  return ret
end


"""
    mad_tpsa_mono(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar})::Cuchar

### Input
- `t`
- `i`
- `n`
- `m_`

### Output
- `ret`
"""
function mad_tpsa_mono(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_tpsa_mono(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar})::Cuchar
  return ret
end


"""
    mad_tpsa_idxs(t::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring)::Cint

### Input
- `t`
- `n`
- `s`

### Output
- `ret`
"""
function mad_tpsa_idxs(t::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring)::Cint
  ret = @ccall MAD_TPSA.mad_tpsa_idxs(t::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring)::Cint
  return ret
end



"""
    mad_tpsa_idxm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint


???

### Input
- `t`
- `n`
- `m`

### Output
- `ret`
"""
function mad_tpsa_idxm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint
  ret = @ccall MAD_TPSA.mad_tpsa_idxm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint
  return ret
end


"""
    mad_tpsa_idxsm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cint})::Cint

???

### Input
- `t`
- `n`
- `m`

### Output
- `ret`
"""
function mad_tpsa_idxsm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cint})::Cint
  ret = @ccall MAD_TPSA.mad_tpsa_idxsm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cint})::Cint
  return ret
end


"""
    mad_tpsa_cycle(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, v_::Ptr{Cdouble})::Cint

???

### Input
- `t`
- `i`
- `n`
- `m_`
- `v_`

### Output
- `i`
"""
function mad_tpsa_cycle(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, v_::Ptr{Cdouble})::Cint
  i = @ccall MAD_TPSA.mad_tpsa_cycle(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, v_::Ptr{Cdouble})::Cint
  return i
end


"""
    mad_tpsa_get0(t::Ptr{RTPSA{Desc}})::Cdouble

???

### Input
- `t`

### Output
- `ret`
"""
function mad_tpsa_get0(t::Ptr{RTPSA{Desc}})::Cdouble
  ret = @ccall MAD_TPSA.mad_tpsa_get0(t::Ptr{RTPSA{Desc}})::Cdouble
  return ret
end


"""
    mad_tpsa_geti(t::Ptr{RTPSA{Desc}}, i::Cint)::Cdouble

???

### Input
- `t`
- `i`

### Output
- `ret`
"""
function mad_tpsa_geti(t::Ptr{RTPSA{Desc}}, i::Cint)::Cdouble
  ret = @ccall MAD_TPSA.mad_tpsa_geti(t::Ptr{RTPSA{Desc}}, i::Cint)::Cdouble
  return ret
end


"""
    mad_tpsa_gets(t::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring)::Cdouble

???

### Input
- `t`
- `n`
- `s`

### Output
- `ret`
"""
function mad_tpsa_gets(t::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring)::Cdouble
  ret = @ccall MAD_TPSA.mad_tpsa_gets(t::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring)::Cdouble
  return ret
end


"""
    mad_tpsa_getm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cdouble

???

### Input
- `t`
- `n`
- `m`

### Output
- `ret`
"""
function mad_tpsa_getm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cdouble
  val = @ccall MAD_TPSA.mad_tpsa_getm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cdouble
  return ret
end


"""
    mad_tpsa_getsm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cint})::Cdouble

???

### Input
- `t`
- `n`
- `m`

### Output
- `ret`
"""
function mad_tpsa_getsm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cint})::Cdouble
  ret = @ccall MAD_TPSA.mad_tpsa_getsm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cint})::Cdouble
  return ret
end


"""
    mad_tpsa_set0!(t::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble)

???

### Input
- `t`
- `a`
- `b`
"""
function mad_tpsa_set0!(t::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble)
  @ccall MAD_TPSA.mad_tpsa_set0(t::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble)::Cvoid
end


"""
    mad_tpsa_seti!(t::Ptr{RTPSA{Desc}}, i::Cint, a::Cdouble, b::Cdouble)

???

### Input
- `t`
- `i`
- `a`
- `b`
"""
function mad_tpsa_seti!(t::Ptr{RTPSA{Desc}}, i::Cint, a::Cdouble, b::Cdouble)
  @ccall MAD_TPSA.mad_tpsa_seti(t::Ptr{RTPSA{Desc}}, i::Cint, a::Cdouble, b::Cdouble)::Cvoid
end


"""
    mad_tpsa_sets!(t::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring, a::Cdouble, b::Cdouble)

???

### Input
- `t`
- `n`
- `s`
- `a`
- `b`
"""
function mad_tpsa_sets!(t::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring, a::Cdouble, b::Cdouble)
  @ccall MAD_TPSA.mad_tpsa_sets(t::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring, a::Cdouble, b::Cdouble)::Cvoid
end


"""
    mad_tpsa_setm!(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::Cdouble, b::Cdouble)

???

### Input
- `t`
- `n`
- `m`
- `a`
- `b`
"""
function mad_tpsa_setm!(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::Cdouble, b::Cdouble)
  @ccall MAD_TPSA.mad_tpsa_setm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::Cdouble, b::Cdouble)::Cvoid
end


"""
    mad_tpsa_setsm!(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cint}, a::Cdouble, b::Cdouble)

???

### Input
- `t`
- `n`
- `m`
- `a`
- `b`
"""
function mad_tpsa_setsm!(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cint}, a::Cdouble, b::Cdouble)
  @ccall MAD_TPSA.mad_tpsa_setsm(t::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cint}, a::Cdouble, b::Cdouble)::Cvoid
end


"""
    mad_tpsa_getv!(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{Cdouble})

???

### Input
- `t`
- `i`
- `n`
- `v`
"""
function mad_tpsa_getv!(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{Cdouble})
  @ccall MAD_TPSA.mad_tpsa_getv(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{Cdouble})::Cvoid
end



"""
    mad_tpsa_setv!(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{Cdouble})

???

### Input
- `t`
- `i`
- `n`
- `v`
"""
function mad_tpsa_setv!(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{Cdouble})
  @ccall MAD_TPSA.mad_tpsa_setv(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{Cdouble})::Cvoid
end


"""
    mad_tpsa_equ(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, tol_::Cdouble)::Cuchar

Checks if the TPSAs a and b are equal within the specified tolerance tol_

### Input
- `a`    -- TPSA a
- `b`    -- TPSA b
- `tol_` -- difference below which the TPSAs are considered equal

### Output
- `ret`   - True if a == b within tol_
"""
function mad_tpsa_equ(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, tol_::Cdouble)::Cuchar
  ret = @ccall MAD_TPSA.mad_tpsa_equ(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, tol_::Cdouble)::Cuchar
  return ret
end


"""
    mad_tpsa_dif!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

???  // (a_i-b_i)/max(|a_i|,1)

### Input
- `a` -- Source TPSA a
- `b` -- Source TPSA b
- `c` -- Destination TPSA c 
"""
function mad_tpsa_dif!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_dif(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_add!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets the destination TPSA c = a + b

### Input
- `a` -- Source TPSA a
- `b` -- Source TPSA b
- `c` -- Destination TPSA c = a + b
"""
function mad_tpsa_add!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_add(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_sub!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets the destination TPSA c = a - b

### Input
- `a` -- Source TPSA a
- `b` -- Source TPSA b
- `c` -- Destination TPSA c = a - b
"""
function mad_tpsa_sub!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sub(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_mul!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets the destination TPSA c = a * b

### Input
- `a` -- Source TPSA a
- `b` -- Source TPSA b
- `c` -- Destination TPSA c = a * b
"""
function mad_tpsa_mul!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_mul(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_div!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets the destination TPSA c = a / b

### Input
- `a` -- Source TPSA a
- `b` -- Source TPSA b
- `c` -- Destination TPSA c = a / b
"""
function mad_tpsa_div!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_div(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_pow!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets the destination TPSA c = a ^ b

### Input
- `a` -- Source TPSA a
- `b` -- Source TPSA b
- `c` -- Destination TPSA c = a ^ b
"""
function mad_tpsa_pow!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_pow(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_powi!(a::Ptr{RTPSA{Desc}}, n::Cint, c::Ptr{RTPSA{Desc}})

Sets the destination TPSA c = a ^ n where n is an integer.

### Input
- `a` -- Source TPSA a
- `n` -- Integer power
- `c` -- Destination TPSA c = a ^ n
"""
function mad_tpsa_powi!(a::Ptr{RTPSA{Desc}}, n::Cint, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_powi(a::Ptr{RTPSA{Desc}}, n::Cint, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_pown!(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})

Sets the destination TPSA c = a ^ v where v is of double precision.

### Input
- `a` -- Source TPSA a
- `v` -- "double" precision power
- `c` -- Destination TPSA c = a ^ v
"""
function mad_tpsa_pown!(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_pown(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_nrm(a::Ptr{RTPSA{Desc}})::Cdouble

Calculates the norm of (which???) of TPSA a.

### Input
- `a`   -- TPSA

### Output
- `nrm` -- Norm of TPSA a
"""
function mad_tpsa_nrm(a::Ptr{RTPSA{Desc}})::Cdouble
  nrm = @ccall MAD_TPSA.mad_tpsa_nrm(a::Ptr{RTPSA{Desc}}, tpsa_b_::Ptr{RTPSA{Desc}})::Cdouble
  return nrm
end


"""
    mad_tpsa_abs!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the absolute value of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = |a|
"""
function mad_tpsa_abs!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_abs(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_sqrt!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the sqrt of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = sqrt(a)
"""
function mad_tpsa_sqrt!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sqrt(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_exp!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the exponential of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = exp(a)
"""
function mad_tpsa_exp!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_exp(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end



"""
    mad_tpsa_log!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the log of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = log(a)
"""
function mad_tpsa_log!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_log(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_sincos!(a::Ptr{RTPSA{Desc}}, s::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA s = sin(a) and TPSA c = cos(a)

### Input
- `a` -- Source TPSA a
- `s` -- Destination TPSA s = sin(a)
- `c` -- Destination TPSA c = cos(a)
"""
function mad_tpsa_sincos!(a::Ptr{RTPSA{Desc}}, s::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sincos(a::Ptr{RTPSA{Desc}}, s::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_sin!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the sin of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = sin(a)
"""
function mad_tpsa_sin!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sin(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_cos!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the cos of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = cos(a)
"""
function mad_tpsa_cos!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_cos(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_tan!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the tan of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = tan(a)
"""
function mad_tpsa_tan!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_tan(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_cot!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the cot of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = cot(a)
"""
function mad_tpsa_cot!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_cot(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_sinc!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the sinc of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = sinc(a)
"""
function mad_tpsa_sinc!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sinc(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_sincosh!(a::Ptr{RTPSA{Desc}}, s::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA s = sinh(a) and TPSA c = cosh(a)

### Input
- `a` -- Source TPSA a
- `s` -- Destination TPSA s = sinh(a)
- `c` -- Destination TPSA c = cosh(a)
"""
function mad_tpsa_sincosh!(a::Ptr{RTPSA{Desc}}, s::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sincosh(a::Ptr{RTPSA{Desc}}, s::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_sinh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

  Sets TPSA c to the sinh of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = sinh(a)
"""
function mad_tpsa_sinh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sinh(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_cosh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the cosh of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = cosh(a)
"""
function mad_tpsa_cosh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_cosh(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_tanh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the tanh of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = tanh(a)
"""
function mad_tpsa_tanh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_tanh(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_coth!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the coth of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = coth(a)
"""
function mad_tpsa_coth!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_coth(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_sinhc!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the sinhc of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = sinhc(a)
"""
function mad_tpsa_sinhc!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sinhc(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_asin!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the asin of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = asin(a)
"""
function mad_tpsa_asin!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_asin(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_acos!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the acos of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = acos(a)
"""
function mad_tpsa_acos!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_acos(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_atan!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the atan of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = atan(a)
"""
function mad_tpsa_atan!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_atan(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_acot!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the acot of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = acot(a)
"""
function mad_tpsa_acot!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_acot(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end

"""
    mad_tpsa_asinc!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the asinc of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = asinc(a)
"""
function mad_tpsa_asinc!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_asinc(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_asinh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the asinh of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = asinh(a)
"""
function mad_tpsa_asinh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_asinh(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_acosh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the acosh of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = acosh(a)
"""
function mad_tpsa_acosh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_acosh(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_atanh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the atanh of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = atanh(a)
"""
function mad_tpsa_atanh!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_atanh(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_acoth!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the acoth of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = acoth(a)
"""
function mad_tpsa_acoth!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_acoth(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_asinhc!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the asinhc of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = asinhc(a)
"""
function mad_tpsa_asinhc!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_asinhc(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_erf!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the erf of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = erf(a)
"""
function mad_tpsa_erf!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_erf(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_erfc!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

Sets TPSA c to the erfc of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = erfc(a)
"""
function mad_tpsa_erfc!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_erfc(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_acc!(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})

Adds a*v to TPSA c. Aliasing OK.

### Input
- `a` -- Source TPSA a
- `v` -- Scalar with double precision
- `c` -- Destination TPSA c += v*a
"""
function mad_tpsa_acc!(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_acc(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_scl!(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})

Sets TPSA c to v*a. 

### Input
- `a` -- Source TPSA a
- `v` -- Scalar with double precision
- `c` -- Destination TPSA c = v*a
"""
function mad_tpsa_scl!(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_scl(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_inv!(a::Ptr{RTPSA{Desc}},  v::Cdouble, c::Ptr{RTPSA{Desc}})

Sets TPSA c to v/a. 

### Input
- `a` -- Source TPSA a
- `v` -- Scalar with double precision
- `c` -- Destination TPSA c = v*a
"""
function mad_tpsa_inv!(a::Ptr{RTPSA{Desc}},  v::Cdouble, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_inv(a::Ptr{RTPSA{Desc}},  v::Cdouble, c::Ptr{RTPSA{Desc}})::Cvoid
end

"""
    mad_tpsa_invsqrt!(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})

Sets TPSA c to v/sqrt(a). 

### Input
- `a` -- Source TPSA a
- `v` -- Scalar with double precision
- `c` -- Destination TPSA c = v*a
"""
function mad_tpsa_invsqrt!(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_invsqrt(a::Ptr{RTPSA{Desc}}, v::Cdouble, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_unit!(x::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

???

### Input
- `x` -- Source TPSA x
- `r` -- Destination TPSA r
"""
function  mad_tpsa_unit!(x::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_unit(x::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_atan2!(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

Sets TPSA r to atan2(x,y)

### Input
- `x` -- Source TPSA x
- `y` -- Source TPSA y
- `r` -- Destination TPSA r = atan2(x,y)
"""
function  mad_tpsa_atan2!(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_atan2(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end

"""
    mad_tpsa_hypot!(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

Sets TPSA r to sqrt(x^2+y^2)

### Input
- `x` -- Source TPSA x
- `y` -- Source TPSA y
- `r` -- Destination TPSA r = sqrt(x^2+y^2)
"""
function  mad_tpsa_hypot!(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_hypot(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end

"""
    mad_tpsa_hypot3!(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, z::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

Sets TPSA r to sqrt(x^2+y^2+z^2)

### Input
- `x` -- Source TPSA x
- `y` -- Source TPSA y
- `z` -- Source TPSA z
- `r` -- Destination TPSA r = sqrt(x^2+y^2+z^2)
"""
function  mad_tpsa_hypot3!(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, z::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_hypot3(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, z::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end



"""
    mad_tpsa_integ!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, iv::Cint)

??? Integrates TPSA

### Input
- `a`  -- Source TPSA to integrate
- `c`  -- Destination TPSA
- `iv` -- Domain
"""
function mad_tpsa_integ!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, iv::Cint)
  @ccall MAD_TPSA.mad_tpsa_integ(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, iv::Cint)::Cvoid
end


"""
    mad_tpsa_deriv!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, iv::Cint)

??? Differentiates TPSA

### Input
- `a`  -- Source TPSA to differentiate
- `c`  -- Destination TPSA
- `iv` -- Domain
"""
function mad_tpsa_deriv!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, iv::Cint)
  @ccall MAD_TPSA.mad_tpsa_deriv(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, iv::Cint)::Cvoid
end


"""
    mad_tpsa_derivm!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})

???

### Input
- `a`
- `c`
- `n`
- `m`
"""
function mad_tpsa_derivm!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})
  @ccall MAD_TPSA.mad_tpsa_derivm(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cvoid
end


"""
    mad_tpsa_poisbra!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, nv::Cint)

Sets TPSA c to the poisson bracket of TPSAs a and b.

### Input
- `a`  -- Source TPSA a
- `b`  -- Source TPSA b
- `c`  -- Destination TPSA c = [a, b]
- `nv` -- Number of variables in the TPSA
"""
function mad_tpsa_poisbra!(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, nv::Cint)
  @ccall MAD_TPSA.mad_tpsa_poisbra(a::Ptr{RTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, nv::Cint)::Cvoid
end


"""
    mad_tpsa_taylor!(a::Ptr{RTPSA{Desc}}, n::Cint, coef::Ptr{Cdouble}, c::Ptr{RTPSA{Desc}})

???

### Input
- `a`
- `n`
- `coef`
- `c`
"""
function mad_tpsa_taylor!(a::Ptr{RTPSA{Desc}}, n::Cint, coef::Ptr{Cdouble}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_taylor(a::Ptr{RTPSA{Desc}}, n::Cint, coef::Ptr{Cdouble}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_axpb!(a::Cdouble, x::Ptr{RTPSA{Desc}}, b::Cdouble, r::Ptr{RTPSA{Desc}})

??? r = a*x/b?

### Input
- `a`
- `x`
- `b`
- `r`
"""
function mad_tpsa_axpb!(a::Cdouble, x::Ptr{RTPSA{Desc}}, b::Cdouble, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_axpb(a::Cdouble, x::Ptr{RTPSA{Desc}}, b::Cdouble, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_axpbypc!(a::Cdouble, x::Ptr{RTPSA{Desc}}, b::Cdouble, y::Ptr{RTPSA{Desc}}, c::Cdouble, r::Ptr{RTPSA{Desc}})

### Input
- `a`
- `x`
- `b`
- `y`
- `c`
- `r`
"""
function mad_tpsa_axpbypc!(a::Cdouble, x::Ptr{RTPSA{Desc}}, b::Cdouble, y::Ptr{RTPSA{Desc}}, c::Cdouble, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_axpbypc(a::Cdouble, x::Ptr{RTPSA{Desc}}, b::Cdouble, y::Ptr{RTPSA{Desc}}, c::Cdouble, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_axypb!(a::Cdouble, x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, b::Cdouble, r::Ptr{RTPSA{Desc}})

???

### Input
- `a`
- `x`
- `y`
- `b`
- `r`
"""
function mad_tpsa_axypb!(a::Cdouble, x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, b::Cdouble, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_axypb(a::Cdouble, x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, b::Cdouble, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_axypbzpc!(a::Cdouble, x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, b::Cdouble, z::Ptr{RTPSA{Desc}}, c::Cdouble, r::Ptr{RTPSA{Desc}})

???

### Input
- `a`
- `x`
- `y`
- `b`
- `z`
- `c`
- `r`
"""
function mad_tpsa_axypbzpc!(a::Cdouble, x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, b::Cdouble, z::Ptr{RTPSA{Desc}}, c::Cdouble, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_axypbzpc(a::Cdouble, x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, b::Cdouble, z::Ptr{RTPSA{Desc}}, c::Cdouble, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_axypbvwpc!(a::Cdouble, x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, b::Cdouble, v::Ptr{RTPSA{Desc}}, w::Ptr{RTPSA{Desc}}, c::Cdouble, r::Ptr{RTPSA{Desc}})

???

### Input
- `a`
- `x`
- `y`
- `b`
- `v`
- `w`
- `c`
- `r`
"""
function mad_tpsa_axypbvwpc!(a::Cdouble, x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, b::Cdouble, v::Ptr{RTPSA{Desc}}, w::Ptr{RTPSA{Desc}}, c::Cdouble, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_axypbvwpc(a::Cdouble, x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, b::Cdouble, v::Ptr{RTPSA{Desc}}, w::Ptr{RTPSA{Desc}}, c::Cdouble, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_ax2pby2pcz2!(a::Cdouble, x::Ptr{RTPSA{Desc}}, b::Cdouble, y::Ptr{RTPSA{Desc}}, c::Cdouble, z::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

???

### Input
- `a`
- `x`
- `b`
- `y`
- `c`
- `z`
- `r`
"""
function mad_tpsa_ax2pby2pcz2!(a::Cdouble, x::Ptr{RTPSA{Desc}}, b::Cdouble, y::Ptr{RTPSA{Desc}}, c::Cdouble, z::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_ax2pby2pcz2(a::Cdouble, x::Ptr{RTPSA{Desc}}, b::Cdouble, y::Ptr{RTPSA{Desc}}, c::Cdouble, z::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_axpsqrtbpcx2!(x::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Cdouble, r::Ptr{RTPSA{Desc}})

???

### Input
- `x`
- `a`
- `b`
- `c`
- `r`
"""
function mad_tpsa_axpsqrtbpcx2!(x::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Cdouble, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_axpsqrtbpcx2(x::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Cdouble, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_logaxpsqrtbpcx2!(x::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Cdouble, r::Ptr{RTPSA{Desc}})

???

### Input
- `x`
- `a`
- `b`
- `c`
- `r`
"""
function mad_tpsa_logaxpsqrtbpcx2!(x::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Cdouble, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_logaxpsqrtbpcx2(x::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Cdouble, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_logxdy!(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

???

### Input
- `x`
- `y`
- `r`
"""
function mad_tpsa_logxdy!(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_logxdy(x::Ptr{RTPSA{Desc}}, y::Ptr{RTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_vec2fld!(na::Cint, a::Ptr{RTPSA{Desc}}, mc::Ptr{Ptr{RTPSA{Desc}}})

???

### Input
- `na`
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

???

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

???

### Input
- `na`
- `ma`
- `mb`
- `mc`
"""
function mad_tpsa_liebra!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})
  @ccall MAD_TPSA.mad_tpsa_liebra(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})::Cvoid
end


"""
    mad_tpsa_exppb!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})

???

### Input
- `na`
- `ma`
- `mb`
- `mc`
"""
function mad_tpsa_exppb!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})
  @ccall MAD_TPSA.mad_tpsa_exppb(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})::Cvoid
end


"""
    mad_tpsa_logpb!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})

???

### Input
- `na`
- `ma`
- `mb`
- `mc`
"""
function mad_tpsa_logpb!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})
  @ccall MAD_TPSA.mad_tpsa_logpb(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})::Cvoid
end



"""
    mad_tpsa_mnrm(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}})::Cdouble

???

### Input
- `na`
- `ma`

### Output
- `nrm`
"""
function mad_tpsa_mnrm(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}})::Cdouble
  nrm = @ccall MAD_TPSA.mad_tpsa_mnrm(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}})::Cdouble
  return nrm
end


"""
    mad_tpsa_minv!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})

???

### Input
- `na`
- `ma`
- `mc`
"""
function mad_tpsa_minv!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})
  @ccall MAD_TPSA.mad_tpsa_minv(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})::Cvoid
end


"""
    mad_tpsa_pminv!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}}, select::Ptr{Cint})

???

### Input
- `na`
- `ma`
- `mc`
- `select`
"""
function mad_tpsa_pminv!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}}, select::Ptr{Cint})
  @ccall MAD_TPSA.mad_tpsa_pminv(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}}, select::Ptr{Cint})::Cvoid
end


"""
    mad_tpsa_compose!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nb::Cint, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})

???

### Input
- `na`
- `ma`
- `nb`
- `mb`
- `mc`
"""
function mad_tpsa_compose!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nb::Cint, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})
  @ccall MAD_TPSA.mad_tpsa_compose(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nb::Cint, mb::Ptr{Ptr{RTPSA{Desc}}}, mc::Ptr{Ptr{RTPSA{Desc}}})::Cvoid
end


"""
    mad_tpsa_translate!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nb::Cint, tb::Ptr{Cdouble}, mc::Ptr{RTPSA{Desc}})

???

### Input
- `na`
- `ma`
- `nb`
- `tb`
- `mc`
"""
function mad_tpsa_translate!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nb::Cint, tb::Ptr{Cdouble}, mc::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_translate(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nb::Cint, tb::Ptr{Cdouble}, mc::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_eval!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nb::Cint, tb::Ptr{Cdouble}, tc::Ptr{Cdouble})

???

### Input
- `na`
- `ma`
- `nb`
- `tb`
- `tc`
"""
function mad_tpsa_eval!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nb::Cint, tb::Ptr{Cdouble}, tc::Ptr{Cdouble})
  @ccall MAD_TPSA.mad_tpsa_eval(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nb::Cint, tb::Ptr{Cdouble}, tc::Ptr{Cdouble})::Cvoid
end


"""
    mad_tpsa_mconv!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nc::Cint, mc::Ptr{Ptr{RTPSA{Desc}}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)

???

### Input
- `na`
- `ma`
- `nc`
- `mc`
- `n`
- `t2r_`
- `pb`
"""
function mad_tpsa_mconv!(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nc::Cint, mc::Ptr{Ptr{RTPSA{Desc}}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)
  @ccall MAD_TPSA.mad_tpsa_mconv(na::Cint, ma::Ptr{Ptr{RTPSA{Desc}}}, nc::Cint, mc::Ptr{Ptr{RTPSA{Desc}}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)::Cvoid
end


"""
    mad_tpsa_print(t::Ptr{RTPSA{Desc}}, name_::Cstring, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid})

Prints the TPSA coefficients to stdout with precision eps_. If nohdr_ is not zero, 
the header is not printed. 

### Input
- `t`       -- TPSA to print
- `name_`   -- Name of TPSA
- `eps_`    -- Precision to output
- `nohdr_`  -- If True, no header is printed
- `stream_` --  FILE pointer of output stream. If null, printed to stdout
"""
function mad_tpsa_print(t::Ptr{RTPSA{Desc}}, name_::Cstring, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_tpsa_print(t::Ptr{RTPSA{Desc}}, name_::Cstring, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid})::Cvoid
end


"""
    mad_tpsa_scan(stream_::Ptr{Cvoid})::Ptr{RTPSA{Desc}}

Scans in a TPSA from the stream_.

### Input
- `stream_` -- C FILE pointer I/O stream from which to read the TPSA

### Output
- `t`    -- TPSA scanned from I/O stream_
"""
function mad_tpsa_scan(stream_::Ptr{Cvoid})::Ptr{RTPSA{Desc}}
  t = @ccall MAD_TPSA.mad_tpsa_scan(stream_::Ptr{Cvoid})::Ptr{RTPSA{Desc}}
  return t
end


"""
    mad_tpsa_scan_hdr(kind_::Cint, name_::Cstring, stream_::Ptr{Cvoid})::Ptr{Desc{RTPSA,CTPSA}}

???

### Input
- `kind_`
- `name_`
- `stream_`

### Output
- `ret`
"""
function mad_tpsa_scan_hdr(kind_::Cint, name_::Cstring, stream_::Ptr{Cvoid})::Ptr{Desc{RTPSA,CTPSA}}
  desc = @ccall MAD_TPSA.mad_tpsa_scan_hdr(kind_::Cint, name_::Cstring, stream_::Ptr{Cvoid})::Ptr{Desc{RTPSA,CTPSA}}
  return ret
end


"""
    mad_tpsa_scan_coef!(t::Ptr{RTPSA{Desc}}, stream_::Ptr{Cvoid})

???

### Input
- `t`
- `stream_`
"""
function mad_tpsa_scan_coef!(t::Ptr{RTPSA{Desc}}, stream_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_tpsa_scan_coef(t::Ptr{RTPSA{Desc}}, stream_::Ptr{Cvoid})::Cvoid
end


"""
    mad_tpsa_debug(t::Ptr{RTPSA{Desc}}, name_::Cstring, fnam_::Cstring, line_::Cint, stream_::Ptr{Cvoid})

???

### Input
- `t`
- `name_`
- `fnam_`
- `line_`
- `stream_`
"""
function mad_tpsa_debug(t::Ptr{RTPSA{Desc}}, name_::Cstring, fnam_::Cstring, line_::Cint, stream_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_tpsa_debug(t::Ptr{RTPSA{Desc}}, name_::Cstring, fnam_::Cstring, line_::Cint, stream_::Ptr{Cvoid})::Cvoid
end

"""
    mad_tpsa_isvalid(t::Ptr{RTPSA{Desc}})::Cuchar

Sanity check of the TPSA integrity.

### Input
- `t` -- Real TPSA to check if valid

### Output
- `ret`  -- True if valid TPSA, false otherwise
"""
function mad_tpsa_isvalid(t::Ptr{RTPSA{Desc}})::Cuchar
  ret = @ccall MAD_TPSA.mad_tpsa_isvalid(t::Ptr{RTPSA{Desc}})::Cuchar
  return ret
end


"""
    mad_tpsa_init(t::Ptr{RTPSA{Desc}}, d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{RTPSA{Desc}}

UNSAFE OPERATION! (mo vs allocated!!) ???

### Input
- `t` 
- `d`
- `mo`

### Output
- `t`  
"""
function mad_tpsa_init!(t::Ptr{RTPSA{Desc}}, d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
  t = @ccall MAD_TPSA.mad_tpsa_init(t::Ptr{RTPSA{Desc}}, d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
  return t
end


end
