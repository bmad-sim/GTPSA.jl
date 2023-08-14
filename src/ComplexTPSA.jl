module ComplexTPSA
include("Structs.jl")
using .Structs


"""
    mad_ctpsa_newd(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{CTPSA{Desc}}

Creates a complex TPSA defined by the specified descriptor and maximum order. If mad_ctpsa_DEFAULT 
is passed for mo, the mo defined in the descriptor is used. If mo > d_mo, mo = d_mo.

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
If mad_ctpsa_SAME is passed for mo, the mo currently in t is used for the created TPSA.
  ok with t=(tpsa_t*)ctpsa

### Input
- `t`   -- Complex TPSA to copy
- `mo`  -- Maximum order of new TPSA

### Output
- `ret` -- New complex TPSA with maximum order mo
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

Sets the TPSA uid if uid_ != 0, and returns the current (previous if set) TPSA uid. 

### Input
- `t`    -- Complex TPSA
- `uid_` -- uid to set in the TPSA if uid_ != 0

### Output
- `ret`  -- Current (previous if set) TPSA uid
"""
function mad_ctpsa_uid!(t::Ptr{CTPSA{Desc}}, uid_::Cint)::Cint
  ret = @ccall MAD_TPSA.mad_ctpsa_uid(t::Ptr{CTPSA{Desc}}, uid_::Cint)::Cint
  return ret
end


"""
    mad_ctpsa_len(t::Ptr{CTPSA{Desc}})::Cint

???

### Input
- `t`   -- Complex TPSA

### Output
- `ret` -- Monomials in CTPSA
"""
function mad_ctpsa_len(t::Ptr{CTPSA{Desc}})::Cint
  ret = @ccall MAD_TPSA.mad_ctpsa_len(t::Ptr{CTPSA{Desc}})::Cint
  return ret
end


"""
    mad_ctpsa_nam(t::Ptr{CTPSA{Desc}})::Cstring

Get the name of the TPSA.

### Input
- `t` -- Complex TPSA

### Output
- `ret`  -- Name of CTPSA (nul term in C)
"""
function mad_ctpsa_nam(t::Ptr{CTPSA{Desc}})::Cstring
  ret = @ccall MAD_TPSA.mad_ctpsa_nam(t::Ptr{CTPSA{Desc}})::Cstring
  return ret
end


"""
    mad_ctpsa_ord(t::Ptr{CTPSA{Desc}})::Cuchar

Gets the TPSA order.

### Input
- `t` -- Complex TPSA

### Output
- `ret`  -- Order of TPSA
"""
function mad_ctpsa_ord(t::Ptr{CTPSA{Desc}})::Cuchar
  ret = @ccall MAD_TPSA.mad_ctpsa_ord(t::Ptr{CTPSA{Desc}})::Cuchar
  return ret
end

"""
  mad_ctpsa_ordv(t::Ptr{CTPSA{Desc}}, ts::Ptr{CTPSA{Desc}}...)::Cuchar

???

### Input
- `t` -- TPSA
- `ts`

### Output
- `mo` -- Order
"""
#function mad_ctpsa_ordv(t::Ptr{CTPSA{Desc}}, ts::Ptr{CTPSA{Desc}}...)::Cuchar
#  mo = @ccall MAD_TPSA.mad_ctpsa_ordv(t::Ptr{CTPSA{Desc}}, ts::Ptr{CTPSA{Desc}}..., 0::Cint)::Cuchar # null pointer after args for safe use
#  return mo
#end


"""
    mad_ctpsa_ordn(n::Cint, t::Ptr{Ptr{CTPSA{Desc}}})::Cuchar

Gets the max order of all TPSAs in t.

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

Makes a copy of the complex TPSA t to r.

### Input
- `t` -- Source complex TPSA
- `r` -- Destination complex TPSA
"""
function mad_ctpsa_copy!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_copy(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_sclord!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, inv::Cuchar)

???

### Input
- `t`  -- Source complex TPSA
- `r`  -- Destination complex TPSA
- `inv`-- scl by inverse
"""
function mad_ctpsa_sclord!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, inv::Cuchar)
  @ccall MAD_TPSA.mad_ctpsa_sclord(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, inv::Cuchar)::Cvoid
end


"""
    mad_ctpsa_getord!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, ord::Cuchar)

???
is ! ?

### Input
- `t``  -- Source complex TPSA
- `r`   -- Destination complex TPSA
- `ord` -- Order to retrieve
"""
function mad_ctpsa_getord!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, ord::Cuchar)
  @ccall MAD_TPSA.mad_ctpsa_getord(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, ord::Cuchar)::Cvoid
end


"""
    mad_ctpsa_cutord!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, ord::Cint)

???

### Input
- `t`   -- Source complex TPSA
- `r`   -- Destination complex TPSA
- `ord` -- Cut order: 0..-ord or ord..mo
"""
function mad_ctpsa_cutord!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, ord::Cint)
  @ccall MAD_TPSA.mad_ctpsa_cutord(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, ord::Cint)::Cvoid
end

"""
  mad_ctpsa_maxord(t::Ptr{CTPSA{Desc}}, n::Cint, idx_::Ptr{Cint})::Cint

???

### Input
- `t`    -- Complex TPSA
- `n`    -- Length of idx_
- `idx_`

### Output
- `mi` -- ?
"""
function mad_ctpsa_maxord(t::Ptr{CTPSA{Desc}}, n::Cint, idx_::Ptr{Cint})::Cint
  mi = @ccall MAD_TPSA.mad_ctpsa_maxord(t::Ptr{CTPSA{Desc}}, n::Cint, idx_::Ptr{Cint})::Cint
  return mi
end

"""
    mad_ctpsa_convert!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)

???

### Input
- `t`    -- Source complex TPSA
- `r`    -- Destination complex TPSA
- `n`    -- Length of vector
- `t2r_` -- Vector of index lookup
- `pb`   -- Poisson bracket, 0,1:fwd,-1:bwd
"""
function mad_ctpsa_convert!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)
  @ccall MAD_TPSA.mad_ctpsa_convert(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)::Cvoid
end


"""
    mad_ctpsa_setvar!(t::Ptr{CTPSA{Desc}}, v::ComplexF64, iv_::Cint, scl_::ComplexF64)

???

### Input
- `t`    -- Complex TPSA
- `v`    -- 0th order value
- `iv_`  -- Variable index
- `scl_` -- 1st order variable value
"""
function mad_ctpsa_setvar!(t::Ptr{CTPSA{Desc}}, v::ComplexF64, iv_::Cint, scl_::ComplexF64)
  @ccall MAD_TPSA.mad_ctpsa_setvar(t::Ptr{CTPSA{Desc}}, v::ComplexF64, iv_::Cint, scl_::ComplexF64)::Cvoid
end

"""
    mad_ctpsa_setvar_r!(t::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, iv_::Cint, scl_re::Cdouble, scl_im::Cdouble)

???

### Input
- `t`      -- Complex TPSA
- `v_re`   -- Real part of 0th order value
- `v_im`   -- Imaginary part of 0th order value
- `iv_`    -- Variable index
- `scl_re` -- Real part of 1st order variable value
- `scl_im` -- Imaginary part of 1st order variable value
"""
function mad_ctpsa_setvar_r!(t::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, iv_::Cint, scl_re::Cdouble, scl_im::Cdouble)
  @ccall MAD_TPSA.mad_ctpsa_setvar_r(t::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, iv_::Cint, scl_re::Cdouble, scl_im::Cdouble)::Cvoid
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

??? checks if null c2i?

### Input
- `t` -- Complex TPSA to check

### Output
- `ret`  -- True or false
"""
function mad_ctpsa_isnul(t::Ptr{CTPSA{Desc}})::Cuchar
  ret = @ccall MAD_TPSA.mad_ctpsa_isnul(t::Ptr{CTPSA{Desc}})::Cuchar
  return ret
end



""" 
    mad_ctpsa_cplx!(re_::Ptr{RTPSA{Desc}}, im_::Ptr{RTPSA{Desc}}, r::Ptr{CTPSA{Desc}})

Creates a CTPSA with real and imaginary parts from the RTPSAs re_ and im_ respectively.

### Input
- `re_` -- Real part of CTPSA to make
- `im_` -- Imaginary part of CTPSA to make
- `r`   -- Destination CTPSA with r = re_ + im*im_
"""
function mad_ctpsa_cplx!(re_::Ptr{RTPSA{Desc}}, im_::Ptr{RTPSA{Desc}}, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_cplx(re_::Ptr{RTPSA{Desc}}, im_::Ptr{RTPSA{Desc}}, r::Ptr{CTPSA{Desc}})::Cvoid
end


""" 
    mad_ctpsa_real!(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

Sets the RTPSA r equal to the real part of CTPSA t.

### Input
- `t` -- Source CTPSA
- `r` -- Destination RTPSA with r = Re(t)
"""
function mad_ctpsa_real!(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_real(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end


""" 
    mad_ctpsa_imag!(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

Sets the RTPSA r equal to the imaginary part of CTPSA t.

### Input
- `t` -- Source CTPSA
- `r` -- Destination RTPSA with r = Im(t)
"""
function mad_ctpsa_imag!(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_imag(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end

""" 
    mad_ctpsa_cabs!(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

Sets the RTPSA r equal to the aboslute value of CTPSA t

### Input
- `t` -- Source CTPSA
- `r` -- Destination RTPSA with r = |t|
"""
function mad_ctpsa_cabs!(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_cabs(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end


""" 
    mad_ctpsa_carg!(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})

Sets the RTPSA r equal to the argument (phase) of CTPSA t

### Input
- `t` -- Source CTPSA
- `r` -- Destination RTPSA with r = carg(t)
"""
function mad_ctpsa_carg!(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_carg(t::Ptr{CTPSA{Desc}}, r::Ptr{RTPSA{Desc}})::Cvoid
end


""" 
    mad_ctpsa_unit!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})

???

### Input
- `t` -- Source CTPSA
- `r` -- Destination CTPSA
"""
function mad_ctpsa_unit!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_unit(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})::Cvoid
end


""" 
    mad_ctpsa_rect!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})

???

### Input
- `t` -- Source CTPSA
- `r` -- Destination CTPSA
"""
function mad_ctpsa_rect!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_rect(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})::Cvoid
end


""" 
    mad_ctpsa_polar!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})

???

### Input
- `t` -- Source CTPSA
- `r` -- Destination CTPSA
"""
function mad_ctpsa_polar!(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_polar(t::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_mono(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar})::Cuchar

### Input
- `t`
- `i`
- `n`
- `m_`

### Output
- `ret`
"""
function mad_ctpsa_mono(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_ctpsa_mono(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar})::Cuchar
  return ret
end


"""
    mad_ctpsa_idxs(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring)::Cint

### Input
- `t`
- `n`
- `s`

### Output
- `ret`
"""
function mad_ctpsa_idxs(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring)::Cint
  ret = @ccall MAD_TPSA.mad_ctpsa_idxs(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring)::Cint
  return ret
end



"""
    mad_ctpsa_idxm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint


???

### Input
- `t`
- `n`
- `m`

### Output
- `ret`
"""
function mad_ctpsa_idxm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint
  ret = @ccall MAD_TPSA.mad_ctpsa_idxm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint
  return ret
end


"""
    mad_ctpsa_idxsm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint})::Cint

???

### Input
- `t`
- `n`
- `m`

### Output
- `ret`
"""
function mad_ctpsa_idxsm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint})::Cint
  ret = @ccall MAD_TPSA.mad_ctpsa_idxsm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint})::Cint
  return ret
end


"""
    mad_ctpsa_cycle(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, v_::Ptr{Cdouble})::Cint

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
function mad_ctpsa_cycle(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, v_::Ptr{ComplexF64})::Cint
  i = @ccall MAD_TPSA.mad_ctpsa_cycle(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, v_::Ptr{ComplexF64})::Cint
  return i
end


"""
    mad_ctpsa_get0(t::Ptr{CTPSA{Desc}})::ComplexF64

???

### Input
- `t`

### Output
- `ret`
"""
function mad_ctpsa_get0(t::Ptr{CTPSA{Desc}})::ComplexF64
  ret = @ccall MAD_TPSA.mad_ctpsa_get0(t::Ptr{CTPSA{Desc}})::ComplexF64
  return ret
end


"""
    mad_ctpsa_geti(t::Ptr{CTPSA{Desc}}, i::Cint)::ComplexF64

???

### Input
- `t`
- `i`

### Output
- `ret`
"""
function mad_ctpsa_geti(t::Ptr{CTPSA{Desc}}, i::Cint)::ComplexF64
  ret = @ccall MAD_TPSA.mad_ctpsa_geti(t::Ptr{CTPSA{Desc}}, i::Cint)::ComplexF64
  return ret
end


"""
    mad_ctpsa_gets(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring)::ComplexF64

???

### Input
- `t`
- `n`
- `s`

### Output
- `ret`
"""
function mad_ctpsa_gets(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring)::ComplexF64
  ret = @ccall MAD_TPSA.mad_ctpsa_gets(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring)::ComplexF64
  return ret
end


"""
    mad_ctpsa_getm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::ComplexF64

???

### Input
- `t`
- `n`
- `m`

### Output
- `ret`
"""
function mad_ctpsa_getm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::ComplexF64
  val = @ccall MAD_TPSA.mad_ctpsa_getm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::ComplexF64
  return ret
end


"""
    mad_ctpsa_getsm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint})::ComplexF64

???

### Input
- `t`
- `n`
- `m`

### Output
- `ret`
"""
function mad_ctpsa_getsm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint})::ComplexF64
  ret = @ccall MAD_TPSA.mad_ctpsa_getsm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint})::ComplexF64
  return ret
end


"""
    mad_ctpsa_set0!(t::Ptr{CTPSA{Desc}}, a::ComplexF64, b::ComplexF64)

???

### Input
- `t`
- `a`
- `b`
"""
function mad_ctpsa_set0!(t::Ptr{CTPSA{Desc}}, a::ComplexF64, b::ComplexF64)
  @ccall MAD_TPSA.mad_ctpsa_set0(t::Ptr{CTPSA{Desc}}, a::ComplexF64, b::ComplexF64)::Cvoid
end


"""
    mad_ctpsa_seti!(t::Ptr{CTPSA{Desc}}, i::Cint, a::ComplexF64, b::ComplexF64)

???

### Input
- `t`
- `i`
- `a`
- `b`
"""
function mad_ctpsa_seti!(t::Ptr{CTPSA{Desc}}, i::Cint, a::ComplexF64, b::ComplexF64)
  @ccall MAD_TPSA.mad_ctpsa_seti(t::Ptr{CTPSA{Desc}}, i::Cint, a::ComplexF64, b::ComplexF64)::Cvoid
end


"""
    mad_ctpsa_sets!(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, a::ComplexF64, b::ComplexF64)

???

### Input
- `t`
- `n`
- `s`
- `a`
- `b`
"""
function mad_ctpsa_sets!(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, a::ComplexF64, b::ComplexF64)
  @ccall MAD_TPSA.mad_ctpsa_sets(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, a::ComplexF64, b::ComplexF64)::Cvoid
end


"""
    mad_ctpsa_setm!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::ComplexF64, b::ComplexF64)

???

### Input
- `t`
- `n`
- `m`
- `a`
- `b`
"""
function mad_ctpsa_setm!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::ComplexF64, b::ComplexF64)
  @ccall MAD_TPSA.mad_ctpsa_setm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::ComplexF64, b::ComplexF64)::Cvoid
end


"""
    mad_ctpsa_setsm!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint}, a::ComplexF64, b::ComplexF64)

???

### Input
- `t`
- `n`
- `m`
- `a`
- `b`
"""
function mad_ctpsa_setsm!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint}, a::ComplexF64, b::ComplexF64)
  @ccall MAD_TPSA.mad_ctpsa_setsm(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint}, a::ComplexF64, b::ComplexF64)::Cvoid
end


# Accessors without complex-by-value
"""
    mad_ctpsa_get0_r!(t::Ptr{CTPSA{Desc}}, r::Ptr{ComplexF64})

???

### Input
- `t`
- `r`
"""
function mad_ctpsa_get0_r!(t::Ptr{CTPSA{Desc}}, r::Ptr{ComplexF64})
  ret = @ccall MAD_TPSA.mad_ctpsa_get0_r(t::Ptr{CTPSA{Desc}}, r::Ptr{ComplexF64})::Cvoid
  return ret
end


"""
    mad_ctpsa_geti_r!(t::Ptr{CTPSA{Desc}}, i::Cint,  r::Ptr{ComplexF64})

???

### Input
- `t`
- `i`
- `r`
"""
function mad_ctpsa_geti_r!(t::Ptr{CTPSA{Desc}}, i::Cint, r::Ptr{ComplexF64})
  ret = @ccall MAD_TPSA.mad_ctpsa_geti_r(t::Ptr{CTPSA{Desc}}, i::Cint, r::Ptr{ComplexF64})::Cvoid
  return ret
end


"""
    mad_ctpsa_gets_r!(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, r::Ptr{ComplexF64})

???

### Input
- `t`
- `n`
- `s`
- `r`
"""
function mad_ctpsa_gets_r!(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, r::Ptr{ComplexF64})
  ret = @ccall MAD_TPSA.mad_ctpsa_gets_r(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, r::Ptr{ComplexF64})::Cvoid
  return ret
end


"""
    mad_ctpsa_getm_r!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, r::Ptr{ComplexF64})

???

### Input
- `t`
- `n`
- `m`
- `r`
"""
function mad_ctpsa_getm_r!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, r::Ptr{ComplexF64})
  val = @ccall MAD_TPSA.mad_ctpsa_getm_r(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, r::Ptr{ComplexF64})::Cvoid
  return ret
end


"""
    mad_ctpsa_getsm_r!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint}, r::Ptr{ComplexF64})

???

### Input
- `t`
- `n`
- `m`
- `r`
"""
function mad_ctpsa_getsm_r!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint}, r::Ptr{ComplexF64})
  ret = @ccall MAD_TPSA.mad_ctpsa_getsm_r(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint}, r::Ptr{ComplexF64})::Cvoid
  return ret
end


"""
    mad_ctpsa_set0_r!(t::Ptr{CTPSA{Desc}}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)

???

### Input
- `t`
- `a_re`
- `a_im`
- `b_re`
- `b_im`
"""
function mad_ctpsa_set0_r!(t::Ptr{CTPSA{Desc}}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)
  @ccall MAD_TPSA.mad_ctpsa_set0_r(t::Ptr{CTPSA{Desc}}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)::Cvoid
end


"""
    mad_ctpsa_seti_r!(t::Ptr{CTPSA{Desc}}, i::Cint, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)

???

### Input
- `t`
- `i`
- `a_re`
- `a_im`
- `b_re`
- `b_im`
"""
function mad_ctpsa_seti_r!(t::Ptr{CTPSA{Desc}}, i::Cint, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)
  @ccall MAD_TPSA.mad_ctpsa_seti_r(t::Ptr{CTPSA{Desc}}, i::Cint, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)::Cvoid
end


"""
    mad_ctpsa_sets_r!(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)

???

### Input
- `t`
- `n`
- `s`
- `a`
- `b`
"""
function mad_ctpsa_sets_r!(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)
  @ccall MAD_TPSA.mad_ctpsa_sets_r(t::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)::Cvoid
end


"""
    mad_ctpsa_setm_r!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)

???

### Input
- `t`
- `n`
- `m`
- `a_re`
- `a_im`
- `b_re`
- `b_im`
"""
function mad_ctpsa_setm_r!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)
  @ccall MAD_TPSA.mad_ctpsa_setm_r(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)::Cvoid
end


"""
    mad_ctpsa_setsm_r!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)

???

### Input
- `t`
- `n`
- `m`
- `a_re`
- `a_im`
- `b_re`
- `b_im`
"""
function mad_ctpsa_setsm_r!(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)
  @ccall MAD_TPSA.mad_ctpsa_setsm_r(t::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cint}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble)::Cvoid
end


"""
    mad_ctpsa_getv!(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{ComplexF64})

???

### Input
- `t`
- `i`
- `n`
- `v`
"""
function mad_ctpsa_getv!(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{ComplexF64})
  @ccall MAD_TPSA.mad_ctpsa_getv(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{ComplexF64})::Cvoid
end



"""
    mad_ctpsa_setv!(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{ComplexF64})

???

### Input
- `t`
- `i`
- `n`
- `v`
"""
function mad_ctpsa_setv!(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{ComplexF64})
  @ccall MAD_TPSA.mad_ctpsa_setv(t::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, v::Ptr{ComplexF64})::Cvoid
end


"""
    mad_ctpsa_equ(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, tol_::Cdouble)::Cuchar

Checks if the TPSAs a and b are equal within the specified tolerance tol_

### Input
- `a`    -- TPSA a
- `b`    -- TPSA b
- `tol_` -- difference below which the TPSAs are considered equal

### Output
- `ret`   - True if a == b within tol_
"""
function mad_ctpsa_equ(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, tol_::Cdouble)::Cuchar
  ret = @ccall MAD_TPSA.mad_ctpsa_equ(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, tol_::Cdouble)::Cuchar
  return ret
end


"""
    mad_ctpsa_dif!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

???  // (a_i-b_i)/max(|a_i|,1)

### Input
- `a` -- Source TPSA a
- `b` -- Source TPSA b
- `c` -- Destination TPSA c 
"""
function mad_ctpsa_dif!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_dif(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_add!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination TPSA c = a + b

### Input
- `a` -- Source TPSA a
- `b` -- Source TPSA b
- `c` -- Destination TPSA c = a + b
"""
function mad_ctpsa_add!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_add(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_sub!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination TPSA c = a - b

### Input
- `a` -- Source TPSA a
- `b` -- Source TPSA b
- `c` -- Destination TPSA c = a - b
"""
function mad_ctpsa_sub!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sub(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_mul!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination TPSA c = a * b

### Input
- `a` -- Source TPSA a
- `b` -- Source TPSA b
- `c` -- Destination TPSA c = a * b
"""
function mad_ctpsa_mul!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_mul(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_div!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination TPSA c = a / b

### Input
- `a` -- Source TPSA a
- `b` -- Source TPSA b
- `c` -- Destination TPSA c = a / b
"""
function mad_ctpsa_div!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_div(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_pow!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination TPSA c = a ^ b

### Input
- `a` -- Source TPSA a
- `b` -- Source TPSA b
- `c` -- Destination TPSA c = a ^ b
"""
function mad_ctpsa_pow!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_pow(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_powi!(a::Ptr{CTPSA{Desc}}, n::Cint, c::Ptr{CTPSA{Desc}})

Sets the destination TPSA c = a ^ n where n is an integer.

### Input
- `a` -- Source TPSA a
- `n` -- Integer power
- `c` -- Destination TPSA c = a ^ n
"""
function mad_ctpsa_powi!(a::Ptr{CTPSA{Desc}}, n::Cint, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_powi(a::Ptr{CTPSA{Desc}}, n::Cint, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_pown!(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})

Sets the destination TPSA c = a ^ v where v is of double precision.

### Input
- `a` -- Source TPSA a
- `v` -- Power, ComplexF64
- `c` -- Destination TPSA c = a ^ v
"""
function mad_ctpsa_pown!(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_pown(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_pown_r!(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})

Sets the destination TPSA c = a ^ v where v is of double precision. Without complex-by-power arguments.

### Input
- `a`    -- Source TPSA a
- `v_re` -- Real part of power
- `v_im` -- Imaginary part of power
- `c`    -- Destination TPSA c = a ^ v
"""
function mad_ctpsa_pown!(a::Ptr{CTPSA{Desc}},  v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_pown_r(a::Ptr{CTPSA{Desc}},  v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_equt(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, tol_::Cdouble)::Cuchar

Checks if the CTPSA a is equal to the RTPSA b within the specified tolerance tol_ 
(internal real-to-complex conversion).

### Input
- `a`    -- CTPSA a
- `b`    -- RTPSA b
- `tol_` -- difference below which the TPSAs are considered equal

### Output
- `ret`   - True if a == b within tol_
"""
function mad_ctpsa_equt(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, tol_::Cdouble)::Cuchar
  ret = @ccall MAD_TPSA.mad_ctpsa_equt(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, tol_::Cdouble)::Cuchar
  return ret
end


"""
    mad_ctpsa_dift!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

???  // (a_i-b_i)/max(|a_i|,1)
(internal real-to-complex conversion).

### Input
- `a` -- Source CTPSA a
- `b` -- Source RTPSA b
- `c` -- Destination TPSA c 
"""
function mad_ctpsa_dift!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_dift(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_tdif!(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

???  // (a_i-b_i)/max(|a_i|,1)
(internal real-to-complex conversion).

### Input
- `a` -- Source RTPSA a
- `b` -- Source CTPSA b
- `c` -- Destination TPSA c 
"""
function mad_ctpsa_tdif!(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tdif(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end

"""
    mad_ctpsa_addt!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination CTPSA c = a + b (internal real-to-complex conversion).

### Input
- `a` -- Source CTPSA a
- `b` -- Source RTPSA b
- `c` -- Destination CTPSA c = a + b
"""
function mad_ctpsa_addt!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_addt(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_subt!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination CTPSA c = a - b (internal real-to-complex conversion).

### Input
- `a` -- Source CTPSA a
- `b` -- Source RTPSA b
- `c` -- Destination CTPSA c = a - b
"""
function mad_ctpsa_subt!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_subt(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_tsub!(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination CTPSA c = a - b (internal real-to-complex conversion).

### Input
- `a` -- Source RTPSA a
- `b` -- Source CTPSA b
- `c` -- Destination CTPSA c = a - b
"""
function mad_ctpsa_tsub!(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tsub(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_mult!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination CTPSA c = a * b (internal real-to-complex conversion).

### Input
- `a` -- Source CTPSA a
- `b` -- Source RTPSA b
- `c` -- Destination CTPSA c = a * b
"""
function mad_ctpsa_mult!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_mult(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_divt!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination CTPSA c = a / b (internal real-to-complex conversion).

### Input
- `a` -- Source CTPSA a
- `b` -- Source RTPSA b
- `c` -- Destination CTPSA c = a / b
"""
function mad_ctpsa_divt!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_divt(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_tdiv!(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination CTPSA c = a / b (internal real-to-complex conversion).

### Input
- `a` -- Source RTPSA a
- `b` -- Source CTPSA b
- `c` -- Destination CTPSA c = a / b
"""
function mad_ctpsa_tdiv!(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tdiv(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end

"""
    mad_ctpsa_powt!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination CTPSA c = a ^ b (internal real-to-complex conversion).

### Input
- `a` -- Source CTPSA a
- `b` -- Source RTPSA b
- `c` -- Destination CTPSA c = a ^ b
"""
function mad_ctpsa_powt!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_powt(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_tpow!(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets the destination CTPSA c = a ^ b (internal real-to-complex conversion).

### Input
- `a` -- Source RTPSA a
- `b` -- Source CTPSA b
- `c` -- Destination TPSA c = a ^ b
"""
function mad_ctpsa_tpow!(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tpow(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_nrm(a::Ptr{CTPSA{Desc}})::ComplexF64

Calculates the norm of (which???) of TPSA a.

### Input
- `a`   -- TPSA

### Output
- `nrm` -- Norm of TPSA a
"""
function mad_ctpsa_nrm(a::Ptr{CTPSA{Desc}})::ComplexF64
  nrm = @ccall MAD_TPSA.mad_ctpsa_nrm(a::Ptr{CTPSA{Desc}}, tpsa_b_::Ptr{CTPSA{Desc}})::ComplexF64
  return nrm
end


"""
    mad_ctpsa_abs!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the absolute value of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = |a|
"""
function mad_ctpsa_abs!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_abs(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_sqrt!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the sqrt of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = sqrt(a)
"""
function mad_ctpsa_sqrt!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sqrt(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_exp!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the exponential of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = exp(a)
"""
function mad_ctpsa_exp!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_exp(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end



"""
    mad_ctpsa_log!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the log of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = log(a)
"""
function mad_ctpsa_log!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_log(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_sincos!(a::Ptr{CTPSA{Desc}}, s::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA s = sin(a) and TPSA c = cos(a)

### Input
- `a` -- Source TPSA a
- `s` -- Destination TPSA s = sin(a)
- `c` -- Destination TPSA c = cos(a)
"""
function mad_ctpsa_sincos!(a::Ptr{CTPSA{Desc}}, s::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sincos(a::Ptr{CTPSA{Desc}}, s::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_sin!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the sin of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = sin(a)
"""
function mad_ctpsa_sin!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sin(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_cos!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the cos of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = cos(a)
"""
function mad_ctpsa_cos!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_cos(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_tan!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the tan of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = tan(a)
"""
function mad_ctpsa_tan!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tan(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_cot!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the cot of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = cot(a)
"""
function mad_ctpsa_cot!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_cot(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_sinc!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the sinc of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = sinc(a)
"""
function mad_ctpsa_sinc!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sinc(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_sincosh!(a::Ptr{CTPSA{Desc}}, s::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA s = sinh(a) and TPSA c = cosh(a)

### Input
- `a` -- Source TPSA a
- `s` -- Destination TPSA s = sinh(a)
- `c` -- Destination TPSA c = cosh(a)
"""
function mad_ctpsa_sincosh!(a::Ptr{CTPSA{Desc}}, s::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sincosh(a::Ptr{CTPSA{Desc}}, s::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_sinh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

  Sets TPSA c to the sinh of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = sinh(a)
"""
function mad_ctpsa_sinh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sinh(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_cosh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the cosh of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = cosh(a)
"""
function mad_ctpsa_cosh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_cosh(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_tanh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the tanh of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = tanh(a)
"""
function mad_ctpsa_tanh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tanh(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_coth!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the coth of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = coth(a)
"""
function mad_ctpsa_coth!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_coth(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_sinhc!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the sinhc of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = sinhc(a)
"""
function mad_ctpsa_sinhc!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sinhc(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_asin!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the asin of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = asin(a)
"""
function mad_ctpsa_asin!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_asin(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_acos!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the acos of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = acos(a)
"""
function mad_ctpsa_acos!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_acos(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_atan!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the atan of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = atan(a)
"""
function mad_ctpsa_atan!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_atan(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_acot!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the acot of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = acot(a)
"""
function mad_ctpsa_acot!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_acot(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end

"""
    mad_ctpsa_asinc!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the asinc of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = asinc(a)
"""
function mad_ctpsa_asinc!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_asinc(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_asinh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the asinh of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = asinh(a)
"""
function mad_ctpsa_asinh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_asinh(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_acosh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the acosh of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = acosh(a)
"""
function mad_ctpsa_acosh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_acosh(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_atanh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the atanh of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = atanh(a)
"""
function mad_ctpsa_atanh!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_atanh(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_acoth!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the acoth of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = acoth(a)
"""
function mad_ctpsa_acoth!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_acoth(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_asinhc!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the asinhc of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = asinhc(a)
"""
function mad_ctpsa_asinhc!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_asinhc(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_erf!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the erf of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = erf(a)
"""
function mad_ctpsa_erf!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_erf(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_erfc!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})

Sets TPSA c to the erfc of TPSA a.

### Input
- `a` -- Source TPSA a
- `c` -- Destination TPSA c = erfc(a)
"""
function mad_ctpsa_erfc!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_erfc(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_acc!(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})

Adds a*v to TPSA c. Aliasing OK.

### Input
- `a` -- Source TPSA a
- `v` -- Scalar with double precision
- `c` -- Destination TPSA c += v*a
"""
function mad_ctpsa_acc!(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_acc(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_scl!(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})

Sets TPSA c to v*a. 

### Input
- `a` -- Source TPSA a
- `v` -- Scalar with double precision
- `c` -- Destination TPSA c = v*a
"""
function mad_ctpsa_scl!(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_scl(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_inv!(a::Ptr{CTPSA{Desc}},  v::ComplexF64, c::Ptr{CTPSA{Desc}})

Sets TPSA c to v/a. 

### Input
- `a` -- Source TPSA a
- `v` -- Scalar with double precision
- `c` -- Destination TPSA c = v*a
"""
function mad_ctpsa_inv!(a::Ptr{CTPSA{Desc}},  v::ComplexF64, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_inv(a::Ptr{CTPSA{Desc}},  v::ComplexF64, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_invsqrt!(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})

Sets TPSA c to v/sqrt(a). 

### Input
- `a` -- Source TPSA a
- `v` -- Scalar with double precision
- `c` -- Destination TPSA c = v*a
"""
function mad_ctpsa_invsqrt!(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_invsqrt(a::Ptr{CTPSA{Desc}}, v::ComplexF64, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_hypot!(x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})

Sets TPSA r to sqrt(x^2+y^2)

### Input
- `x` -- Source TPSA x
- `y` -- Source TPSA y
- `r` -- Destination TPSA r = sqrt(x^2+y^2)
"""
function  mad_ctpsa_hypot!(x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_hypot(x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})::Cvoid
end

"""
    mad_ctpsa_hypot3!(x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, z::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})

Sets TPSA r to sqrt(x^2+y^2+z^2)

### Input
- `x` -- Source TPSA x
- `y` -- Source TPSA y
- `z` -- Source TPSA z
- `r` -- Destination TPSA r = sqrt(x^2+y^2+z^2)
"""
function  mad_ctpsa_hypot3!(x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, z::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_hypot3(x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, z::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_integ!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, iv::Cint)

??? Integrates TPSA

### Input
- `a`  -- Source TPSA to integrate
- `c`  -- Destination TPSA
- `iv` -- Domain
"""
function mad_ctpsa_integ!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, iv::Cint)
  @ccall MAD_TPSA.mad_ctpsa_integ(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, iv::Cint)::Cvoid
end


"""
    mad_ctpsa_deriv!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, iv::Cint)

??? Differentiates TPSA

### Input
- `a`  -- Source TPSA to differentiate
- `c`  -- Destination TPSA
- `iv` -- Domain
"""
function mad_ctpsa_deriv!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, iv::Cint)
  @ccall MAD_TPSA.mad_ctpsa_deriv(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, iv::Cint)::Cvoid
end


"""
    mad_ctpsa_derivm!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})

???

### Input
- `a`
- `c`
- `n`
- `m`
"""
function mad_ctpsa_derivm!(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})
  @ccall MAD_TPSA.mad_ctpsa_derivm(a::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cvoid
end


"""
    mad_ctpsa_poisbra!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, nv::Cint)

Sets TPSA c to the poisson bracket of TPSAs a and b.

### Input
- `a`  -- Source TPSA a
- `b`  -- Source TPSA b
- `c`  -- Destination TPSA c = [a, b]
- `nv` -- Number of variables in the TPSA
"""
function mad_ctpsa_poisbra!(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, nv::Cint)
  @ccall MAD_TPSA.mad_ctpsa_poisbra(a::Ptr{CTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, nv::Cint)::Cvoid
end


"""
    mad_ctpsa_taylor!(a::Ptr{CTPSA{Desc}}, n::Cint, coef::Ptr{ComplexF64}, c::Ptr{CTPSA{Desc}})

???

### Input
- `a`
- `n`
- `coef`
- `c`
"""
function mad_ctpsa_taylor!(a::Ptr{CTPSA{Desc}}, n::Cint, coef::Ptr{ComplexF64}, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_taylor(a::Ptr{CTPSA{Desc}}, n::Cint, coef::Ptr{ComplexF64}, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_poisbrat!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, nv::Cint)

Sets TPSA c to the poisson bracket of CTPSA a and RTPSA b (internal real-to-complex conversion).

### Input
- `a`  -- Source CTPSA a
- `b`  -- Source RTPSA b
- `c`  -- Destination CTPSA c = [a, b]
- `nv` -- Number of variables in the TPSA
"""
function mad_ctpsa_poisbrat!(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, nv::Cint)
  @ccall MAD_TPSA.mad_ctpsa_poisbrat(a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, nv::Cint)::Cvoid
end


"""
    mad_ctpsa_tpoisbra!(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, nv::Cint)

Sets TPSA c to the poisson bracket of RTPSA a and CTPSA b (internal real-to-complex conversion).

### Input
- `a`  -- Source RTPSA a
- `b`  -- Source CTPSA b
- `c`  -- Destination TPSA c = [a, b]
- `nv` -- Number of variables in the TPSA
"""
function mad_ctpsa_tpoisbra!(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, nv::Cint)
  @ccall MAD_TPSA.mad_ctpsa_tpoisbra(a::Ptr{RTPSA{Desc}}, b::Ptr{CTPSA{Desc}}, c::Ptr{CTPSA{Desc}}, nv::Cint)::Cvoid
end


"""
    mad_ctpsa_acc_r!(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})

Adds a*v to TPSA c. Aliasing OK. Without complex-by-value arguments.

### Input
- `a`    -- Source TPSA a
- `v_re` -- Real part of scalar with double precision
- `v_im` -- Imaginary part of scalar with double precision
- `c`    -- Destination TPSA c += v*a
"""
function mad_ctpsa_acc_r!(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_acc_r(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_scl_r!(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble,, c::Ptr{CTPSA{Desc}})

Sets TPSA c to v*a.  Without complex-by-value arguments.

### Input
- `a`    -- Source TPSA a
- `v_re` -- Real part of scalar with double precision
- `v_im` -- Imaginary part of scalar with double precision
- `c`    -- Destination TPSA c = v*a
"""
function mad_ctpsa_scl_r!(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_scl_r(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_inv_r!(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})

Sets TPSA c to v/a.  Without complex-by-value arguments.

### Input
- `a`    -- Source TPSA a
- `v_re` -- Real part of scalar with double precision
- `v_im` -- Imaginary part of scalar with double precision
- `c`    -- Destination TPSA c = v*a
"""
function mad_ctpsa_inv_r!(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_inv_r(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})::Cvoid
end

"""
    mad_ctpsa_invsqrt_r!(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})

Sets TPSA c to v/sqrt(a). Without complex-by-value arguments.

### Input
- `a`    -- Source TPSA a
- `v_re` -- Real part of scalar with double precision
- `v_im` -- Imaginary part of scalar with double precision
- `c`    -- Destination TPSA c = v*a
"""
function mad_ctpsa_invsqrt_r!(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_invsqrt_r(a::Ptr{CTPSA{Desc}}, v_re::Cdouble, v_im::Cdouble, c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axpb!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, b::ComplexF64, r::Ptr{CTPSA{Desc}})

??? r = a*x/b?

### Input
- `a`
- `x`
- `b`
- `r`
"""
function mad_ctpsa_axpb!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, b::ComplexF64, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axpb(a::ComplexF64, x::Ptr{CTPSA{Desc}}, b::ComplexF64, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axpbypc!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, b::ComplexF64, y::Ptr{CTPSA{Desc}}, c::ComplexF64, r::Ptr{CTPSA{Desc}})

### Input
- `a`
- `x`
- `b`
- `y`
- `c`
- `r`
"""
function mad_ctpsa_axpbypc!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, b::ComplexF64, y::Ptr{CTPSA{Desc}}, c::ComplexF64, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axpbypc(a::ComplexF64, x::Ptr{CTPSA{Desc}}, b::ComplexF64, y::Ptr{CTPSA{Desc}}, c::ComplexF64, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axypb!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b::ComplexF64, r::Ptr{CTPSA{Desc}})

???

### Input
- `a`
- `x`
- `y`
- `b`
- `r`
"""
function mad_ctpsa_axypb!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b::ComplexF64, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axypb(a::ComplexF64, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b::ComplexF64, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axypbzpc!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b::ComplexF64, z::Ptr{CTPSA{Desc}}, c::ComplexF64, r::Ptr{CTPSA{Desc}})

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
function mad_ctpsa_axypbzpc!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b::ComplexF64, z::Ptr{CTPSA{Desc}}, c::ComplexF64, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axypbzpc(a::ComplexF64, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b::ComplexF64, z::Ptr{CTPSA{Desc}}, c::ComplexF64, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axypbvwpc!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b::ComplexF64, v::Ptr{CTPSA{Desc}}, w::Ptr{CTPSA{Desc}}, c::ComplexF64, r::Ptr{CTPSA{Desc}})

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
function mad_ctpsa_axypbvwpc!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b::ComplexF64, v::Ptr{CTPSA{Desc}}, w::Ptr{CTPSA{Desc}}, c::ComplexF64, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axypbvwpc(a::ComplexF64, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b::ComplexF64, v::Ptr{CTPSA{Desc}}, w::Ptr{CTPSA{Desc}}, c::ComplexF64, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_ax2pby2pcz2!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, b::ComplexF64, y::Ptr{CTPSA{Desc}}, c::ComplexF64, z::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})

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
function mad_ctpsa_ax2pby2pcz2!(a::ComplexF64, x::Ptr{CTPSA{Desc}}, b::ComplexF64, y::Ptr{CTPSA{Desc}}, c::ComplexF64, z::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_ax2pby2pcz2(a::ComplexF64, x::Ptr{CTPSA{Desc}}, b::ComplexF64, y::Ptr{CTPSA{Desc}}, c::ComplexF64, z::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axpsqrtbpcx2!(x::Ptr{CTPSA{Desc}}, a::ComplexF64, b::ComplexF64, c::ComplexF64, r::Ptr{CTPSA{Desc}})

???

### Input
- `x`
- `a`
- `b`
- `c`
- `r`
"""
function mad_ctpsa_axpsqrtbpcx2!(x::Ptr{CTPSA{Desc}}, a::ComplexF64, b::ComplexF64, c::ComplexF64, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axpsqrtbpcx2(x::Ptr{CTPSA{Desc}}, a::ComplexF64, b::ComplexF64, c::ComplexF64, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_logaxpsqrtbpcx2!(x::Ptr{CTPSA{Desc}}, a::ComplexF64, b::ComplexF64, c::ComplexF64, r::Ptr{CTPSA{Desc}})

???

### Input
- `x`
- `a`
- `b`
- `c`
- `r`
"""
function mad_ctpsa_logaxpsqrtbpcx2!(x::Ptr{CTPSA{Desc}}, a::ComplexF64, b::ComplexF64, c::ComplexF64, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_logaxpsqrtbpcx2(x::Ptr{CTPSA{Desc}}, a::ComplexF64, b::ComplexF64, c::ComplexF64, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_logxdy!(x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})

???

### Input
- `x`
- `y`
- `r`
"""
function mad_ctpsa_logxdy!(x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_logxdy(x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axpb_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, r::Ptr{CTPSA{Desc}})

??? Without complex-by-value r = a*x/b?

### Input
- `a_re`
- `a_im`
- `x`
- `b_re`
- `b_im`
- `r`
"""
function mad_ctpsa_axpb_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axpb_r(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axpbypc_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, y::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})

### Input
- `a_re`
- `a_im`
- `x`
- `b_re`
- `b_im`
- `y`
- `c_re`
- `c_im`
- `r`
"""
function mad_ctpsa_axpbypc_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, y::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axpbypc_r(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, y::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axypb_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, r::Ptr{CTPSA{Desc}})

??? Without complex-by-value

### Input
- `a_re`
- `a_im`
- `x`
- `y`
- `b_re`
- `b_im`
- `r`
"""
function mad_ctpsa_axypb_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axypb_r(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axypbzpc_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, z::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})

??? Without complex-by-value

### Input
- `a_re`
- `a_im`
- `x`
- `y`
- `b_re`
- `b_im`
- `z`
- `c_re`
- `c_im`
- `r`
"""
function mad_ctpsa_axypbzpc_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, z::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axypbzpc_r(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, z::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axypbvwpc_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, v::Ptr{CTPSA{Desc}}, w::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})

??? Without complex-by-value

### Input
- `a_re`
- `a_im`
- `x`
- `y`
- `b_re`
- `b_im`
- `v`
- `w`
- `c_re`
- `c_im`
- `r`
"""
function mad_ctpsa_axypbvwpc_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, v::Ptr{CTPSA{Desc}}, w::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axypbvwpc_r(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, y::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, v::Ptr{CTPSA{Desc}}, w::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_ax2pby2pcz2_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, y::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, z::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})

??? Without complex-by-value

### Input
- `a_re`
- `a_im`
- `x`
- `b_re`
- `b_im`
- `y`
- `c_re`
- `c_im`
- `z`
- `r`
"""
function mad_ctpsa_ax2pby2pcz2_r!(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, y::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, z::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_ax2pby2pcz2_r(a_re::Cdouble, a_im::Cdouble, x::Ptr{CTPSA{Desc}}, b_re::Cdouble, b_im::Cdouble, y::Ptr{CTPSA{Desc}}, c_re::Cdouble, c_im::Cdouble, z::Ptr{CTPSA{Desc}}, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_axpsqrtbpcx2_r!(x::Ptr{CTPSA{Desc}}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})

??? Without complex-by-value

### Input
- `x`
- `a_re`
- `a_im`
- `b_re`
- `b_im`
- `c_re`
- `c_im`
- `r`
"""
function mad_ctpsa_axpsqrtbpcx2_r!(x::Ptr{CTPSA{Desc}}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axpsqrtbpcx2_r(x::Ptr{CTPSA{Desc}}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_logaxpsqrtbpcx2_r!(x::Ptr{CTPSA{Desc}}, a_re::Cdouble, a_im::Cdouble, b_re::Cdouble, b_im::Cdouble, c_re::Cdouble, c_im::Cdouble, r::Ptr{CTPSA{Desc}})

??? Without complex-by-value

### Input
- `x`
- `a_re`
- `a_im`
- `b_re`
- `b_im`
- `c_re`
- `c_im`
- `r`
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

???

### Input
- `na`
- `ma`
- `mb`
- `mc`
"""
function mad_ctpsa_liebra!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})
  @ccall MAD_TPSA.mad_ctpsa_liebra(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})::Cvoid
end


"""
    mad_ctpsa_exppb!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})

???

### Input
- `na`
- `ma`
- `mb`
- `mc`
"""
function mad_ctpsa_exppb!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})
  @ccall MAD_TPSA.mad_ctpsa_exppb(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})::Cvoid
end


"""
    mad_ctpsa_logpb!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})

???

### Input
- `na`
- `ma`
- `mb`
- `mc`
"""
function mad_ctpsa_logpb!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})
  @ccall MAD_TPSA.mad_ctpsa_logpb(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})::Cvoid
end


"""
    mad_ctpsa_mnrm(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}})::Cdouble

???

### Input
- `na`
- `ma`

### Output
- `nrm`
"""
function mad_ctpsa_mnrm(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}})::Cdouble
  nrm = @ccall MAD_TPSA.mad_ctpsa_mnrm(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}})::Cdouble
  return nrm
end


"""
    mad_ctpsa_minv!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})

???

### Input
- `na`
- `ma`
- `mc`
"""
function mad_ctpsa_minv!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})
  @ccall MAD_TPSA.mad_ctpsa_minv(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})::Cvoid
end


"""
    mad_ctpsa_pminv!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}}, select::Ptr{Cint})

???

### Input
- `na`
- `ma`
- `mc`
- `select`
"""
function mad_ctpsa_pminv!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}}, select::Ptr{Cint})
  @ccall MAD_TPSA.mad_ctpsa_pminv(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}}, select::Ptr{Cint})::Cvoid
end


"""
    mad_ctpsa_compose!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})

???

### Input
- `na`
- `ma`
- `nb`
- `mb`
- `mc`
"""
function mad_ctpsa_compose!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})
  @ccall MAD_TPSA.mad_ctpsa_compose(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})::Cvoid
end


"""
    mad_ctpsa_translate!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, tb::Ptr{ComplexF64}, mc::Ptr{CTPSA{Desc}})

???

### Input
- `na`
- `ma`
- `nb`
- `tb`
- `mc`
"""
function mad_ctpsa_translate!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, tb::Ptr{ComplexF64}, mc::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_translate(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, tb::Ptr{ComplexF64}, mc::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_eval!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, tb::Ptr{ComplexF64}, tc::Ptr{ComplexF64})

???

### Input
- `na`
- `ma`
- `nb`
- `tb`
- `tc`
"""
function mad_ctpsa_eval!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, tb::Ptr{ComplexF64}, tc::Ptr{ComplexF64})
  @ccall MAD_TPSA.mad_ctpsa_eval(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, tb::Ptr{ComplexF64}, tc::Ptr{ComplexF64})::Cvoid
end


"""
    mad_ctpsa_mconv!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nc::Cint, mc::Ptr{Ptr{CTPSA{Desc}}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)

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
function mad_ctpsa_mconv!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nc::Cint, mc::Ptr{Ptr{CTPSA{Desc}}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)
  @ccall MAD_TPSA.mad_ctpsa_mconv(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nc::Cint, mc::Ptr{Ptr{CTPSA{Desc}}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)::Cvoid
end


"""
    mad_ctpsa_print(t::Ptr{CTPSA{Desc}}, name_::Cstring, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid})

Prints the TPSA coefficients to stdout with precision eps_. If nohdr_ is not zero, 
the header is not printed. 

### Input
- `t`       -- TPSA to print
- `name_`   -- Name of TPSA
- `eps_`    -- Precision to output
- `nohdr_`  -- If True, no header is printed
- `stream_` --  FILE pointer of output stream. If null, printed to stdout
"""
function mad_ctpsa_print(t::Ptr{CTPSA{Desc}}, name_::Cstring, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_ctpsa_print(t::Ptr{CTPSA{Desc}}, name_::Cstring, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid})::Cvoid
end


"""
    mad_ctpsa_scan(stream_::Ptr{Cvoid})::Ptr{CTPSA{Desc}}

Scans in a TPSA from the stream_.

### Input
- `stream_` -- C FILE pointer I/O stream from which to read the TPSA

### Output
- `t`    -- TPSA scanned from I/O stream_
"""
function mad_ctpsa_scan(stream_::Ptr{Cvoid})::Ptr{CTPSA{Desc}}
  t = @ccall MAD_TPSA.mad_ctpsa_scan(stream_::Ptr{Cvoid})::Ptr{CTPSA{Desc}}
  return t
end


"""
    mad_ctpsa_scan_hdr(kind_::Cint, name_::Cstring, stream_::Ptr{Cvoid})::Ptr{Desc{RTPSA,CTPSA}}

???

### Input
- `kind_`
- `name_`
- `stream_`

### Output
- `ret`
"""
function mad_ctpsa_scan_hdr(kind_::Cint, name_::Cstring, stream_::Ptr{Cvoid})::Ptr{Desc{RTPSA,CTPSA}}
  desc = @ccall MAD_TPSA.mad_ctpsa_scan_hdr(kind_::Cint, name_::Cstring, stream_::Ptr{Cvoid})::Ptr{Desc{RTPSA,CTPSA}}
  return ret
end


"""
    mad_ctpsa_scan_coef!(t::Ptr{CTPSA{Desc}}, stream_::Ptr{Cvoid})

???

### Input
- `t`
- `stream_`
"""
function mad_ctpsa_scan_coef!(t::Ptr{CTPSA{Desc}}, stream_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_ctpsa_scan_coef(t::Ptr{CTPSA{Desc}}, stream_::Ptr{Cvoid})::Cvoid
end


"""
    mad_ctpsa_debug(t::Ptr{CTPSA{Desc}}, name_::Cstring, fnam_::Cstring, line_::Cint, stream_::Ptr{Cvoid})

???

### Input
- `t`
- `name_`
- `fnam_`
- `line_`
- `stream_`
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

UNSAFE OPERATION! (mo vs allocated!!) ???

### Input
- `t` 
- `d`
- `mo`

### Output
- `t`  
"""
function mad_ctpsa_init!(t::Ptr{CTPSA{Desc}}, d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{CTPSA{Desc}}
  t = @ccall MAD_TPSA.mad_ctpsa_init(t::Ptr{CTPSA{Desc}}, d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{CTPSA{Desc}}
  return t
end

end
