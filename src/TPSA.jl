module TPSA
include("Descriptor.jl")
include("RealTPSA.jl")
include("ComplexTPSA.jl")
using .Descriptor
using .RealTPSA
using .ComplexTPSA
#import Base: sin
export Desc, RTPSA, CTPSA, new_desc,new_TPSA,set_TPSA!,print_TPSA,sin!,del!,asin!,mad_tpsa_default, mad_tpsa_same

const mad_tpsa = :("libmad_tpsa")
const mad_tpsa_default::Cuchar = 255
const mad_tpsa_same::Cuchar = 254

"""
    new_desc(nv::Integer, mo::Integer)::Ptr{Desc}

  Creates a TPSA descriptor with the specified number of variables and maximum order. 
  The number of parameters is set to 0. 

  Input:

    nv -- Number of variables 
    mo -- Maximum order

  Output:

    A pointer to the TPSA descriptor created, with:
    Desc.nv = nv 
    Desc.mo = mo

"""
function new_desc(nv::Integer, mo::Integer)::Ptr{Desc}
  d = @ccall mad_tpsa.mad_desc_newv(nv::Cint,mo::Cuchar)::Ptr{Desc}
  return d
end


"""
    new_desc(nv::Integer, mo::Integer, np::Integer, po::Integer)::Ptr{Desc}

  Creates a TPSA descriptor with the specifed number of variables, maximum order,
  number of parameters, and parameter order.
  
  Input:

    nv -- Number of variables 
    mo -- Maximum order
    np -- Number of parameters
    po -- Parameter order

  Output:

    A pointer to the TPSA descriptor created, with:
    Desc.nv = nv 
    Desc.mo = mo
    Desc.np = np
    Desc.po = po
    
"""
function new_desc(nv::Integer, mo::Integer, np::Integer, po::Integer)::Ptr{Desc}
  d = @ccall mad_tpsa.mad_desc_newvp(nv::Cint, mo::Cuchar, np::Cint, po::Cuchar)::Ptr{Desc}
  return d
end

"""
    new_TPSA(d::Ptr{Desc}, mo::Integer)::Ptr{RTPSA}

  Creates a real TPSA defined by the specified descriptor and maximum order.
  If mad_tpsa_default is passed for mo, the mo defined in the descriptor is used.
  
  Input:

    d  -- Descriptor for TPSA
    mo -- Maximum order of TPSA

  Output:

    A pointer to the real TPSA created, with:
    RTPSA.d   = d 
    RTPSA.mo  = mo
    and all other members initialized to 0.

"""
function new_TPSA(d::Ptr{Desc}, mo::Integer)::Ptr{RTPSA}
  @ccall mad_tpsa.mad_tpsa_newd(d::Ptr{Desc},mo::Cuchar)::Ptr{RTPSA}
end

"""
    new_TPSA(t::Ptr{RTPSA}, mo::Integer)::Ptr{RTPSA}

  Creates a real TPSA copy of the inputted TPSA, with maximum order specified by mo.
  If mad_tpsa_same is passed for mo, the mo currently in t is used for the created TPSA.
  
  Input:

    t  -- Pointer to real TPSA to copy
    mo -- Maximum order of new TPSA

  Output:

    A pointer to the real TPSA copy created with maximum order mo.

"""
function new_TPSA(t::Ptr{RTPSA}, mo::Integer)::Ptr{RTPSA}
  @ccall mad_tpsa.mad_tpsa_new(t::Ptr{RTPSA}, mo::Cuchar)::Ptr{RTPSA}
end

"""
    set_TPSA(t::Ptr{RTPSA}, i::Integer, n::Integer, v::Vector{Real})

  Sets the coefficients of the TPSA in indices i:i+n to those in v. That is,
  t.coefs[i:i+n] = v. v must be length n.
  
  Input:

    t -- Pointer to real TPSA
    i -- Starting index of coefficients in TPSA to set
    n -- Number of coefficients to set in TPSA
    v -- Vector values to set coefficients in TPSA

  Output:

    None.

"""
function set_TPSA!(t::Ptr{RTPSA}, i::Integer, n::Integer, v::Vector{<:Real})
  @ccall mad_tpsa.mad_tpsa_setv(t::Ptr{RTPSA}, i::Cint, n::Cuint, v::Ptr{Cdouble})::Cvoid
end

"""
NOTE: THIS WILL BE REWRITTEN IN JULIA AND WILL PRINT TO FILES AS WELL.

    print_TPSA(t::Ptr{RTPSA}, name::AbstractString, eps_::Real,nohdr_::Integer)

  Prints the TPSA coefficients to stdout with precision eps_. If nohdr_ is not zero, 
  the header is not printed. 

"""
function print_TPSA(t::Ptr{RTPSA}, name::AbstractString, eps_::Real,nohdr_::Integer)
  @ccall mad_tpsa.mad_tpsa_print(t::Ptr{RTPSA}, name::Cstring, eps_::Cint,nohdr_::Cint,0::Cint)::Cvoid
end


"""
NOTE: WILL MODIFY IN JULIA TO RETURN PROBABLY

    sin!(a::Ptr{RTPSA}, c::Ptr{RTPSA})

  Sets c = sin(a)

  Input:

    a -- Source TPSA 
    c -- Destination TPSA

  Output:

    None.

"""
function sin!(a::Ptr{RTPSA}, c::Ptr{RTPSA})
  @ccall mad_tpsa.mad_tpsa_sin(a::Ptr{RTPSA}, c::Ptr{RTPSA})::Cvoid
end

"""
NOTE: WILL MODIFY IN JULIA TO RETURN PROBABLY

    asin!(a::Ptr{RTPSA}, c::Ptr{RTPSA})

  Sets c = asin(a)

  Input:

    a -- Source TPSA 
    c -- Destination TPSA

  Output:

    None.

"""
function asin!(a::Ptr{RTPSA}, c::Ptr{RTPSA})
  @ccall "libmad_tpsa".mad_tpsa_asin(a::Ptr{RTPSA}, c::Ptr{RTPSA})::Cvoid 
end

function del!(t::Ptr{RTPSA})
  @ccall mad_tpsa.mad_tpsa_del(t::Ptr{RTPSA})::Cvoid
end

end
