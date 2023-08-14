module TPSA
include("Monomial.jl")
include("Descriptor.jl")
include("RealTPSA.jl")
include("ComplexTPSA.jl")
include("Structs.jl")
using .Structs
using .Descriptor
using .RealTPSA
using .ComplexTPSA
using .Monomial
using Printf
#import Base: sin
export Desc, RTPSA, CTPSA, new_desc,new_TPSA,set_TPSA!,print_TPSA,sin!,del!,asin!,set_name!,cleanup,desc_maxlen,MAD_TPSA_DEFAULT, MAD_TPSA_SAME

const MAD_TPSA = :("libmad_tpsa")
const MAD_TPSA_DEFAULT::Cuchar = 255
const MAD_TPSA_SAME::Cuchar = 254

"""
Variable type conversions:
"ord_t": "Cuchar",
"c_int": "Cint",
"c_num_t": "Cdouble",
"c_cpx_t": "ComplexF64",
"c_ssz_t": "Cint",
"c_idx_t": "Cint",
"c_ptr desc": "Ptr{Desc{RTPSA,CTPSA}}",
"c_ptr tpsa": "Ptr{RTPSA{Desc}}",
"c_ptr ctpsa": "Ptr{CTPSA{Desc}}"
"""

# Higher level functions
# ------------------------------------------------------------------------------------------
# DESCRIPTOR FUNCTIONS:

"""
    new_desc(nv::Integer, mo::Integer)::Ptr{Desc{RTPSA,CTPSA}}

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
function new_desc(nv::Integer, mo::Integer)::Ptr{Desc{RTPSA,CTPSA}}
  d = @ccall MAD_TPSA.mad_desc_newv(nv::Cint,mo::Cuchar)::Ptr{Desc{RTPSA,CTPSA}}
  return d
end


"""
    new_desc(nv::Integer, mo::Integer, np_::Integer, po_::Integer)::Ptr{Desc{RTPSA,CTPSA}}

  Creates a TPSA descriptor with the specifed number of variables, maximum order,
  number of parameters, and parameter order.
  
  Input:
    nv -- Number of variables 
    mo -- Maximum order
    np_ -- Number of parameters
    po_ -- Parameter order

  Output:
    A pointer to the TPSA descriptor created, with:
    Desc.nv = nv 
    Desc.mo = mo
    Desc.np = np_
    Desc.po = po_
"""
function new_desc(nv::Integer, mo::Integer, np_::Integer, po_::Integer)::Ptr{Desc{RTPSA,CTPSA}}
  d = @ccall MAD_TPSA.mad_desc_newvp(nv::Cint, mo::Cuchar, np_::Cint, po_::Cuchar)::Ptr{Desc{RTPSA,CTPSA}}
  return d
end

"""
  new_desc(nv::Integer, mo::Integer, np_::Integer, po_::Integer, no_::Vector{<:Integer})::Ptr{Desc{RTPSA,CTPSA}}

Creates a TPSA descriptor with the specifed number of variables, maximum order,
number of parameters, parameter order, and individual variable/parameter orders 
specified in no. The first nv entries in no correspond to the variables' orders 
and the next np entries correspond the parameters' orders.

Input:
  nv -- Number of variables 
  mo -- Maximum order
  np_ -- Number of parameters
  po_ -- Parameter order
  no_ -- Vector of variable and parameter orders, in order. Must be length nv+np_ (FIGURE OUT order). 

Output:
  A pointer to the TPSA descriptor created, with:
  Desc.nv = nv 
  Desc.mo = mo
  Desc.np = np_
  Desc.po = po_
  Desc.no = no_
"""
function new_desc(nv::Integer, mo::Integer, np_::Integer, po_::Integer, no_::Vector{<:Integer})::Ptr{Desc{RTPSA,CTPSA}}
  no = convert(Vector{UInt8}, no_)
  d = @ccall MAD_TPSA.mad_desc_newvpo(nv::Cint, mo::Cuchar, np_::Cint, po_::Cuchar, no::Ptr{Cuchar})::Ptr{Desc{RTPSA,CTPSA}}
  return d
end

"""
    del!(d::Ptr{Desc{RTPSA,CTPSA}})

  Destroys the descriptor d.
  
  Input:
    d -- Descriptor to destroy

  Output:
    None.
"""
function del!(d::Ptr{Desc{RTPSA,CTPSA}})
  @ccall MAD_TPSA.mad_desc_del(d::Ptr{Desc{RTPSA,CTPSA}})::Cvoid
end

"""
    cleanup()

  Destroys all descriptors.
"""
function cleanup()
  @ccall MAD_TPSA.mad_desc_del(0::Cint)::Cvoid
end


# ------------------------------------------------------------------------------------------
# TPSA FUNCTIONS

"""
    new_TPSA(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Integer)::Ptr{RTPSA{Desc}}

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
function new_TPSA(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Integer)::Ptr{RTPSA{Desc}}
  @ccall MAD_TPSA.mad_tpsa_newd(d::Ptr{Desc{RTPSA,CTPSA}},mo::Cuchar)::Ptr{RTPSA{Desc}}
end

"""
    new_TPSA(t::Ptr{RTPSA{Desc}}, mo::Integer)::Ptr{RTPSA{Desc}}

  Creates a real TPSA copy of the inputted TPSA, with maximum order specified by mo.
  If mad_tpsa_same is passed for mo, the mo currently in t is used for the created TPSA.
  
  Input:
    t  -- Pointer to real TPSA to copy
    mo -- Maximum order of new TPSA

  Output:
    A pointer to the real TPSA copy created with maximum order mo.
"""
function new_TPSA(t::Ptr{RTPSA{Desc}}, mo::Integer)::Ptr{RTPSA{Desc}}
  @ccall MAD_TPSA.mad_tpsa_new(t::Ptr{RTPSA{Desc}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
end

"""
    set_TPSA(t::Ptr{RTPSA{Desc}}, i::Integer, n::Integer, v::Vector{<:Float64})

  Sets the coefficients of the TPSA in indices i:i+n to those in v. That is,
  t.coefs[i:i+n] = v. The coefficients are sorted by order. v must be length n.
  
  Input:
    t -- Pointer to real TPSA
    i -- Starting index of coefficients in TPSA to set
    n -- Number of coefficients to set in TPSA
    v -- Vector values to set coefficients in TPSA. 

  Output:
    Sets the coefficients in the TPSA t accordingly. 
"""
function set_TPSA!(t::Ptr{RTPSA{Desc}}, i::Integer, n::Integer, v::Vector{<:Float64})
  @ccall MAD_TPSA.mad_tpsa_setv(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cuint, v::Ptr{Cdouble})::Cvoid
end


# ONly 1 function for setting needed
function set_TPSA!(t::Ptr{RTPSA{Desc}}, a::Float64, b::Float64)
  @ccall MAD_TPSA.mad_tpsa_set0(t::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble)::Cvoid
end

"""

    print_TPSA(t::Ptr{RTPSA{Desc}}, name::AbstractString, eps_::Real = 0.0, nohdr_::Bool = false, filename::AbstractString = "", mode::AbstractString="w+")

  Prints the TPSA coefficients to stdout with precision eps_. If nohdr_ is not zero, 
  the header is not printed. 
"""
function print_TPSA(t::Ptr{RTPSA{Desc}}, name::AbstractString, eps_::Real = 0.0, nohdr_::Bool = false, filename::AbstractString = "", mode::AbstractString="w+")
  if filename=="" # print to stdout
    @ccall MAD_TPSA.mad_tpsa_print(t::Ptr{RTPSA{Desc}}, name::Cstring, eps_::Cdouble,nohdr_::Cint,0::Cint)::Cvoid
  else
    fp = @ccall fopen(filename::Cstring, mode::Cstring)::Ptr{Cvoid}
    @ccall MAD_TPSA.mad_tpsa_print(t::Ptr{RTPSA{Desc}}, name::Cstring, eps_::Cdouble,nohdr_::Cint,fp::Ptr{Cvoid})::Cvoid
    @ccall fclose(fp::Ptr{Cvoid})::Cvoid
  end
end

"""
    set_name(t::Ptr{RTPSA{Desc}}, nam::AbstractString)

  Set the name of a TPSA.

  Input:
    t   -- Source TPSA
    nam -- String of new name for TPSA

  Output:
    Sets the TPSA name (RTPSA.nam).
"""
function set_name!(t::Ptr{RTPSA{Desc}}, nam::AbstractString)
  @ccall MAD_TPSA.mad_tpsa_setnam(t::Ptr{RTPSA{Desc}}, nam::Cstring)::Cvoid
end

"""
NEEDS DOCUMENTATION
"""
function desc_maxlen(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Integer)::Int64
  ret = @ccall MAD_TPSA.mad_desc_maxlen(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Int64
  return ret
end


"""
    sin!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

  Sets c = sin(a) in place. Aliasing is supported (i.e. a == c).

  Input:
    a -- Source TPSA 
    c -- Destination TPSA

  Output:
    Sets the TPSA c = sin(a).
"""
function sin!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sin(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end

"""
    asin!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

  Sets c = asin(a) in place. Aliasing is supported (i.e. a == c).

  Input:
    a -- Source TPSA 
    c -- Destination TPSA

  Output:
    Sets the TPSA c = asin(a).
"""
function asin!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall "libmad_tpsa".mad_tpsa_asin(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid 
end

"""
    del!(t::Ptr{RTPSA{Desc}})

  Destroys the TPSA at t.

  Input:
    t -- TPSA to destroy

  Output:
    None.
"""
function del!(t::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_del(t::Ptr{RTPSA{Desc}})::Cvoid
end



end