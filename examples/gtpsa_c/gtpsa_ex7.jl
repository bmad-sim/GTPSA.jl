using Pkg
Pkg.activate("../../GTPSA.jl")
Pkg.instantiate()
using GTPSA
using Printf

# descriptor for TPSA with 2 variables of order 63 (max) and 1 parameter order 1
d = mad_desc_newvp(Int32(2), UInt8(63), Int32(1), 0x1)

# two TPSAs, t1 has maximum order, t2 is same as t1
t1 = mad_tpsa_newd(d, MAD_TPSA_DEFAULT)
t2 = mad_tpsa_new(t1, MAD_TPSA_SAME)

# set order 0 and 1 (quick and dirty!)
mad_tpsa_setv!(t1, Int32(0), Int32(1+3), Base.unsafe_convert(Ptr{Float64}, [pi/6,1,1,1])) # Need to convert from Vector{Float64} to Ptr{Float64} for C
mad_tpsa_print(t1, Base.unsafe_convert(Cstring, "ini"), 0., Int32(0),C_NULL) # Similiar conversion here for Ptr

# t2=sin(t1)
mad_tpsa_sin!(t1, t2)
mad_tpsa_print(t2, Base.unsafe_convert(Cstring, "sin"), 0., Int32(0),C_NULL)
mad_tpsa_del!(t1)

# tpsa functions and operators support aliasing (i.e. src == dst)
mad_tpsa_asin!(t2, t2)             # asin(x) = -i*ln(i*x + sqrt(1-x^2))
mad_tpsa_print(t2, Base.unsafe_convert(Cstring, "asin"), 0., Int32(0), C_NULL) # see the accuracy of asin(sin)
mad_tpsa_del!(t2)

# destroy all created descriptors (optional cleanup)
mad_desc_del!(Base.unsafe_convert(Ptr{Desc{RTPSA,CTPSA}}, C_NULL))
