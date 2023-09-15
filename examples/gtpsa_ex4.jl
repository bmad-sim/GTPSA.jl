include("../src/TPSA.jl")
using .TPSA
using Printf

# descriptor for TPSA with 6 variables of order 10,10,10,10,10,10 without parameters
d10 = mad_desc_newvpo(Int32(6), 0x0, Int32(0), 0x0, Base.unsafe_convert(Ptr{UInt8}, [0xa, 0xa, 0xa, 0xa, 0xa, 0xa]))
@printf("d10 length=%4d coefs\n", mad_desc_maxlen(d10, MAD_TPSA_DEFAULT))
mad_desc_del!(d10)

# descriptor for TPSA of order 12 with 6 variables of order 2,2,2,2,1,10 without parameters
d = mad_desc_newvpo(Int32(6), 0xc, Int32(0), 0x0, Base.unsafe_convert(Ptr{UInt8}, [0x2, 0x2, 0x2, 0x2, 0x1, 0xa]))
@printf("d   length=%4d coefs\n", mad_desc_maxlen(d, MAD_TPSA_DEFAULT))

# two TPSAs, t2 is same as t1
t1 = mad_tpsa_newd(d, MAD_TPSA_DEFAULT)
t2 = mad_tpsa_new(t1, MAD_TPSA_SAME)

# set order 0 and 1 (quick and dirty!)
mad_tpsa_setv!(t1, Int32(0), Int32(1+6), Base.unsafe_convert(Ptr{Float64}, [pi/6,1,1,1,1,1,1])) # Need to convert from Vector{Float64} to Ptr{Float64} for C
mad_tpsa_print(t1, Base.unsafe_convert(Cstring, "ini"), 0.,Int32(0),C_NULL) # Similiar conversion here for Ptr

# t2=sin(t1)
mad_tpsa_sin!(t1, t2)
mad_tpsa_print(t2, Base.unsafe_convert(Cstring, "sin"), 0.,Int32(0),C_NULL)
mad_tpsa_del!(t1)

# tpsa functions and operators support aliasing (i.e. src == dst)
mad_tpsa_asin!(t2, t2);             # asin(x) = -i*ln(i*x + sqrt(1-x^2))
mad_tpsa_print(t2, Base.unsafe_convert(Cstring, "asin"), 0.,Int32(0),C_NULL) # see the accuracy of asin(sin)
mad_tpsa_del!(t2)

# destroy all created descriptors (optional cleanup)
mad_desc_del!(C_NULL)
