using Pkg
Pkg.activate("../../TPSA.jl")
Pkg.instantiate()
using TPSA
using Printf

# descriptor for TPSA with 100 variables of order 2 without parameters
d2 = mad_desc_newv(Int32(100), 0x2)
@printf("d2 length=%4d coefs\n", mad_desc_maxlen(d2, MAD_TPSA_DEFAULT))
mad_desc_del!(d2)

# descriptor for TPSA with 6  variables  of order 2 and 94 parameters of order 1
d = mad_desc_newvp(Int32(6), 0x2, Int32(94), 0x1)
@printf("d  length=%4d coefs\n", mad_desc_maxlen(d, MAD_TPSA_DEFAULT))

# two TPSAs, t2 is same as t1
t1 = mad_tpsa_newd(d, MAD_TPSA_DEFAULT)
t2 = mad_tpsa_new(t1, MAD_TPSA_SAME)

# set order 0 and 1 (quick and dirty!)
vec = [pi/6; ones(100,1);]
mad_tpsa_setv!(t1, Int32(0), Int32(1+100), Base.unsafe_convert(Ptr{Float64},vec))
mad_tpsa_print(t1, Base.unsafe_convert(Cstring, "ini"), 0., Int32(0), C_NULL)

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