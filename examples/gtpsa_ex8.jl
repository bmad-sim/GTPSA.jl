using Pkg
Pkg.activate("../../TPSA.jl")
Pkg.instantiate()
using TPSA
using Printf

# descriptor for TPSA with 6 variables of order 2 (max)
d = mad_desc_newv(Int32(6), 0x2)

# d0 = mad_desc_newv(Int32(6), 0x0)
# -> error: mad_desc.c:1215: : invalid maximum order: 0 (0<?<=63)

# create three TPSAs of order 0 (as scalars) and set values
t1 = mad_tpsa_newd(d, 0x0)
mad_tpsa_set0!(t1, 0., 1.5)
t2 = mad_tpsa_newd(d, 0x0)
mad_tpsa_set0!(t2, 0., 2.0)
t3 = mad_tpsa_newd(d, 0x0)

mad_tpsa_print(t1, Base.unsafe_convert(Cstring, "T1"), 0., Int32(0), C_NULL)
mad_tpsa_print(t2, Base.unsafe_convert(Cstring, "T2"), 0., Int32(0), C_NULL)

# add the scalars
mad_tpsa_add!(t1, t2, t3)
mad_tpsa_print(t3, Base.unsafe_convert(Cstring, "T1+T2"), 0., Int32(0), C_NULL)

# multiply the scalars
mad_tpsa_mul!(t1, t2, t3)
mad_tpsa_print(t3, Base.unsafe_convert(Cstring, "T1*T2"), 0., Int32(0), C_NULL)


# log of the scalar
mad_tpsa_log!(t3,t3)
mad_tpsa_print(t3, Base.unsafe_convert(Cstring, "log(T1*T2)"), 0., Int32(0), C_NULL)

# destroy the three TPSAs
mad_tpsa_del!(t1)
mad_tpsa_del!(t2)
mad_tpsa_del!(t3)

# destroy all created descriptors (optional cleanup)
mad_desc_del!(Base.unsafe_convert(Ptr{Desc{RTPSA,CTPSA}}, C_NULL))
