include("TPSA.jl")
using .TPSA

# descriptor for TPSA with 6 variables of order 4
d  = @ccall "libmad_tpsa".mad_desc_newv(6::Cint,4::Cuchar)::Ptr{Desc{RTPSA,CTPSA}}

# two TPSAs, t1 has maximum order, t2 is same as t1
t1 = @ccall "libmad_tpsa".mad_tpsa_newd(d::Ptr{Desc{RTPSA,CTPSA}},255::Cuchar)::Ptr{RTPSA}
t2 = @ccall "libmad_tpsa".mad_tpsa_new(t1::Ptr{RTPSA}, 254::Cuchar)::Ptr{RTPSA}

# set order 0 and 1 (quick and dirty!)
@ccall "libmad_tpsa".mad_tpsa_setv(t1::Ptr{RTPSA}, 0::Cint, (1+6)::Cuint, [pi/6, 1,1,1,1,1,1]::Ptr{Cdouble})::Cvoid
@ccall "libmad_tpsa".mad_tpsa_print(t1::Ptr{RTPSA}, "ini"::Cstring, 0::Cint,0::Cint,0::Cint)::Cvoid

# t2=sin(t1)
@ccall "libmad_tpsa".mad_tpsa_sin(t1::Ptr{RTPSA}, t2::Ptr{RTPSA})::Cvoid
@ccall "libmad_tpsa".mad_tpsa_print(t2::Ptr{RTPSA}, "sin"::Cstring, 0::Cint,0::Cint,0::Cint)::Cvoid
@ccall "libmad_tpsa".mad_tpsa_del(t1::Ptr{RTPSA})::Cvoid

# tpsa functions and operators support aliasing (i.e. src == dst)
@ccall "libmad_tpsa".mad_tpsa_asin(t2::Ptr{RTPSA}, t2::Ptr{RTPSA})::Cvoid                             # asin(x) = -i*ln(i*x + sqrt(1-x^2))
@ccall "libmad_tpsa".mad_tpsa_print(t2::Ptr{RTPSA}, "asin"::Cstring, 0::Cint,0::Cint,0::Cint)::Cvoid # see the accuracy of asin(sin)
@ccall "libmad_tpsa".mad_tpsa_del(t2::Ptr{RTPSA})::Cvoid

# destroy all created descriptors (optional cleanup)
@ccall "libmad_tpsa".mad_desc_del(0::Cint)::Cvoid