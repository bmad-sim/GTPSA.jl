module TPSA
include("Descriptor.jl")
using .Descriptor

# descriptor for TPSA with 6 variables of order 4
d  = @ccall "libmad_tpsa".mad_desc_newv(6::Cint,4::Cuchar)::Ptr{Desc{Tpsa,Ctpsa}}

# two TPSAs, t1 has maximum order, t2 is same as t1
t1 = @ccall "libmad_tpsa".mad_tpsa_newd(d::Ptr{Desc{Tpsa,Ctpsa}},255::Cuchar)::Ptr{Tpsa}
t2 = @ccall "libmad_tpsa".mad_tpsa_new(t1::Ptr{Tpsa}, 254::Cuchar)::Ptr{Tpsa}

# set order 0 and 1 (quick and dirty!)
@ccall "libmad_tpsa".mad_tpsa_setv(t1::Ptr{Tpsa}, 0::Cint, (1+6)::Cuint, [pi/6, 1,1,1,1,1,1]::Ptr{Cdouble})::Cvoid
@ccall "libmad_tpsa".mad_tpsa_print(t1::Ptr{Tpsa}, "ini"::Cstring, 0::Cint,0::Cint,0::Cint)::Cvoid

# t2=sin(t1)
@ccall "libmad_tpsa".mad_tpsa_sin(t1::Ptr{Tpsa}, t2::Ptr{Tpsa})::Cvoid
@ccall "libmad_tpsa".mad_tpsa_print(t2::Ptr{Tpsa}, "sin"::Cstring, 0::Cint,0::Cint,0::Cint)::Cvoid
@ccall "libmad_tpsa".mad_tpsa_del(t1::Ptr{Tpsa})::Cvoid

# tpsa functions and operators support aliasing (i.e. src == dst)
@ccall "libmad_tpsa".mad_tpsa_asin(t2::Ptr{Tpsa}, t2::Ptr{Tpsa})::Cvoid                             # asin(x) = -i*ln(i*x + sqrt(1-x^2))
@ccall "libmad_tpsa".mad_tpsa_print(t2::Ptr{Tpsa}, "asin"::Cstring, 0::Cint,0::Cint,0::Cint)::Cvoid # see the accuracy of asin(sin)
@ccall "libmad_tpsa".mad_tpsa_del(t2::Ptr{Tpsa})::Cvoid

# destroy all created descriptors (optional cleanup)
@ccall "libmad_tpsa".mad_desc_del(0::Cint)::Cvoid;

end
