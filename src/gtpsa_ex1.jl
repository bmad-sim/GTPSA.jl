include("TPSA.jl")
using .TPSA

# descriptor for TPSA with 6 variables with maximum order 4
d  = new_desc(6,4)

# two TPSAs, t1 has maximum order, t2 is same as t1
t1 = new_TPSA(d, mad_tpsa_default)
t2 = new_TPSA(t1, mad_tpsa_same)

# set order 0 and 1 (quick and dirty!)
set_TPSA!(t1, 0, 1+6, [pi/6, 1,1,1,1,1,1])
print_TPSA(t1, "ini", 0, 0)

# t2=sin(t1)
sin!(t1, t2)
print_TPSA(t2, "sin", 0, 0)
del!(t1)

# tpsa functions and operators support aliasing (i.e. src == dst)
asin!(t2,t2) # asin(x) = -i*ln(i*x + sqrt(1-x^2))
print_TPSA(t2, "asin", 0, 0) # see the accuracy of asin(sin)
del!(t2)

# destroy all created descriptors (optional cleanup)
@ccall "libmad_tpsa".mad_desc_del(0::Cint)::Cvoid
