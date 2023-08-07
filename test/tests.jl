include("../src/TPSA.jl")
using .TPSA


function gtpsa_ex0()
  # descriptor for TPSA with 1 variable of order 1 (smallest construction)
  d = new_desc(1, 1)

  # two TPSAs, t1 has maximum order, t2 is same as t1
  t1 = new_TPSA(d, MAD_TPSA_DEFAULT)
  t2 = new_TPSA(t1, MAD_TPSA_SAME)

  # set order 0 to pi/6 and order 1 to 0 (scalar-like)
  set_TPSA!(t1, 0, 1, [pi/6])
  print_TPSA(t1, "ini", 0, 0)

  # t2=sin(t1)
  sin!(t1, t2)
  print_TPSA(t2, "sin", 0, 0)
  del!(t1)

  # tpsa functions and operators support aliasing (i.e. src == dst)
  asin!(t2, t2) # asin(x) = -i*ln(i*x + sqrt(1-x^2))
  print_TPSA(t2, "asin", 0, 0) # see the accuracy of asin(sin)
  del!(t2)

  # destroy all created descriptors (optional cleanup)
  cleanup()
end

function gtpsa_ex1()
  # descriptor for TPSA with 6 variables with maximum order 4
  d = new_desc(6, 4)

  # two TPSAs, t1 has maximum order, t2 is same as t1
  t1 = new_TPSA(d, MAD_TPSA_DEFAULT)
  t2 = new_TPSA(t1, MAD_TPSA_SAME)

  # set order 0 and 1 (quick and dirty!)
  set_TPSA!(t1, 0, 1+6, [pi/6, 1.,1.,1.,1.,1.,1.])
  print_TPSA(t1, "ini", 0, 0)

  # t2=sin(t1)
  sin!(t1, t2)
  print_TPSA(t2, "sin", 0, 0)
  del!(t1)

  # tpsa functions and operators support aliasing (i.e. src == dst)
  asin!(t2, t2) # asin(x) = -i*ln(i*x + sqrt(1-x^2))
  print_TPSA(t2, "asin", 0, 0) # see the accuracy of asin(sin)
  del!(t2)

  # destroy all created descriptors (optional cleanup)
  cleanup()
end

function gtpsa_ex2()
  d = new_desc(4,4,2,3)

  t1 = new_TPSA(d, MAD_TPSA_DEFAULT)
  t2 = new_TPSA(t1, MAD_TPSA_SAME)

  set_TPSA!(t1, 0, 1+6, [pi/6, 1., 1., 1., 1., 1., 1.])
  print_TPSA(t1, "ini", 0, 0)

  sin!(t1, t2)
  print_TPSA(t2, "sin", 0, 0)
  del!(t1)

  asin!(t2, t2)
  print_TPSA(t2, "asin", 0, 0)
  del!(t2)

  cleanup()
end

function gtpsa_ex3()
  d = new_desc(6, 0, 0, 0, [0x3,0x3,0x2,0x2,0x1,0x1])
  
  t1 = new_TPSA(d, MAD_TPSA_DEFAULT)
  t2 = new_TPSA(t1, MAD_TPSA_SAME)

  set_TPSA!(t1, 0, 1+6, [pi/6, 1., 1., 1., 1., 1., 1.])
  print_TPSA(t1, "ini", 0, 0)

  sin!(t1, t2)
  print_TPSA(t2, "sin", 0, 0)
  del!(t1)

  asin!(t2, t2)
  print_TPSA(t2, "asin", 0, 0)
  del!(t2)

  cleanup()
end

function gtpsa_ex4()
  d10 = new_desc(6, 0, 0, 0, [0x10,0x10,0x10,0x10,0x10,0x10])


end

d = new_desc(6, 0, 0, 0, [0x10,0x10,0x10,0x10,0x10,0x10])
t1 = new_TPSA(d, MAD_TPSA_DEFAULT)
set_name(t1, "Test123test456")
set_TPSA!(t1, 0, 1+6, [pi/6, 1., 1., 1., 1., 1., 1.])
print_TPSA(t1,"init",0)
print_TPSA_mad(t1,"init",0,0)
#t = unsafe_load(t1)
#str = t.nam::Ptr{UInt8}
#len = unsafe_load(Ptr{UInt8}(str))
#print(unsafe_string(str + Core.sizeof(UInt8), len))

#gtpsa_ex0()
#gtpsa_ex1()
#gtpsa_ex2()
#gtpsa_ex3()