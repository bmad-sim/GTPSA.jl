include("../src/TPSA.jl")
using .TPSA
using Printf


function gtpsa_ex0()
  # descriptor for TPSA with 1 variable of order 1 (smallest construction)
  d = new_desc(1, 1)

  # two TPSAs, t1 has maximum order, t2 is same as t1
  t1 = new_TPSA(d, MAD_TPSA_DEFAULT)
  t2 = new_TPSA(t1, MAD_TPSA_SAME)

  # set order 0 to pi/6 and order 1 to 0 (scalar-like)
  set_TPSA!(t1, 0, 1, [pi/6]) 
  print_TPSA(t1, "ini", 0.0, false, "test0.txt", "w+")

  # t2=sin(t1)
  sin!(t1, t2)
  print_TPSA(t2, "sin", 0.0, false, "test0.txt", "a")
  del!(t1)

  # tpsa functions and operators support aliasing (i.e. src == dst)
  asin!(t2, t2) # asin(x) = -i*ln(i*x + sqrt(1-x^2))
  print_TPSA(t2, "asin",  0.0, false, "test0.txt", "a") # see the accuracy of asin(sin)
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
  print_TPSA(t1, "ini", 0.0, false, "test1.txt", "w+")

  # t2=sin(t1)
  sin!(t1, t2)
  print_TPSA(t2, "sin", 0.0, false, "test1.txt", "a") 
  del!(t1)

  # tpsa functions and operators support aliasing (i.e. src == dst)
  asin!(t2, t2) # asin(x) = -i*ln(i*x + sqrt(1-x^2))
  print_TPSA(t2, "asin", 0.0, false, "test1.txt", "a") # see the accuracy of asin(sin)
  del!(t2)

  # destroy all created descriptors (optional cleanup)
  cleanup()
end

function gtpsa_ex2()
  d = new_desc(4, 4, 2, 3)

  t1 = new_TPSA(d, MAD_TPSA_DEFAULT)
  t2 = new_TPSA(t1, MAD_TPSA_SAME)

  set_TPSA!(t1, 0, 1+6, [pi/6, 1., 1., 1., 1., 1., 1.])
  print_TPSA(t1, "ini", 0.0, false, "test2.txt", "w+") 

  sin!(t1, t2)
  print_TPSA(t2, "sin", 0.0, false, "test2.txt", "a")
  del!(t1)

  asin!(t2, t2)
  print_TPSA(t2, "asin", 0.0, false, "test2.txt", "a")
  del!(t2)

  cleanup()
end

function gtpsa_ex3()
  d = new_desc(6, 0, 0, 0, [3, 3, 2, 2, 1, 1])
  
  t1 = new_TPSA(d, MAD_TPSA_DEFAULT)
  t2 = new_TPSA(t1, MAD_TPSA_SAME)

  set_TPSA!(t1, 0, 1+6, [pi/6, 1., 1., 1., 1., 1., 1.])
  print_TPSA(t1, "ini", 0.0, false, "test3.txt", "w+")

  sin!(t1, t2)
  print_TPSA(t2, "sin", 0.0, false, "test3.txt", "a")
  del!(t1)

  asin!(t2, t2)
  print_TPSA(t2, "asin", 0.0, false, "test3.txt", "a")
  del!(t2)

  cleanup()
end

function gtpsa_ex4()
  d10 = new_desc(6, 0, 0, 0, [10, 10, 10, 10, 10, 10])
  io = open("test4.txt", "w+");
  @printf(io, "d10 length=%4d coefs\n", desc_maxlen(d10, MAD_TPSA_DEFAULT))
  del!(d10)

  d = new_desc(6, 12, 0, 0, [2, 2, 2, 2, 1, 10])
  @printf(io, "d   length=%4d coefs\n", desc_maxlen(d, MAD_TPSA_DEFAULT))
  close(io)

  t1 = new_TPSA(d, MAD_TPSA_DEFAULT)
  t2 = new_TPSA(t1, MAD_TPSA_SAME)

  set_TPSA!(t1, 0, 1+6, [pi/6, 1, 1, 1, 1, 1, 1])
  print_TPSA(t1, "ini", 0.0, false, "test4.txt", "a")

  sin!(t1, t2)
  print_TPSA(t2, "sin", 0.0, false, "test4.txt", "a")
  del!(t1)

  asin!(t2, t2)
  print_TPSA(t2, "asin", 0.0, false, "test4.txt", "a")
  del!(t2)

  cleanup()
end

function gtpsa_ex5()
  d2 = new_desc(100, 2)
  io = open("test5.txt", "w+");
  @printf(io, "d2 length=%4d coefs\n", desc_maxlen(d2, MAD_TPSA_DEFAULT))
  del!(d2)

  d = new_desc(6, 2, 94, 1)
  @printf(io, "d  length=%4d coefs\n", desc_maxlen(d, MAD_TPSA_DEFAULT))
  close(io)

  t1 = new_TPSA(d, MAD_TPSA_DEFAULT)
  t2 = new_TPSA(t1, MAD_TPSA_SAME)

  vec = ones(101)
  vec[1] = pi/6

  set_TPSA!(t1, 0, 101, vec)
  print_TPSA(t1, "ini", 0.0, false, "test5.txt", "a")

  sin!(t1, t2)
  print_TPSA(t2, "sin", 0.0, false, "test5.txt", "a")
  del!(t1)

  asin!(t2, t2)
  print_TPSA(t2, "asin", 0.0, false, "test5.txt", "a")
  del!(t2)

  cleanup()
end

function gtpsa_ex7()
  d = new_desc(2, 63, 1, 1)

  t1 = new_TPSA(d, MAD_TPSA_DEFAULT)
  t2 = new_TPSA(t1, MAD_TPSA_SAME)

  set_TPSA!(t1, 0, 1+3, [pi/6, 1, 1, 1])
  print_TPSA(t1, "ini", 0.0, false, "test7.txt", "w+")

  sin!(t1, t2)
  print_TPSA(t2, "sin", 0.0, false, "test7.txt", "a")
  del!(t1)

  asin!(t2, t2)
  print_TPSA(t2, "asin", 0.0, false, "test7.txt", "a")
  del!(t2)

  cleanup()

end

function gtpsa_ex8()
  d = new_desc(6, 2)

  t1 = new_TPSA(d, 0)

end


#d = new_desc(6, 0, 0, 0, [0x10,0x10,0x10,0x10,0x10,0x10])
#t1 = new_TPSA(d, MAD_TPSA_DEFAULT)
#set_name(t1, "Test123test456")
#set_TPSA!(t1, 0, 1+6, [pi/6, 1., 1., 1., 1., 1., 1.])
#print_TPSA(t1,"init",0,true,"firsttest.txt")
#t = unsafe_load(t1)
#str = t.nam::Ptr{UInt8}
#len = unsafe_load(Ptr{UInt8}(str))
#print(unsafe_string(str + Core.sizeof(UInt8), len))

gtpsa_ex0()
gtpsa_ex1()
gtpsa_ex2()
gtpsa_ex3()
gtpsa_ex4()
gtpsa_ex5()

#gtpsa_ex7()
