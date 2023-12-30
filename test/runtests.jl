using Test
using GTPSA

@testset "Compare with MAD" begin
  include("compare_MAD.jl")
  expected_out = """mad_mono.h downloaded.
  Comparing mad_mono.h to mono.jl...
  mad_desc.h downloaded.
  Comparing mad_desc.h to desc.jl...
  mad_tpsa.h downloaded.
  Comparing mad_tpsa.h to rtpsa.jl...
  mad_tpsa_ordv: Variable in C tpsa_t* ... => ...::Ptr{RTPSA} not equal to Julia ts::Ptr{RTPSA}...
  mad_ctpsa.h downloaded.
  Comparing mad_ctpsa.h to ctpsa.jl...
  mad_ctpsa_ordv: Variable in C ctpsa_t* ... => ...::Ptr{CTPSA} not equal to Julia ts::Ptr{CTPSA}...
  mad_ctpsa_equt: Variable in C num_t tol => tol::Cdouble not equal to Julia tol_::Cdouble
  mad_ctpsa_unit: Variable in C ctpsa_t* x => x::Ptr{CTPSA} not equal to Julia t::Ptr{CTPSA}
  """
  @test compare_MAD() == expected_out
end

@testset "Operators" begin
  d = Descriptor(1, 5)
  t = TPS(d)
  ct = ComplexTPS(t)

  # Basics
  @test t == 0
  @test 0 == t
  @test ct == 0
  @test 0 == ct
  @test ct == t
  @test t == ct
  @test !(t === ct)
  @test t == zero(t)
  @test ct == zero(ct)

  # Set scalar part so both TPSs are 1
  t[0] = 1
  ct[0] = 1

  @test t == 1
  @test ct == 1

  # Check +, - unary functions and real, imag
  @test t == +t
  @test !(t === +t)
  @test -1 == -t
  @test !(t === -t)

  @test ct == +ct
  @test !(ct === +ct)
  @test -1 == -ct
  @test !(ct === -ct)

  @test t == real(t) == ct == real(ct) == 1
  @test imag(t) == imag(ct) == 0
  @test !(t == im) && !(im == t)
  
  # Set ct = im
  ct[0] = im
  @test ct == im
  @test im == ct
  @test real(ct) == 0
  @test imag(ct) == t == 1

  # Now do operators
  t1 = t
  t1[0] = 1
  t2 = zero(t1)
  t2[0] = 2
  t3 = zero(t1)
  t3[0] = 3

  ct1 = ct
  ct1[0] = 1 + 1im
  ct2 = zero(ct1)
  ct2[0] = 2 + 2im
  ct3 = zero(ct1)
  ct3[0] = 3 + 3im

  tol = 1e-14

  # TPS:
  @test TPS_error(t1 + t2 , t3)[0] < tol
  @test TPS_error(t2 + t1 , t3)[0] < tol
  @test TPS_error(t1 + 2 , t3)[0] < tol
  @test TPS_error(2 + t1 , t3)[0] < tol
  @test TPS_error(t3 - t2 , t1)[0] < tol
  @test TPS_error(t2 - t3 , -t1)[0] < tol
  @test TPS_error(t3 - 2 , t1)[0] < tol
  @test TPS_error(2 - t3 , -t1)[0] < tol
  @test TPS_error(t2 * t3 , 6)[0] < tol
  @test TPS_error(t3 * t2 , 6)[0] < tol
  @test TPS_error(t2 * 5 , 10)[0] < tol
  @test TPS_error(5 * t2 , 10 * t1)[0] < tol
  @test TPS_error(t1 / t2 , 1/2)[0] < tol
  @test TPS_error(t2 / t1 , 2)[0] < tol
  @test TPS_error(1 / t2 , 1/2)[0] < tol
  @test TPS_error(t2 / 3 , 2/3)[0] < tol
  @test TPS_error(t2 / t2 , t1)[0] < tol
  @test TPS_error(t2 / t2 , 1)[0] < tol
  @test TPS_error(t2 ^ t3 , 8)[0] < tol
  @test TPS_error(t3 ^ t2 , 9)[0] < tol
  @test TPS_error(t2 ^ 3 , 8)[0] < tol
  @test TPS_error(t2 ^ (1/2) , sqrt(2))[0] < tol
  @test TPS_error(t2 ^ (1/2) , sqrt(t2))[0] < tol
  @test TPS_error(2 ^ t3 , 8)[0] < tol
  @test TPS_error(inv(t3) , 1/t3)[0] < tol
  @test TPS_error(inv(t3) , 1/3)[0] < tol
  @test TPS_error(atan(t3,t2) , atan(3,2))[0] < tol
  @test TPS_error(atan(t3,2) , atan(3,2))[0] < tol
  @test TPS_error(atan(3,t2) , atan(3,2))[0] < tol
  @test TPS_error(hypot(t2,t3) , hypot(2,3))[0] < tol
  @test TPS_error(hypot(2,t3) , hypot(2,3))[0] < tol
  @test TPS_error(hypot(t2,3) , hypot(2,3))[0] < tol

  # ComplexTPS:
  @test TPS_error(ct1 + ct2 , ct3)[0] < tol
  @test TPS_error(ct2 + ct1 , ct3)[0] < tol
  @test TPS_error(ct1 + (2+2im) , ct3)[0] < tol
  @test TPS_error((2+2im) + ct1 , ct3)[0] < tol
  @test TPS_error(ct3 - ct2 , ct1)[0] < tol
  @test TPS_error(ct2 - ct3 , -ct1)[0] < tol
  @test TPS_error(ct3 - (2+2im) , ct1)[0] < tol
  @test TPS_error((2+2im) - ct3 , -ct1)[0] < tol
  @test TPS_error(ct2 * ct3 , (2+2im)*(3+3im))[0] < tol
  @test TPS_error(ct3 * ct2 , (2+2im)*(3+3im))[0] < tol
  @test TPS_error(ct2 * 5 , 10+10im)[0] < tol
  @test TPS_error(5 * ct2 , 10 * ct1)[0] < tol
  @test TPS_error(ct1 / ct2 , (1+im)/(2+2im))[0] < tol
  @test TPS_error(ct2 / ct1 , 2)[0] < tol
  @test TPS_error(1 / ct2 , 1/(2+2im))[0] < tol
  @test TPS_error(ct2 / 3 , (2+2im)/3)[0] < tol
  @test TPS_error(ct2 / ct2 , 1)[0] < tol
  @test TPS_error(ct2 ^ ct3 , (2+2im)^(3+3im))[0] < tol
  @test TPS_error(ct3 ^ ct2 , (3+3im)^(2+2im))[0] < tol
  @test TPS_error(ct2 ^ 3 , (2+2im)^3)[0] < tol
  @test TPS_error(ct2 ^ (1/2) , sqrt(2+2im))[0] < tol
  @test TPS_error(ct2 ^ (1/2) , sqrt(ct2))[0] < tol
  @test TPS_error(2 ^ ct3 , 2^(3+3im))[0] < tol
  @test TPS_error(inv(ct3) , 1/ct3)[0] < tol
  @test TPS_error(inv(ct3) , 1/(3+3im))[0] < tol
  #@test TPS_error(hypot(ct2,ct3) , hypot(2,3)*(1+im))[0] < tol
  #@test TPS_error(hypot(2,ct3) , hypot(2,3)*(1+im))[0] < tol
  #@test TPS_error(hypot(ct2,3) , hypot(2,3)*(1+im))[0] < tol



  

  # Promotion of TPS to ComplexTPS
  @test TPS_error(t1 + ct2 , 1 + (2+2im))[0] < tol
  @test TPS_error(ct2 + t1 , 1 + (2+2im))[0] < tol
  @test TPS_error(t1 + (2+2im) , 1 + (2+2im))[0] < tol
  @test TPS_error((2+2im) + t1 , 1 + (2+2im))[0] < tol
  @test TPS_error(t3 - ct2 , 3 - (2+2im))[0] < tol
  @test TPS_error(ct2 - t3 , (2+2im) - 3)[0] < tol
  @test TPS_error(t3 - (2+2im) , 3 - (2+2im))[0] < tol
  @test TPS_error((2+2im) - t3 , (2+2im) - 3)[0] < tol
  @test TPS_error(t2 * ct3 , 2 * (3+3im))[0] < tol
  @test TPS_error(ct3 * t2 , 2 * (3+3im))[0] < tol
  @test TPS_error(t2 * (3+3im) , 2 * (3+3im))[0] < tol
  @test TPS_error((3+3im) * t2 , 2 * (3+3im))[0] < tol
  @test TPS_error(t2 / ct3 , 2/(3+3im))[0] < tol
  @test TPS_error(ct3 / t2 , (3+3im)/2)[0] < tol
  @test TPS_error(t2 / (3+3im) , 2/(3+3im))[0] < tol
  @test TPS_error((3+3im) / t2 , (3+3im)/2)[0] < tol
  @test TPS_error(t2 ^ ct3 , 2^(3+3im))[0] < tol
  @test TPS_error(ct3 ^ t2 , (3+3im)^2)[0] < tol
  @test TPS_error(t2 ^ (3+3im) , 2^(3+3im))[0] < tol
  @test TPS_error((3+3im)^t2 , (3+3im)^2)[0] < tol
  #@test TPS_error(hypot(t2, ct3) , hypot(2, 3)+3im)[0] < tol
  #@test TPS_error(hypot(ct3, t2) , hypot(3,2)+3im)[0] < tol
  #@test TPS_error(hypot(t2, 3+3im) , hypot(2,3)+3im)[0] < tol
  #@test TPS_error(hypot(3+3im, t2) , hypot(3,2)+3im)[0] < tol
end

@testset "Taylor map benchmark against ForwardDiff" begin
  include("../benchmark/taylormap.jl")
  m_GTPSA = benchmark_GTPSA()
  coefs_FD = benchmark_ForwardDiff()
  tol = 1e-14

  # Create Taylor map to compare with GTPSA from ForwardDiff
  x_FD = zero(m_GTPSA[1])
  px_FD = zero(m_GTPSA[2])
  y_FD = zero(m_GTPSA[3])
  py_FD = zero(m_GTPSA[4])

  m_FD = [x_FD, px_FD, y_FD, py_FD]

  for i=1:length(m_FD)
    t = m_FD[i]
    t[1=>1] = coefs_FD[1 ,i]
    t[2=>1] = coefs_FD[2 ,i]
    t[3=>1] = coefs_FD[3 ,i]
    t[4=>1] = coefs_FD[4 ,i]
    t[params=(1=>1,)] = coefs_FD[5 ,i]
    t[params=(2=>1,)] = coefs_FD[6 ,i]

    t[1=>2] = coefs_FD[7 ,i]
    t[2=>2] = coefs_FD[8 ,i]
    t[3=>2] = coefs_FD[9 ,i]
    t[4=>2] = coefs_FD[10,i]
    t[params=(1=>2,)] = coefs_FD[11,i]
    t[params=(2=>2,)] = coefs_FD[12,i]

    t[1=>1,2=>1] = coefs_FD[13,i]
    t[1=>1,3=>1] = coefs_FD[14,i]
    t[1=>1,4=>1] = coefs_FD[15,i]
    t[1=>1,params=(1=>1,)] = coefs_FD[16,i]
    t[1=>1,params=(2=>1,)] = coefs_FD[17,i]

    t[2=>1,3=>1] = coefs_FD[18,i]
    t[2=>1,4=>1] = coefs_FD[19,i]
    t[2=>1,params=(1=>1,)] = coefs_FD[20,i]
    t[2=>1,params=(2=>1,)] = coefs_FD[21,i]

    t[3=>1,4=>1] = coefs_FD[22,i]
    t[3=>1,params=(1=>1,)] = coefs_FD[23,i]
    t[3=>1,params=(2=>1,)] = coefs_FD[24,i]

    t[4=>1,params=(1=>1,)] = coefs_FD[25,i]
    t[4=>1,params=(2=>1,)] = coefs_FD[26,i]

    t[params=(1=>1,2=>1)] = coefs_FD[27,i]
  end

  @test GTPSA.norm(TPS_error(m_FD[1], m_GTPSA[1])) < tol
  @test GTPSA.norm(TPS_error(m_FD[2], m_GTPSA[2])) < tol
  @test GTPSA.norm(TPS_error(m_FD[3], m_GTPSA[3])) < tol
  @test GTPSA.norm(TPS_error(m_FD[4], m_GTPSA[4])) < tol

  



end