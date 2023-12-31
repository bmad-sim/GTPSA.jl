using Test
using GTPSA

@testset "Arithmetic operators" begin
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

  # Test definition of 1-norm:
  tn = zero(t1)
  tn[0] = 1; tn[1] = 2; tn[2] = 3; tn[3] = 4; tn[4] = 5; tn[5] = 6
  tcn = zero(ct1)
  tcn[0] = 1+1im; tcn[1] = 2+2im; tcn[2] = 3+3im; tcn[3] = 4+4im; tcn[4] = 5+5im; tcn[5] = 6+6im

  @test norm(tn) == sum([i for i in 1:6])
  @test norm(tcn) == sum([abs(i+i*im) for i in 1:6])

  # TPS:
  @test norm(t1 + t2 - t3) < tol
  @test norm(t2 + t1 - t3) < tol
  @test norm(t1 + 2 - t3) < tol
  @test norm(2 + t1 - t3) < tol
  @test norm(t3 - t2 - t1) < tol
  @test norm(t2 - t3 - -t1) < tol
  @test norm(t3 - 2 - t1) < tol
  @test norm(2 - t3 - -t1) < tol
  @test norm(t2 * t3 - 6) < tol
  @test norm(t3 * t2 - 6) < tol
  @test norm(t2 * 5 - 10) < tol
  @test norm(5 * t2 - 10 * t1) < tol
  @test norm(t1 / t2 - 1/2) < tol
  @test norm(t2 / t1 - 2) < tol
  @test norm(1 / t2 - 1/2) < tol
  @test norm(t2 / 3 - 2/3) < tol
  @test norm(t2 / t2 - t1) < tol
  @test norm(t2 / t2 - 1) < tol
  @test norm(t2 ^ t3 - 8) < tol
  @test norm(t3 ^ t2 - 9) < tol
  @test norm(t2 ^ 3 - 8) < tol
  @test norm(t2 ^ (1/2) - sqrt(2)) < tol
  @test norm(t2 ^ (1/2) - sqrt(t2)) < tol
  @test norm(2 ^ t3 - 8) < tol
  @test norm(inv(t3) - 1/t3) < tol
  @test norm(inv(t3) - 1/3) < tol

  # ComplexTPS:
  @test norm(ct1 + ct2 - ct3) < tol
  @test norm(ct2 + ct1 - ct3) < tol
  @test norm(ct1 + (2+2im) - ct3) < tol
  @test norm((2+2im) + ct1 - ct3) < tol
  @test norm(ct3 - ct2 - ct1) < tol
  @test norm(ct2 - ct3 - -ct1) < tol
  @test norm(ct3 - (2+2im) - ct1) < tol
  @test norm((2+2im) - ct3 - -ct1) < tol
  @test norm(ct2 * ct3 - (2+2im)*(3+3im)) < tol
  @test norm(ct3 * ct2 - (2+2im)*(3+3im)) < tol
  @test norm(ct2 * 5 - (10+10im)) < tol
  @test norm(5 * ct2 - (10 * ct1)) < tol
  @test norm(ct1 / ct2 - (1+im)/(2+2im)) < tol
  @test norm(ct2 / ct1 - 2) < tol
  @test norm(1 / ct2 - 1/(2+2im)) < tol
  @test norm(ct2 / 3 - (2+2im)/3) < tol
  @test norm(ct2 / ct2 - 1) < tol
  @test norm(ct2 ^ ct3 - (2+2im)^(3+3im)) < tol
  @test norm(ct3 ^ ct2 - (3+3im)^(2+2im)) < tol
  @test norm(ct2 ^ 3 - (2+2im)^3) < tol
  @test norm(ct2 ^ (1/2) - sqrt(2+2im)) < tol
  @test norm(ct2 ^ (1/2) - sqrt(ct2)) < tol
  @test norm(2 ^ ct3 - 2^(3+3im)) < tol
  @test norm(inv(ct3) - 1/ct3) < tol
  @test norm(inv(ct3) - 1/(3+3im)) < tol

  # Promotion of TPS to ComplexTPS
  @test norm(t1 + ct2 - (1 + (2+2im))) < tol
  @test norm(ct2 + t1 - (1 + (2+2im))) < tol
  @test norm(t1 + (2+2im) - (1 + (2+2im))) < tol
  @test norm((2+2im) + t1 - (1 + (2+2im))) < tol
  @test norm(t3 - ct2 - (3 - (2+2im))) < tol
  @test norm(ct2 - t3 - ((2+2im) - 3)) < tol
  @test norm(t3 - (2+2im) - (3 - (2+2im))) < tol
  @test norm((2+2im) - t3 - ((2+2im) - 3)) < tol
  @test norm(t2 * ct3 - 2 * (3+3im)) < tol
  @test norm(ct3 * t2 - 2 * (3+3im)) < tol
  @test norm(t2 * (3+3im) - 2 * (3+3im)) < tol
  @test norm((3+3im) * t2 - 2 * (3+3im)) < tol
  @test norm(t2 / ct3 - 2/(3+3im)) < tol
  @test norm(ct3 / t2 - (3+3im)/2) < tol
  @test norm(t2 / (3+3im) - 2/(3+3im)) < tol
  @test norm((3+3im) / t2 - (3+3im)/2) < tol
  @test norm(t2 ^ ct3 - 2^(3+3im)) < tol
  @test norm(ct3 ^ t2 - (3+3im)^2) < tol
  @test norm(t2 ^ (3+3im) - 2^(3+3im)) < tol
  @test norm((3+3im)^t2 - (3+3im)^2) < tol
end

@testset "Functions: scalar TPSs vs. Julia scalars" begin
  using SpecialFunctions
  SF = SpecialFunctions
  d = Descriptor(1, 5)
  t = TPS(d)
  v = 0.5
  t[0] = v
  tol = 1e-14
  t1 = TPS(t)
  t1[0] = 1
  t2 = zero(t1)
  t2[0] = 2
  t3 = zero(t1)
  t3[0] = 3


  @test norm(abs(-t) - abs(-v) ) < tol
  @test norm(sqrt(t) - sqrt(v)) < tol
  @test norm(exp(t) - exp(v)) < tol
  @test norm(log(t) - log(v)) < tol
  @test norm(sin(t) - sin(v)) < tol
  @test norm(cos(t) - cos(v)) < tol
  @test norm(tan(t) - tan(v)) < tol
  @test norm(csc(t) - csc(v)) < tol
  @test norm(sec(t) - sec(v)) < tol
  @test norm(cot(t) - cot(v)) < tol
  @test norm(sinc(t) - sinc(v)) < tol
  @test norm(sinh(t) - sinh(v)) < tol
  @test norm(cosh(t) - cosh(v)) < tol
  @test norm(tanh(t) - tanh(v)) < tol
  @test norm(csch(t) - csch(v)) < tol
  @test norm(sech(t) - sech(v)) < tol
  @test norm(coth(t) - coth(v)) < tol
  @test norm(asin(t) - asin(v)) < tol
  @test norm(acos(t) - acos(v)) < tol
  @test norm(atan(t) - atan(v)) < tol
  @test norm(acsc(1/t) - acsc(1/v)) < tol
  @test norm(asec(1/t) - asec(1/v)) < tol
  @test norm(acot(1/t) - acot(1/v)) < tol
  @test norm(asinh(t) - asinh(v)) < tol
  @test norm(acosh(1/t) - acosh(1/v)) < tol
  @test norm(atanh(t) - atanh(v)) < tol
  @test norm(acsch(1/t) - acsch(1/v)) < tol
  @test norm(asech(t) - asech(v)) < tol
  @test norm(acoth(1/t) - acoth(1/v)) < tol
  @test norm(asinc(t/pi) - asin(v)/(v)) < tol
  @test norm(asinhc(t/pi) - asinh(v)/(v)) < tol
  @test norm(zero(t) - zero(v)) < tol
  @test norm(real(t) - real(v)) < tol
  @test norm(imag(t) - imag(v)) < tol
  @test norm(conj(t) - conj(v)) < tol
  @test norm(sinhc(t/pi) - sinh(v)/v) < tol
  @test norm(GTPSA.erf(t) - SF.erf(v)) < tol
  @test norm(GTPSA.erfc(t) - SF.erfc(v)) < tol
  @test norm(-im*GTPSA.erf(t*im) - SF.erfi(v)) < tol
  @test norm(atan(t3,t2) - atan(3,2)) < tol
  @test norm(atan(t3,2) - atan(3,2)) < tol
  @test norm(atan(3,t2) - atan(3,2)) < tol
  @test norm(hypot(t2,t3) - hypot(2,3)) < tol
  @test norm(hypot(2,t3) - hypot(2,3)) < tol
  @test norm(hypot(t2,3) - hypot(2,3)) < tol
  @test norm(hypot(t1,t2,t3) - hypot(1,2,3)) < tol
  @test norm(hypot(1, t2, t3) - hypot(1,2,3)) < tol
  @test norm(hypot(t1, 2, t3) - hypot(1,2,3)) < tol
  @test norm(hypot(t1, t2, 3) - hypot(1,2,3)) < tol
  @test norm(hypot(1, 2, t3) - hypot(1,2,3)) < tol
  @test norm(hypot(1, t2, 3) - hypot(1,2,3)) < tol
  @test norm(hypot(t1, 2, 3) - hypot(1,2,3)) < tol
  @test norm(angle(t2) - angle(2)) < tol
  @test norm(complex(t3) - complex(3)) < tol
  @test norm(complex(t2,t3) - complex(2,3)) < tol
  
  v = 0.5+0.5im
  t = ComplexTPS(t)
  t[0] = v
  ct1 = ComplexTPS(t)
  ct1[0] = 1 + 1im
  ct2 = zero(ct1)
  ct2[0] = 2 + 2im
  ct3 = zero(ct1)
  ct3[0] = 3 + 3im
  @test norm(abs(-t) - abs(-v) ) < tol
  @test norm(sqrt(t) - sqrt(v)) < tol
  @test norm(exp(t) - exp(v)) < tol
  @test norm(log(t) - log(v)) < tol
  @test norm(sin(t) - sin(v)) < tol
  @test norm(cos(t) - cos(v)) < tol
  @test norm(tan(t) - tan(v)) < tol
  @test norm(csc(t) - csc(v)) < tol
  @test norm(sec(t) - sec(v)) < tol
  @test norm(cot(t) - cot(v)) < tol
  @test norm(sinc(t) - sinc(v)) < tol
  @test norm(sinh(t) - sinh(v)) < tol
  @test norm(cosh(t) - cosh(v)) < tol
  @test norm(tanh(t) - tanh(v)) < tol
  @test norm(csch(t) - csch(v)) < tol
  @test norm(sech(t) - sech(v)) < tol
  @test norm(coth(t) - coth(v)) < tol
  #= Uncomment these when inverse trig C code is fixed
  @test norm(asin(t) - asin(v)) < tol
  @test norm(acos(t) - acos(v)) < tol
  @test norm(atan(t) - atan(v)) < tol
  @test norm(acsc(t) - acsc(v)) < tol
  @test norm(asec(t) - asec(v)) < tol
  @test norm(acot(t) - acot(v)) < tol
  @test norm(asinh(t) - asinh(v)) < tol
  @test norm(acosh(t) - acosh(v)) < tol
  @test norm(atanh(t) - atanh(v)) < tol
  @test norm(acsch(t) - acsch(v)) < tol
  @test norm(asech(t) - asech(v)) < tol
  @test norm(acoth(t) - acoth(v)) < tol
  @test norm(asinc(t) - asin(v)/v) < tol
  @test norm(asinhc(t) - asinh(v)/v) < tol
  =#
  @test norm(zero(t) - zero(v)) < tol
  @test norm(real(t) - real(v)) < tol
  @test norm(imag(t) - imag(v)) < tol
  @test norm(conj(t) - conj(v)) < tol
  @test norm(sinhc(t/pi) - sinh(v)/v) < tol
  @test norm(GTPSA.erf(t) - SF.erf(v)) < tol
  @test norm(GTPSA.erfc(t) - SF.erfc(v)) < tol
  @test norm(-im*GTPSA.erf(t*im) - SF.erfi(v)) < tol
  @test norm(hypot(ct2,ct3) - hypot(2+2im,3+3im)) < tol
  @test norm(hypot(2+2im,ct3) - hypot(2+2im,3+3im)) < tol
  @test norm(hypot(ct2,3+3im) - hypot(2+2im,3+3im)) < tol
  @test norm(hypot(ct1,ct2,ct3) - hypot(1+1im,2+2im,3+3im)) < tol
  @test norm(hypot(1+1im, ct2, ct3) - hypot(1+1im,2+2im,3+3im)) < tol
  @test norm(hypot(ct1, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)) < tol
  @test norm(hypot(ct1, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)) < tol
  @test norm(hypot(1+1im, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)) < tol
  @test norm(hypot(1+1im, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)) < tol
  @test norm(hypot(ct1, 2+2im, 3+3im) - hypot(1+1im,2+2im,3+3im)) < tol
  @test norm(angle(ct2) - angle(2+2im)) < tol
  @test norm(complex(ct3) - complex(3+3im)) < tol

  # Hypot, mixing TPS with ComplexTPS
  @test norm(hypot(ct1, ct2, t3) - hypot(1+1im,2+2im,3)) < tol
  @test norm(hypot(ct1, t2, ct3) - hypot(1+1im,2,3+3im)) < tol
  @test norm(hypot(t1, ct2, ct3) - hypot(1,2+2im,3+3im)) < tol
  @test norm(hypot(ct1, t2, t3) - hypot(1+1im,2,3)) < tol
  @test norm(hypot(t1, ct2, t3) - hypot(1,2+2im,3)) < tol
  @test norm(hypot(t1, t2, ct3) - hypot(1,2,3+3im)) < tol
  @test norm(hypot(ct1,t2,3+3im) - hypot(1+1im,2,3+3im)) < tol
  @test norm(hypot(t1, ct2, 3+3im) - hypot(1,2+2im,3+3im)) < tol
  @test norm(hypot(ct1,2+2im,t3) - hypot(1+1im,2+2im,3)) < tol
  @test norm(hypot(t1,2+2im,ct3) - hypot(1,2+2im,3+3im)) < tol
  @test norm(hypot(1+1im,ct2,t3) - hypot(1+1im,2+2im,3)) < tol
  @test norm(hypot(1+1im, t2, ct3) - hypot(1+1im,2,3+3im)) < tol
  @test norm(hypot(t1,t2,3+3im) - hypot(1,2,3+3im)) < tol
  @test norm(hypot(t1,2+2im,t3) - hypot(1,2+2im,3)) < tol
  @test norm(hypot(1+1im,t2,t3) - hypot(1+1im,2,3)) < tol
  @test norm(hypot(t1,2,3+3im) - hypot(1,2,3+3im)) < tol
  @test norm(hypot(1,t2,3+3im) - hypot(1,2,3+3im)) < tol
  @test norm(hypot(1+1im,2,t3) - hypot(1+1im,2,3)) < tol
end

@testset "Functions: identities, using TPSs" begin
  d = Descriptor(1, 5)
  t = TPS(d)
  t[0] = 0.5; t[1] = 2; t[2] = 3; t[3] = 4; t[4] = 5; t[5] = 6

  tol = 1e-10

  @test norm(sin(t)^2+cos(t)^2 - 1) < tol
  @test norm(1/sin(t) - csc(t)) < tol
  @test norm(1/cos(t) - sec(t)) < tol
  @test norm(1/tan(t) - cot(t)) < tol
  @test norm(sin(t)/cos(t) - tan(t)) < tol
  @test norm(cos(2*t) - cos(t)^2 + sin(t)^2) < tol
  @test norm(sec(t)^2 - 1 - tan(t)^2) < tol
  @test norm(sin(t/2) - sqrt((1-cos(t))/2)) < tol
  @test norm(cos(t/2) - sqrt((1+cos(t))/2)) < tol
  @test norm(sqrt(t^2) - abs(t)) < tol
  @test norm(csc(t)^2 - cot(t)^2 - 1) < tol
  @test norm(exp(log(t)) - t) < tol
  @test norm(log(exp(t)) - t) < tol
  @test norm(log(exp(t)) - exp(log(t))) < tol
  @test norm(log(t^2) - 2*log(t)) < tol
  @test norm(5*log(t) - log(t^5)) < tol
  @test norm(t*log(5) - log(5^t)) < tol
  @test norm(sinc(t) - sin(pi*t)/(pi*t)) < tol
  @test norm(sinhc(t/pi) - sinh(t)/t) < tol
  @test norm(exp(im*t) - cos(t) - im*sin(t)) < tol
  @test norm(real(exp(im*t)) - cos(t)) < tol
  @test norm(imag(exp(im*t)) - sin(t)) < tol
  @test norm(sinh(t) - (exp(t) - exp(-t))/2) < tol
  @test norm(cosh(t) - (exp(t) + exp(-t))/2) < tol
  @test norm(tanh(t) - sinh(t)/cosh(t)) < tol
  @test norm(csch(t) - 1/sinh(t)) < tol
  @test norm(sech(t) - 1/cosh(t)) < tol
  @test norm(coth(t) - cosh(t)/sinh(t)) < tol
  @test norm(coth(t) - 1/tanh(t)) < tol
  @test norm(cosh(t)^2 - sinh(t)^2 - 1) < tol
  @test norm(1 - tanh(t)^2 - sech(t)^2) < tol
  @test norm(coth(t)^2 - 1 - csch(t)^2) < tol
  @test norm(asin(sin(t)) - t) < tol
  @test norm(acos(cos(t)) - t) < tol
  @test norm(atan(tan(t)) - t) < tol
  @test norm(acsc(1/t) - asin(t)) < tol
  @test norm(asec(1/t) - acos(t)) < tol
  @test norm(acot(1/t) - atan(t)) < tol
  @test norm(asinh(sinh(t)) - t) < tol
  @test norm(acosh(cosh(t)) - t) < tol
  @test norm(atanh(tanh(t)) - t) < tol
  @test norm(acsch(t) - asinh(1/t)) < tol
  @test norm(asech(t) - acosh(1/t)) < tol
  @test norm(acoth(1/t) - atanh(t)) < tol
  @test norm(asinc(t/pi) - asin(t)/t) < tol
  @test norm(asinhc(t/pi) - asinh(t)/t) < tol
  @test norm(GTPSA.erfc(t) - 1 + GTPSA.erf(t)) < tol
  @test norm(GTPSA.erf(-t) + GTPSA.erf(t)) < tol
  @test norm(angle(t)) < tol
  @test norm(complex(t) - t) < tol
  @test norm(complex(t,t) - (t+im*t)) < tol

  t = ComplexTPS(t)
  t[0] = 0.5+0.5im; t[1] = 2+2im; t[2] = 3+3im; t[3] = 4+4im; t[4] = 5+5im; t[5] = 6+6im
  @test norm(sin(t)^2+cos(t)^2 - 1) < tol
  @test norm(1/sin(t) - csc(t)) < tol
  @test norm(1/cos(t) - sec(t)) < tol
  @test norm(1/tan(t) - cot(t)) < tol
  @test norm(sin(t)/cos(t) - tan(t)) < tol
  @test norm(cos(2*t) - cos(t)^2 + sin(t)^2) < tol
  @test norm(sec(t)^2 - 1 - tan(t)^2) < tol
  @test norm(sin(t/2) - sqrt((1-cos(t))/2)) < tol
  @test norm(cos(t/2) - sqrt((1+cos(t))/2)) < tol
  @test norm(sqrt(t^2) - t) < tol
  @test norm(csc(t)^2 - cot(t)^2 - 1) < tol
  @test norm(exp(log(t)) - t) < tol
  @test norm(log(exp(t)) - t) < tol
  @test norm(log(exp(t)) - exp(log(t))) < tol
  @test norm(log(t^2) - 2*log(t)) < tol
  @test norm(5*log(t) - log(t^5) - 2*pi*im) < tol
  @test norm(t*log(5) - log(5^t)) < tol
  @test norm(sinc(t/pi) - sin(t)/t) < tol
  @test norm(sinhc(t/pi) - sinh(t)/t) < tol
  @test norm(exp(im*t) - cos(t) - im*sin(t)) < tol
  @test norm(sinh(t) - (exp(t) - exp(-t))/2) < tol
  @test norm(cosh(t) - (exp(t) + exp(-t))/2) < tol
  @test norm(tanh(t) - sinh(t)/cosh(t)) < tol
  @test norm(csch(t) - 1/sinh(t)) < tol
  @test norm(sech(t) - 1/cosh(t)) < tol
  @test norm(coth(t) - cosh(t)/sinh(t)) < tol
  @test norm(coth(t) - 1/tanh(t)) < tol
  @test norm(cosh(t)^2 - sinh(t)^2 - 1) < tol
  @test norm(1 - tanh(t)^2 - sech(t)^2) < tol
  @test norm(coth(t)^2 - 1 - csch(t)^2) < tol
  #= Uncomment these when C code corrects for unbounded imaginary domain
  @test norm(asin(sin(t)) - t) < tol
  @test norm(acos(cos(t)) - t) < tol
  @test norm(atan(tan(t)) - t) < tol
  @test norm(acsc(t) - asin(1/t)) < tol
  @test norm(asec(t) - acos(1/t)) < tol
  @test norm(acot(t) - acot(1/t)) < tol
  @test norm(asinh(sinh(t)) - t) < tol
  @test norm(acosh(cosh(t)) - t) < tol
  @test norm(atanh(tanh(t)) - t) < tol
  @test norm(acsch(t) - asinh(1/t)) < tol
  @test norm(asech(t) - acosh(1/t)) < tol
  @test norm(acoth(t) - acoth(1/t)) < tol
  @test norm(asinc(t) - asin(t)/t) < tol
  @test norm(asinhc(t) - asinh(t)/t) < tol
  =#
  @test norm(GTPSA.erfc(t) - 1 + GTPSA.erf(t)) < tol
  @test norm(GTPSA.erf(-t) + GTPSA.erf(t)) < tol
  @test norm(angle(t) - atan(imag(t),real(t))) < tol
  @test norm(complex(t) - t) < tol
end

@testset "Taylor map benchmark against ForwardDiff" begin
  include("../benchmark/taylormap.jl")
  m_GTPSA = benchmark_GTPSA()
  coefs_FD = benchmark_ForwardDiff()
  tol = 1e-12

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

  @test GTPSA.norm(m_FD[1] - m_GTPSA[1]) < tol
  @test GTPSA.norm(m_FD[2] - m_GTPSA[2]) < tol
  @test GTPSA.norm(m_FD[3] - m_GTPSA[3]) < tol
  @test GTPSA.norm(m_FD[4] - m_GTPSA[4]) < tol

end

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