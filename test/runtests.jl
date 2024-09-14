using Test, JET
using SpecialFunctions
using GTPSA
import GTPSA: Desc

@testset "Arithmetic operators" begin
  d = Descriptor(1, 5)
  t = TPS(use=d)
  ct = ComplexTPS64(t)

  # Basics
  @test isequal(t , 0)
  @test isequal(0 , t)
  @test isequal(ct , 0)
  @test isequal(0 , ct)
  @test isequal(ct , t)
  @test isequal(t , ct)
  @test !(t === ct)
  @test isequal(t , zero(t))
  @test isequal(ct , zero(ct))
  @test t == 0
  @test 0 == t
  @test ct == 0
  @test 0 == ct
  @test t == ct
  @test ct == t
  @test t == zero(t)
  @test ct == zero(t)

  # Set scalar part so both TPSs are 1
  t[0] = 1
  ct[0] = 1

  @test isequal(t, 1)
  @test isequal(1, t)
  @test isequal(ct, 1)
  @test isequal(1, ct)
  @test t == 1
  @test 1 == t
  @test ct == 1
  @test 1 == ct

  # Check +, - unary functions and real, imag
  @test isequal(t, +t)
  @test t == +t
  @test (t === +t)
  @test isequal(-1, -t)
  @test -1 == -t
  @test !(t === -t)

  @test isequal(ct, +ct)
  @test ct == +ct
  @test (ct === +ct)
  @test isequal(-1, -ct)
  @test -t == -ct
  @test !(ct === -ct)

  @test isequal(t, real(t))
  @test isequal(real(t), ct)
  @test isequal(ct, real(ct))
  @test isequal(real(ct),1)
  @test isequal(imag(t), imag(ct))
  @test isequal(imag(ct), 0)
  @test !isequal(t,im) && !isequal(im, ct)
  @test t == real(t)
  @test real(t) == ct
  @test ct == real(ct)
  @test real(ct) == 1
  @test imag(t) == imag(ct)
  @test imag(ct) == 0
  @test t != im && ct != im

  @test ct == t
  @test t == ct
  @test ct+im != t
  @test t != ct+im
  @test ct+im == t+im
  @test t+im == ct+im
  
  # Set ct = im
  ct[0] = im
  @test isequal(ct, im)
  @test isequal(im, ct)
  @test isequal(real(ct), 0)
  @test isequal(imag(ct), t)
  @test isequal(t, 1)


  t1 = t
  t1[0] = 1
  t2 = zero(t1)
  t2[0] = 2
  t3 = zero(t1)
  t3[0] = 3

  @test t1 == 1
  @test 1 == t1
  @test t1 != 2
  @test 2 != t1
  @test t1 != t2
  @test t2 == 2
  @test 2 == t2
  @test t3 == t2+t1
  @test t2 < t3
  @test t2 < 3
  @test t2 <= t3
  @test t2 <= 3
  @test t2+t1 <= t3
  @test 3 <= t3

  @test !(t2 > t3)
  @test !(t2 > 3)
  @test !(t2 >= t3)
  @test !(t2 >= 3)

  @test t3 > t2
  @test t3 > 2
  @test t3 >= t2
  @test t3 >= 2
  @test t3 >= t1+t2
  @test t3 >= 3

  @test !(t3 < t2)
  @test !(t3 < 2)
  @test !(t3 <= t2)
  @test !(t3 <= 2)

  ct1 = ct
  ct1[0] = 1 + 1im
  ct2 = zero(ct1)
  ct2[0] = 2 + 2im
  ct3 = zero(ct1)
  ct3[0] = 3 + 3im

  tol = 1e-14
  # Now do operators
  # Test definition of 1-norm:
  tn = zero(t1)
  tn[0] = 1; tn[[1]] = 2; tn[[2]] = 3; tn[[3]] = 4; tn[[4]] = 5; tn[[5]] = 6
  tcn = zero(ct1)
  tcn[0] = 1+1im; tcn[[1]] = 2+2im; tcn[[2]] = 3+3im; tcn[[3]] = 4+4im; tcn[[4]] = 5+5im; tcn[[5]] = 6+6im

  @test normTPS(tn) == sum([i for i in 1:6])
  @test normTPS(tcn) == sum([abs(i+i*im) for i in 1:6])

  # TPS:
  @test normTPS(t1 + t2 - t3) < tol
  @test normTPS(t2 + t1 - t3) < tol
  @test normTPS(t1 + 2 - t3) < tol
  @test normTPS(2 + t1 - t3) < tol
  @test normTPS(t3 - t2 - t1) < tol
  @test normTPS(t2 - t3 - -t1) < tol
  @test normTPS(t3 - 2 - t1) < tol
  @test normTPS(2 - t3 - -t1) < tol
  @test normTPS(t2 * t3 - 6) < tol
  @test normTPS(t3 * t2 - 6) < tol
  @test normTPS(t2 * 5 - 10) < tol
  @test normTPS(5 * t2 - 10 * t1) < tol
  @test normTPS(t1 / t2 - 1/2) < tol
  @test normTPS(t2 / t1 - 2) < tol
  @test normTPS(1 / t2 - 1/2) < tol
  @test normTPS(t2 / 3 - 2/3) < tol
  @test normTPS(t2 / t2 - t1) < tol
  @test normTPS(t2 / t2 - 1) < tol
  @test normTPS(t2 ^ t3 - 8) < tol
  @test normTPS(t3 ^ t2 - 9) < tol
  @test normTPS(t2 ^ 3 - 8) < tol
  @test normTPS(t2 ^ (1/2) - sqrt(2)) < tol
  @test normTPS(t2 ^ (1/2) - sqrt(t2)) < tol
  @test normTPS(2 ^ t3 - 8) < tol
  @test normTPS(inv(t3) - 1/t3) < tol
  @test normTPS(inv(t3) - 1/3) < tol

  # ComplexTPS64:
  @test normTPS(ct1 + ct2 - ct3) < tol
  @test normTPS(ct2 + ct1 - ct3) < tol
  @test normTPS(ct1 + (2+2im) - ct3) < tol
  @test normTPS((2+2im) + ct1 - ct3) < tol
  @test normTPS(ct3 - ct2 - ct1) < tol
  @test normTPS(ct2 - ct3 - -ct1) < tol
  @test normTPS(ct3 - (2+2im) - ct1) < tol
  @test normTPS((2+2im) - ct3 - -ct1) < tol
  @test normTPS(ct2 * ct3 - (2+2im)*(3+3im)) < tol
  @test normTPS(ct3 * ct2 - (2+2im)*(3+3im)) < tol
  @test normTPS(ct2 * 5 - (10+10im)) < tol
  @test normTPS(5 * ct2 - (10 * ct1)) < tol
  @test normTPS(ct1 / ct2 - (1+im)/(2+2im)) < tol
  @test normTPS(ct2 / ct1 - 2) < tol
  @test normTPS(1 / ct2 - 1/(2+2im)) < tol
  @test normTPS(ct2 / 3 - (2+2im)/3) < tol
  @test normTPS(ct2 / ct2 - 1) < tol
  @test normTPS(ct2 ^ ct3 - (2+2im)^(3+3im)) < tol
  @test normTPS(ct3 ^ ct2 - (3+3im)^(2+2im)) < tol
  @test normTPS(ct2 ^ 3 - (2+2im)^3) < tol
  @test normTPS(ct2 ^ (1/2) - sqrt(2+2im)) < tol
  @test normTPS(ct2 ^ (1/2) - sqrt(ct2)) < tol
  @test normTPS(2 ^ ct3 - 2^(3+3im)) < tol
  @test normTPS(inv(ct3) - 1/ct3) < tol
  @test normTPS(inv(ct3) - 1/(3+3im)) < tol

  # Promotion of TPS to ComplexTPS64
  @test normTPS(t1 + ct2 - (1 + (2+2im))) < tol
  @test normTPS(ct2 + t1 - (1 + (2+2im))) < tol
  @test normTPS(t1 + (2+2im) - (1 + (2+2im))) < tol
  @test normTPS((2+2im) + t1 - (1 + (2+2im))) < tol
  @test normTPS(t3 - ct2 - (3 - (2+2im))) < tol
  @test normTPS(ct2 - t3 - ((2+2im) - 3)) < tol
  @test normTPS(t3 - (2+2im) - (3 - (2+2im))) < tol
  @test normTPS((2+2im) - t3 - ((2+2im) - 3)) < tol
  @test normTPS(t2 * ct3 - 2 * (3+3im)) < tol
  @test normTPS(ct3 * t2 - 2 * (3+3im)) < tol
  @test normTPS(t2 * (3+3im) - 2 * (3+3im)) < tol
  @test normTPS((3+3im) * t2 - 2 * (3+3im)) < tol
  @test normTPS(t2 / ct3 - 2/(3+3im)) < tol
  @test normTPS(ct3 / t2 - (3+3im)/2) < tol
  @test normTPS(t2 / (3+3im) - 2/(3+3im)) < tol
  @test normTPS((3+3im) / t2 - (3+3im)/2) < tol
  @test normTPS(t2 ^ ct3 - 2^(3+3im)) < tol
  @test normTPS(ct3 ^ t2 - (3+3im)^2) < tol
  @test normTPS(t2 ^ (3+3im) - 2^(3+3im)) < tol
  @test normTPS((3+3im)^t2 - (3+3im)^2) < tol
end

@testset "Functions: scalar TPSs vs. Julia scalars" begin
  d = Descriptor(1, 5)
  t = TPS(use=d)
  v = 0.5
  t[0] = v
  tol = 1e-14
  t1 = TPS(t)
  t1[0] = 1
  t2 = zero(t1)
  t2[0] = 2
  t3 = zero(t1)
  t3[0] = 3


  @test normTPS(abs(-t) - abs(-v) ) < tol
  @test normTPS(sqrt(t) - sqrt(v)) < tol
  @test normTPS(exp(t) - exp(v)) < tol
  @test normTPS(log(t) - log(v)) < tol
  @test normTPS(sin(t) - sin(v)) < tol
  @test normTPS(cos(t) - cos(v)) < tol
  @test normTPS(tan(t) - tan(v)) < tol
  @test normTPS(csc(t) - csc(v)) < tol
  @test normTPS(sec(t) - sec(v)) < tol
  @test normTPS(cot(t) - cot(v)) < tol
  @test normTPS(sinc(t) - sinc(v)) < tol
  @test normTPS(sinh(t) - sinh(v)) < tol
  @test normTPS(cosh(t) - cosh(v)) < tol
  @test normTPS(tanh(t) - tanh(v)) < tol
  @test normTPS(csch(t) - csch(v)) < tol
  @test normTPS(sech(t) - sech(v)) < tol
  @test normTPS(coth(t) - coth(v)) < tol
  @test normTPS(asin(t) - asin(v)) < tol
  @test normTPS(acos(t) - acos(v)) < tol
  @test normTPS(atan(t) - atan(v)) < tol
  @test normTPS(acsc(1/t) - acsc(1/v)) < tol
  @test normTPS(asec(1/t) - asec(1/v)) < tol
  @test normTPS(acot(1/t) - acot(1/v)) < tol
  @test normTPS(asinh(t) - asinh(v)) < tol
  @test normTPS(acosh(1/t) - acosh(1/v)) < tol
  @test normTPS(atanh(t) - atanh(v)) < tol
  @test normTPS(acsch(1/t) - acsch(1/v)) < tol
  @test normTPS(asech(t) - asech(v)) < tol
  @test normTPS(acoth(1/t) - acoth(1/v)) < tol
  @test normTPS(asinc(t/pi) - asin(v)/(v)) < tol
  @test normTPS(asinhc(t/pi) - asinh(v)/(v)) < tol
  @test normTPS(zero(t) - zero(v)) < tol
  @test normTPS(real(t) - real(v)) < tol
  @test normTPS(imag(t) - imag(v)) < tol
  @test normTPS(conj(t) - conj(v)) < tol
  @test normTPS(sinhc(t/pi) - sinh(v)/v) < tol
  @test normTPS(erf(t) - erf(v)) < tol
  @test normTPS(erfc(t) - erfc(v)) < tol
  @test normTPS(-im*erf(t*im) - erfi(v)) < tol
  
  @test normTPS(atan(t3,t2) - atan(3,2)) < tol
  @test normTPS(atan(t3,2) - atan(3,2)) < tol
  @test normTPS(atan(3,t2) - atan(3,2)) < tol
  @test normTPS(atan(t3,-t2) - atan(3,-2)) < tol
  @test normTPS(atan(t3,-2) - atan(3,-2)) < tol
  @test normTPS(atan(3,-t2) - atan(3,-2)) < tol
  @test normTPS(atan(-t3,-t2) - atan(-3,-2)) < tol
  @test normTPS(atan(-t3,-2) - atan(-3,-2)) < tol
  @test normTPS(atan(-3,-t2) - atan(-3,-2)) < tol
  @test normTPS(atan(-t3,t2) - atan(-3,2)) < tol
  @test normTPS(atan(-t3,2) - atan(-3,2)) < tol
  @test normTPS(atan(-3,t2) - atan(-3,2)) < tol
  
  @test normTPS(hypot(t2,t3) - hypot(2,3)) < tol
  @test normTPS(hypot(2,t3) - hypot(2,3)) < tol
  @test normTPS(hypot(t2,3) - hypot(2,3)) < tol
  @test normTPS(hypot(t1,t2,t3) - hypot(1,2,3)) < tol
  @test normTPS(hypot(1, t2, t3) - hypot(1,2,3)) < tol
  @test normTPS(hypot(t1, 2, t3) - hypot(1,2,3)) < tol
  @test normTPS(hypot(t1, t2, 3) - hypot(1,2,3)) < tol
  @test normTPS(hypot(1, 2, t3) - hypot(1,2,3)) < tol
  @test normTPS(hypot(1, t2, 3) - hypot(1,2,3)) < tol
  @test normTPS(hypot(t1, 2, 3) - hypot(1,2,3)) < tol
  @test normTPS(angle(t2) - angle(2)) < tol
  @test normTPS(angle(-t2) - angle(-2)) < tol
  @test normTPS(complex(t3) - complex(3)) < tol
  @test normTPS(complex(t2,t3) - complex(2,3)) < tol
  @test normTPS(polar(t2) - (abs(2)+im*atan(0,2))) < tol
  @test normTPS(polar(-t1) - (abs(-1)+im*atan(0,-1))) < tol
  @test normTPS(rect(t2) - (2*cos(0) + im*2*sin(0))) < tol
  @test normTPS(rect(-t1) - (-1*cos(0) + im*-1*sin(0))) < tol
  

  v = 0.5+0.5im
  t = ComplexTPS64(t)
  t[0] = v
  ct1 = ComplexTPS64(t)
  ct1[0] = 1 + 1im
  ct2 = zero(ct1)
  ct2[0] = 2 + 2im
  ct3 = zero(ct1)
  ct3[0] = 3 + 3im
  @test normTPS(abs(-t) - abs(-v) ) < tol
  @test normTPS(sqrt(t) - sqrt(v)) < tol
  @test normTPS(exp(t) - exp(v)) < tol
  @test normTPS(log(t) - log(v)) < tol
  @test normTPS(sin(t) - sin(v)) < tol
  @test normTPS(cos(t) - cos(v)) < tol
  @test normTPS(tan(t) - tan(v)) < tol
  @test normTPS(csc(t) - csc(v)) < tol
  @test normTPS(sec(t) - sec(v)) < tol
  @test normTPS(cot(t) - cot(v)) < tol
  @test normTPS(sinc(t) - sinc(v)) < tol
  @test normTPS(sinh(t) - sinh(v)) < tol
  @test normTPS(cosh(t) - cosh(v)) < tol
  @test normTPS(tanh(t) - tanh(v)) < tol
  @test normTPS(csch(t) - csch(v)) < tol
  @test normTPS(sech(t) - sech(v)) < tol
  @test normTPS(coth(t) - coth(v)) < tol

  @test normTPS(asin(t) - asin(v)) < tol
  @test normTPS(acos(t) - acos(v)) < tol
  @test normTPS(atan(t) - atan(v)) < tol
  @test normTPS(acsc(t) - acsc(v)) < tol
  @test normTPS(asec(t) - asec(v)) < tol
  @test normTPS(acot(t) - acot(v)) < tol
  @test normTPS(asinh(t) - asinh(v)) < tol
  @test normTPS(acosh(t) - acosh(v)) < tol
  @test normTPS(atanh(t) - atanh(v)) < tol
  @test normTPS(acsch(t) - acsch(v)) < tol
  @test normTPS(asech(t) - asech(v)) < tol
  @test normTPS(acoth(t) - acoth(v)) < tol
  @test normTPS(asinc(t/pi) - asin(v)/v) < tol
  @test normTPS(asinhc(t/pi) - asinh(v)/v) < tol

  @test normTPS(zero(t) - zero(v)) < tol
  @test normTPS(real(t) - real(v)) < tol
  @test normTPS(imag(t) - imag(v)) < tol
  @test normTPS(conj(t) - conj(v)) < tol
  @test normTPS(sinhc(t/pi) - sinh(v)/v) < tol
  @test normTPS(erf(t) - erf(v)) < tol
  @test normTPS(erfc(t) - erfc(v)) < tol
  @test normTPS(-im*erf(t*im) - erfi(v)) < tol
  @test normTPS(hypot(ct2,ct3) - hypot(2+2im,3+3im)) < tol
  @test normTPS(hypot(2+2im,ct3) - hypot(2+2im,3+3im)) < tol
  @test normTPS(hypot(ct2,3+3im) - hypot(2+2im,3+3im)) < tol
  @test normTPS(hypot(ct1,ct2,ct3) - hypot(1+1im,2+2im,3+3im)) < tol
  @test normTPS(hypot(1+1im, ct2, ct3) - hypot(1+1im,2+2im,3+3im)) < tol
  @test normTPS(hypot(ct1, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)) < tol
  @test normTPS(hypot(ct1, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)) < tol
  @test normTPS(hypot(1+1im, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)) < tol
  @test normTPS(hypot(1+1im, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)) < tol
  @test normTPS(hypot(ct1, 2+2im, 3+3im) - hypot(1+1im,2+2im,3+3im)) < tol
  
  @test normTPS(angle(t2+im*t3) - angle(2+3im)) < tol
  @test normTPS(angle(t2-im*t3) - angle(2-3im)) < tol
  @test normTPS(angle(-t2-im*t3) - angle(-2-3im)) < tol
  @test normTPS(angle(-t2+im*t3) - angle(-2+3im)) < tol
  @test normTPS(angle(ct2) - angle(2+2im)) < tol
  @test normTPS(angle(-ct2) - angle(-2-2im)) < tol
  @test normTPS(complex(ct3) - complex(3+3im)) < tol
  @test normTPS(polar(ct2) - (abs(2+2im)+im*angle(2+2im))) < tol
  @test normTPS(polar(-ct1) - (abs(-1-im)+im*angle(-1-im))) < tol
  @test normTPS(rect(ct2) - (2*cos(2) + im*2*sin(2))) < tol
  @test normTPS(rect(-ct1) - (-1*cos(-1) + im*-1*sin(-1))) < tol
  
  # Hypot, mixing TPS with ComplexTPS64
  @test normTPS(hypot(ct1, ct2, t3) - hypot(1+1im,2+2im,3)) < tol
  @test normTPS(hypot(ct1, t2, ct3) - hypot(1+1im,2,3+3im)) < tol
  @test normTPS(hypot(t1, ct2, ct3) - hypot(1,2+2im,3+3im)) < tol
  @test normTPS(hypot(ct1, t2, t3) - hypot(1+1im,2,3)) < tol
  @test normTPS(hypot(t1, ct2, t3) - hypot(1,2+2im,3)) < tol
  @test normTPS(hypot(t1, t2, ct3) - hypot(1,2,3+3im)) < tol
  @test normTPS(hypot(ct1,t2,3+3im) - hypot(1+1im,2,3+3im)) < tol
  @test normTPS(hypot(t1, ct2, 3+3im) - hypot(1,2+2im,3+3im)) < tol
  @test normTPS(hypot(ct1,2+2im,t3) - hypot(1+1im,2+2im,3)) < tol
  @test normTPS(hypot(t1,2+2im,ct3) - hypot(1,2+2im,3+3im)) < tol
  @test normTPS(hypot(1+1im,ct2,t3) - hypot(1+1im,2+2im,3)) < tol
  @test normTPS(hypot(1+1im, t2, ct3) - hypot(1+1im,2,3+3im)) < tol
  @test normTPS(hypot(t1,t2,3+3im) - hypot(1,2,3+3im)) < tol
  @test normTPS(hypot(t1,2+2im,t3) - hypot(1,2+2im,3)) < tol
  @test normTPS(hypot(1+1im,t2,t3) - hypot(1+1im,2,3)) < tol
  @test normTPS(hypot(t1,2,3+3im) - hypot(1,2,3+3im)) < tol
  @test normTPS(hypot(1,t2,3+3im) - hypot(1,2,3+3im)) < tol
  @test normTPS(hypot(1+1im,2,t3) - hypot(1+1im,2,3)) < tol
end

@testset "Functions: identities, using TPSs" begin
  d = Descriptor(1, 5)
  t = TPS(use=d)
  t[0] = 0.5; t[[1]] = 2; t[[2]] = 3; t[[3]] = 4; t[[4]] = 5; t[[5]] = 6

  tol = 1e-10

  @test normTPS(sin(t)^2+cos(t)^2 - 1) < tol
  @test normTPS(1/sin(t) - csc(t)) < tol
  @test normTPS(1/cos(t) - sec(t)) < tol
  @test normTPS(1/tan(t) - cot(t)) < tol
  @test normTPS(sin(t)/cos(t) - tan(t)) < tol
  @test normTPS(cos(2*t) - cos(t)^2 + sin(t)^2) < tol
  @test normTPS(sec(t)^2 - 1 - tan(t)^2) < tol
  @test normTPS(sin(t/2) - sqrt((1-cos(t))/2)) < tol
  @test normTPS(cos(t/2) - sqrt((1+cos(t))/2)) < tol
  @test normTPS(sqrt(t^2) - abs(t)) < tol
  @test normTPS(csc(t)^2 - cot(t)^2 - 1) < tol
  @test normTPS(exp(log(t)) - t) < tol
  @test normTPS(log(exp(t)) - t) < tol
  @test normTPS(log(exp(t)) - exp(log(t))) < tol
  @test normTPS(log(t^2) - 2*log(t)) < tol
  @test normTPS(5*log(t) - log(t^5)) < tol
  @test normTPS(t*log(5) - log(5^t)) < tol
  @test normTPS(sinc(t) - sin(pi*t)/(pi*t)) < tol
  @test normTPS(sinhc(t/pi) - sinh(t)/t) < tol
  @test normTPS(exp(im*t) - cos(t) - im*sin(t)) < tol
  @test normTPS(real(exp(im*t)) - cos(t)) < tol
  @test normTPS(imag(exp(im*t)) - sin(t)) < tol
  @test normTPS(sinh(t) - (exp(t) - exp(-t))/2) < tol
  @test normTPS(cosh(t) - (exp(t) + exp(-t))/2) < tol
  @test normTPS(tanh(t) - sinh(t)/cosh(t)) < tol
  @test normTPS(csch(t) - 1/sinh(t)) < tol
  @test normTPS(sech(t) - 1/cosh(t)) < tol
  @test normTPS(coth(t) - cosh(t)/sinh(t)) < tol
  @test normTPS(coth(t) - 1/tanh(t)) < tol
  @test normTPS(cosh(t)^2 - sinh(t)^2 - 1) < tol
  @test normTPS(1 - tanh(t)^2 - sech(t)^2) < tol
  @test normTPS(coth(t)^2 - 1 - csch(t)^2) < tol
  @test normTPS(asin(sin(t)) - t) < tol
  @test normTPS(acos(cos(t)) - t) < tol
  @test normTPS(atan(tan(t)) - t) < tol
  @test normTPS(acsc(1/t) - asin(t)) < tol
  @test normTPS(asec(1/t) - acos(t)) < tol
  @test normTPS(acot(1/t) - atan(t)) < tol
  @test normTPS(asinh(sinh(t)) - t) < tol
  @test normTPS(acosh(cosh(t)) - t) < tol
  @test normTPS(atanh(tanh(t)) - t) < tol
  @test normTPS(acsch(t) - asinh(1/t)) < tol
  @test normTPS(asech(t) - acosh(1/t)) < tol
  @test normTPS(acoth(1/t) - atanh(t)) < tol
  @test normTPS(asinc(t/pi) - asin(t)/t) < tol
  @test normTPS(asinhc(t/pi) - asinh(t)/t) < tol
  @test normTPS(erfc(t) - 1 + erf(t)) < tol
  @test normTPS(erf(-t) + erf(t)) < tol
  @test normTPS(angle(t)) < tol
  @test normTPS(complex(t) - t) < tol
  @test normTPS(complex(t,t) - (t+im*t)) < tol

  t = ComplexTPS64(t)
  t[0] = 0.5+0.5im; t[[1]] = 2+2im; t[[2]] = 3+3im; t[[3]] = 4+4im; t[[4]] = 5+5im; t[[5]] = 6+6im
  @test normTPS(sin(t)^2+cos(t)^2 - 1) < tol
  @test normTPS(1/sin(t) - csc(t)) < tol
  @test normTPS(1/cos(t) - sec(t)) < tol
  @test normTPS(1/tan(t) - cot(t)) < tol
  @test normTPS(sin(t)/cos(t) - tan(t)) < tol
  @test normTPS(cos(2*t) - cos(t)^2 + sin(t)^2) < tol
  @test normTPS(sec(t)^2 - 1 - tan(t)^2) < tol
  @test normTPS(sin(t/2) - sqrt((1-cos(t))/2)) < tol
  @test normTPS(cos(t/2) - sqrt((1+cos(t))/2)) < tol
  @test normTPS(sqrt(t^2) - t) < tol
  @test normTPS(csc(t)^2 - cot(t)^2 - 1) < tol
  @test normTPS(exp(log(t)) - t) < tol
  @test normTPS(log(exp(t)) - t) < tol
  @test normTPS(log(exp(t)) - exp(log(t))) < tol
  @test normTPS(log(t^2) - 2*log(t)) < tol
  @test normTPS(5*log(t) - log(t^5) - 2*pi*im) < tol
  @test normTPS(t*log(5) - log(5^t)) < tol
  @test normTPS(sinc(t/pi) - sin(t)/t) < tol
  @test normTPS(sinhc(t/pi) - sinh(t)/t) < tol
  @test normTPS(exp(im*t) - cos(t) - im*sin(t)) < tol
  @test normTPS(sinh(t) - (exp(t) - exp(-t))/2) < tol
  @test normTPS(cosh(t) - (exp(t) + exp(-t))/2) < tol
  @test normTPS(tanh(t) - sinh(t)/cosh(t)) < tol
  @test normTPS(csch(t) - 1/sinh(t)) < tol
  @test normTPS(sech(t) - 1/cosh(t)) < tol
  @test normTPS(coth(t) - cosh(t)/sinh(t)) < tol
  @test normTPS(coth(t) - 1/tanh(t)) < tol
  @test normTPS(cosh(t)^2 - sinh(t)^2 - 1) < tol
  @test normTPS(1 - tanh(t)^2 - sech(t)^2) < tol
  @test normTPS(coth(t)^2 - 1 - csch(t)^2) < tol
  
  @test normTPS(asin(sin(t)) - t) < tol
  @test normTPS(acos(cos(t)) - t) < tol
  @test normTPS(atan(tan(t)) - t) < tol
  @test normTPS(acsc(t) - asin(1/t)) < tol
  @test normTPS(asec(t) - acos(1/t)) < tol
  @test normTPS(acot(t) - atan(1/t)) < tol
  @test normTPS(asinh(sinh(t)) - t) < tol
  @test normTPS(acosh(cosh(t)) - t) < tol
  @test normTPS(atanh(tanh(t)) - t) < tol
  @test normTPS(acsch(t) - asinh(1/t)) < tol
  @test normTPS(asech(t) - acosh(1/t)) < tol
  @test normTPS(acoth(t) - atanh(1/t)) < tol
  @test normTPS(asinc(t/pi) - asin(t)/t) < tol
  @test normTPS(asinhc(t/pi) - asinh(t)/t) < tol
  
  @test normTPS(erfc(t) - 1 + erf(t)) < tol
  @test normTPS(erf(-t) + erf(t)) < tol
  @test normTPS(angle(t) - atan(imag(t),real(t))) < tol
  @test normTPS(complex(t) - t) < tol
end

@testset "Indexing" begin
  d = Descriptor(3,10,2,10)
  v = vars(d)
  p = params(d)
  tol = 1e-18

  f = sin(v[1])
  @test abs(f[[0]] - 0) < tol
  @test abs(f[[1]] - 1) < tol
  @test abs(f[[2]] - 0) < tol
  @test abs(f[[3]] - -1/factorial(3)) < tol
  @test abs(f[[4]] - 0) < tol
  @test abs(f[[5]] - 1/factorial(5)) < tol
  @test abs(f[[6]] - 0) < tol
  @test abs(f[[7]] - -1/factorial(7)) < tol
  @test abs(f[[8]] - 0) < tol
  @test abs(f[[9]] - 1/factorial(9)) < tol
  @test abs(f[[10]] - 0) < tol

  @test abs(f[[1=>1]] - f[[1]]) < tol
  @test abs(f[[1=>2]] - f[[2]]) < tol
  @test abs(f[[1=>3]] - f[[3]]) < tol
  @test abs(f[[1=>4]] - f[[4]]) < tol
  @test abs(f[[1=>5]] - f[[5]]) < tol
  @test abs(f[[1=>6]] - f[[6]]) < tol
  @test abs(f[[1=>7]] - f[[7]]) < tol
  @test abs(f[[1=>8]] - f[[8]]) < tol
  @test abs(f[[1=>9]] - f[[9]]) < tol
  @test abs(f[[1=>10]] - f[[10]]) < tol

  fc = complex(f)
  @test abs(fc[[0]] - 0) < tol
  @test abs(fc[[1]] - 1) < tol
  @test abs(fc[[2]] - 0) < tol
  @test abs(fc[[3]] - -1/factorial(3)) < tol
  @test abs(fc[[4]] - 0) < tol
  @test abs(fc[[5]] - 1/factorial(5)) < tol
  @test abs(fc[[6]] - 0) < tol
  @test abs(fc[[7]] - -1/factorial(7)) < tol
  @test abs(fc[[8]] - 0) < tol
  @test abs(fc[[9]] - 1/factorial(9)) < tol
  @test abs(fc[[10]] - 0) < tol

  @test abs(fc[[1=>1]] - f[[1]]) < tol
  @test abs(fc[[1=>2]] - f[[2]]) < tol
  @test abs(fc[[1=>3]] - f[[3]]) < tol
  @test abs(fc[[1=>4]] - f[[4]]) < tol
  @test abs(fc[[1=>5]] - f[[5]]) < tol
  @test abs(fc[[1=>6]] - f[[6]]) < tol
  @test abs(fc[[1=>7]] - f[[7]]) < tol
  @test abs(fc[[1=>8]] - f[[8]]) < tol
  @test abs(fc[[1=>9]] - f[[9]]) < tol
  @test abs(fc[[1=>10]] - f[[10]]) < tol

  f2 = sin(v[1]) + cos(v[2])
  @test abs(f2[[0]] - 1) < tol
  @test abs(f2[[1]] - 1) < tol
  @test abs(f2[[2]] - 0) < tol
  @test abs(f2[[3]] - -1/factorial(3)) < tol
  @test abs(f2[[4]] - 0) < tol
  @test abs(f2[[5]] - 1/factorial(5)) < tol
  @test abs(f2[[6]] - 0) < tol
  @test abs(f2[[7]] - -1/factorial(7)) < tol
  @test abs(f2[[8]] - 0) < tol
  @test abs(f2[[9]] - 1/factorial(9)) < tol
  @test abs(f2[[10]] - 0) < tol

  @test abs(f2[[0,0]] - 1) < tol
  @test abs(f2[[0,1]] - 0) < tol
  @test abs(f2[[0,2]] - -1/factorial(2)) < tol
  @test abs(f2[[0,3]] - 0) < tol
  @test abs(f2[[0,4]] - 1/factorial(4)) < tol
  @test abs(f2[[0,5]] - 0) < tol
  @test abs(f2[[0,6]] - -1/factorial(6)) < tol
  @test abs(f2[[0,7]] - 0) < tol
  @test abs(f2[[0,8]] - 1/factorial(8)) < tol
  @test abs(f2[[0,9]] - 0) < tol
  @test abs(f2[[0,10]] - -1/factorial(10)) < tol

  @test abs(f2[[1=>1]] - f2[[1]]) < tol
  @test abs(f2[[1=>2]] - f2[[2]]) < tol
  @test abs(f2[[1=>3]] - f2[[3]]) < tol
  @test abs(f2[[1=>4]] - f2[[4]]) < tol
  @test abs(f2[[1=>5]] - f2[[5]]) < tol
  @test abs(f2[[1=>6]] - f2[[6]]) < tol
  @test abs(f2[[1=>7]] - f2[[7]]) < tol
  @test abs(f2[[1=>8]] - f2[[8]]) < tol
  @test abs(f2[[1=>9]] - f2[[9]]) < tol
  @test abs(f2[[1=>10]] - f2[[10]]) < tol

  @test abs(f2[[2=>1]] - f2[[0,1]]) < tol
  @test abs(f2[[2=>2]] - f2[[0,2]]) < tol
  @test abs(f2[[2=>3]] - f2[[0,3]]) < tol
  @test abs(f2[[2=>4]] - f2[[0,4]]) < tol
  @test abs(f2[[2=>5]] - f2[[0,5]]) < tol
  @test abs(f2[[2=>6]] - f2[[0,6]]) < tol
  @test abs(f2[[2=>7]] - f2[[0,7]]) < tol
  @test abs(f2[[2=>8]] - f2[[0,8]]) < tol
  @test abs(f2[[2=>9]] - f2[[0,9]]) < tol
  @test abs(f2[[2=>10]] - f2[[0,10]]) < tol

  f2c = complex(sin(v[1]) + cos(v[2]))
  @test abs(f2c[[0]] - 1) < tol
  @test abs(f2c[[1]] - 1) < tol
  @test abs(f2c[[2]] - 0) < tol
  @test abs(f2c[[3]] - -1/factorial(3)) < tol
  @test abs(f2c[[4]] - 0) < tol
  @test abs(f2c[[5]] - 1/factorial(5)) < tol
  @test abs(f2c[[6]] - 0) < tol
  @test abs(f2c[[7]] - -1/factorial(7)) < tol
  @test abs(f2c[[8]] - 0) < tol
  @test abs(f2c[[9]] - 1/factorial(9)) < tol
  @test abs(f2c[[10]] - 0) < tol

  @test abs(f2c[[0,0]] - 1) < tol
  @test abs(f2c[[0,1]] - 0) < tol
  @test abs(f2c[[0,2]] - -1/factorial(2)) < tol
  @test abs(f2c[[0,3]] - 0) < tol
  @test abs(f2c[[0,4]] - 1/factorial(4)) < tol
  @test abs(f2c[[0,5]] - 0) < tol
  @test abs(f2c[[0,6]] - -1/factorial(6)) < tol
  @test abs(f2c[[0,7]] - 0) < tol
  @test abs(f2c[[0,8]] - 1/factorial(8)) < tol
  @test abs(f2c[[0,9]] - 0) < tol
  @test abs(f2c[[0,10]] - -1/factorial(10)) < tol

  @test abs(f2c[[1=>1]] - f2c[[1]]) < tol
  @test abs(f2c[[1=>2]] - f2c[[2]]) < tol
  @test abs(f2c[[1=>3]] - f2c[[3]]) < tol
  @test abs(f2c[[1=>4]] - f2c[[4]]) < tol
  @test abs(f2c[[1=>5]] - f2c[[5]]) < tol
  @test abs(f2c[[1=>6]] - f2c[[6]]) < tol
  @test abs(f2c[[1=>7]] - f2c[[7]]) < tol
  @test abs(f2c[[1=>8]] - f2c[[8]]) < tol
  @test abs(f2c[[1=>9]] - f2c[[9]]) < tol
  @test abs(f2c[[1=>10]] - f2c[[10]]) < tol

  @test abs(f2c[[2=>1]] - f2c[[0,1]]) < tol
  @test abs(f2c[[2=>2]] - f2c[[0,2]]) < tol
  @test abs(f2c[[2=>3]] - f2c[[0,3]]) < tol
  @test abs(f2c[[2=>4]] - f2c[[0,4]]) < tol
  @test abs(f2c[[2=>5]] - f2c[[0,5]]) < tol
  @test abs(f2c[[2=>6]] - f2c[[0,6]]) < tol
  @test abs(f2c[[2=>7]] - f2c[[0,7]]) < tol
  @test abs(f2c[[2=>8]] - f2c[[0,8]]) < tol
  @test abs(f2c[[2=>9]] - f2c[[0,9]]) < tol
  @test abs(f2c[[2=>10]] - f2c[[0,10]]) < tol

  f3 = sin(v[1]) + cos(v[2]) + exp(p[1])
  @test abs(f3[[0]] - 2) < tol
  @test abs(f3[[1]] - 1) < tol
  @test abs(f3[[2]] - 0) < tol
  @test abs(f3[[3]] - -1/factorial(3)) < tol
  @test abs(f3[[4]] - 0) < tol
  @test abs(f3[[5]] - 1/factorial(5)) < tol
  @test abs(f3[[6]] - 0) < tol
  @test abs(f3[[7]] - -1/factorial(7)) < tol
  @test abs(f3[[8]] - 0) < tol
  @test abs(f3[[9]] - 1/factorial(9)) < tol
  @test abs(f3[[10]] - 0) < tol

  @test abs(f3[[0,0]] - 2) < tol
  @test abs(f3[[0,1]] - 0) < tol
  @test abs(f3[[0,2]] - -1/factorial(2)) < tol
  @test abs(f3[[0,3]] - 0) < tol
  @test abs(f3[[0,4]] - 1/factorial(4)) < tol
  @test abs(f3[[0,5]] - 0) < tol
  @test abs(f3[[0,6]] - -1/factorial(6)) < tol
  @test abs(f3[[0,7]] - 0) < tol
  @test abs(f3[[0,8]] - 1/factorial(8)) < tol
  @test abs(f3[[0,9]] - 0) < tol
  @test abs(f3[[0,10]] - -1/factorial(10)) < tol

  @test abs(f3[[0,0,0,0]] - 2) < tol
  @test abs(f3[[0,0,0,1]] - 1/factorial(1)) < tol
  @test abs(f3[[0,0,0,2]] - 1/factorial(2)) < tol
  @test abs(f3[[0,0,0,3]] - 1/factorial(3)) < tol
  @test abs(f3[[0,0,0,4]] - 1/factorial(4)) < tol
  @test abs(f3[[0,0,0,5]] - 1/factorial(5)) < tol
  @test abs(f3[[0,0,0,6]] - 1/factorial(6)) < tol
  @test abs(f3[[0,0,0,7]] - 1/factorial(7)) < tol
  @test abs(f3[[0,0,0,8]] - 1/factorial(8)) < tol
  @test abs(f3[[0,0,0,9]] - 1/factorial(9)) < tol
  @test abs(f3[[0,0,0,10]] - 1/factorial(10)) < tol

  @test abs(f3[[1=>1]] - f3[[1]]) < tol
  @test abs(f3[[1=>2]] - f3[[2]]) < tol
  @test abs(f3[[1=>3]] - f3[[3]]) < tol
  @test abs(f3[[1=>4]] - f3[[4]]) < tol
  @test abs(f3[[1=>5]] - f3[[5]]) < tol
  @test abs(f3[[1=>6]] - f3[[6]]) < tol
  @test abs(f3[[1=>7]] - f3[[7]]) < tol
  @test abs(f3[[1=>8]] - f3[[8]]) < tol
  @test abs(f3[[1=>9]] - f3[[9]]) < tol
  @test abs(f3[[1=>10]] - f3[[10]]) < tol

  @test abs(f3[[2=>1]]- f3[[0,1]]) < tol
  @test abs(f3[[2=>2]]- f3[[0,2]]) < tol
  @test abs(f3[[2=>3]]- f3[[0,3]]) < tol
  @test abs(f3[[2=>4]]- f3[[0,4]]) < tol
  @test abs(f3[[2=>5]]- f3[[0,5]]) < tol
  @test abs(f3[[2=>6]]- f3[[0,6]]) < tol
  @test abs(f3[[2=>7]]- f3[[0,7]]) < tol
  @test abs(f3[[2=>8]]- f3[[0,8]]) < tol
  @test abs(f3[[2=>9]]- f3[[0,9]]) < tol
  @test abs(f3[[2=>10]]- f3[[0,10]]) < tol

  @test abs(f3[params=[1=>1]] - f3[[0,0,0,1]]) < tol
  @test abs(f3[params=[1=>2]] - f3[[0,0,0,2]]) < tol
  @test abs(f3[params=[1=>3]] - f3[[0,0,0,3]]) < tol
  @test abs(f3[params=[1=>4]] - f3[[0,0,0,4]]) < tol
  @test abs(f3[params=[1=>5]] - f3[[0,0,0,5]]) < tol
  @test abs(f3[params=[1=>6]] - f3[[0,0,0,6]]) < tol
  @test abs(f3[params=[1=>7]] - f3[[0,0,0,7]]) < tol
  @test abs(f3[params=[1=>8]] - f3[[0,0,0,8]]) < tol
  @test abs(f3[params=[1=>9]] - f3[[0,0,0,9]]) < tol
  @test abs(f3[params=[1=>10]] - f3[[0,0,0,10]]) < tol



end

@testset "FastGTPSA - Arithmetic operators" begin
  d = Descriptor(1, 5)
  t = TPS(use=d)
  ct = ComplexTPS64(t)
  # Set scalar part so both TPSs are 1
  t[0] = 1
  ct[0] = 1
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
  @test @FastGTPSA(normTPS(t1 + t2 - t3)) < tol
  @test @FastGTPSA(normTPS(t2 + t1 - t3)) < tol
  @test @FastGTPSA(normTPS(t1 + 2 - t3)) < tol
  @test @FastGTPSA(normTPS(2 + t1 - t3)) < tol
  @test @FastGTPSA(normTPS(t3 - t2 - t1)) < tol
  @test @FastGTPSA(normTPS(t2 - t3 - -t1)) < tol
  @test @FastGTPSA(normTPS(t3 - 2 - t1)) < tol
  @test @FastGTPSA(normTPS(2 - t3 - -t1)) < tol
  @test @FastGTPSA(normTPS(t2 * t3 - 6)) < tol
  @test @FastGTPSA(normTPS(t3 * t2 - 6)) < tol
  @test @FastGTPSA(normTPS(t2 * 5 - 10)) < tol
  @test @FastGTPSA(normTPS(5 * t2 - 10 * t1)) < tol
  @test @FastGTPSA(normTPS(t1 / t2 - 1/2)) < tol
  @test @FastGTPSA(normTPS(t2 / t1 - 2)) < tol
  @test @FastGTPSA(normTPS(1 / t2 - 1/2)) < tol
  @test @FastGTPSA(normTPS(t2 / 3 - 2/3)) < tol
  @test @FastGTPSA(normTPS(t2 / t2 - t1)) < tol
  @test @FastGTPSA(normTPS(t2 / t2 - 1)) < tol
  @test @FastGTPSA(normTPS(t2 ^ t3 - 8)) < tol
  @test @FastGTPSA(normTPS(t3 ^ t2 - 9)) < tol
  @test @FastGTPSA(normTPS(t2 ^ 3 - 8)) < tol
  @test @FastGTPSA(normTPS(t2 ^ (1/2) - sqrt(2))) < tol
  @test @FastGTPSA(normTPS(t2 ^ (1/2) - sqrt(t2))) < tol
  @test @FastGTPSA(normTPS(2 ^ t3 - 8)) < tol
  @test @FastGTPSA(normTPS(inv(t3) - 1/t3)) < tol
  @test @FastGTPSA(normTPS(inv(t3) - 1/3)) < tol

  # ComplexTPS64:
  @test @FastGTPSA(normTPS(ct1 + ct2 - ct3)) < tol
  @test @FastGTPSA(normTPS(ct2 + ct1 - ct3)) < tol
  @test @FastGTPSA(normTPS(ct1 + (2+2im) - ct3)) < tol
  @test @FastGTPSA(normTPS((2+2im) + ct1 - ct3)) < tol
  @test @FastGTPSA(normTPS(ct3 - ct2 - ct1)) < tol
  @test @FastGTPSA(normTPS(ct2 - ct3 - -ct1)) < tol
  @test @FastGTPSA(normTPS(ct3 - (2+2im) - ct1)) < tol
  @test @FastGTPSA(normTPS((2+2im) - ct3 - -ct1)) < tol
  @test @FastGTPSA(normTPS(ct2 * ct3 - (2+2im)*(3+3im))) < tol
  @test @FastGTPSA(normTPS(ct3 * ct2 - (2+2im)*(3+3im))) < tol
  @test @FastGTPSA(normTPS(ct2 * 5 - (10+10im))) < tol
  @test @FastGTPSA(normTPS(5 * ct2 - (10 * ct1))) < tol
  @test @FastGTPSA(normTPS(ct1 / ct2 - (1+im)/(2+2im))) < tol
  @test @FastGTPSA(normTPS(ct2 / ct1 - 2)) < tol
  @test @FastGTPSA(normTPS(1 / ct2 - 1/(2+2im))) < tol
  @test @FastGTPSA(normTPS(ct2 / 3 - (2+2im)/3)) < tol
  @test @FastGTPSA(normTPS(ct2 / ct2 - 1)) < tol
  @test @FastGTPSA(normTPS(ct2 ^ ct3 - (2+2im)^(3+3im))) < tol
  @test @FastGTPSA(normTPS(ct3 ^ ct2 - (3+3im)^(2+2im))) < tol
  @test @FastGTPSA(normTPS(ct2 ^ 3 - (2+2im)^3)) < tol
  @test @FastGTPSA(normTPS(ct2 ^ (1/2) - sqrt(2+2im))) < tol
  @test @FastGTPSA(normTPS(ct2 ^ (1/2) - sqrt(ct2))) < tol
  @test @FastGTPSA(normTPS(2 ^ ct3 - 2^(3+3im))) < tol
  @test @FastGTPSA(normTPS(inv(ct3) - 1/ct3)) < tol
  @test @FastGTPSA(normTPS(inv(ct3) - 1/(3+3im))) < tol

  # Promotion of TPS to ComplexTPS64
  @test @FastGTPSA(normTPS(t1 + ct2 - (1 + (2+2im)))) < tol
  @test @FastGTPSA(normTPS(ct2 + t1 - (1 + (2+2im)))) < tol
  @test @FastGTPSA(normTPS(t1 + (2+2im) - (1 + (2+2im)))) < tol
  @test @FastGTPSA(normTPS((2+2im) + t1 - (1 + (2+2im)))) < tol
  @test @FastGTPSA(normTPS(t3 - ct2 - (3 - (2+2im)))) < tol
  @test @FastGTPSA(normTPS(ct2 - t3 - ((2+2im) - 3))) < tol
  @test @FastGTPSA(normTPS(t3 - (2+2im) - (3 - (2+2im)))) < tol
  @test @FastGTPSA(normTPS((2+2im) - t3 - ((2+2im) - 3))) < tol
  @test @FastGTPSA(normTPS(t2 * ct3 - 2 * (3+3im))) < tol
  @test @FastGTPSA(normTPS(ct3 * t2 - 2 * (3+3im))) < tol
  @test @FastGTPSA(normTPS(t2 * (3+3im) - 2 * (3+3im))) < tol
  @test @FastGTPSA(normTPS((3+3im) * t2 - 2 * (3+3im))) < tol
  @test @FastGTPSA(normTPS(t2 / ct3 - 2/(3+3im))) < tol
  @test @FastGTPSA(normTPS(ct3 / t2 - (3+3im)/2)) < tol
  @test @FastGTPSA(normTPS(t2 / (3+3im) - 2/(3+3im))) < tol
  @test @FastGTPSA(normTPS((3+3im) / t2 - (3+3im)/2)) < tol
  @test @FastGTPSA(normTPS(t2 ^ ct3 - 2^(3+3im))) < tol
  @test @FastGTPSA(normTPS(ct3 ^ t2 - (3+3im)^2)) < tol
  @test @FastGTPSA(normTPS(t2 ^ (3+3im) - 2^(3+3im))) < tol
  @test @FastGTPSA(normTPS((3+3im)^t2 - (3+3im)^2)) < tol

  out = ComplexTPS64()
  # TPS:
  @test (@FastGTPSA!(out = t1 + t2 - t3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 + t1 - t3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t1 + 2 - t3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 2 + t1 - t3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t3 - t2 - t1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 - t3 - -t1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t3 - 2 - t1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 2 - t3 - -t1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 * t3 - 6); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t3 * t2 - 6); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 * 5 - 10); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 5 * t2 - 10 * t1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t1 / t2 - 1/2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 / t1 - 2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 1 / t2 - 1/2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 / 3 - 2/3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 / t2 - t1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 / t2 - 1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 ^ t3 - 8); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t3 ^ t2 - 9); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 ^ 3 - 8); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 ^ (1/2) - sqrt(2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 ^ (1/2) - sqrt(t2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 2 ^ t3 - 8); normTPS(out)) < tol
  @test (@FastGTPSA!(out = inv(t3) - 1/t3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = inv(t3) - 1/3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct1 + ct2 - ct3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 + ct1 - ct3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct1 + (2+2im) - ct3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = (2+2im) + ct1 - ct3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct3 - ct2 - ct1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 - ct3 - -ct1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct3 - (2+2im) - ct1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = (2+2im) - ct3 - -ct1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 * ct3 - (2+2im)*(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct3 * ct2 - (2+2im)*(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 * 5 - (10+10im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 5 * ct2 - (10 * ct1)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct1 / ct2 - (1+im)/(2+2im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 / ct1 - 2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 1 / ct2 - 1/(2+2im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 / 3 - (2+2im)/3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 / ct2 - 1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 ^ ct3 - (2+2im)^(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct3 ^ ct2 - (3+3im)^(2+2im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 ^ 3 - (2+2im)^3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 ^ (1/2) - sqrt(2+2im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 ^ (1/2) - sqrt(ct2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 2 ^ ct3 - 2^(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = inv(ct3) - 1/ct3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = inv(ct3) - 1/(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t1 + ct2 - (1 + (2+2im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 + t1 - (1 + (2+2im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t1 + (2+2im) - (1 + (2+2im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = (2+2im) + t1 - (1 + (2+2im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t3 - ct2 - (3 - (2+2im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 - t3 - ((2+2im) - 3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t3 - (2+2im) - (3 - (2+2im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = (2+2im) - t3 - ((2+2im) - 3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 * ct3 - 2 * (3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct3 * t2 - 2 * (3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 * (3+3im) - 2 * (3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = (3+3im) * t2 - 2 * (3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 / ct3 - 2/(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct3 / t2 - (3+3im)/2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 / (3+3im) - 2/(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = (3+3im) / t2 - (3+3im)/2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 ^ ct3 - 2^(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct3 ^ t2 - (3+3im)^2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 ^ (3+3im) - 2^(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = (3+3im)^t2 - (3+3im)^2); normTPS(out)) < tol

  # Make sure stack is 0:
  @test GTPSA.checktemps()
end

@testset "FastGTPSA - Functions: scalar TPSs vs. Julia scalars" begin
  d = Descriptor(1, 5)
  t = TPS(use=d)
  v = 0.5
  t[0] = v
  tol = 1e-14
  t1 = TPS(t)
  t1[0] = 1
  t2 = zero(t1)
  t2[0] = 2
  t3 = zero(t1)
  t3[0] = 3


  @test @FastGTPSA(normTPS(abs(-t) - abs(-v) )) < tol
  @test @FastGTPSA(normTPS(sqrt(t) - sqrt(v))) < tol
  @test @FastGTPSA(normTPS(exp(t) - exp(v))) < tol
  @test @FastGTPSA(normTPS(log(t) - log(v))) < tol
  @test @FastGTPSA(normTPS(sin(t) - sin(v))) < tol
  @test @FastGTPSA(normTPS(cos(t) - cos(v))) < tol
  @test @FastGTPSA(normTPS(tan(t) - tan(v))) < tol
  @test @FastGTPSA(normTPS(csc(t) - csc(v))) < tol
  @test @FastGTPSA(normTPS(sec(t) - sec(v))) < tol
  @test @FastGTPSA(normTPS(cot(t) - cot(v))) < tol
  @test @FastGTPSA(normTPS(sinc(t) - sinc(v))) < tol
  @test @FastGTPSA(normTPS(sinh(t) - sinh(v))) < tol
  @test @FastGTPSA(normTPS(cosh(t) - cosh(v))) < tol
  @test @FastGTPSA(normTPS(tanh(t) - tanh(v))) < tol
  @test @FastGTPSA(normTPS(csch(t) - csch(v))) < tol
  @test @FastGTPSA(normTPS(sech(t) - sech(v))) < tol
  @test @FastGTPSA(normTPS(coth(t) - coth(v))) < tol
  @test @FastGTPSA(normTPS(asin(t) - asin(v))) < tol
  @test @FastGTPSA(normTPS(acos(t) - acos(v))) < tol
  @test @FastGTPSA(normTPS(atan(t) - atan(v))) < tol
  @test @FastGTPSA(normTPS(acsc(1/t) - acsc(1/v))) < tol
  @test @FastGTPSA(normTPS(asec(1/t) - asec(1/v))) < tol
  @test @FastGTPSA(normTPS(acot(1/t) - acot(1/v))) < tol
  @test @FastGTPSA(normTPS(asinh(t) - asinh(v))) < tol
  @test @FastGTPSA(normTPS(acosh(1/t) - acosh(1/v))) < tol
  @test @FastGTPSA(normTPS(atanh(t) - atanh(v))) < tol
  @test @FastGTPSA(normTPS(acsch(1/t) - acsch(1/v))) < tol
  @test @FastGTPSA(normTPS(asech(t) - asech(v))) < tol
  @test @FastGTPSA(normTPS(acoth(1/t) - acoth(1/v))) < tol
  @test @FastGTPSA(normTPS(asinc(t/pi) - asin(v)/(v))) < tol
  @test @FastGTPSA(normTPS(asinhc(t/pi) - asinh(v)/(v))) < tol
  @test @FastGTPSA(normTPS(zero(t) - zero(v))) < tol
  @test @FastGTPSA(normTPS(real(t) - real(v))) < tol
  @test @FastGTPSA(normTPS(imag(t) - imag(v))) < tol
  @test @FastGTPSA(normTPS(conj(t) - conj(v))) < tol
  @test @FastGTPSA(normTPS(sinhc(t/pi) - sinh(v)/v)) < tol
  @test @FastGTPSA(normTPS(erf(t) - erf(v))) < tol
  @test @FastGTPSA(normTPS(erfc(t) - erfc(v))) < tol
  @test @FastGTPSA(normTPS(-im*erf(t*im) - erfi(v))) < tol
  @test @FastGTPSA(normTPS(atan(t3,t2) - atan(3,2))) < tol
  @test @FastGTPSA(normTPS(atan(t3,2) - atan(3,2))) < tol
  @test @FastGTPSA(normTPS(atan(3,t2) - atan(3,2))) < tol
  @test @FastGTPSA(normTPS(atan(t3,-t2) - atan(3,-2))) < tol
  @test @FastGTPSA(normTPS(atan(t3,-2) - atan(3,-2))) < tol
  @test @FastGTPSA(normTPS(atan(3,-t2) - atan(3,-2))) < tol
  @test @FastGTPSA(normTPS(atan(-t3,-t2) - atan(-3,-2))) < tol
  @test @FastGTPSA(normTPS(atan(-t3,-2) - atan(-3,-2))) < tol
  @test @FastGTPSA(normTPS(atan(-3,-t2) - atan(-3,-2))) < tol
  @test @FastGTPSA(normTPS(atan(-t3,t2) - atan(-3,2))) < tol
  @test @FastGTPSA(normTPS(atan(-t3,2) - atan(-3,2))) < tol
  @test @FastGTPSA(normTPS(atan(-3,t2) - atan(-3,2))) < tol
  
  @test @FastGTPSA(normTPS(hypot(t2,t3) - hypot(2,3))) < tol
  @test @FastGTPSA(normTPS(hypot(2,t3) - hypot(2,3))) < tol
  @test @FastGTPSA(normTPS(hypot(t2,3) - hypot(2,3))) < tol
  @test @FastGTPSA(normTPS(hypot(t1,t2,t3) - hypot(1,2,3))) < tol
  @test @FastGTPSA(normTPS(hypot(1, t2, t3) - hypot(1,2,3))) < tol
  @test @FastGTPSA(normTPS(hypot(t1, 2, t3) - hypot(1,2,3))) < tol
  @test @FastGTPSA(normTPS(hypot(t1, t2, 3) - hypot(1,2,3))) < tol
  @test @FastGTPSA(normTPS(hypot(1, 2, t3) - hypot(1,2,3))) < tol
  @test @FastGTPSA(normTPS(hypot(1, t2, 3) - hypot(1,2,3))) < tol
  @test @FastGTPSA(normTPS(hypot(t1, 2, 3) - hypot(1,2,3))) < tol
  
  @test @FastGTPSA(normTPS(angle(t2) - angle(2))) < tol
  @test @FastGTPSA(normTPS(angle(-t2) - angle(-2))) < tol
  @test @FastGTPSA(normTPS(complex(t3) - complex(3))) < tol
  @test @FastGTPSA(normTPS(complex(t2,t3) - complex(2,3))) < tol
  @test @FastGTPSA(normTPS(polar(t2) - (abs(2)+im*atan(0,2)))) < tol
  @test @FastGTPSA(normTPS(polar(-t1) - (abs(-1)+im*atan(0,-1)))) < tol
  @test @FastGTPSA(normTPS(rect(t2) - (2*cos(0) + im*2*sin(0)))) < tol
  @test @FastGTPSA(normTPS(rect(-t1) - (-1*cos(0) + im*-1*sin(0)))) < tol
  

  v = 0.5+0.5im
  t = ComplexTPS64(t)
  t[0] = v
  ct1 = ComplexTPS64(t)
  ct1[0] = 1 + 1im
  ct2 = zero(ct1)
  ct2[0] = 2 + 2im
  ct3 = zero(ct1)
  ct3[0] = 3 + 3im
  @test @FastGTPSA(normTPS(abs(-t) - abs(-v) )) < tol
  @test @FastGTPSA(normTPS(sqrt(t) - sqrt(v))) < tol
  @test @FastGTPSA(normTPS(exp(t) - exp(v))) < tol
  @test @FastGTPSA(normTPS(log(t) - log(v))) < tol
  @test @FastGTPSA(normTPS(sin(t) - sin(v))) < tol
  @test @FastGTPSA(normTPS(cos(t) - cos(v))) < tol
  @test @FastGTPSA(normTPS(tan(t) - tan(v))) < tol
  @test @FastGTPSA(normTPS(csc(t) - csc(v))) < tol
  @test @FastGTPSA(normTPS(sec(t) - sec(v))) < tol
  @test @FastGTPSA(normTPS(cot(t) - cot(v))) < tol
  @test @FastGTPSA(normTPS(sinc(t) - sinc(v))) < tol
  @test @FastGTPSA(normTPS(sinh(t) - sinh(v))) < tol
  @test @FastGTPSA(normTPS(cosh(t) - cosh(v))) < tol
  @test @FastGTPSA(normTPS(tanh(t) - tanh(v))) < tol
  @test @FastGTPSA(normTPS(csch(t) - csch(v))) < tol
  @test @FastGTPSA(normTPS(sech(t) - sech(v))) < tol
  @test @FastGTPSA(normTPS(coth(t) - coth(v))) < tol

  @test @FastGTPSA(normTPS(asin(t) - asin(v))) < tol
  @test @FastGTPSA(normTPS(acos(t) - acos(v))) < tol
  @test @FastGTPSA(normTPS(atan(t) - atan(v))) < tol
  @test @FastGTPSA(normTPS(acsc(t) - acsc(v))) < tol
  @test @FastGTPSA(normTPS(asec(t) - asec(v))) < tol
  @test @FastGTPSA(normTPS(acot(t) - acot(v))) < tol
  @test @FastGTPSA(normTPS(asinh(t) - asinh(v))) < tol
  @test @FastGTPSA(normTPS(acosh(t) - acosh(v))) < tol
  @test @FastGTPSA(normTPS(atanh(t) - atanh(v))) < tol
  @test @FastGTPSA(normTPS(acsch(t) - acsch(v))) < tol
  @test @FastGTPSA(normTPS(asech(t) - asech(v))) < tol
  @test @FastGTPSA(normTPS(acoth(t) - acoth(v))) < tol
  @test @FastGTPSA(normTPS(asinc(t/pi) - asin(v)/v)) < tol
  @test @FastGTPSA(normTPS(asinhc(t/pi) - asinh(v)/v)) < tol
  
  @test @FastGTPSA(normTPS(zero(t) - zero(v))) < tol
  @test @FastGTPSA(normTPS(real(t) - real(v))) < tol
  @test @FastGTPSA(normTPS(imag(t) - imag(v))) < tol
  @test @FastGTPSA(normTPS(conj(t) - conj(v))) < tol
  @test @FastGTPSA(normTPS(sinhc(t/pi) - sinh(v)/v)) < tol
  @test @FastGTPSA(normTPS(erf(t) - erf(v))) < tol
  @test @FastGTPSA(normTPS(erfc(t) - erfc(v))) < tol
  @test @FastGTPSA(normTPS(-im*erf(t*im) - erfi(v))) < tol
  @test @FastGTPSA(normTPS(hypot(ct2,ct3) - hypot(2+2im,3+3im))) < tol
  @test @FastGTPSA(normTPS(hypot(2+2im,ct3) - hypot(2+2im,3+3im))) < tol
  @test @FastGTPSA(normTPS(hypot(ct2,3+3im) - hypot(2+2im,3+3im))) < tol
  @test @FastGTPSA(normTPS(hypot(ct1,ct2,ct3) - hypot(1+1im,2+2im,3+3im))) < tol
  @test @FastGTPSA(normTPS(hypot(1+1im, ct2, ct3) - hypot(1+1im,2+2im,3+3im))) < tol
  @test @FastGTPSA(normTPS(hypot(ct1, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im))) < tol
  @test @FastGTPSA(normTPS(hypot(ct1, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im))) < tol
  @test @FastGTPSA(normTPS(hypot(1+1im, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im))) < tol
  @test @FastGTPSA(normTPS(hypot(1+1im, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im))) < tol
  @test @FastGTPSA(normTPS(hypot(ct1, 2+2im, 3+3im) - hypot(1+1im,2+2im,3+3im))) < tol
  
  @test @FastGTPSA(normTPS(angle(t2+im*t3) - angle(2+3im))) < tol
  @test @FastGTPSA(normTPS(angle(t2-im*t3) - angle(2-3im))) < tol
  @test @FastGTPSA(normTPS(angle(-t2-im*t3) - angle(-2-3im))) < tol
  @test @FastGTPSA(normTPS(angle(-t2+im*t3) - angle(-2+3im))) < tol
  @test @FastGTPSA(normTPS(angle(ct2) - angle(2+2im))) < tol
  @test @FastGTPSA(normTPS(angle(-ct2) - angle(-2-2im))) < tol
  @test @FastGTPSA(normTPS(complex(ct3) - complex(3+3im))) < tol
  @test @FastGTPSA(normTPS(polar(ct2) - (abs(2+2im)+im*angle(2+2im)))) < tol
  @test @FastGTPSA(normTPS(polar(-ct1) - (abs(-1-im)+im*angle(-1-im)))) < tol
  @test @FastGTPSA(normTPS(rect(ct2) - (2*cos(2) + im*2*sin(2)))) < tol
  @test @FastGTPSA(normTPS(rect(-ct1) - (-1*cos(-1) + im*-1*sin(-1)))) < tol
  
  # Hypot, mixing TPS with ComplexTPS64
  @test @FastGTPSA(normTPS(hypot(ct1, ct2, t3) - hypot(1+1im,2+2im,3))) < tol
  @test @FastGTPSA(normTPS(hypot(ct1, t2, ct3) - hypot(1+1im,2,3+3im))) < tol
  @test @FastGTPSA(normTPS(hypot(t1, ct2, ct3) - hypot(1,2+2im,3+3im))) < tol
  @test @FastGTPSA(normTPS(hypot(ct1, t2, t3) - hypot(1+1im,2,3))) < tol
  @test @FastGTPSA(normTPS(hypot(t1, ct2, t3) - hypot(1,2+2im,3))) < tol
  @test @FastGTPSA(normTPS(hypot(t1, t2, ct3) - hypot(1,2,3+3im))) < tol
  @test @FastGTPSA(normTPS(hypot(ct1,t2,3+3im) - hypot(1+1im,2,3+3im))) < tol
  @test @FastGTPSA(normTPS(hypot(t1, ct2, 3+3im) - hypot(1,2+2im,3+3im))) < tol
  @test @FastGTPSA(normTPS(hypot(ct1,2+2im,t3) - hypot(1+1im,2+2im,3))) < tol
  @test @FastGTPSA(normTPS(hypot(t1,2+2im,ct3) - hypot(1,2+2im,3+3im))) < tol
  @test @FastGTPSA(normTPS(hypot(1+1im,ct2,t3) - hypot(1+1im,2+2im,3))) < tol
  @test @FastGTPSA(normTPS(hypot(1+1im, t2, ct3) - hypot(1+1im,2,3+3im))) < tol
  @test @FastGTPSA(normTPS(hypot(t1,t2,3+3im) - hypot(1,2,3+3im))) < tol
  @test @FastGTPSA(normTPS(hypot(t1,2+2im,t3) - hypot(1,2+2im,3))) < tol
  @test @FastGTPSA(normTPS(hypot(1+1im,t2,t3) - hypot(1+1im,2,3))) < tol
  @test @FastGTPSA(normTPS(hypot(t1,2,3+3im) - hypot(1,2,3+3im))) < tol
  @test @FastGTPSA(normTPS(hypot(1,t2,3+3im) - hypot(1,2,3+3im))) < tol
  @test @FastGTPSA(normTPS(hypot(1+1im,2,t3) - hypot(1+1im,2,3))) < tol



  d = Descriptor(1, 5)
  t = TPS(use=d)
  v = 0.5
  t[0] = v
  tol = 1e-14
  t1 = TPS(t)
  t1[0] = 1
  t2 = zero(t1)
  t2[0] = 2
  t3 = zero(t1)
  t3[0] = 3
  out = ComplexTPS64()

  @test (@FastGTPSA!(out = abs(-t) - abs(-v) ); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sqrt(t) - sqrt(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = exp(t) - exp(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = log(t) - log(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sin(t) - sin(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = cos(t) - cos(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = tan(t) - tan(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = csc(t) - csc(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sec(t) - sec(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = cot(t) - cot(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sinc(t) - sinc(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sinh(t) - sinh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = cosh(t) - cosh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = tanh(t) - tanh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = csch(t) - csch(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sech(t) - sech(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = coth(t) - coth(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asin(t) - asin(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acos(t) - acos(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = atan(t) - atan(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acsc(1/t) - acsc(1/v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asec(1/t) - asec(1/v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acot(1/t) - acot(1/v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asinh(t) - asinh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acosh(1/t) - acosh(1/v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = atanh(t) - atanh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acsch(1/t) - acsch(1/v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asech(t) - asech(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acoth(1/t) - acoth(1/v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asinc(t/pi) - asin(v)/(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asinhc(t/pi) - asinh(v)/(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = zero(t) - zero(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = real(t) - real(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = imag(t) - imag(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = conj(t) - conj(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sinhc(t/pi) - sinh(v)/v); normTPS(out)) < tol
  @test (@FastGTPSA!(out = erf(t) - erf(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = erfc(t) - erfc(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = -im*erf(t*im) - erfi(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = atan(t3,t2) - atan(3,2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = atan(t3,2) - atan(3,2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = atan(3,t2) - atan(3,2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = atan(t3,-t2) - atan(3,-2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = atan(t3,-2) - atan(3,-2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = atan(3,-t2) - atan(3,-2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = atan(-t3,-t2) - atan(-3,-2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = atan(-t3,-2) - atan(-3,-2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = atan(-3,-t2) - atan(-3,-2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = atan(-t3,t2) - atan(-3,2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = atan(-t3,2) - atan(-3,2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = atan(-3,t2) - atan(-3,2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(t2,t3) - hypot(2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(2,t3) - hypot(2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(t2,3) - hypot(2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(t1,t2,t3) - hypot(1,2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(1, t2, t3) - hypot(1,2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(t1, 2, t3) - hypot(1,2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(t1, t2, 3) - hypot(1,2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(1, 2, t3) - hypot(1,2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(1, t2, 3) - hypot(1,2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(t1, 2, 3) - hypot(1,2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = angle(t2) - angle(2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = angle(-t2) - angle(-2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = complex(t3) - complex(3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = complex(t2,t3) - complex(2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = polar(t2) - (abs(2)+im*atan(0,2))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = polar(-t1) - (abs(-1)+im*atan(0,-1))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = rect(t2) - (2*cos(0) + im*2*sin(0))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = rect(-t1) - (-1*cos(0) + im*-1*sin(0))); normTPS(out)) < tol


  v = 0.5+0.5im
  t = ComplexTPS64(t)
  t[0] = v
  ct1 = ComplexTPS64(t)
  ct1[0] = 1 + 1im
  ct2 = zero(ct1)
  ct2[0] = 2 + 2im
  ct3 = zero(ct1)
  ct3[0] = 3 + 3im
  @test (@FastGTPSA!(out = abs(-t) - abs(-v) ); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sqrt(t) - sqrt(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = exp(t) - exp(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = log(t) - log(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sin(t) - sin(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = cos(t) - cos(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = tan(t) - tan(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = csc(t) - csc(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sec(t) - sec(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = cot(t) - cot(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sinc(t) - sinc(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sinh(t) - sinh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = cosh(t) - cosh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = tanh(t) - tanh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = csch(t) - csch(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sech(t) - sech(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = coth(t) - coth(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asin(t) - asin(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acos(t) - acos(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = atan(t) - atan(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acsc(t) - acsc(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asec(t) - asec(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acot(t) - acot(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asinh(t) - asinh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acosh(t) - acosh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = atanh(t) - atanh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acsch(t) - acsch(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asech(t) - asech(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acoth(t) - acoth(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asinc(t/pi) - asin(v)/v); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asinhc(t/pi) - asinh(v)/v); normTPS(out)) < tol
  @test (@FastGTPSA!(out = zero(t) - zero(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = real(t) - real(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = imag(t) - imag(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = conj(t) - conj(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sinhc(t/pi) - sinh(v)/v); normTPS(out)) < tol
  @test (@FastGTPSA!(out = erf(t) - erf(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = erfc(t) - erfc(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = -im*erf(t*im) - erfi(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(ct2,ct3) - hypot(2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(2+2im,ct3) - hypot(2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(ct2,3+3im) - hypot(2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(ct1,ct2,ct3) - hypot(1+1im,2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(1+1im, ct2, ct3) - hypot(1+1im,2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(ct1, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(ct1, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(1+1im, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(1+1im, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(ct1, 2+2im, 3+3im) - hypot(1+1im,2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = angle(t2+im*t3) - angle(2+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = angle(t2-im*t3) - angle(2-3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = angle(-t2-im*t3) - angle(-2-3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = angle(-t2+im*t3) - angle(-2+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = angle(ct2) - angle(2+2im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = angle(-ct2) - angle(-2-2im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = complex(ct3) - complex(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = polar(ct2) - (abs(2+2im)+im*angle(2+2im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = polar(-ct1) - (abs(-1-im)+im*angle(-1-im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = rect(ct2) - (2*cos(2) + im*2*sin(2))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = rect(-ct1) - (-1*cos(-1) + im*-1*sin(-1))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(ct1, ct2, t3) - hypot(1+1im,2+2im,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(ct1, t2, ct3) - hypot(1+1im,2,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(t1, ct2, ct3) - hypot(1,2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(ct1, t2, t3) - hypot(1+1im,2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(t1, ct2, t3) - hypot(1,2+2im,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(t1, t2, ct3) - hypot(1,2,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(ct1,t2,3+3im) - hypot(1+1im,2,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(t1, ct2, 3+3im) - hypot(1,2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(ct1,2+2im,t3) - hypot(1+1im,2+2im,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(t1,2+2im,ct3) - hypot(1,2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(1+1im,ct2,t3) - hypot(1+1im,2+2im,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(1+1im, t2, ct3) - hypot(1+1im,2,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(t1,t2,3+3im) - hypot(1,2,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(t1,2+2im,t3) - hypot(1,2+2im,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(1+1im,t2,t3) - hypot(1+1im,2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(t1,2,3+3im) - hypot(1,2,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(1,t2,3+3im) - hypot(1,2,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = hypot(1+1im,2,t3) - hypot(1+1im,2,3)); normTPS(out)) < tol

  # Make sure stack is 0:
  @test GTPSA.checktemps()
end

@testset "FastGTPSA - Functions: identities, using TPSs" begin
  d = Descriptor(1, 5)
  t = TPS(use=d)
  t[0] = 0.5; t[[1]] = 2; t[[2]] = 3; t[[3]] = 4; t[[4]] = 5; t[[5]] = 6

  tol = 1e-10

  @test @FastGTPSA(normTPS(sin(t)^2+cos(t)^2 - 1)) < tol
  @test @FastGTPSA(normTPS(1/sin(t) - csc(t))) < tol
  @test @FastGTPSA(normTPS(1/cos(t) - sec(t))) < tol
  @test @FastGTPSA(normTPS(1/tan(t) - cot(t))) < tol
  @test @FastGTPSA(normTPS(sin(t)/cos(t) - tan(t))) < tol
  @test @FastGTPSA(normTPS(cos(2*t) - cos(t)^2 + sin(t)^2)) < tol
  @test @FastGTPSA(normTPS(sec(t)^2 - 1 - tan(t)^2)) < tol
  @test @FastGTPSA(normTPS(sin(t/2) - sqrt((1-cos(t))/2))) < tol
  @test @FastGTPSA(normTPS(cos(t/2) - sqrt((1+cos(t))/2))) < tol
  @test @FastGTPSA(normTPS(sqrt(t^2) - abs(t))) < tol
  @test @FastGTPSA(normTPS(csc(t)^2 - cot(t)^2 - 1)) < tol
  @test @FastGTPSA(normTPS(exp(log(t)) - t)) < tol
  @test @FastGTPSA(normTPS(log(exp(t)) - t)) < tol
  @test @FastGTPSA(normTPS(log(exp(t)) - exp(log(t)))) < tol
  @test @FastGTPSA(normTPS(log(t^2) - 2*log(t))) < tol
  @test @FastGTPSA(normTPS(5*log(t) - log(t^5))) < tol
  @test @FastGTPSA(normTPS(t*log(5) - log(5^t))) < tol
  @test @FastGTPSA(normTPS(sinc(t) - sin(pi*t)/(pi*t))) < tol
  @test @FastGTPSA(normTPS(sinhc(t/pi) - sinh(t)/t)) < tol
  @test @FastGTPSA(normTPS(exp(im*t) - cos(t) - im*sin(t))) < tol
  @test @FastGTPSA(normTPS(real(exp(im*t)) - cos(t))) < tol
  @test @FastGTPSA(normTPS(imag(exp(im*t)) - sin(t))) < tol
  @test @FastGTPSA(normTPS(sinh(t) - (exp(t) - exp(-t))/2)) < tol
  @test @FastGTPSA(normTPS(cosh(t) - (exp(t) + exp(-t))/2)) < tol
  @test @FastGTPSA(normTPS(tanh(t) - sinh(t)/cosh(t))) < tol
  @test @FastGTPSA(normTPS(csch(t) - 1/sinh(t))) < tol
  @test @FastGTPSA(normTPS(sech(t) - 1/cosh(t))) < tol
  @test @FastGTPSA(normTPS(coth(t) - cosh(t)/sinh(t))) < tol
  @test @FastGTPSA(normTPS(coth(t) - 1/tanh(t))) < tol
  @test @FastGTPSA(normTPS(cosh(t)^2 - sinh(t)^2 - 1)) < tol
  @test @FastGTPSA(normTPS(1 - tanh(t)^2 - sech(t)^2)) < tol
  @test @FastGTPSA(normTPS(coth(t)^2 - 1 - csch(t)^2)) < tol
  @test @FastGTPSA(normTPS(asin(sin(t)) - t)) < tol
  @test @FastGTPSA(normTPS(acos(cos(t)) - t)) < tol
  @test @FastGTPSA(normTPS(atan(tan(t)) - t)) < tol
  @test @FastGTPSA(normTPS(acsc(1/t) - asin(t))) < tol
  @test @FastGTPSA(normTPS(asec(1/t) - acos(t))) < tol
  @test @FastGTPSA(normTPS(acot(1/t) - atan(t))) < tol
  @test @FastGTPSA(normTPS(asinh(sinh(t)) - t)) < tol
  @test @FastGTPSA(normTPS(acosh(cosh(t)) - t)) < tol
  @test @FastGTPSA(normTPS(atanh(tanh(t)) - t)) < tol
  @test @FastGTPSA(normTPS(acsch(t) - asinh(1/t))) < tol
  @test @FastGTPSA(normTPS(asech(t) - acosh(1/t))) < tol
  @test @FastGTPSA(normTPS(acoth(1/t) - atanh(t))) < tol
  @test @FastGTPSA(normTPS(asinc(t/pi) - asin(t)/t)) < tol
  @test @FastGTPSA(normTPS(asinhc(t/pi) - asinh(t)/t)) < tol
  @test @FastGTPSA(normTPS(erfc(t) - 1 + erf(t))) < tol
  @test @FastGTPSA(normTPS(erf(-t) + erf(t))) < tol
  @test @FastGTPSA(normTPS(angle(t))) < tol
  @test @FastGTPSA(normTPS(complex(t) - t)) < tol
  @test @FastGTPSA(normTPS(complex(t,t) - (t+im*t))) < tol

  t = ComplexTPS64(t)
  t[0] = 0.5+0.5im; t[[1]] = 2+2im; t[[2]] = 3+3im; t[[3]] = 4+4im; t[[4]] = 5+5im; t[[5]] = 6+6im
  @test @FastGTPSA(normTPS(sin(t)^2+cos(t)^2 - 1)) < tol
  @test @FastGTPSA(normTPS(1/sin(t) - csc(t))) < tol
  @test @FastGTPSA(normTPS(1/cos(t) - sec(t))) < tol
  @test @FastGTPSA(normTPS(1/tan(t) - cot(t))) < tol
  @test @FastGTPSA(normTPS(sin(t)/cos(t) - tan(t))) < tol
  @test @FastGTPSA(normTPS(cos(2*t) - cos(t)^2 + sin(t)^2)) < tol
  @test @FastGTPSA(normTPS(sec(t)^2 - 1 - tan(t)^2)) < tol
  @test @FastGTPSA(normTPS(sin(t/2) - sqrt((1-cos(t))/2))) < tol
  @test @FastGTPSA(normTPS(cos(t/2) - sqrt((1+cos(t))/2))) < tol
  @test @FastGTPSA(normTPS(sqrt(t^2) - t)) < tol
  @test @FastGTPSA(normTPS(csc(t)^2 - cot(t)^2 - 1)) < tol
  @test @FastGTPSA(normTPS(exp(log(t)) - t)) < tol
  @test @FastGTPSA(normTPS(log(exp(t)) - t)) < tol
  @test @FastGTPSA(normTPS(log(exp(t)) - exp(log(t)))) < tol
  @test @FastGTPSA(normTPS(log(t^2) - 2*log(t))) < tol
  @test @FastGTPSA(normTPS(5*log(t) - log(t^5) - 2*pi*im)) < tol
  @test @FastGTPSA(normTPS(t*log(5) - log(5^t))) < tol
  @test @FastGTPSA(normTPS(sinc(t/pi) - sin(t)/t)) < tol
  @test @FastGTPSA(normTPS(sinhc(t/pi) - sinh(t)/t)) < tol
  @test @FastGTPSA(normTPS(exp(im*t) - cos(t) - im*sin(t))) < tol
  @test @FastGTPSA(normTPS(sinh(t) - (exp(t) - exp(-t))/2)) < tol
  @test @FastGTPSA(normTPS(cosh(t) - (exp(t) + exp(-t))/2)) < tol
  @test @FastGTPSA(normTPS(tanh(t) - sinh(t)/cosh(t))) < tol
  @test @FastGTPSA(normTPS(csch(t) - 1/sinh(t))) < tol
  @test @FastGTPSA(normTPS(sech(t) - 1/cosh(t))) < tol
  @test @FastGTPSA(normTPS(coth(t) - cosh(t)/sinh(t))) < tol
  @test @FastGTPSA(normTPS(coth(t) - 1/tanh(t))) < tol
  @test @FastGTPSA(normTPS(cosh(t)^2 - sinh(t)^2 - 1)) < tol
  @test @FastGTPSA(normTPS(1 - tanh(t)^2 - sech(t)^2)) < tol
  @test @FastGTPSA(normTPS(coth(t)^2 - 1 - csch(t)^2)) < tol
  
  @test @FastGTPSA(normTPS(asin(sin(t)) - t)) < tol
  @test @FastGTPSA(normTPS(acos(cos(t)) - t)) < tol
  @test @FastGTPSA(normTPS(atan(tan(t)) - t)) < tol
  @test @FastGTPSA(normTPS(acsc(t) - asin(1/t))) < tol
  @test @FastGTPSA(normTPS(asec(t) - acos(1/t))) < tol
  @test @FastGTPSA(normTPS(acot(t) - atan(1/t))) < tol
  @test @FastGTPSA(normTPS(asinh(sinh(t)) - t)) < tol
  @test @FastGTPSA(normTPS(acosh(cosh(t)) - t)) < tol
  @test @FastGTPSA(normTPS(atanh(tanh(t)) - t)) < tol
  @test @FastGTPSA(normTPS(acsch(t) - asinh(1/t))) < tol
  @test @FastGTPSA(normTPS(asech(t) - acosh(1/t))) < tol
  @test @FastGTPSA(normTPS(acoth(t) - atanh(1/t))) < tol
  @test @FastGTPSA(normTPS(asinc(t/pi) - asin(t)/t)) < tol
  @test @FastGTPSA(normTPS(asinhc(t/pi) - asinh(t)/t)) < tol
  
  @test @FastGTPSA(normTPS(erfc(t) - 1 + erf(t))) < tol
  @test @FastGTPSA(normTPS(erf(-t) + erf(t))) < tol
  @test @FastGTPSA(normTPS(angle(t) - atan(imag(t),real(t)))) < tol
  @test @FastGTPSA(normTPS(complex(t) - t)) < tol



  d = Descriptor(1, 5)
  t = TPS(use=d)
  t[0] = 0.5; t[[1]] = 2; t[[2]] = 3; t[[3]] = 4; t[[4]] = 5; t[[5]] = 6

  tol = 1e-10
  out = ComplexTPS64()
  @test (@FastGTPSA!(out = sin(t)^2+cos(t)^2 - 1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 1/sin(t) - csc(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 1/cos(t) - sec(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 1/tan(t) - cot(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sin(t)/cos(t) - tan(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = cos(2*t) - cos(t)^2 + sin(t)^2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sec(t)^2 - 1 - tan(t)^2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sin(t/2) - sqrt((1-cos(t))/2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = cos(t/2) - sqrt((1+cos(t))/2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sqrt(t^2) - abs(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = csc(t)^2 - cot(t)^2 - 1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = exp(log(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = log(exp(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = log(exp(t)) - exp(log(t))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = log(t^2) - 2*log(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 5*log(t) - log(t^5)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t*log(5) - log(5^t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sinc(t) - sin(pi*t)/(pi*t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sinhc(t/pi) - sinh(t)/t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = exp(im*t) - cos(t) - im*sin(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = real(exp(im*t)) - cos(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = imag(exp(im*t)) - sin(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sinh(t) - (exp(t) - exp(-t))/2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = cosh(t) - (exp(t) + exp(-t))/2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = tanh(t) - sinh(t)/cosh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = csch(t) - 1/sinh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sech(t) - 1/cosh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = coth(t) - cosh(t)/sinh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = coth(t) - 1/tanh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = cosh(t)^2 - sinh(t)^2 - 1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 1 - tanh(t)^2 - sech(t)^2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = coth(t)^2 - 1 - csch(t)^2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asin(sin(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acos(cos(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = atan(tan(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acsc(1/t) - asin(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asec(1/t) - acos(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acot(1/t) - atan(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asinh(sinh(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acosh(cosh(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = atanh(tanh(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acsch(t) - asinh(1/t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asech(t) - acosh(1/t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acoth(1/t) - atanh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asinc(t/pi) - asin(t)/t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asinhc(t/pi) - asinh(t)/t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = erfc(t) - 1 + erf(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = erf(-t) + erf(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = angle(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = complex(t) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = complex(t,t) - (t+im*t)); normTPS(out)) < tol

  t = ComplexTPS64(t)
  t[0] = 0.5+0.5im; t[[1]] = 2+2im; t[[2]] = 3+3im; t[[3]] = 4+4im; t[[4]] = 5+5im; t[[5]] = 6+6im
  @test (@FastGTPSA!(out = sin(t)^2+cos(t)^2 - 1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 1/sin(t) - csc(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 1/cos(t) - sec(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 1/tan(t) - cot(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sin(t)/cos(t) - tan(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = cos(2*t) - cos(t)^2 + sin(t)^2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sec(t)^2 - 1 - tan(t)^2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sin(t/2) - sqrt((1-cos(t))/2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = cos(t/2) - sqrt((1+cos(t))/2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sqrt(t^2) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = csc(t)^2 - cot(t)^2 - 1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = exp(log(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = log(exp(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = log(exp(t)) - exp(log(t))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = log(t^2) - 2*log(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 5*log(t) - log(t^5) - 2*pi*im); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t*log(5) - log(5^t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sinc(t/pi) - sin(t)/t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sinhc(t/pi) - sinh(t)/t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = exp(im*t) - cos(t) - im*sin(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sinh(t) - (exp(t) - exp(-t))/2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = cosh(t) - (exp(t) + exp(-t))/2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = tanh(t) - sinh(t)/cosh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = csch(t) - 1/sinh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = sech(t) - 1/cosh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = coth(t) - cosh(t)/sinh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = coth(t) - 1/tanh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = cosh(t)^2 - sinh(t)^2 - 1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 1 - tanh(t)^2 - sech(t)^2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = coth(t)^2 - 1 - csch(t)^2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asin(sin(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acos(cos(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = atan(tan(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acsc(t) - asin(1/t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asec(t) - acos(1/t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acot(t) - atan(1/t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asinh(sinh(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acosh(cosh(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = atanh(tanh(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acsch(t) - asinh(1/t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asech(t) - acosh(1/t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = acoth(t) - atanh(1/t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asinc(t/pi) - asin(t)/t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = asinhc(t/pi) - asinh(t)/t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = erfc(t) - 1 + erf(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = erf(-t) + erf(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = angle(t) - atan(imag(t),real(t))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = complex(t) - t); normTPS(out)) < tol

  # Make sure stack is 0:
  @test GTPSA.checktemps()
end


@testset "Type stability" begin
  include("type_stable.jl")
  include("../benchmark/track.jl")
  @test_opt type_stable_test()
  @test_opt benchmark_GTPSA3()
end


@testset "FastGTPSA - Broadcasting" begin
  d = Descriptor(1, 5)
  t = TPS(use=d)
  ct = ComplexTPS64(t)
  # Set scalar part so both TPSs are 1
  t[0] = 1
  ct[0] = 1
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

  out = ComplexTPS64()

  # TPS:
  @test @FastGTPSA(@.(normTPS(t1 + t2 - t3))) < tol
  @test @FastGTPSA(@.(normTPS(t2 + t1 - t3))) < tol
  @test @FastGTPSA(@.(normTPS(t1 + 2 - t3))) < tol
  @test @FastGTPSA(@.(normTPS(2 + t1 - t3))) < tol
  @test @FastGTPSA(@.(normTPS(t3 - t2 - t1))) < tol
  @test @FastGTPSA(@.(normTPS(t2 - t3 - -t1))) < tol
  @test @FastGTPSA(@.(normTPS(t3 - 2 - t1))) < tol
  @test @FastGTPSA(@.(normTPS(2 - t3 - -t1))) < tol
  @test @FastGTPSA(@.(normTPS(t2 * t3 - 6))) < tol
  @test @FastGTPSA(@.(normTPS(t3 * t2 - 6))) < tol
  @test @FastGTPSA(@.(normTPS(t2 * 5 - 10))) < tol
  @test @FastGTPSA(@.(normTPS(5 * t2 - 10 * t1))) < tol
  @test @FastGTPSA(@.(normTPS(t1 / t2 - 1/2))) < tol
  @test @FastGTPSA(@.(normTPS(t2 / t1 - 2))) < tol
  @test @FastGTPSA(@.(normTPS(1 / t2 - 1/2))) < tol
  @test @FastGTPSA(@.(normTPS(t2 / 3 - 2/3))) < tol
  @test @FastGTPSA(@.(normTPS(t2 / t2 - t1))) < tol
  @test @FastGTPSA(@.(normTPS(t2 / t2 - 1))) < tol
  @test @FastGTPSA(@.(normTPS(t2 ^ t3 - 8))) < tol
  @test @FastGTPSA(@.(normTPS(t3 ^ t2 - 9))) < tol
  @test @FastGTPSA(@.(normTPS(t2 ^ 3 - 8))) < tol
  @test @FastGTPSA(@.(normTPS(t2 ^ (1/2) - sqrt(2)))) < tol
  @test @FastGTPSA(@.(normTPS(t2 ^ (1/2) - sqrt(t2)))) < tol
  @test @FastGTPSA(@.(normTPS(2 ^ t3 - 8))) < tol
  @test @FastGTPSA(@.(normTPS(inv(t3) - 1/t3))) < tol
  @test @FastGTPSA(@.(normTPS(inv(t3) - 1/3))) < tol

  # ComplexTPS:
  @test @FastGTPSA(@.(normTPS(ct1 + ct2 - ct3))) < tol
  @test @FastGTPSA(@.(normTPS(ct2 + ct1 - ct3))) < tol
  @test @FastGTPSA(@.(normTPS(ct1 + (2+2im) - ct3))) < tol
  @test @FastGTPSA(@.(normTPS((2+2im) + ct1 - ct3))) < tol
  @test @FastGTPSA(@.(normTPS(ct3 - ct2 - ct1))) < tol
  @test @FastGTPSA(@.(normTPS(ct2 - ct3 - -ct1))) < tol
  @test @FastGTPSA(@.(normTPS(ct3 - (2+2im) - ct1))) < tol
  @test @FastGTPSA(@.(normTPS((2+2im) - ct3 - -ct1))) < tol
  @test @FastGTPSA(@.(normTPS(ct2 * ct3 - (2+2im)*(3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(ct3 * ct2 - (2+2im)*(3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(ct2 * 5 - (10+10im)))) < tol
  @test @FastGTPSA(@.(normTPS(5 * ct2 - (10 * ct1)))) < tol
  @test @FastGTPSA(@.(normTPS(ct1 / ct2 - (1+im)/(2+2im)))) < tol
  @test @FastGTPSA(@.(normTPS(ct2 / ct1 - 2))) < tol
  @test @FastGTPSA(@.(normTPS(1 / ct2 - 1/(2+2im)))) < tol
  @test @FastGTPSA(@.(normTPS(ct2 / 3 - (2+2im)/3))) < tol
  @test @FastGTPSA(@.(normTPS(ct2 / ct2 - 1))) < tol
  @test @FastGTPSA(@.(normTPS(ct2 ^ ct3 - (2+2im)^(3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(ct3 ^ ct2 - (3+3im)^(2+2im)))) < tol
  @test @FastGTPSA(@.(normTPS(ct2 ^ 3 - (2+2im)^3))) < tol
  @test @FastGTPSA(@.(normTPS(ct2 ^ (1/2) - sqrt(2+2im)))) < tol
  @test @FastGTPSA(@.(normTPS(ct2 ^ (1/2) - sqrt(ct2)))) < tol
  @test @FastGTPSA(@.(normTPS(2 ^ ct3 - 2^(3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(inv(ct3) - 1/ct3))) < tol
  @test @FastGTPSA(@.(normTPS(inv(ct3) - 1/(3+3im)))) < tol

  # Promotion of TPS to ComplexTPS
  @test @FastGTPSA(@.(normTPS(t1 + ct2 - (1 + (2+2im))))) < tol
  @test @FastGTPSA(@.(normTPS(ct2 + t1 - (1 + (2+2im))))) < tol
  @test @FastGTPSA(@.(normTPS(t1 + (2+2im) - (1 + (2+2im))))) < tol
  @test @FastGTPSA(@.(normTPS((2+2im) + t1 - (1 + (2+2im))))) < tol
  @test @FastGTPSA(@.(normTPS(t3 - ct2 - (3 - (2+2im))))) < tol
  @test @FastGTPSA(@.(normTPS(ct2 - t3 - ((2+2im) - 3)))) < tol
  @test @FastGTPSA(@.(normTPS(t3 - (2+2im) - (3 - (2+2im))))) < tol
  @test @FastGTPSA(@.(normTPS((2+2im) - t3 - ((2+2im) - 3)))) < tol
  @test @FastGTPSA(@.(normTPS(t2 * ct3 - 2 * (3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(ct3 * t2 - 2 * (3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(t2 * (3+3im) - 2 * (3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS((3+3im) * t2 - 2 * (3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(t2 / ct3 - 2/(3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(ct3 / t2 - (3+3im)/2))) < tol
  @test @FastGTPSA(@.(normTPS(t2 / (3+3im) - 2/(3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS((3+3im) / t2 - (3+3im)/2))) < tol
  @test @FastGTPSA(@.(normTPS(t2 ^ ct3 - 2^(3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(ct3 ^ t2 - (3+3im)^2))) < tol
  @test @FastGTPSA(@.(normTPS(t2 ^ (3+3im) - 2^(3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS((3+3im)^t2 - (3+3im)^2))) < tol

  
  @test (@FastGTPSA!(out = @. t1 + t2 - t3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t2 + t1 - t3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t1 + 2 - t3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. 2 + t1 - t3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t3 - t2 - t1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t2 - t3 - -t1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t3 - 2 - t1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. 2 - t3 - -t1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t2 * t3 - 6); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t3 * t2 - 6); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t2 * 5 - 10); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. 5 * t2 - 10 * t1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t1 / t2 - 1/2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t2 / t1 - 2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. 1 / t2 - 1/2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t2 / 3 - 2/3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t2 / t2 - t1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t2 / t2 - 1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t2 ^ t3 - 8); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t3 ^ t2 - 9); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t2 ^ 3 - 8); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t2 ^ (1/2) - sqrt(2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t2 ^ (1/2) - sqrt(t2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. 2 ^ t3 - 8); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. inv(t3) - 1/t3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. inv(t3) - 1/3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct1 + ct2 - ct3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct2 + ct1 - ct3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct1 + (2+2im) - ct3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. (2+2im) + ct1 - ct3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct3 - ct2 - ct1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct2 - ct3 - -ct1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct3 - (2+2im) - ct1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. (2+2im) - ct3 - -ct1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct2 * ct3 - (2+2im)*(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct3 * ct2 - (2+2im)*(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct2 * 5 - (10+10im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. 5 * ct2 - (10 * ct1)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct1 / ct2 - (1+im)/(2+2im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct2 / ct1 - 2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. 1 / ct2 - 1/(2+2im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct2 / 3 - (2+2im)/3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct2 / ct2 - 1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct2 ^ ct3 - (2+2im)^(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct3 ^ ct2 - (3+3im)^(2+2im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct2 ^ 3 - (2+2im)^3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct2 ^ (1/2) - sqrt(2+2im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct2 ^ (1/2) - sqrt(ct2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. 2 ^ ct3 - 2^(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. inv(ct3) - 1/ct3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. inv(ct3) - 1/(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t1 + ct2 - (1 + (2+2im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct2 + t1 - (1 + (2+2im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t1 + (2+2im) - (1 + (2+2im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. (2+2im) + t1 - (1 + (2+2im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t3 - ct2 - (3 - (2+2im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct2 - t3 - ((2+2im) - 3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t3 - (2+2im) - (3 - (2+2im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. (2+2im) - t3 - ((2+2im) - 3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t2 * ct3 - 2 * (3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct3 * t2 - 2 * (3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t2 * (3+3im) - 2 * (3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. (3+3im) * t2 - 2 * (3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t2 / ct3 - 2/(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct3 / t2 - (3+3im)/2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t2 / (3+3im) - 2/(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. (3+3im) / t2 - (3+3im)/2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t2 ^ ct3 - 2^(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. ct3 ^ t2 - (3+3im)^2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t2 ^ (3+3im) - 2^(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. (3+3im)^t2 - (3+3im)^2); normTPS(out)) < tol

  # Vectorized DOT:
  t1 = [t1]
  t2 = [t2]
  t3 = [t3]
  ct1 = [ct1]
  ct2 = [ct2]
  ct3 = [ct3]
  out = [out]

  @test @FastGTPSA(norm(@.(normTPS(t1 + t2 - t3)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t2 + t1 - t3)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t1 + 2 - t3)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(2 + t1 - t3)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t3 - t2 - t1)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t2 - t3 - -t1)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t3 - 2 - t1)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(2 - t3 - -t1)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t2 * t3 - 6)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t3 * t2 - 6)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t2 * 5 - 10)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(5 * t2 - 10 * t1)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t1 / t2 - 1/2)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t2 / t1 - 2)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(1 / t2 - 1/2)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t2 / 3 - 2/3)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t2 / t2 - t1)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t2 / t2 - 1)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t2 ^ t3 - 8)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t3 ^ t2 - 9)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t2 ^ 3 - 8)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t2 ^ (1/2) - sqrt(2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t2 ^ (1/2) - sqrt(t2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(2 ^ t3 - 8)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(inv(t3) - 1/t3)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(inv(t3) - 1/3)))) < tol
  
  # ComplexTPS
  @test @FastGTPSA(norm(@.(normTPS(ct1 + ct2 - ct3)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct2 + ct1 - ct3)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct1 + (2+2im) - ct3)))) < tol
  @test @FastGTPSA(norm(@.(normTPS((2+2im) + ct1 - ct3)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct3 - ct2 - ct1)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct2 - ct3 - -ct1)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct3 - (2+2im) - ct1)))) < tol
  @test @FastGTPSA(norm(@.(normTPS((2+2im) - ct3 - -ct1)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct2 * ct3 - (2+2im)*(3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct3 * ct2 - (2+2im)*(3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct2 * 5 - (10+10im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(5 * ct2 - (10 * ct1))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct1 / ct2 - (1+im)/(2+2im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct2 / ct1 - 2)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(1 / ct2 - 1/(2+2im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct2 / 3 - (2+2im)/3)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct2 / ct2 - 1)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct2 ^ ct3 - (2+2im)^(3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct3 ^ ct2 - (3+3im)^(2+2im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct2 ^ 3 - (2+2im)^3)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct2 ^ (1/2) - sqrt(2+2im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct2 ^ (1/2) - sqrt(ct2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(2 ^ ct3 - 2^(3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(inv(ct3) - 1/ct3)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(inv(ct3) - 1/(3+3im))))) < tol

  # Promotion of TPS to ComplexTPS
  @test @FastGTPSA(norm(@.(normTPS(t1 + ct2 - (1 + (2+2im)))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct2 + t1 - (1 + (2+2im)))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t1 + (2+2im) - (1 + (2+2im)))))) < tol
  @test @FastGTPSA(norm(@.(normTPS((2+2im) + t1 - (1 + (2+2im)))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t3 - ct2 - (3 - (2+2im)))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct2 - t3 - ((2+2im) - 3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t3 - (2+2im) - (3 - (2+2im)))))) < tol
  @test @FastGTPSA(norm(@.(normTPS((2+2im) - t3 - ((2+2im) - 3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t2 * ct3 - 2 * (3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct3 * t2 - 2 * (3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t2 * (3+3im) - 2 * (3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS((3+3im) * t2 - 2 * (3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t2 / ct3 - 2/(3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct3 / t2 - (3+3im)/2)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t2 / (3+3im) - 2/(3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS((3+3im) / t2 - (3+3im)/2)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t2 ^ ct3 - 2^(3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(ct3 ^ t2 - (3+3im)^2)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t2 ^ (3+3im) - 2^(3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS((3+3im)^t2 - (3+3im)^2)))) < tol


  @test (@FastGTPSA!(@. out = t1 + t2 - t3); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t2 + t1 - t3); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t1 + 2 - t3); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = 2 + t1 - t3); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t3 - t2 - t1); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t2 - t3 - -t1); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t3 - 2 - t1); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = 2 - t3 - -t1); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t2 * t3 - 6); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t3 * t2 - 6); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t2 * 5 - 10); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = 5 * t2 - 10 * t1); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t1 / t2 - 1/2); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t2 / t1 - 2); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = 1 / t2 - 1/2); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t2 / 3 - 2/3); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t2 / t2 - t1); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t2 / t2 - 1); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t2 ^ t3 - 8); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t3 ^ t2 - 9); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t2 ^ 3 - 8); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t2 ^ (1/2) - sqrt(2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t2 ^ (1/2) - sqrt(t2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = 2 ^ t3 - 8); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = inv(t3) - 1/t3); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = inv(t3) - 1/3); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct1 + ct2 - ct3); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct2 + ct1 - ct3); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct1 + (2+2im) - ct3); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = (2+2im) + ct1 - ct3); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct3 - ct2 - ct1); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct2 - ct3 - -ct1); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct3 - (2+2im) - ct1); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = (2+2im) - ct3 - -ct1); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct2 * ct3 - (2+2im)*(3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct3 * ct2 - (2+2im)*(3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct2 * 5 - (10+10im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = 5 * ct2 - (10 * ct1)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct1 / ct2 - (1+im)/(2+2im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct2 / ct1 - 2); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = 1 / ct2 - 1/(2+2im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct2 / 3 - (2+2im)/3); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct2 / ct2 - 1); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct2 ^ ct3 - (2+2im)^(3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct3 ^ ct2 - (3+3im)^(2+2im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct2 ^ 3 - (2+2im)^3); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct2 ^ (1/2) - sqrt(2+2im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct2 ^ (1/2) - sqrt(ct2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = 2 ^ ct3 - 2^(3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = inv(ct3) - 1/ct3); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = inv(ct3) - 1/(3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t1 + ct2 - (1 + (2+2im))); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct2 + t1 - (1 + (2+2im))); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t1 + (2+2im) - (1 + (2+2im))); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = (2+2im) + t1 - (1 + (2+2im))); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t3 - ct2 - (3 - (2+2im))); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct2 - t3 - ((2+2im) - 3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t3 - (2+2im) - (3 - (2+2im))); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = (2+2im) - t3 - ((2+2im) - 3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t2 * ct3 - 2 * (3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct3 * t2 - 2 * (3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t2 * (3+3im) - 2 * (3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = (3+3im) * t2 - 2 * (3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t2 / ct3 - 2/(3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct3 / t2 - (3+3im)/2); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t2 / (3+3im) - 2/(3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = (3+3im) / t2 - (3+3im)/2); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t2 ^ ct3 - 2^(3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = ct3 ^ t2 - (3+3im)^2); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t2 ^ (3+3im) - 2^(3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = (3+3im)^t2 - (3+3im)^2); norm(normTPS.(out))) < tol

  d = Descriptor(1, 5)
  t = TPS(use=d)
  v = 0.5
  t[0] = v
  tol = 1e-14
  t1 = TPS(t)
  t1[0] = 1
  t2 = zero(t1)
  t2[0] = 2
  t3 = zero(t1)
  t3[0] = 3
  out = ComplexTPS64()

  @test @FastGTPSA(@.(normTPS(abs(-t) - abs(-v) ))) < tol
  @test @FastGTPSA(@.(normTPS(sqrt(t) - sqrt(v)))) < tol
  @test @FastGTPSA(@.(normTPS(exp(t) - exp(v)))) < tol
  @test @FastGTPSA(@.(normTPS(log(t) - log(v)))) < tol
  @test @FastGTPSA(@.(normTPS(sin(t) - sin(v)))) < tol
  @test @FastGTPSA(@.(normTPS(cos(t) - cos(v)))) < tol
  @test @FastGTPSA(@.(normTPS(tan(t) - tan(v)))) < tol
  @test @FastGTPSA(@.(normTPS(csc(t) - csc(v)))) < tol
  @test @FastGTPSA(@.(normTPS(sec(t) - sec(v)))) < tol
  @test @FastGTPSA(@.(normTPS(cot(t) - cot(v)))) < tol
  @test @FastGTPSA(@.(normTPS(sinc(t) - sinc(v)))) < tol
  @test @FastGTPSA(@.(normTPS(sinh(t) - sinh(v)))) < tol
  @test @FastGTPSA(@.(normTPS(cosh(t) - cosh(v)))) < tol
  @test @FastGTPSA(@.(normTPS(tanh(t) - tanh(v)))) < tol
  @test @FastGTPSA(@.(normTPS(csch(t) - csch(v)))) < tol
  @test @FastGTPSA(@.(normTPS(sech(t) - sech(v)))) < tol
  @test @FastGTPSA(@.(normTPS(coth(t) - coth(v)))) < tol
  @test @FastGTPSA(@.(normTPS(asin(t) - asin(v)))) < tol
  @test @FastGTPSA(@.(normTPS(acos(t) - acos(v)))) < tol
  @test @FastGTPSA(@.(normTPS(atan(t) - atan(v)))) < tol
  @test @FastGTPSA(@.(normTPS(acsc(1/t) - acsc(1/v)))) < tol
  @test @FastGTPSA(@.(normTPS(asec(1/t) - asec(1/v)))) < tol
  @test @FastGTPSA(@.(normTPS(acot(1/t) - acot(1/v)))) < tol
  @test @FastGTPSA(@.(normTPS(asinh(t) - asinh(v)))) < tol
  @test @FastGTPSA(@.(normTPS(acosh(1/t) - acosh(1/v)))) < tol
  @test @FastGTPSA(@.(normTPS(atanh(t) - atanh(v)))) < tol
  @test @FastGTPSA(@.(normTPS(acsch(1/t) - acsch(1/v)))) < tol
  @test @FastGTPSA(@.(normTPS(asech(t) - asech(v)))) < tol
  @test @FastGTPSA(@.(normTPS(acoth(1/t) - acoth(1/v)))) < tol
  @test @FastGTPSA(@.(normTPS(asinc(t/pi) - asin(v)/(v)))) < tol
  @test @FastGTPSA(@.(normTPS(asinhc(t/pi) - asinh(v)/(v)))) < tol
  @test @FastGTPSA(@.(normTPS(zero(t) - zero(v)))) < tol
  @test @FastGTPSA(@.(normTPS(real(t) - real(v)))) < tol
  @test @FastGTPSA(@.(normTPS(imag(t) - imag(v)))) < tol
  @test @FastGTPSA(@.(normTPS(conj(t) - conj(v)))) < tol
  @test @FastGTPSA(@.(normTPS(sinhc(t/pi) - sinh(v)/v))) < tol
  @test @FastGTPSA(@.(normTPS(erf(t) - erf(v)))) < tol
  @test @FastGTPSA(@.(normTPS(erfc(t) - erfc(v)))) < tol
  @test @FastGTPSA(@.(normTPS(-im*erf(t*im) - erfi(v)))) < tol
  @test @FastGTPSA(@.(normTPS(atan(t3,t2) - atan(3,2)))) < tol
  @test @FastGTPSA(@.(normTPS(atan(t3,2) - atan(3,2)))) < tol
  @test @FastGTPSA(@.(normTPS(atan(3,t2) - atan(3,2)))) < tol
  @test @FastGTPSA(@.(normTPS(atan(t3,-t2) - atan(3,-2)))) < tol
  @test @FastGTPSA(@.(normTPS(atan(t3,-2) - atan(3,-2)))) < tol
  @test @FastGTPSA(@.(normTPS(atan(3,-t2) - atan(3,-2)))) < tol
  @test @FastGTPSA(@.(normTPS(atan(-t3,-t2) - atan(-3,-2)))) < tol
  @test @FastGTPSA(@.(normTPS(atan(-t3,-2) - atan(-3,-2)))) < tol
  @test @FastGTPSA(@.(normTPS(atan(-3,-t2) - atan(-3,-2)))) < tol
  @test @FastGTPSA(@.(normTPS(atan(-t3,t2) - atan(-3,2)))) < tol
  @test @FastGTPSA(@.(normTPS(atan(-t3,2) - atan(-3,2)))) < tol
  @test @FastGTPSA(@.(normTPS(atan(-3,t2) - atan(-3,2)))) < tol

  @test @FastGTPSA(@.(normTPS(hypot(t2,t3) - hypot(2,3)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(2,t3) - hypot(2,3)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(t2,3) - hypot(2,3)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(t1,t2,t3) - hypot(1,2,3)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(1, t2, t3) - hypot(1,2,3)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(t1, 2, t3) - hypot(1,2,3)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(t1, t2, 3) - hypot(1,2,3)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(1, 2, t3) - hypot(1,2,3)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(1, t2, 3) - hypot(1,2,3)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(t1, 2, 3) - hypot(1,2,3)))) < tol

  @test @FastGTPSA(@.(normTPS(angle(t2) - angle(2)))) < tol
  @test @FastGTPSA(@.(normTPS(angle(-t2) - angle(-2)))) < tol
  @test @FastGTPSA(@.(normTPS(complex(t3) - complex(3)))) < tol
  @test @FastGTPSA(@.(normTPS(complex(t2,t3) - complex(2,3)))) < tol
  @test @FastGTPSA(@.(normTPS(polar(t2) - (abs(2)+im*atan(0,2))))) < tol
  @test @FastGTPSA(@.(normTPS(polar(-t1) - (abs(-1)+im*atan(0,-1))))) < tol
  @test @FastGTPSA(@.(normTPS(rect(t2) - (2*cos(0) + im*2*sin(0))))) < tol
  @test @FastGTPSA(@.(normTPS(rect(-t1) - (-1*cos(0) + im*-1*sin(0))))) < tol


  @test (@FastGTPSA!(out = @. abs(-t) - abs(-v) ); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sqrt(t) - sqrt(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. exp(t) - exp(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. log(t) - log(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sin(t) - sin(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. cos(t) - cos(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. tan(t) - tan(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. csc(t) - csc(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sec(t) - sec(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. cot(t) - cot(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sinc(t) - sinc(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sinh(t) - sinh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. cosh(t) - cosh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. tanh(t) - tanh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. csch(t) - csch(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sech(t) - sech(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. coth(t) - coth(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asin(t) - asin(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acos(t) - acos(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. atan(t) - atan(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acsc(1/t) - acsc(1/v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asec(1/t) - asec(1/v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acot(1/t) - acot(1/v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asinh(t) - asinh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acosh(1/t) - acosh(1/v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. atanh(t) - atanh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acsch(1/t) - acsch(1/v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asech(t) - asech(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acoth(1/t) - acoth(1/v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asinc(t/pi) - asin(v)/(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asinhc(t/pi) - asinh(v)/(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. zero(t) - zero(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. real(t) - real(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. imag(t) - imag(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. conj(t) - conj(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sinhc(t/pi) - sinh(v)/v); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. erf(t) - erf(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. erfc(t) - erfc(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. -im*erf(t*im) - erfi(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. atan(t3,t2) - atan(3,2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. atan(t3,2) - atan(3,2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. atan(3,t2) - atan(3,2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. atan(t3,-t2) - atan(3,-2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. atan(t3,-2) - atan(3,-2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. atan(3,-t2) - atan(3,-2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. atan(-t3,-t2) - atan(-3,-2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. atan(-t3,-2) - atan(-3,-2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. atan(-3,-t2) - atan(-3,-2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. atan(-t3,t2) - atan(-3,2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. atan(-t3,2) - atan(-3,2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. atan(-3,t2) - atan(-3,2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(t2,t3) - hypot(2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(2,t3) - hypot(2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(t2,3) - hypot(2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(t1,t2,t3) - hypot(1,2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(1, t2, t3) - hypot(1,2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(t1, 2, t3) - hypot(1,2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(t1, t2, 3) - hypot(1,2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(1, 2, t3) - hypot(1,2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(1, t2, 3) - hypot(1,2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(t1, 2, 3) - hypot(1,2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. angle(t2) - angle(2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. angle(-t2) - angle(-2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. complex(t3) - complex(3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. complex(t2,t3) - complex(2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. polar(t2) - (abs(2)+im*atan(0,2))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. polar(-t1) - (abs(-1)+im*atan(0,-1))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. rect(t2) - (2*cos(0) + im*2*sin(0))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. rect(-t1) - (-1*cos(0) + im*-1*sin(0))); normTPS(out)) < tol
  

  v = 0.5+0.5im
  t = ComplexTPS64(t)
  t[0] = v
  ct1 = ComplexTPS64(t)
  ct1[0] = 1 + 1im
  ct2 = zero(ct1)
  ct2[0] = 2 + 2im
  ct3 = zero(ct1)
  ct3[0] = 3 + 3im
  @test @FastGTPSA(@.(normTPS(abs(-t) - abs(-v) ))) < tol
  @test @FastGTPSA(@.(normTPS(sqrt(t) - sqrt(v)))) < tol
  @test @FastGTPSA(@.(normTPS(exp(t) - exp(v)))) < tol
  @test @FastGTPSA(@.(normTPS(log(t) - log(v)))) < tol
  @test @FastGTPSA(@.(normTPS(sin(t) - sin(v)))) < tol
  @test @FastGTPSA(@.(normTPS(cos(t) - cos(v)))) < tol
  @test @FastGTPSA(@.(normTPS(tan(t) - tan(v)))) < tol
  @test @FastGTPSA(@.(normTPS(csc(t) - csc(v)))) < tol
  @test @FastGTPSA(@.(normTPS(sec(t) - sec(v)))) < tol
  @test @FastGTPSA(@.(normTPS(cot(t) - cot(v)))) < tol
  @test @FastGTPSA(@.(normTPS(sinc(t) - sinc(v)))) < tol
  @test @FastGTPSA(@.(normTPS(sinh(t) - sinh(v)))) < tol
  @test @FastGTPSA(@.(normTPS(cosh(t) - cosh(v)))) < tol
  @test @FastGTPSA(@.(normTPS(tanh(t) - tanh(v)))) < tol
  @test @FastGTPSA(@.(normTPS(csch(t) - csch(v)))) < tol
  @test @FastGTPSA(@.(normTPS(sech(t) - sech(v)))) < tol
  @test @FastGTPSA(@.(normTPS(coth(t) - coth(v)))) < tol
  @test @FastGTPSA(@.(normTPS(asin(t) - asin(v)))) < tol
  @test @FastGTPSA(@.(normTPS(acos(t) - acos(v)))) < tol
  @test @FastGTPSA(@.(normTPS(atan(t) - atan(v)))) < tol
  @test @FastGTPSA(@.(normTPS(acsc(t) - acsc(v)))) < tol
  @test @FastGTPSA(@.(normTPS(asec(t) - asec(v)))) < tol
  @test @FastGTPSA(@.(normTPS(acot(t) - acot(v)))) < tol
  @test @FastGTPSA(@.(normTPS(asinh(t) - asinh(v)))) < tol
  @test @FastGTPSA(@.(normTPS(acosh(t) - acosh(v)))) < tol
  @test @FastGTPSA(@.(normTPS(atanh(t) - atanh(v)))) < tol
  @test @FastGTPSA(@.(normTPS(acsch(t) - acsch(v)))) < tol
  @test @FastGTPSA(@.(normTPS(asech(t) - asech(v)))) < tol
  @test @FastGTPSA(@.(normTPS(acoth(t) - acoth(v)))) < tol
  @test @FastGTPSA(@.(normTPS(asinc(t/pi) - asin(v)/v))) < tol
  @test @FastGTPSA(@.(normTPS(asinhc(t/pi) - asinh(v)/v))) < tol
  @test @FastGTPSA(@.(normTPS(zero(t) - zero(v)))) < tol
  @test @FastGTPSA(@.(normTPS(real(t) - real(v)))) < tol
  @test @FastGTPSA(@.(normTPS(imag(t) - imag(v)))) < tol
  @test @FastGTPSA(@.(normTPS(conj(t) - conj(v)))) < tol
  @test @FastGTPSA(@.(normTPS(sinhc(t/pi) - sinh(v)/v))) < tol
  @test @FastGTPSA(@.(normTPS(erf(t) - erf(v)))) < tol
  @test @FastGTPSA(@.(normTPS(erfc(t) - erfc(v)))) < tol
  @test @FastGTPSA(@.(normTPS(-im*erf(t*im) - erfi(v)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(ct2,ct3) - hypot(2+2im,3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(2+2im,ct3) - hypot(2+2im,3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(ct2,3+3im) - hypot(2+2im,3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(ct1,ct2,ct3) - hypot(1+1im,2+2im,3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(1+1im, ct2, ct3) - hypot(1+1im,2+2im,3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(ct1, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(ct1, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(1+1im, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(1+1im, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(ct1, 2+2im, 3+3im) - hypot(1+1im,2+2im,3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(angle(t2+im*t3) - angle(2+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(angle(t2-im*t3) - angle(2-3im)))) < tol
  @test @FastGTPSA(@.(normTPS(angle(-t2-im*t3) - angle(-2-3im)))) < tol
  @test @FastGTPSA(@.(normTPS(angle(-t2+im*t3) - angle(-2+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(angle(ct2) - angle(2+2im)))) < tol
  @test @FastGTPSA(@.(normTPS(angle(-ct2) - angle(-2-2im)))) < tol
  @test @FastGTPSA(@.(normTPS(complex(ct3) - complex(3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(polar(ct2) - (abs(2+2im)+im*angle(2+2im))))) < tol
  @test @FastGTPSA(@.(normTPS(polar(-ct1) - (abs(-1-im)+im*angle(-1-im))))) < tol
  @test @FastGTPSA(@.(normTPS(rect(ct2) - (2*cos(2) + im*2*sin(2))))) < tol
  @test @FastGTPSA(@.(normTPS(rect(-ct1) - (-1*cos(-1) + im*-1*sin(-1))))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(ct1, ct2, t3) - hypot(1+1im,2+2im,3)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(ct1, t2, ct3) - hypot(1+1im,2,3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(t1, ct2, ct3) - hypot(1,2+2im,3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(ct1, t2, t3) - hypot(1+1im,2,3)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(t1, ct2, t3) - hypot(1,2+2im,3)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(t1, t2, ct3) - hypot(1,2,3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(ct1,t2,3+3im) - hypot(1+1im,2,3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(t1, ct2, 3+3im) - hypot(1,2+2im,3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(ct1,2+2im,t3) - hypot(1+1im,2+2im,3)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(t1,2+2im,ct3) - hypot(1,2+2im,3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(1+1im,ct2,t3) - hypot(1+1im,2+2im,3)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(1+1im, t2, ct3) - hypot(1+1im,2,3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(t1,t2,3+3im) - hypot(1,2,3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(t1,2+2im,t3) - hypot(1,2+2im,3)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(1+1im,t2,t3) - hypot(1+1im,2,3)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(t1,2,3+3im) - hypot(1,2,3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(1,t2,3+3im) - hypot(1,2,3+3im)))) < tol
  @test @FastGTPSA(@.(normTPS(hypot(1+1im,2,t3) - hypot(1+1im,2,3)))) < tol

  @test (@FastGTPSA!(out = @. abs(-t) - abs(-v) ); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sqrt(t) - sqrt(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. exp(t) - exp(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. log(t) - log(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sin(t) - sin(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. cos(t) - cos(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. tan(t) - tan(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. csc(t) - csc(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sec(t) - sec(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. cot(t) - cot(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sinc(t) - sinc(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sinh(t) - sinh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. cosh(t) - cosh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. tanh(t) - tanh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. csch(t) - csch(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sech(t) - sech(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. coth(t) - coth(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asin(t) - asin(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acos(t) - acos(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. atan(t) - atan(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acsc(t) - acsc(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asec(t) - asec(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acot(t) - acot(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asinh(t) - asinh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acosh(t) - acosh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. atanh(t) - atanh(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acsch(t) - acsch(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asech(t) - asech(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acoth(t) - acoth(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asinc(t/pi) - asin(v)/v); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asinhc(t/pi) - asinh(v)/v); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. zero(t) - zero(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. real(t) - real(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. imag(t) - imag(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. conj(t) - conj(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sinhc(t/pi) - sinh(v)/v); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. erf(t) - erf(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. erfc(t) - erfc(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. -im*erf(t*im) - erfi(v)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(ct2,ct3) - hypot(2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(2+2im,ct3) - hypot(2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(ct2,3+3im) - hypot(2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(ct1,ct2,ct3) - hypot(1+1im,2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(1+1im, ct2, ct3) - hypot(1+1im,2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(ct1, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(ct1, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(1+1im, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(1+1im, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(ct1, 2+2im, 3+3im) - hypot(1+1im,2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. angle(t2+im*t3) - angle(2+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. angle(t2-im*t3) - angle(2-3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. angle(-t2-im*t3) - angle(-2-3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. angle(-t2+im*t3) - angle(-2+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. angle(ct2) - angle(2+2im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. angle(-ct2) - angle(-2-2im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. complex(ct3) - complex(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. polar(ct2) - (abs(2+2im)+im*angle(2+2im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. polar(-ct1) - (abs(-1-im)+im*angle(-1-im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. rect(ct2) - (2*cos(2) + im*2*sin(2))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. rect(-ct1) - (-1*cos(-1) + im*-1*sin(-1))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(ct1, ct2, t3) - hypot(1+1im,2+2im,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(ct1, t2, ct3) - hypot(1+1im,2,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(t1, ct2, ct3) - hypot(1,2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(ct1, t2, t3) - hypot(1+1im,2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(t1, ct2, t3) - hypot(1,2+2im,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(t1, t2, ct3) - hypot(1,2,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(ct1,t2,3+3im) - hypot(1+1im,2,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(t1, ct2, 3+3im) - hypot(1,2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(ct1,2+2im,t3) - hypot(1+1im,2+2im,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(t1,2+2im,ct3) - hypot(1,2+2im,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(1+1im,ct2,t3) - hypot(1+1im,2+2im,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(1+1im, t2, ct3) - hypot(1+1im,2,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(t1,t2,3+3im) - hypot(1,2,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(t1,2+2im,t3) - hypot(1,2+2im,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(1+1im,t2,t3) - hypot(1+1im,2,3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(t1,2,3+3im) - hypot(1,2,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(1,t2,3+3im) - hypot(1,2,3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. hypot(1+1im,2,t3) - hypot(1+1im,2,3)); normTPS(out)) < tol


  # Now with vectors:
  d = Descriptor(1, 5)
  t = TPS(use=d)
  v = 0.5
  t[0] = v
  tol = 1e-14
  t1 = TPS(t)
  t1[0] = 1
  t2 = zero(t1)
  t2[0] = 2
  t3 = zero(t1)
  t3[0] = 3

  t = [t]
  v = [v]
  t1 = [t1]
  t2 = [t2]
  t3 = [t3]
  out = [out]


  @test @FastGTPSA(norm(@.(normTPS(abs(-t) - abs(-v) )))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sqrt(t) - sqrt(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(exp(t) - exp(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(log(t) - log(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sin(t) - sin(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(cos(t) - cos(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(tan(t) - tan(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(csc(t) - csc(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sec(t) - sec(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(cot(t) - cot(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sinc(t) - sinc(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sinh(t) - sinh(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(cosh(t) - cosh(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(tanh(t) - tanh(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(csch(t) - csch(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sech(t) - sech(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(coth(t) - coth(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asin(t) - asin(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acos(t) - acos(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(atan(t) - atan(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acsc(1/t) - acsc(1/v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asec(1/t) - asec(1/v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acot(1/t) - acot(1/v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asinh(t) - asinh(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acosh(1/t) - acosh(1/v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(atanh(t) - atanh(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acsch(1/t) - acsch(1/v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asech(t) - asech(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acoth(1/t) - acoth(1/v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asinc(t/pi) - asin(v)/(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asinhc(t/pi) - asinh(v)/(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(zero(t) - zero(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(real(t) - real(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(imag(t) - imag(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(conj(t) - conj(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sinhc(t/pi) - sinh(v)/v)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(erf(t) - erf(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(erfc(t) - erfc(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(-im*erf(t*im) - erfi(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(atan(t3,t2) - atan(3,2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(atan(t3,2) - atan(3,2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(atan(3,t2) - atan(3,2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(atan(t3,-t2) - atan(3,-2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(atan(t3,-2) - atan(3,-2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(atan(3,-t2) - atan(3,-2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(atan(-t3,-t2) - atan(-3,-2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(atan(-t3,-2) - atan(-3,-2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(atan(-3,-t2) - atan(-3,-2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(atan(-t3,t2) - atan(-3,2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(atan(-t3,2) - atan(-3,2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(atan(-3,t2) - atan(-3,2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(t2,t3) - hypot(2,3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(2,t3) - hypot(2,3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(t2,3) - hypot(2,3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(t1,t2,t3) - hypot(1,2,3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(1, t2, t3) - hypot(1,2,3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(t1, 2, t3) - hypot(1,2,3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(t1, t2, 3) - hypot(1,2,3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(1, 2, t3) - hypot(1,2,3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(1, t2, 3) - hypot(1,2,3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(t1, 2, 3) - hypot(1,2,3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(angle(t2) - angle(2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(angle(-t2) - angle(-2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(complex(t3) - complex(3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(complex(t2,t3) - complex(2,3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(polar(t2) - (abs(2)+im*atan(0,2)))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(polar(-t1) - (abs(-1)+im*atan(0,-1)))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(rect(t2) - (2*cos(0) + im*2*sin(0)))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(rect(-t1) - (-1*cos(0) + im*-1*sin(0)))))) < tol


  @test (@FastGTPSA!(@. out = abs(-t) - abs(-v) ); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sqrt(t) - sqrt(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = exp(t) - exp(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = log(t) - log(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sin(t) - sin(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = cos(t) - cos(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = tan(t) - tan(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = csc(t) - csc(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sec(t) - sec(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = cot(t) - cot(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sinc(t) - sinc(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sinh(t) - sinh(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = cosh(t) - cosh(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = tanh(t) - tanh(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = csch(t) - csch(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sech(t) - sech(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = coth(t) - coth(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asin(t) - asin(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acos(t) - acos(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = atan(t) - atan(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acsc(1/t) - acsc(1/v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asec(1/t) - asec(1/v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acot(1/t) - acot(1/v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asinh(t) - asinh(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acosh(1/t) - acosh(1/v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = atanh(t) - atanh(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acsch(1/t) - acsch(1/v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asech(t) - asech(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acoth(1/t) - acoth(1/v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asinc(t/pi) - asin(v)/(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asinhc(t/pi) - asinh(v)/(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = zero(t) - zero(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = real(t) - real(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = imag(t) - imag(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = conj(t) - conj(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sinhc(t/pi) - sinh(v)/v); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = erf(t) - erf(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = erfc(t) - erfc(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = -im*erf(t*im) - erfi(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = atan(t3,t2) - atan(3,2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = atan(t3,2) - atan(3,2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = atan(3,t2) - atan(3,2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = atan(t3,-t2) - atan(3,-2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = atan(t3,-2) - atan(3,-2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = atan(3,-t2) - atan(3,-2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = atan(-t3,-t2) - atan(-3,-2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = atan(-t3,-2) - atan(-3,-2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = atan(-3,-t2) - atan(-3,-2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = atan(-t3,t2) - atan(-3,2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = atan(-t3,2) - atan(-3,2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = atan(-3,t2) - atan(-3,2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(t2,t3) - hypot(2,3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(2,t3) - hypot(2,3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(t2,3) - hypot(2,3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(t1,t2,t3) - hypot(1,2,3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(1, t2, t3) - hypot(1,2,3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(t1, 2, t3) - hypot(1,2,3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(t1, t2, 3) - hypot(1,2,3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(1, 2, t3) - hypot(1,2,3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(1, t2, 3) - hypot(1,2,3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(t1, 2, 3) - hypot(1,2,3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = angle(t2) - angle(2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = angle(-t2) - angle(-2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = complex(t3) - complex(3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = complex(t2,t3) - complex(2,3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = polar(t2) - (abs(2)+im*atan(0,2))); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = polar(-t1) - (abs(-1)+im*atan(0,-1))); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = rect(t2) - (2*cos(0) + im*2*sin(0))); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = rect(-t1) - (-1*cos(0) + im*-1*sin(0))); norm(normTPS.(out))) < tol
  

  v = 0.5+0.5im
  t = ComplexTPS64(first(t))
  t[0] = v
  ct1 = ComplexTPS64(t)
  ct1[0] = 1 + 1im
  ct2 = zero(ct1)
  ct2[0] = 2 + 2im
  ct3 = zero(ct1)
  ct3[0] = 3 + 3im

  t = [t]
  v = [v]
  ct1 = [ct1]
  ct2 = [ct2]
  ct3 = [ct3]

  @test @FastGTPSA(norm(@.(normTPS(abs(-t) - abs(-v) )))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sqrt(t) - sqrt(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(exp(t) - exp(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(log(t) - log(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sin(t) - sin(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(cos(t) - cos(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(tan(t) - tan(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(csc(t) - csc(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sec(t) - sec(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(cot(t) - cot(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sinc(t) - sinc(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sinh(t) - sinh(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(cosh(t) - cosh(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(tanh(t) - tanh(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(csch(t) - csch(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sech(t) - sech(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(coth(t) - coth(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asin(t) - asin(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acos(t) - acos(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(atan(t) - atan(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acsc(t) - acsc(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asec(t) - asec(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acot(t) - acot(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asinh(t) - asinh(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acosh(t) - acosh(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(atanh(t) - atanh(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acsch(t) - acsch(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asech(t) - asech(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acoth(t) - acoth(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asinc(t/pi) - asin(v)/v)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asinhc(t/pi) - asinh(v)/v)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(zero(t) - zero(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(real(t) - real(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(imag(t) - imag(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(conj(t) - conj(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sinhc(t/pi) - sinh(v)/v)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(erf(t) - erf(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(erfc(t) - erfc(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(-im*erf(t*im) - erfi(v))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(ct2,ct3) - hypot(2+2im,3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(2+2im,ct3) - hypot(2+2im,3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(ct2,3+3im) - hypot(2+2im,3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(ct1,ct2,ct3) - hypot(1+1im,2+2im,3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(1+1im, ct2, ct3) - hypot(1+1im,2+2im,3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(ct1, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(ct1, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(1+1im, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(1+1im, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(ct1, 2+2im, 3+3im) - hypot(1+1im,2+2im,3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(angle(t2+im*t3) - angle(2+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(angle(t2-im*t3) - angle(2-3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(angle(-t2-im*t3) - angle(-2-3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(angle(-t2+im*t3) - angle(-2+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(angle(ct2) - angle(2+2im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(angle(-ct2) - angle(-2-2im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(complex(ct3) - complex(3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(polar(ct2) - (abs(2+2im)+im*angle(2+2im)))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(polar(-ct1) - (abs(-1-im)+im*angle(-1-im)))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(rect(ct2) - (2*cos(2) + im*2*sin(2)))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(rect(-ct1) - (-1*cos(-1) + im*-1*sin(-1)))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(ct1, ct2, t3) - hypot(1+1im,2+2im,3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(ct1, t2, ct3) - hypot(1+1im,2,3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(t1, ct2, ct3) - hypot(1,2+2im,3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(ct1, t2, t3) - hypot(1+1im,2,3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(t1, ct2, t3) - hypot(1,2+2im,3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(t1, t2, ct3) - hypot(1,2,3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(ct1,t2,3+3im) - hypot(1+1im,2,3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(t1, ct2, 3+3im) - hypot(1,2+2im,3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(ct1,2+2im,t3) - hypot(1+1im,2+2im,3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(t1,2+2im,ct3) - hypot(1,2+2im,3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(1+1im,ct2,t3) - hypot(1+1im,2+2im,3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(1+1im, t2, ct3) - hypot(1+1im,2,3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(t1,t2,3+3im) - hypot(1,2,3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(t1,2+2im,t3) - hypot(1,2+2im,3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(1+1im,t2,t3) - hypot(1+1im,2,3))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(t1,2,3+3im) - hypot(1,2,3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(1,t2,3+3im) - hypot(1,2,3+3im))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(hypot(1+1im,2,t3) - hypot(1+1im,2,3))))) < tol


  @test (@FastGTPSA!(@. out = abs(-t) - abs(-v) ); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sqrt(t) - sqrt(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = exp(t) - exp(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = log(t) - log(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sin(t) - sin(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = cos(t) - cos(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = tan(t) - tan(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = csc(t) - csc(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sec(t) - sec(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = cot(t) - cot(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sinc(t) - sinc(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sinh(t) - sinh(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = cosh(t) - cosh(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = tanh(t) - tanh(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = csch(t) - csch(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sech(t) - sech(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = coth(t) - coth(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asin(t) - asin(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acos(t) - acos(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = atan(t) - atan(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acsc(t) - acsc(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asec(t) - asec(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acot(t) - acot(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asinh(t) - asinh(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acosh(t) - acosh(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = atanh(t) - atanh(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acsch(t) - acsch(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asech(t) - asech(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acoth(t) - acoth(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asinc(t/pi) - asin(v)/v); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asinhc(t/pi) - asinh(v)/v); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = zero(t) - zero(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = real(t) - real(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = imag(t) - imag(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = conj(t) - conj(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sinhc(t/pi) - sinh(v)/v); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = erf(t) - erf(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = erfc(t) - erfc(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = -im*erf(t*im) - erfi(v)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(ct2,ct3) - hypot(2+2im,3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(2+2im,ct3) - hypot(2+2im,3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(ct2,3+3im) - hypot(2+2im,3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(ct1,ct2,ct3) - hypot(1+1im,2+2im,3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(1+1im, ct2, ct3) - hypot(1+1im,2+2im,3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(ct1, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(ct1, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(1+1im, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(1+1im, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(ct1, 2+2im, 3+3im) - hypot(1+1im,2+2im,3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = angle(t2+im*t3) - angle(2+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = angle(t2-im*t3) - angle(2-3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = angle(-t2-im*t3) - angle(-2-3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = angle(-t2+im*t3) - angle(-2+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = angle(ct2) - angle(2+2im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = angle(-ct2) - angle(-2-2im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = complex(ct3) - complex(3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = polar(ct2) - (abs(2+2im)+im*angle(2+2im))); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = polar(-ct1) - (abs(-1-im)+im*angle(-1-im))); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = rect(ct2) - (2*cos(2) + im*2*sin(2))); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = rect(-ct1) - (-1*cos(-1) + im*-1*sin(-1))); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(ct1, ct2, t3) - hypot(1+1im,2+2im,3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(ct1, t2, ct3) - hypot(1+1im,2,3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(t1, ct2, ct3) - hypot(1,2+2im,3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(ct1, t2, t3) - hypot(1+1im,2,3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(t1, ct2, t3) - hypot(1,2+2im,3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(t1, t2, ct3) - hypot(1,2,3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(ct1,t2,3+3im) - hypot(1+1im,2,3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(t1, ct2, 3+3im) - hypot(1,2+2im,3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(ct1,2+2im,t3) - hypot(1+1im,2+2im,3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(t1,2+2im,ct3) - hypot(1,2+2im,3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(1+1im,ct2,t3) - hypot(1+1im,2+2im,3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(1+1im, t2, ct3) - hypot(1+1im,2,3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(t1,t2,3+3im) - hypot(1,2,3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(t1,2+2im,t3) - hypot(1,2+2im,3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(1+1im,t2,t3) - hypot(1+1im,2,3)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(t1,2,3+3im) - hypot(1,2,3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(1,t2,3+3im) - hypot(1,2,3+3im)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = hypot(1+1im,2,t3) - hypot(1+1im,2,3)); norm(normTPS.(out))) < tol


  d = Descriptor(1, 5)
  t = TPS(use=d)
  t[0] = 0.5; t[[1]] = 2; t[[2]] = 3; t[[3]] = 4; t[[4]] = 5; t[[5]] = 6
  tol = 1e-10
  out = ComplexTPS64()
  @test @FastGTPSA(@.(normTPS(sin(t)^2+cos(t)^2 - 1))) < tol
  @test @FastGTPSA(@.(normTPS(1/sin(t) - csc(t)))) < tol
  @test @FastGTPSA(@.(normTPS(1/cos(t) - sec(t)))) < tol
  @test @FastGTPSA(@.(normTPS(1/tan(t) - cot(t)))) < tol
  @test @FastGTPSA(@.(normTPS(sin(t)/cos(t) - tan(t)))) < tol
  @test @FastGTPSA(@.(normTPS(cos(2*t) - cos(t)^2 + sin(t)^2))) < tol
  @test @FastGTPSA(@.(normTPS(sec(t)^2 - 1 - tan(t)^2))) < tol
  @test @FastGTPSA(@.(normTPS(sin(t/2) - sqrt((1-cos(t))/2)))) < tol
  @test @FastGTPSA(@.(normTPS(cos(t/2) - sqrt((1+cos(t))/2)))) < tol
  @test @FastGTPSA(@.(normTPS(sqrt(t^2) - abs(t)))) < tol
  @test @FastGTPSA(@.(normTPS(csc(t)^2 - cot(t)^2 - 1))) < tol
  @test @FastGTPSA(@.(normTPS(exp(log(t)) - t))) < tol
  @test @FastGTPSA(@.(normTPS(log(exp(t)) - t))) < tol
  @test @FastGTPSA(@.(normTPS(log(exp(t)) - exp(log(t))))) < tol
  @test @FastGTPSA(@.(normTPS(log(t^2) - 2*log(t)))) < tol
  @test @FastGTPSA(@.(normTPS(5*log(t) - log(t^5)))) < tol
  @test @FastGTPSA(@.(normTPS(t*log(5) - log(5^t)))) < tol
  @test @FastGTPSA(@.(normTPS(sinc(t) - sin(pi*t)/(pi*t)))) < tol
  @test @FastGTPSA(@.(normTPS(sinhc(t/pi) - sinh(t)/t))) < tol
  @test @FastGTPSA(@.(normTPS(exp(im*t) - cos(t) - im*sin(t)))) < tol
  @test @FastGTPSA(@.(normTPS(real(exp(im*t)) - cos(t)))) < tol
  @test @FastGTPSA(@.(normTPS(imag(exp(im*t)) - sin(t)))) < tol
  @test @FastGTPSA(@.(normTPS(sinh(t) - (exp(t) - exp(-t))/2))) < tol
  @test @FastGTPSA(@.(normTPS(cosh(t) - (exp(t) + exp(-t))/2))) < tol
  @test @FastGTPSA(@.(normTPS(tanh(t) - sinh(t)/cosh(t)))) < tol
  @test @FastGTPSA(@.(normTPS(csch(t) - 1/sinh(t)))) < tol
  @test @FastGTPSA(@.(normTPS(sech(t) - 1/cosh(t)))) < tol
  @test @FastGTPSA(@.(normTPS(coth(t) - cosh(t)/sinh(t)))) < tol
  @test @FastGTPSA(@.(normTPS(coth(t) - 1/tanh(t)))) < tol
  @test @FastGTPSA(@.(normTPS(cosh(t)^2 - sinh(t)^2 - 1))) < tol
  @test @FastGTPSA(@.(normTPS(1 - tanh(t)^2 - sech(t)^2))) < tol
  @test @FastGTPSA(@.(normTPS(coth(t)^2 - 1 - csch(t)^2))) < tol
  @test @FastGTPSA(@.(normTPS(asin(sin(t)) - t))) < tol
  @test @FastGTPSA(@.(normTPS(acos(cos(t)) - t))) < tol
  @test @FastGTPSA(@.(normTPS(atan(tan(t)) - t))) < tol
  @test @FastGTPSA(@.(normTPS(acsc(1/t) - asin(t)))) < tol
  @test @FastGTPSA(@.(normTPS(asec(1/t) - acos(t)))) < tol
  @test @FastGTPSA(@.(normTPS(acot(1/t) - atan(t)))) < tol
  @test @FastGTPSA(@.(normTPS(asinh(sinh(t)) - t))) < tol
  @test @FastGTPSA(@.(normTPS(acosh(cosh(t)) - t))) < tol
  @test @FastGTPSA(@.(normTPS(atanh(tanh(t)) - t))) < tol
  @test @FastGTPSA(@.(normTPS(acsch(t) - asinh(1/t)))) < tol
  @test @FastGTPSA(@.(normTPS(asech(t) - acosh(1/t)))) < tol
  @test @FastGTPSA(@.(normTPS(acoth(1/t) - atanh(t)))) < tol
  @test @FastGTPSA(@.(normTPS(asinc(t/pi) - asin(t)/t))) < tol
  @test @FastGTPSA(@.(normTPS(asinhc(t/pi) - asinh(t)/t))) < tol
  @test @FastGTPSA(@.(normTPS(erfc(t) - 1 + erf(t)))) < tol
  @test @FastGTPSA(@.(normTPS(erf(-t) + erf(t)))) < tol
  @test @FastGTPSA(@.(normTPS(angle(t)))) < tol
  @test @FastGTPSA(@.(normTPS(complex(t) - t))) < tol
  @test @FastGTPSA(@.(normTPS(complex(t,t) - (t+im*t)))) < tol

  @test (@FastGTPSA!(out = @. sin(t)^2+cos(t)^2 - 1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. 1/sin(t) - csc(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. 1/cos(t) - sec(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. 1/tan(t) - cot(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sin(t)/cos(t) - tan(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. cos(2*t) - cos(t)^2 + sin(t)^2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sec(t)^2 - 1 - tan(t)^2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sin(t/2) - sqrt((1-cos(t))/2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. cos(t/2) - sqrt((1+cos(t))/2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sqrt(t^2) - abs(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. csc(t)^2 - cot(t)^2 - 1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. exp(log(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. log(exp(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. log(exp(t)) - exp(log(t))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. log(t^2) - 2*log(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. 5*log(t) - log(t^5)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t*log(5) - log(5^t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sinc(t) - sin(pi*t)/(pi*t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sinhc(t/pi) - sinh(t)/t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. exp(im*t) - cos(t) - im*sin(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. real(exp(im*t)) - cos(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. imag(exp(im*t)) - sin(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sinh(t) - (exp(t) - exp(-t))/2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. cosh(t) - (exp(t) + exp(-t))/2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. tanh(t) - sinh(t)/cosh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. csch(t) - 1/sinh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sech(t) - 1/cosh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. coth(t) - cosh(t)/sinh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. coth(t) - 1/tanh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. cosh(t)^2 - sinh(t)^2 - 1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. 1 - tanh(t)^2 - sech(t)^2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. coth(t)^2 - 1 - csch(t)^2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asin(sin(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acos(cos(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. atan(tan(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acsc(1/t) - asin(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asec(1/t) - acos(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acot(1/t) - atan(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asinh(sinh(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acosh(cosh(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. atanh(tanh(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acsch(t) - asinh(1/t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asech(t) - acosh(1/t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acoth(1/t) - atanh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asinc(t/pi) - asin(t)/t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asinhc(t/pi) - asinh(t)/t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. erfc(t) - 1 + erf(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. erf(-t) + erf(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. angle(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. complex(t) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. complex(t,t) - (t+im*t)); normTPS(out)) < tol

  t = ComplexTPS64(t)
  t[0] = 0.5+0.5im; t[[1]] = 2+2im; t[[2]] = 3+3im; t[[3]] = 4+4im; t[[4]] = 5+5im; t[[5]] = 6+6im
  @test @FastGTPSA(@.(normTPS(sin(t)^2+cos(t)^2 - 1))) < tol
  @test @FastGTPSA(@.(normTPS(1/sin(t) - csc(t)))) < tol
  @test @FastGTPSA(@.(normTPS(1/cos(t) - sec(t)))) < tol
  @test @FastGTPSA(@.(normTPS(1/tan(t) - cot(t)))) < tol
  @test @FastGTPSA(@.(normTPS(sin(t)/cos(t) - tan(t)))) < tol
  @test @FastGTPSA(@.(normTPS(cos(2*t) - cos(t)^2 + sin(t)^2))) < tol
  @test @FastGTPSA(@.(normTPS(sec(t)^2 - 1 - tan(t)^2))) < tol
  @test @FastGTPSA(@.(normTPS(sin(t/2) - sqrt((1-cos(t))/2)))) < tol
  @test @FastGTPSA(@.(normTPS(cos(t/2) - sqrt((1+cos(t))/2)))) < tol
  @test @FastGTPSA(@.(normTPS(sqrt(t^2) - t))) < tol
  @test @FastGTPSA(@.(normTPS(csc(t)^2 - cot(t)^2 - 1))) < tol
  @test @FastGTPSA(@.(normTPS(exp(log(t)) - t))) < tol
  @test @FastGTPSA(@.(normTPS(log(exp(t)) - t))) < tol
  @test @FastGTPSA(@.(normTPS(log(exp(t)) - exp(log(t))))) < tol
  @test @FastGTPSA(@.(normTPS(log(t^2) - 2*log(t)))) < tol
  @test @FastGTPSA(@.(normTPS(5*log(t) - log(t^5) - 2*pi*im))) < tol
  @test @FastGTPSA(@.(normTPS(t*log(5) - log(5^t)))) < tol
  @test @FastGTPSA(@.(normTPS(sinc(t/pi) - sin(t)/t))) < tol
  @test @FastGTPSA(@.(normTPS(sinhc(t/pi) - sinh(t)/t))) < tol
  @test @FastGTPSA(@.(normTPS(exp(im*t) - cos(t) - im*sin(t)))) < tol
  @test @FastGTPSA(@.(normTPS(sinh(t) - (exp(t) - exp(-t))/2))) < tol
  @test @FastGTPSA(@.(normTPS(cosh(t) - (exp(t) + exp(-t))/2))) < tol
  @test @FastGTPSA(@.(normTPS(tanh(t) - sinh(t)/cosh(t)))) < tol
  @test @FastGTPSA(@.(normTPS(csch(t) - 1/sinh(t)))) < tol
  @test @FastGTPSA(@.(normTPS(sech(t) - 1/cosh(t)))) < tol
  @test @FastGTPSA(@.(normTPS(coth(t) - cosh(t)/sinh(t)))) < tol
  @test @FastGTPSA(@.(normTPS(coth(t) - 1/tanh(t)))) < tol
  @test @FastGTPSA(@.(normTPS(cosh(t)^2 - sinh(t)^2 - 1))) < tol
  @test @FastGTPSA(@.(normTPS(1 - tanh(t)^2 - sech(t)^2))) < tol
  @test @FastGTPSA(@.(normTPS(coth(t)^2 - 1 - csch(t)^2))) < tol
  @test @FastGTPSA(@.(normTPS(asin(sin(t)) - t))) < tol
  @test @FastGTPSA(@.(normTPS(acos(cos(t)) - t))) < tol
  @test @FastGTPSA(@.(normTPS(atan(tan(t)) - t))) < tol
  @test @FastGTPSA(@.(normTPS(acsc(t) - asin(1/t)))) < tol
  @test @FastGTPSA(@.(normTPS(asec(t) - acos(1/t)))) < tol
  @test @FastGTPSA(@.(normTPS(acot(t) - atan(1/t)))) < tol
  @test @FastGTPSA(@.(normTPS(asinh(sinh(t)) - t))) < tol
  @test @FastGTPSA(@.(normTPS(acosh(cosh(t)) - t))) < tol
  @test @FastGTPSA(@.(normTPS(atanh(tanh(t)) - t))) < tol
  @test @FastGTPSA(@.(normTPS(acsch(t) - asinh(1/t)))) < tol
  @test @FastGTPSA(@.(normTPS(asech(t) - acosh(1/t)))) < tol
  @test @FastGTPSA(@.(normTPS(acoth(t) - atanh(1/t)))) < tol
  @test @FastGTPSA(@.(normTPS(asinc(t/pi) - asin(t)/t))) < tol
  @test @FastGTPSA(@.(normTPS(asinhc(t/pi) - asinh(t)/t))) < tol
  @test @FastGTPSA(@.(normTPS(erfc(t) - 1 + erf(t)))) < tol
  @test @FastGTPSA(@.(normTPS(erf(-t) + erf(t)))) < tol
  @test @FastGTPSA(@.(normTPS(angle(t) - atan(imag(t),real(t))))) < tol
  @test @FastGTPSA(@.(normTPS(complex(t) - t))) < tol


  @test (@FastGTPSA!(out = @. sin(t)^2+cos(t)^2 - 1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. 1/sin(t) - csc(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. 1/cos(t) - sec(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. 1/tan(t) - cot(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sin(t)/cos(t) - tan(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. cos(2*t) - cos(t)^2 + sin(t)^2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sec(t)^2 - 1 - tan(t)^2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sin(t/2) - sqrt((1-cos(t))/2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. cos(t/2) - sqrt((1+cos(t))/2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sqrt(t^2) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. csc(t)^2 - cot(t)^2 - 1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. exp(log(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. log(exp(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. log(exp(t)) - exp(log(t))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. log(t^2) - 2*log(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. 5*log(t) - log(t^5) - 2*pi*im); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. t*log(5) - log(5^t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sinc(t/pi) - sin(t)/t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sinhc(t/pi) - sinh(t)/t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. exp(im*t) - cos(t) - im*sin(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sinh(t) - (exp(t) - exp(-t))/2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. cosh(t) - (exp(t) + exp(-t))/2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. tanh(t) - sinh(t)/cosh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. csch(t) - 1/sinh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. sech(t) - 1/cosh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. coth(t) - cosh(t)/sinh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. coth(t) - 1/tanh(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. cosh(t)^2 - sinh(t)^2 - 1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. 1 - tanh(t)^2 - sech(t)^2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. coth(t)^2 - 1 - csch(t)^2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asin(sin(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acos(cos(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. atan(tan(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acsc(t) - asin(1/t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asec(t) - acos(1/t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acot(t) - atan(1/t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asinh(sinh(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acosh(cosh(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. atanh(tanh(t)) - t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acsch(t) - asinh(1/t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asech(t) - acosh(1/t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. acoth(t) - atanh(1/t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asinc(t/pi) - asin(t)/t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. asinhc(t/pi) - asinh(t)/t); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. erfc(t) - 1 + erf(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. erf(-t) + erf(t)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. angle(t) - atan(imag(t),real(t))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = @. complex(t) - t); normTPS(out)) < tol

  d = Descriptor(1, 5)
  t = TPS(use=d)
  t[0] = 0.5; t[[1]] = 2; t[[2]] = 3; t[[3]] = 4; t[[4]] = 5; t[[5]] = 6
  tol = 1e-10
  t = [t]
  out = [out]
  @test @FastGTPSA(norm(@.(normTPS(sin(t)^2+cos(t)^2 - 1)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(1/sin(t) - csc(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(1/cos(t) - sec(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(1/tan(t) - cot(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sin(t)/cos(t) - tan(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(cos(2*t) - cos(t)^2 + sin(t)^2)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sec(t)^2 - 1 - tan(t)^2)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sin(t/2) - sqrt((1-cos(t))/2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(cos(t/2) - sqrt((1+cos(t))/2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sqrt(t^2) - abs(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(csc(t)^2 - cot(t)^2 - 1)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(exp(log(t)) - t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(log(exp(t)) - t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(log(exp(t)) - exp(log(t)))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(log(t^2) - 2*log(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(5*log(t) - log(t^5))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t*log(5) - log(5^t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sinc(t) - sin(pi*t)/(pi*t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sinhc(t/pi) - sinh(t)/t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(exp(im*t) - cos(t) - im*sin(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(real(exp(im*t)) - cos(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(imag(exp(im*t)) - sin(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sinh(t) - (exp(t) - exp(-t))/2)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(cosh(t) - (exp(t) + exp(-t))/2)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(tanh(t) - sinh(t)/cosh(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(csch(t) - 1/sinh(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sech(t) - 1/cosh(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(coth(t) - cosh(t)/sinh(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(coth(t) - 1/tanh(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(cosh(t)^2 - sinh(t)^2 - 1)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(1 - tanh(t)^2 - sech(t)^2)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(coth(t)^2 - 1 - csch(t)^2)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asin(sin(t)) - t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acos(cos(t)) - t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(atan(tan(t)) - t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acsc(1/t) - asin(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asec(1/t) - acos(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acot(1/t) - atan(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asinh(sinh(t)) - t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acosh(cosh(t)) - t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(atanh(tanh(t)) - t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acsch(t) - asinh(1/t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asech(t) - acosh(1/t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acoth(1/t) - atanh(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asinc(t/pi) - asin(t)/t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asinhc(t/pi) - asinh(t)/t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(erfc(t) - 1 + erf(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(erf(-t) + erf(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(angle(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(complex(t) - t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(complex(t,t) - (t+im*t))))) < tol

  @test (@FastGTPSA!(@. out = sin(t)^2+cos(t)^2 - 1); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = 1/sin(t) - csc(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = 1/cos(t) - sec(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = 1/tan(t) - cot(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sin(t)/cos(t) - tan(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = cos(2*t) - cos(t)^2 + sin(t)^2); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sec(t)^2 - 1 - tan(t)^2); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sin(t/2) - sqrt((1-cos(t))/2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = cos(t/2) - sqrt((1+cos(t))/2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sqrt(t^2) - abs(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = csc(t)^2 - cot(t)^2 - 1); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = exp(log(t)) - t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = log(exp(t)) - t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = log(exp(t)) - exp(log(t))); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = log(t^2) - 2*log(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = 5*log(t) - log(t^5)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t*log(5) - log(5^t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sinc(t) - sin(pi*t)/(pi*t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sinhc(t/pi) - sinh(t)/t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = exp(im*t) - cos(t) - im*sin(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = real(exp(im*t)) - cos(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = imag(exp(im*t)) - sin(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sinh(t) - (exp(t) - exp(-t))/2); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = cosh(t) - (exp(t) + exp(-t))/2); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = tanh(t) - sinh(t)/cosh(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = csch(t) - 1/sinh(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sech(t) - 1/cosh(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = coth(t) - cosh(t)/sinh(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = coth(t) - 1/tanh(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = cosh(t)^2 - sinh(t)^2 - 1); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = 1 - tanh(t)^2 - sech(t)^2); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = coth(t)^2 - 1 - csch(t)^2); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asin(sin(t)) - t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acos(cos(t)) - t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = atan(tan(t)) - t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acsc(1/t) - asin(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asec(1/t) - acos(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acot(1/t) - atan(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asinh(sinh(t)) - t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acosh(cosh(t)) - t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = atanh(tanh(t)) - t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acsch(t) - asinh(1/t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asech(t) - acosh(1/t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acoth(1/t) - atanh(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asinc(t/pi) - asin(t)/t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asinhc(t/pi) - asinh(t)/t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = erfc(t) - 1 + erf(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = erf(-t) + erf(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = angle(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = complex(t) - t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = complex(t,t) - (t+im*t)); norm(normTPS.(out))) < tol

  t = ComplexTPS64(first(t))
  t[0] = 0.5+0.5im; t[[1]] = 2+2im; t[[2]] = 3+3im; t[[3]] = 4+4im; t[[4]] = 5+5im; t[[5]] = 6+6im
  t = [t]
  @test @FastGTPSA(norm(@.(normTPS(sin(t)^2+cos(t)^2 - 1)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(1/sin(t) - csc(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(1/cos(t) - sec(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(1/tan(t) - cot(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sin(t)/cos(t) - tan(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(cos(2*t) - cos(t)^2 + sin(t)^2)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sec(t)^2 - 1 - tan(t)^2)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sin(t/2) - sqrt((1-cos(t))/2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(cos(t/2) - sqrt((1+cos(t))/2))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sqrt(t^2) - t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(csc(t)^2 - cot(t)^2 - 1)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(exp(log(t)) - t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(log(exp(t)) - t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(log(exp(t)) - exp(log(t)))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(log(t^2) - 2*log(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(5*log(t) - log(t^5) - 2*pi*im)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(t*log(5) - log(5^t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sinc(t/pi) - sin(t)/t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sinhc(t/pi) - sinh(t)/t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(exp(im*t) - cos(t) - im*sin(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sinh(t) - (exp(t) - exp(-t))/2)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(cosh(t) - (exp(t) + exp(-t))/2)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(tanh(t) - sinh(t)/cosh(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(csch(t) - 1/sinh(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(sech(t) - 1/cosh(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(coth(t) - cosh(t)/sinh(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(coth(t) - 1/tanh(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(cosh(t)^2 - sinh(t)^2 - 1)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(1 - tanh(t)^2 - sech(t)^2)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(coth(t)^2 - 1 - csch(t)^2)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asin(sin(t)) - t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acos(cos(t)) - t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(atan(tan(t)) - t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acsc(t) - asin(1/t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asec(t) - acos(1/t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acot(t) - atan(1/t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asinh(sinh(t)) - t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acosh(cosh(t)) - t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(atanh(tanh(t)) - t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acsch(t) - asinh(1/t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asech(t) - acosh(1/t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(acoth(t) - atanh(1/t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asinc(t/pi) - asin(t)/t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(asinhc(t/pi) - asinh(t)/t)))) < tol
  @test @FastGTPSA(norm(@.(normTPS(erfc(t) - 1 + erf(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(erf(-t) + erf(t))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(angle(t) - atan(imag(t),real(t)))))) < tol
  @test @FastGTPSA(norm(@.(normTPS(complex(t) - t)))) < tol


  @test (@FastGTPSA!(@. out = sin(t)^2+cos(t)^2 - 1); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = 1/sin(t) - csc(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = 1/cos(t) - sec(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = 1/tan(t) - cot(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sin(t)/cos(t) - tan(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = cos(2*t) - cos(t)^2 + sin(t)^2); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sec(t)^2 - 1 - tan(t)^2); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sin(t/2) - sqrt((1-cos(t))/2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = cos(t/2) - sqrt((1+cos(t))/2)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sqrt(t^2) - t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = csc(t)^2 - cot(t)^2 - 1); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = exp(log(t)) - t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = log(exp(t)) - t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = log(exp(t)) - exp(log(t))); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = log(t^2) - 2*log(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = 5*log(t) - log(t^5) - 2*pi*im); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = t*log(5) - log(5^t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sinc(t/pi) - sin(t)/t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sinhc(t/pi) - sinh(t)/t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = exp(im*t) - cos(t) - im*sin(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sinh(t) - (exp(t) - exp(-t))/2); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = cosh(t) - (exp(t) + exp(-t))/2); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = tanh(t) - sinh(t)/cosh(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = csch(t) - 1/sinh(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = sech(t) - 1/cosh(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = coth(t) - cosh(t)/sinh(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = coth(t) - 1/tanh(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = cosh(t)^2 - sinh(t)^2 - 1); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = 1 - tanh(t)^2 - sech(t)^2); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = coth(t)^2 - 1 - csch(t)^2); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asin(sin(t)) - t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acos(cos(t)) - t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = atan(tan(t)) - t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acsc(t) - asin(1/t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asec(t) - acos(1/t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acot(t) - atan(1/t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asinh(sinh(t)) - t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acosh(cosh(t)) - t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = atanh(tanh(t)) - t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acsch(t) - asinh(1/t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asech(t) - acosh(1/t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = acoth(t) - atanh(1/t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asinc(t/pi) - asin(t)/t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = asinhc(t/pi) - asinh(t)/t); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = erfc(t) - 1 + erf(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = erf(-t) + erf(t)); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = angle(t) - atan(imag(t),real(t))); norm(normTPS.(out))) < tol
  @test (@FastGTPSA!(@. out = complex(t) - t); norm(normTPS.(out))) < tol

  # Make sure stack is 0:
  @test GTPSA.checktemps()
end

@testset "FastGTPSA - Block" begin
  d = Descriptor(3, 7); x = vars(d); y= rand(3)
  tol = 1e-14
  @FastGTPSA begin
    t1 = x[1]^3*sin(x[2])/log(2+x[3])-exp(x[1]*x[2])*im;
    t2 = x[1]^3*sin(x[2])/log(2+x[3])-exp(x[1]*x[2])*im;
    z  = y[1]^3*sin(y[2])/log(2+y[3])-exp(y[1]*y[2])*im;
  end

  begin
    tt1 = x[1]^3*sin(x[2])/log(2+x[3])-exp(x[1]*x[2])*im;
    tt2 = x[1]^3*sin(x[2])/log(2+x[3])-exp(x[1]*x[2])*im;
    tz  = y[1]^3*sin(y[2])/log(2+y[3])-exp(y[1]*y[2])*im;
  end
  @test normTPS(tt1-t1) < tol
  @test normTPS(tt2-t2) < tol
  @test norm(tz-z) < tol

  y = TPS.(y)
  z = rand(3)
  w = rand(3)
  @FastGTPSA begin
    t1 = @. x+sin(y)/log(2+x)*exp(y)*im+7*y
    t2 = @. y+sin(x)/log(2+y)*exp(x)*im+7*x
    a  = @. z+sin(w)/log(2+z)*exp(w)*im+7*w
  end

  begin
    tt1 = @. x+sin(y)/log(2+x)*exp(y)*im+7*y
    tt2 = @. y+sin(x)/log(2+y)*exp(x)*im+7*x
    ta  = @. z+sin(w)/log(2+z)*exp(w)*im+7*w
  end
  @test norm(normTPS.(tt1-t1)) < tol
  @test norm(normTPS.(tt2-t2)) < tol
  @test norm(ta-a) < tol


  d = Descriptor(1, 5)
  t = TPS(use=d)
  ct = ComplexTPS64(t)
  # Set scalar part so both TPSs are 1
  t[0] = 1
  ct[0] = 1
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

  t = ComplexTPS64(t)
  t[0] = 0.5+0.5im; t[[1]] = 2+2im; t[[2]] = 3+3im; t[[3]] = 4+4im; t[[4]] = 5+5im; t[[5]] = 6+6im

  tol = 1e-14

  @FastGTPSA begin
    y1 = t1 + t2 - t3
    y2 = t2 + t1 - t3
    y3 = t1 + 2 - t3
    y4 = 2 + t1 - t3
    y5 = t3 - t2 - t1
    y6 = t2 - t3 - -t1
    y7 = t3 - 2 - t1
    y8 = 2 - t3 - -t1
    y9 = t2 * t3 - 6
    y10 = t3 * t2 - 6
    y11 = t2 * 5 - 10
    y12 = 5 * t2 - 10 * t1
    y13 = t1 / t2 - 1/2
    y14 = t2 / t1 - 2
    y15 = 1 / t2 - 1/2
    y16 = t2 / 3 - 2/3
    y17 = t2 / t2 - t1
    y18 = t2 / t2 - 1
    y19 = t2 ^ t3 - 8
    y20 = t3 ^ t2 - 9
    y21 = t2 ^ 3 - 8
    y22 = t2 ^ (1/2) - sqrt(2)
    y23 = t2 ^ (1/2) - sqrt(t2)
    y24 = 2 ^ t3 - 8
    y25 = inv(t3) - 1/t3
    y26 = inv(t3) - 1/3
    y27 = ct1 + ct2 - ct3
    y28 = ct2 + ct1 - ct3
    y29 = ct1 + (2+2im) - ct3
    y30 = (2+2im) + ct1 - ct3
    y31 = ct3 - ct2 - ct1
    y32 = ct2 - ct3 - -ct1
    y33 = ct3 - (2+2im) - ct1
    y34 = (2+2im) - ct3 - -ct1
    y35 = ct2 * ct3 - (2+2im)*(3+3im)
    y36 = ct3 * ct2 - (2+2im)*(3+3im)
    y37 = ct2 * 5 - (10+10im)
    y38 = 5 * ct2 - (10 * ct1)
    y39 = ct1 / ct2 - (1+im)/(2+2im)
    y40 = ct2 / ct1 - 2
    y41 = 1 / ct2 - 1/(2+2im)
    y42 = ct2 / 3 - (2+2im)/3
    y43 = ct2 / ct2 - 1
    y44 = ct2 ^ ct3 - (2+2im)^(3+3im)
    y45 = ct3 ^ ct2 - (3+3im)^(2+2im)
    y46 = ct2 ^ 3 - (2+2im)^3
    y47 = ct2 ^ (1/2) - sqrt(2+2im)
    y48 = ct2 ^ (1/2) - sqrt(ct2)
    y49 = 2 ^ ct3 - 2^(3+3im)
    y50 = inv(ct3) - 1/ct3
    y51 = inv(ct3) - 1/(3+3im)
    y52 = t1 + ct2 - (1 + (2+2im))
    y53 = ct2 + t1 - (1 + (2+2im))
    y54 = t1 + (2+2im) - (1 + (2+2im))
    y55 = (2+2im) + t1 - (1 + (2+2im))
    y56 = t3 - ct2 - (3 - (2+2im))
    y57 = ct2 - t3 - ((2+2im) - 3)
    y58 = t3 - (2+2im) - (3 - (2+2im))
    y59 = (2+2im) - t3 - ((2+2im) - 3)
    y60 = t2 * ct3 - 2 * (3+3im)
    y61 = ct3 * t2 - 2 * (3+3im)
    y62 = t2 * (3+3im) - 2 * (3+3im)
    y63 = (3+3im) * t2 - 2 * (3+3im)
    y64 = t2 / ct3 - 2/(3+3im)
    y65 = ct3 / t2 - (3+3im)/2
    y66 = t2 / (3+3im) - 2/(3+3im)
    y67 = (3+3im) / t2 - (3+3im)/2
    y68 = t2 ^ ct3 - 2^(3+3im)
    y69 = ct3 ^ t2 - (3+3im)^2
    y70 = t2 ^ (3+3im) - 2^(3+3im)
    y71 = (3+3im)^t2 - (3+3im)^2
    y72 = sin(t)^2+cos(t)^2 - 1
    y73 = 1/sin(t) - csc(t)
    y74 = 1/cos(t) - sec(t)
    y75 = 1/tan(t) - cot(t)
    y76 = sin(t)/cos(t) - tan(t)
    y77 = cos(2*t) - cos(t)^2 + sin(t)^2
    y78 = sec(t)^2 - 1 - tan(t)^2
    y79 = sin(t/2) - sqrt((1-cos(t))/2)
    y80 = cos(t/2) - sqrt((1+cos(t))/2)
    y81 = sqrt(t^2) - abs(t)
    y82 = csc(t)^2 - cot(t)^2 - 1
    y83 = exp(log(t)) - t
    y84 = log(exp(t)) - t
    y85 = log(exp(t)) - exp(log(t))
    y86 = log(t^2) - 2*log(t)
    y87 = 5*log(t) - log(t^5)
    y88 = t*log(5) - log(5^t)
    y89 = sinc(t) - sin(pi*t)/(pi*t)
    y90 = sinhc(t/pi) - sinh(t)/t
    y91 = exp(im*t) - cos(t) - im*sin(t)
    y92 = real(exp(im*t)) - cos(t)
    y93 = imag(exp(im*t)) - sin(t)
    y94 = sinh(t) - (exp(t) - exp(-t))/2
    y95 = cosh(t) - (exp(t) + exp(-t))/2
    y96 = tanh(t) - sinh(t)/cosh(t)
    y97 = csch(t) - 1/sinh(t)
    y98 = sech(t) - 1/cosh(t)
    y99 = coth(t) - cosh(t)/sinh(t)
    y100 = coth(t) - 1/tanh(t)
    y101 = cosh(t)^2 - sinh(t)^2 - 1
    y102 = 1 - tanh(t)^2 - sech(t)^2
    y103 = coth(t)^2 - 1 - csch(t)^2
    y104 = asin(sin(t)) - t
    y105 = acos(cos(t)) - t
    y106 = atan(tan(t)) - t
    y107 = acsc(1/t) - asin(t)
    y108 = asec(1/t) - acos(t)
    y109 = acot(1/t) - atan(t)
    y110 = asinh(sinh(t)) - t
    y111 = acosh(cosh(t)) - t
    y112 = atanh(tanh(t)) - t
    y113 = acsch(t) - asinh(1/t)
    y114 = asech(t) - acosh(1/t)
    y115 = acoth(1/t) - atanh(t)
    y116 = asinc(t/pi) - asin(t)/t
    y117 = asinhc(t/pi) - asinh(t)/t
    y118 = erfc(t) - 1 + erf(t)
    y119 = erf(-t) + erf(t)
    y120 = angle(t)
    y121 = complex(t) - t
    y122 = complex(real(t),real(t)) - (t+im*t)
    y123 = sin(t)^2+cos(t)^2 - 1
    y124 = 1/sin(t) - csc(t)
    y125 = 1/cos(t) - sec(t)
    y126 = 1/tan(t) - cot(t)
    y127 = sin(t)/cos(t) - tan(t)
    y128 = cos(2*t) - cos(t)^2 + sin(t)^2
    y129 = sec(t)^2 - 1 - tan(t)^2
    y130 = sin(t/2) - sqrt((1-cos(t))/2)
    y131 = cos(t/2) - sqrt((1+cos(t))/2)
    y132 = sqrt(t^2) - t
    y133 = csc(t)^2 - cot(t)^2 - 1
    y134 = exp(log(t)) - t
    y135 = log(exp(t)) - t
    y136 = log(exp(t)) - exp(log(t))
    y137 = log(t^2) - 2*log(t)
    y138 = 5*log(t) - log(t^5) - 2*pi*im
    y139 = t*log(5) - log(5^t)
    y140 = sinc(t/pi) - sin(t)/t
    y141 = sinhc(t/pi) - sinh(t)/t
    y142 = exp(im*t) - cos(t) - im*sin(t)
    y143 = sinh(t) - (exp(t) - exp(-t))/2
    y144 = cosh(t) - (exp(t) + exp(-t))/2
    y145 = tanh(t) - sinh(t)/cosh(t)
    y146 = csch(t) - 1/sinh(t)
    y147 = sech(t) - 1/cosh(t)
    y148 = coth(t) - cosh(t)/sinh(t)
    y149 = coth(t) - 1/tanh(t)
    y150 = cosh(t)^2 - sinh(t)^2 - 1
    y151 = 1 - tanh(t)^2 - sech(t)^2
    y152 = coth(t)^2 - 1 - csch(t)^2
    y153 = asin(sin(t)) - t
    y154 = acos(cos(t)) - t
    y155 = atan(tan(t)) - t
    y156 = acsc(t) - asin(1/t)
    y157 = asec(t) - acos(1/t)
    y158 = acot(t) - atan(1/t)
    y159 = asinh(sinh(t)) - t
    y160 = acosh(cosh(t)) - t
    y161 = atanh(tanh(t)) - t
    y162 = acsch(t) - asinh(1/t)
    y163 = asech(t) - acosh(1/t)
    y164 = acoth(t) - atanh(1/t)
    y165 = asinc(t/pi) - asin(t)/t
    y166 = asinhc(t/pi) - asinh(t)/t
    y167 = erfc(t) - 1 + erf(t)
    y168 = erf(-t) + erf(t)
    y169 = angle(t) - atan(imag(t),real(t))
    y170 = complex(t) - t
  end

  @test normTPS(y1 ) < tol
  @test normTPS(y2 ) < tol
  @test normTPS(y3 ) < tol
  @test normTPS(y4 ) < tol
  @test normTPS(y5 ) < tol
  @test normTPS(y6 ) < tol
  @test normTPS(y7 ) < tol
  @test normTPS(y8 ) < tol
  @test normTPS(y9 ) < tol
  @test normTPS(y10) < tol
  @test normTPS(y11) < tol
  @test normTPS(y12) < tol
  @test normTPS(y13) < tol
  @test normTPS(y14) < tol
  @test normTPS(y15) < tol
  @test normTPS(y16) < tol
  @test normTPS(y17) < tol
  @test normTPS(y18) < tol
  @test normTPS(y19) < tol
  @test normTPS(y20) < tol
  @test normTPS(y21) < tol
  @test normTPS(y22) < tol
  @test normTPS(y23) < tol
  @test normTPS(y24) < tol
  @test normTPS(y25) < tol
  @test normTPS(y26) < tol
  @test normTPS(y27) < tol
  @test normTPS(y28) < tol
  @test normTPS(y29) < tol
  @test normTPS(y30) < tol
  @test normTPS(y31) < tol
  @test normTPS(y32) < tol
  @test normTPS(y33) < tol
  @test normTPS(y34) < tol
  @test normTPS(y35) < tol
  @test normTPS(y36) < tol
  @test normTPS(y37) < tol
  @test normTPS(y38) < tol
  @test normTPS(y39) < tol
  @test normTPS(y40) < tol
  @test normTPS(y41) < tol
  @test normTPS(y42) < tol
  @test normTPS(y43) < tol
  @test normTPS(y44) < tol
  @test normTPS(y45) < tol
  @test normTPS(y46) < tol
  @test normTPS(y47) < tol
  @test normTPS(y48) < tol
  @test normTPS(y49) < tol
  @test normTPS(y50) < tol
  @test normTPS(y51) < tol
  @test normTPS(y52) < tol
  @test normTPS(y53) < tol
  @test normTPS(y54) < tol
  @test normTPS(y55) < tol
  @test normTPS(y56) < tol
  @test normTPS(y57) < tol
  @test normTPS(y58) < tol
  @test normTPS(y59) < tol
  @test normTPS(y60) < tol
  @test normTPS(y61) < tol
  @test normTPS(y62) < tol
  @test normTPS(y63) < tol
  @test normTPS(y64) < tol
  @test normTPS(y65) < tol
  @test normTPS(y66) < tol
  @test normTPS(y67) < tol
  @test normTPS(y68) < tol
  @test normTPS(y69) < tol
  @test normTPS(y70) < tol
  @test normTPS(y71) < tol


  out = ComplexTPS64()
  # TPS:
  @test (@FastGTPSA!(out = t1 + t2 - t3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 + t1 - t3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t1 + 2 - t3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 2 + t1 - t3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t3 - t2 - t1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 - t3 - -t1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t3 - 2 - t1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 2 - t3 - -t1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 * t3 - 6); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t3 * t2 - 6); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 * 5 - 10); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 5 * t2 - 10 * t1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t1 / t2 - 1/2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 / t1 - 2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 1 / t2 - 1/2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 / 3 - 2/3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 / t2 - t1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 / t2 - 1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 ^ t3 - 8); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t3 ^ t2 - 9); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 ^ 3 - 8); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 ^ (1/2) - sqrt(2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 ^ (1/2) - sqrt(t2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 2 ^ t3 - 8); normTPS(out)) < tol
  @test (@FastGTPSA!(out = inv(t3) - 1/t3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = inv(t3) - 1/3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct1 + ct2 - ct3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 + ct1 - ct3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct1 + (2+2im) - ct3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = (2+2im) + ct1 - ct3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct3 - ct2 - ct1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 - ct3 - -ct1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct3 - (2+2im) - ct1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = (2+2im) - ct3 - -ct1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 * ct3 - (2+2im)*(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct3 * ct2 - (2+2im)*(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 * 5 - (10+10im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 5 * ct2 - (10 * ct1)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct1 / ct2 - (1+im)/(2+2im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 / ct1 - 2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 1 / ct2 - 1/(2+2im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 / 3 - (2+2im)/3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 / ct2 - 1); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 ^ ct3 - (2+2im)^(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct3 ^ ct2 - (3+3im)^(2+2im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 ^ 3 - (2+2im)^3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 ^ (1/2) - sqrt(2+2im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 ^ (1/2) - sqrt(ct2)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = 2 ^ ct3 - 2^(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = inv(ct3) - 1/ct3); normTPS(out)) < tol
  @test (@FastGTPSA!(out = inv(ct3) - 1/(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t1 + ct2 - (1 + (2+2im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 + t1 - (1 + (2+2im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t1 + (2+2im) - (1 + (2+2im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = (2+2im) + t1 - (1 + (2+2im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t3 - ct2 - (3 - (2+2im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct2 - t3 - ((2+2im) - 3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t3 - (2+2im) - (3 - (2+2im))); normTPS(out)) < tol
  @test (@FastGTPSA!(out = (2+2im) - t3 - ((2+2im) - 3)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 * ct3 - 2 * (3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct3 * t2 - 2 * (3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 * (3+3im) - 2 * (3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = (3+3im) * t2 - 2 * (3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 / ct3 - 2/(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct3 / t2 - (3+3im)/2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 / (3+3im) - 2/(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = (3+3im) / t2 - (3+3im)/2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 ^ ct3 - 2^(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = ct3 ^ t2 - (3+3im)^2); normTPS(out)) < tol
  @test (@FastGTPSA!(out = t2 ^ (3+3im) - 2^(3+3im)); normTPS(out)) < tol
  @test (@FastGTPSA!(out = (3+3im)^t2 - (3+3im)^2); normTPS(out)) < tol



  @test GTPSA.checktemps()
end


@testset "FastGTPSA - Allocations" begin
  d = Descriptor(1, 5)
  t = TPS(use=d)
  ct = ComplexTPS64(t)
  # Set scalar part so both TPSs are 1
  t[0] = 1
  ct[0] = 1
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
  out = ComplexTPS64()

  # TPS:
  @test @allocations(@FastGTPSA(normTPS(t1 + t2 - t3))) == 0
  @test @allocations(@FastGTPSA(normTPS(t2 + t1 - t3))) == 0
  @test @allocations(@FastGTPSA(normTPS(t1 + 2 - t3))) == 0
  @test @allocations(@FastGTPSA(normTPS(2 + t1 - t3))) == 0
  @test @allocations(@FastGTPSA(normTPS(t3 - t2 - t1))) == 0
  @test @allocations(@FastGTPSA(normTPS(t2 - t3 - -t1))) == 0
  @test @allocations(@FastGTPSA(normTPS(t3 - 2 - t1))) == 0
  @test @allocations(@FastGTPSA(normTPS(2 - t3 - -t1))) == 0
  @test @allocations(@FastGTPSA(normTPS(t2 * t3 - 6))) == 0
  @test @allocations(@FastGTPSA(normTPS(t3 * t2 - 6))) == 0
  @test @allocations(@FastGTPSA(normTPS(t2 * 5 - 10))) == 0
  @test @allocations(@FastGTPSA(normTPS(5 * t2 - 10 * t1))) == 0
  @test @allocations(@FastGTPSA(normTPS(t1 / t2 - 1/2))) == 0
  @test @allocations(@FastGTPSA(normTPS(t2 / t1 - 2))) == 0
  @test @allocations(@FastGTPSA(normTPS(1 / t2 - 1/2))) == 0
  @test @allocations(@FastGTPSA(normTPS(t2 / 3 - 2/3))) == 0
  @test @allocations(@FastGTPSA(normTPS(t2 / t2 - t1))) == 0
  @test @allocations(@FastGTPSA(normTPS(t2 / t2 - 1))) == 0
  @test @allocations(@FastGTPSA(normTPS(t2 ^ t3 - 8))) == 0
  @test @allocations(@FastGTPSA(normTPS(t3 ^ t2 - 9))) == 0
  @test @allocations(@FastGTPSA(normTPS(t2 ^ 3 - 8))) == 0
  @test @allocations(@FastGTPSA(normTPS(t2 ^ (1/2) - sqrt(2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(t2 ^ (1/2) - sqrt(t2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(2 ^ t3 - 8))) == 0
  @test @allocations(@FastGTPSA(normTPS(inv(t3) - 1/t3))) == 0
  @test @allocations(@FastGTPSA(normTPS(inv(t3) - 1/3))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct1 + ct2 - ct3))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct2 + ct1 - ct3))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct1 + (2+2im) - ct3))) == 0
  @test @allocations(@FastGTPSA(normTPS((2+2im) + ct1 - ct3))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct3 - ct2 - ct1))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct2 - ct3 - -ct1))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct3 - (2+2im) - ct1))) == 0
  @test @allocations(@FastGTPSA(normTPS((2+2im) - ct3 - -ct1))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct2 * ct3 - (2+2im)*(3+3im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct3 * ct2 - (2+2im)*(3+3im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct2 * 5 - (10+10im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(5 * ct2 - (10 * ct1)))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct1 / ct2 - (1+im)/(2+2im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct2 / ct1 - 2))) == 0
  @test @allocations(@FastGTPSA(normTPS(1 / ct2 - 1/(2+2im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct2 / 3 - (2+2im)/3))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct2 / ct2 - 1))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct2 ^ ct3 - (2+2im)^(3+3im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct3 ^ ct2 - (3+3im)^(2+2im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct2 ^ 3 - (2+2im)^3))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct2 ^ (1/2) - sqrt(2+2im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct2 ^ (1/2) - sqrt(ct2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(2 ^ ct3 - 2^(3+3im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(inv(ct3) - 1/ct3))) == 0
  @test @allocations(@FastGTPSA(normTPS(inv(ct3) - 1/(3+3im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(t1 + ct2 - (1 + (2+2im))))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct2 + t1 - (1 + (2+2im))))) == 0
  @test @allocations(@FastGTPSA(normTPS(t1 + (2+2im) - (1 + (2+2im))))) == 0
  @test @allocations(@FastGTPSA(normTPS((2+2im) + t1 - (1 + (2+2im))))) == 0
  @test @allocations(@FastGTPSA(normTPS(t3 - ct2 - (3 - (2+2im))))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct2 - t3 - ((2+2im) - 3)))) == 0
  @test @allocations(@FastGTPSA(normTPS(t3 - (2+2im) - (3 - (2+2im))))) == 0
  @test @allocations(@FastGTPSA(normTPS((2+2im) - t3 - ((2+2im) - 3)))) == 0
  @test @allocations(@FastGTPSA(normTPS(t2 * ct3 - 2 * (3+3im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct3 * t2 - 2 * (3+3im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(t2 * (3+3im) - 2 * (3+3im)))) == 0
  @test @allocations(@FastGTPSA(normTPS((3+3im) * t2 - 2 * (3+3im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(t2 / ct3 - 2/(3+3im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct3 / t2 - (3+3im)/2))) == 0
  @test @allocations(@FastGTPSA(normTPS(t2 / (3+3im) - 2/(3+3im)))) == 0
  @test @allocations(@FastGTPSA(normTPS((3+3im) / t2 - (3+3im)/2))) == 0
  @test @allocations(@FastGTPSA(normTPS(t2 ^ ct3 - 2^(3+3im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(ct3 ^ t2 - (3+3im)^2))) == 0
  @test @allocations(@FastGTPSA(normTPS(t2 ^ (3+3im) - 2^(3+3im)))) == 0
  @test @allocations(@FastGTPSA(normTPS((3+3im)^t2 - (3+3im)^2))) == 0

  @test @allocations(@FastGTPSA!(out = t1 + t2 - t3)) == 0
  @test @allocations(@FastGTPSA!(out = t2 + t1 - t3)) == 0
  @test @allocations(@FastGTPSA!(out = t1 + 2 - t3)) == 0
  @test @allocations(@FastGTPSA!(out = 2 + t1 - t3)) == 0
  @test @allocations(@FastGTPSA!(out = t3 - t2 - t1)) == 0
  @test @allocations(@FastGTPSA!(out = t2 - t3 - -t1)) == 0
  @test @allocations(@FastGTPSA!(out = t3 - 2 - t1)) == 0
  @test @allocations(@FastGTPSA!(out = 2 - t3 - -t1)) == 0
  @test @allocations(@FastGTPSA!(out = t2 * t3 - 6)) == 0
  @test @allocations(@FastGTPSA!(out = t3 * t2 - 6)) == 0
  @test @allocations(@FastGTPSA!(out = t2 * 5 - 10)) == 0
  @test @allocations(@FastGTPSA!(out = 5 * t2 - 10 * t1)) == 0
  @test @allocations(@FastGTPSA!(out = t1 / t2 - 1/2)) == 0
  @test @allocations(@FastGTPSA!(out = t2 / t1 - 2)) == 0
  @test @allocations(@FastGTPSA!(out = 1 / t2 - 1/2)) == 0
  @test @allocations(@FastGTPSA!(out = t2 / 3 - 2/3)) == 0
  @test @allocations(@FastGTPSA!(out = t2 / t2 - t1)) == 0
  @test @allocations(@FastGTPSA!(out = t2 / t2 - 1)) == 0
  @test @allocations(@FastGTPSA!(out = t2 ^ t3 - 8)) == 0
  @test @allocations(@FastGTPSA!(out = t3 ^ t2 - 9)) == 0
  @test @allocations(@FastGTPSA!(out = t2 ^ 3 - 8)) == 0
  @test @allocations(@FastGTPSA!(out = t2 ^ (1/2) - sqrt(2))) == 0
  @test @allocations(@FastGTPSA!(out = t2 ^ (1/2) - sqrt(t2))) == 0
  @test @allocations(@FastGTPSA!(out = 2 ^ t3 - 8)) == 0
  @test @allocations(@FastGTPSA!(out = inv(t3) - 1/t3)) == 0
  @test @allocations(@FastGTPSA!(out = inv(t3) - 1/3)) == 0
  @test @allocations(@FastGTPSA!(out = ct1 + ct2 - ct3)) == 0
  @test @allocations(@FastGTPSA!(out = ct2 + ct1 - ct3)) == 0
  @test @allocations(@FastGTPSA!(out = ct1 + (2+2im) - ct3)) == 0
  @test @allocations(@FastGTPSA!(out = (2+2im) + ct1 - ct3)) == 0
  @test @allocations(@FastGTPSA!(out = ct3 - ct2 - ct1)) == 0
  @test @allocations(@FastGTPSA!(out = ct2 - ct3 - -ct1)) == 0
  @test @allocations(@FastGTPSA!(out = ct3 - (2+2im) - ct1)) == 0
  @test @allocations(@FastGTPSA!(out = (2+2im) - ct3 - -ct1)) == 0
  @test @allocations(@FastGTPSA!(out = ct2 * ct3 - (2+2im)*(3+3im))) == 0
  @test @allocations(@FastGTPSA!(out = ct3 * ct2 - (2+2im)*(3+3im))) == 0
  @test @allocations(@FastGTPSA!(out = ct2 * 5 - (10+10im))) == 0
  @test @allocations(@FastGTPSA!(out = 5 * ct2 - (10 * ct1))) == 0
  @test @allocations(@FastGTPSA!(out = ct1 / ct2 - (1+im)/(2+2im))) == 0
  @test @allocations(@FastGTPSA!(out = ct2 / ct1 - 2)) == 0
  @test @allocations(@FastGTPSA!(out = 1 / ct2 - 1/(2+2im))) == 0
  @test @allocations(@FastGTPSA!(out = ct2 / 3 - (2+2im)/3)) == 0
  @test @allocations(@FastGTPSA!(out = ct2 / ct2 - 1)) == 0
  @test @allocations(@FastGTPSA!(out = ct2 ^ ct3 - (2+2im)^(3+3im))) == 0
  @test @allocations(@FastGTPSA!(out = ct3 ^ ct2 - (3+3im)^(2+2im))) == 0
  @test @allocations(@FastGTPSA!(out = ct2 ^ 3 - (2+2im)^3)) == 0
  @test @allocations(@FastGTPSA!(out = ct2 ^ (1/2) - sqrt(2+2im))) == 0
  @test @allocations(@FastGTPSA!(out = ct2 ^ (1/2) - sqrt(ct2))) == 0
  @test @allocations(@FastGTPSA!(out = 2 ^ ct3 - 2^(3+3im))) == 0
  @test @allocations(@FastGTPSA!(out = inv(ct3) - 1/ct3)) == 0
  @test @allocations(@FastGTPSA!(out = inv(ct3) - 1/(3+3im))) == 0
  @test @allocations(@FastGTPSA!(out = t1 + ct2 - (1 + (2+2im)))) == 0
  @test @allocations(@FastGTPSA!(out = ct2 + t1 - (1 + (2+2im)))) == 0
  @test @allocations(@FastGTPSA!(out = t1 + (2+2im) - (1 + (2+2im)))) == 0
  @test @allocations(@FastGTPSA!(out = (2+2im) + t1 - (1 + (2+2im)))) == 0
  @test @allocations(@FastGTPSA!(out = t3 - ct2 - (3 - (2+2im)))) == 0
  @test @allocations(@FastGTPSA!(out = ct2 - t3 - ((2+2im) - 3))) == 0
  @test @allocations(@FastGTPSA!(out = t3 - (2+2im) - (3 - (2+2im)))) == 0
  @test @allocations(@FastGTPSA!(out = (2+2im) - t3 - ((2+2im) - 3))) == 0
  @test @allocations(@FastGTPSA!(out = t2 * ct3 - 2 * (3+3im))) == 0
  @test @allocations(@FastGTPSA!(out = ct3 * t2 - 2 * (3+3im))) == 0
  @test @allocations(@FastGTPSA!(out = t2 * (3+3im) - 2 * (3+3im))) == 0
  @test @allocations(@FastGTPSA!(out = (3+3im) * t2 - 2 * (3+3im))) == 0
  @test @allocations(@FastGTPSA!(out = t2 / ct3 - 2/(3+3im))) == 0
  @test @allocations(@FastGTPSA!(out = ct3 / t2 - (3+3im)/2)) == 0
  @test @allocations(@FastGTPSA!(out = t2 / (3+3im) - 2/(3+3im))) == 0
  @test @allocations(@FastGTPSA!(out = (3+3im) / t2 - (3+3im)/2)) == 0
  @test @allocations(@FastGTPSA!(out = t2 ^ ct3 - 2^(3+3im))) == 0
  @test @allocations(@FastGTPSA!(out = ct3 ^ t2 - (3+3im)^2)) == 0
  @test @allocations(@FastGTPSA!(out = t2 ^ (3+3im) - 2^(3+3im))) == 0
  @test @allocations(@FastGTPSA!(out = (3+3im)^t2 - (3+3im)^2)) == 0

  d = Descriptor(1, 5)
  t = TPS(use=d)
  v = 0.5
  t[0] = v
  tol = 1e-14
  t1 = TPS(t)
  t1[0] = 1
  t2 = zero(t1)
  t2[0] = 2
  t3 = zero(t1)
  t3[0] = 3


  @test @allocations(@FastGTPSA(normTPS(abs(-t) - abs(-v) ))) == 0
  @test @allocations(@FastGTPSA(normTPS(sqrt(t) - sqrt(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(exp(t) - exp(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(log(t) - log(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sin(t) - sin(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(cos(t) - cos(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(tan(t) - tan(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(csc(t) - csc(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sec(t) - sec(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(cot(t) - cot(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sinc(t) - sinc(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sinh(t) - sinh(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(cosh(t) - cosh(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(tanh(t) - tanh(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(csch(t) - csch(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sech(t) - sech(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(coth(t) - coth(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(asin(t) - asin(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(acos(t) - acos(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(atan(t) - atan(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(acsc(1/t) - acsc(1/v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(asec(1/t) - asec(1/v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(acot(1/t) - acot(1/v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(asinh(t) - asinh(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(acosh(1/t) - acosh(1/v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(atanh(t) - atanh(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(acsch(1/t) - acsch(1/v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(asech(t) - asech(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(acoth(1/t) - acoth(1/v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(asinc(t/pi) - asin(v)/(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(asinhc(t/pi) - asinh(v)/(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(zero(t) - zero(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(one(t) + one(t) - zero(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(real(t) - real(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(imag(t) - imag(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(conj(t) - conj(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sinhc(t/pi) - sinh(v)/v))) == 0
  @test @allocations(@FastGTPSA(normTPS(erf(t) - erf(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(erfc(t) - erfc(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(-im*erf(t*im) - erfi(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(atan(t3,t2) - atan(3,2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(atan(t3,2) - atan(3,2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(atan(3,t2) - atan(3,2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(atan(t3,-t2) - atan(3,-2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(atan(t3,-2) - atan(3,-2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(atan(3,-t2) - atan(3,-2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(atan(-t3,-t2) - atan(-3,-2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(atan(-t3,-2) - atan(-3,-2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(atan(-3,-t2) - atan(-3,-2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(atan(-t3,t2) - atan(-3,2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(atan(-t3,2) - atan(-3,2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(atan(-3,t2) - atan(-3,2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(angle(t2) - angle(2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(angle(-t2) - angle(-2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(complex(t3) - complex(3)))) == 0
  @test @allocations(@FastGTPSA(normTPS(complex(t2,t3) - complex(2,3)))) == 0
  @test @allocations(@FastGTPSA(normTPS(polar(t2) - (abs(2)+im*atan(0,2))))) == 0
  @test @allocations(@FastGTPSA(normTPS(polar(-t1) - (abs(-1)+im*atan(0,-1))))) == 0
  @test @allocations(@FastGTPSA(normTPS(rect(t2) - (2*cos(0) + im*2*sin(0))))) == 0
  @test @allocations(@FastGTPSA(normTPS(rect(-t1) - (-1*cos(0) + im*-1*sin(0))))) == 0

  
  @test @allocations(@FastGTPSA!(out = abs(-t) - abs(-v) )) == 0
  @test @allocations(@FastGTPSA!(out = sqrt(t) - sqrt(v))) == 0
  @test @allocations(@FastGTPSA!(out = exp(t) - exp(v))) == 0
  @test @allocations(@FastGTPSA!(out = log(t) - log(v))) == 0
  @test @allocations(@FastGTPSA!(out = sin(t) - sin(v))) == 0
  @test @allocations(@FastGTPSA!(out = cos(t) - cos(v))) == 0
  @test @allocations(@FastGTPSA!(out = tan(t) - tan(v))) == 0
  @test @allocations(@FastGTPSA!(out = csc(t) - csc(v))) == 0
  @test @allocations(@FastGTPSA!(out = sec(t) - sec(v))) == 0
  @test @allocations(@FastGTPSA!(out = cot(t) - cot(v))) == 0
  @test @allocations(@FastGTPSA!(out = sinc(t) - sinc(v))) == 0
  @test @allocations(@FastGTPSA!(out = sinh(t) - sinh(v))) == 0
  @test @allocations(@FastGTPSA!(out = cosh(t) - cosh(v))) == 0
  @test @allocations(@FastGTPSA!(out = tanh(t) - tanh(v))) == 0
  @test @allocations(@FastGTPSA!(out = csch(t) - csch(v))) == 0
  @test @allocations(@FastGTPSA!(out = sech(t) - sech(v))) == 0
  @test @allocations(@FastGTPSA!(out = coth(t) - coth(v))) == 0
  @test @allocations(@FastGTPSA!(out = asin(t) - asin(v))) == 0
  @test @allocations(@FastGTPSA!(out = acos(t) - acos(v))) == 0
  @test @allocations(@FastGTPSA!(out = atan(t) - atan(v))) == 0
  @test @allocations(@FastGTPSA!(out = acsc(1/t) - acsc(1/v))) == 0
  @test @allocations(@FastGTPSA!(out = asec(1/t) - asec(1/v))) == 0
  @test @allocations(@FastGTPSA!(out = acot(1/t) - acot(1/v))) == 0
  @test @allocations(@FastGTPSA!(out = asinh(t) - asinh(v))) == 0
  @test @allocations(@FastGTPSA!(out = acosh(1/t) - acosh(1/v))) == 0
  @test @allocations(@FastGTPSA!(out = atanh(t) - atanh(v))) == 0
  @test @allocations(@FastGTPSA!(out = acsch(1/t) - acsch(1/v))) == 0
  @test @allocations(@FastGTPSA!(out = asech(t) - asech(v))) == 0
  @test @allocations(@FastGTPSA!(out = acoth(1/t) - acoth(1/v))) == 0
  @test @allocations(@FastGTPSA!(out = asinc(t/pi) - asin(v)/(v))) == 0
  @test @allocations(@FastGTPSA!(out = asinhc(t/pi) - asinh(v)/(v))) == 0
  @test @allocations(@FastGTPSA!(out = zero(t) - zero(v))) == 0
  @test @allocations(@FastGTPSA!(out = one(t) + one(t) - zero(t))) == 0
  @test @allocations(@FastGTPSA!(out = real(t) - real(v))) == 0
  @test @allocations(@FastGTPSA!(out = imag(t) - imag(v))) == 0
  @test @allocations(@FastGTPSA!(out = conj(t) - conj(v))) == 0
  @test @allocations(@FastGTPSA!(out = sinhc(t/pi) - sinh(v)/v)) == 0
  @test @allocations(@FastGTPSA!(out = erf(t) - erf(v))) == 0
  @test @allocations(@FastGTPSA!(out = erfc(t) - erfc(v))) == 0
  @test @allocations(@FastGTPSA!(out = -im*erf(t*im) - erfi(v))) == 0
  @test @allocations(@FastGTPSA!(out = atan(t3,t2) - atan(3,2))) == 0
  @test @allocations(@FastGTPSA!(out = atan(t3,2) - atan(3,2))) == 0
  @test @allocations(@FastGTPSA!(out = atan(3,t2) - atan(3,2))) == 0
  @test @allocations(@FastGTPSA!(out = atan(t3,-t2) - atan(3,-2))) == 0
  @test @allocations(@FastGTPSA!(out = atan(t3,-2) - atan(3,-2))) == 0
  @test @allocations(@FastGTPSA!(out = atan(3,-t2) - atan(3,-2))) == 0
  @test @allocations(@FastGTPSA!(out = atan(-t3,-t2) - atan(-3,-2))) == 0
  @test @allocations(@FastGTPSA!(out = atan(-t3,-2) - atan(-3,-2))) == 0
  @test @allocations(@FastGTPSA!(out = atan(-3,-t2) - atan(-3,-2))) == 0
  @test @allocations(@FastGTPSA!(out = atan(-t3,t2) - atan(-3,2))) == 0
  @test @allocations(@FastGTPSA!(out = atan(-t3,2) - atan(-3,2))) == 0
  @test @allocations(@FastGTPSA!(out = atan(-3,t2) - atan(-3,2))) == 0
  @test @allocations(@FastGTPSA!(out = angle(t2) - angle(2))) == 0
  @test @allocations(@FastGTPSA!(out = angle(-t2) - angle(-2))) == 0
  @test @allocations(@FastGTPSA!(out = complex(t3) - complex(3))) == 0
  @test @allocations(@FastGTPSA!(out = complex(t2,t3) - complex(2,3))) == 0
  @test @allocations(@FastGTPSA!(out = polar(t2) - (abs(2)+im*atan(0,2)))) == 0
  @test @allocations(@FastGTPSA!(out = polar(-t1) - (abs(-1)+im*atan(0,-1)))) == 0
  @test @allocations(@FastGTPSA!(out = rect(t2) - (2*cos(0) + im*2*sin(0)))) == 0
  @test @allocations(@FastGTPSA!(out = rect(-t1) - (-1*cos(0) + im*-1*sin(0)))) == 0
  

  v = 0.5+0.5im
  t = ComplexTPS64(t)
  t[0] = v
  ct1 = ComplexTPS64(t)
  ct1[0] = 1 + 1im
  ct2 = zero(ct1)
  ct2[0] = 2 + 2im
  ct3 = zero(ct1)
  ct3[0] = 3 + 3im
  @test @allocations(@FastGTPSA(normTPS(abs(-t) - abs(-v) ))) == 0
  @test @allocations(@FastGTPSA(normTPS(sqrt(t) - sqrt(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(exp(t) - exp(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(log(t) - log(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sin(t) - sin(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(cos(t) - cos(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(tan(t) - tan(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(csc(t) - csc(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sec(t) - sec(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(cot(t) - cot(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sinc(t) - sinc(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sinh(t) - sinh(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(cosh(t) - cosh(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(tanh(t) - tanh(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(csch(t) - csch(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sech(t) - sech(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(coth(t) - coth(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(asin(t) - asin(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(acos(t) - acos(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(atan(t) - atan(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(acsc(t) - acsc(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(asec(t) - asec(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(acot(t) - acot(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(asinh(t) - asinh(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(acosh(t) - acosh(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(atanh(t) - atanh(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(acsch(t) - acsch(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(asech(t) - asech(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(acoth(t) - acoth(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(asinc(t/pi) - asin(v)/v))) == 0
  @test @allocations(@FastGTPSA(normTPS(asinhc(t/pi) - asinh(v)/v))) == 0
  @test @allocations(@FastGTPSA(normTPS(zero(t) - zero(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(real(t) - real(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(imag(t) - imag(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(conj(t) - conj(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sinhc(t/pi) - sinh(v)/v))) == 0
  @test @allocations(@FastGTPSA(normTPS(erf(t) - erf(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(erfc(t) - erfc(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(-im*erf(t*im) - erfi(v)))) == 0
  @test @allocations(@FastGTPSA(normTPS(angle(t2+im*t3) - angle(2+3im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(angle(t2-im*t3) - angle(2-3im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(angle(-t2-im*t3) - angle(-2-3im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(angle(-t2+im*t3) - angle(-2+3im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(angle(ct2) - angle(2+2im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(angle(-ct2) - angle(-2-2im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(complex(ct3) - complex(3+3im)))) == 0
  @test @allocations(@FastGTPSA(normTPS(polar(ct2) - (abs(2+2im)+im*angle(2+2im))))) == 0
  @test @allocations(@FastGTPSA(normTPS(polar(-ct1) - (abs(-1-im)+im*angle(-1-im))))) == 0
  @test @allocations(@FastGTPSA(normTPS(rect(ct2) - (2*cos(2) + im*2*sin(2))))) == 0
  @test @allocations(@FastGTPSA(normTPS(rect(-ct1) - (-1*cos(-1) + im*-1*sin(-1))))) == 0

  @test @allocations(@FastGTPSA!(out = abs(-t) - abs(-v) )) == 0
  @test @allocations(@FastGTPSA!(out = sqrt(t) - sqrt(v))) == 0
  @test @allocations(@FastGTPSA!(out = exp(t) - exp(v))) == 0
  @test @allocations(@FastGTPSA!(out = log(t) - log(v))) == 0
  @test @allocations(@FastGTPSA!(out = sin(t) - sin(v))) == 0
  @test @allocations(@FastGTPSA!(out = cos(t) - cos(v))) == 0
  @test @allocations(@FastGTPSA!(out = tan(t) - tan(v))) == 0
  @test @allocations(@FastGTPSA!(out = csc(t) - csc(v))) == 0
  @test @allocations(@FastGTPSA!(out = sec(t) - sec(v))) == 0
  @test @allocations(@FastGTPSA!(out = cot(t) - cot(v))) == 0
  @test @allocations(@FastGTPSA!(out = sinc(t) - sinc(v))) == 0
  @test @allocations(@FastGTPSA!(out = sinh(t) - sinh(v))) == 0
  @test @allocations(@FastGTPSA!(out = cosh(t) - cosh(v))) == 0
  @test @allocations(@FastGTPSA!(out = tanh(t) - tanh(v))) == 0
  @test @allocations(@FastGTPSA!(out = csch(t) - csch(v))) == 0
  @test @allocations(@FastGTPSA!(out = sech(t) - sech(v))) == 0
  @test @allocations(@FastGTPSA!(out = coth(t) - coth(v))) == 0
  @test @allocations(@FastGTPSA!(out = asin(t) - asin(v))) == 0
  @test @allocations(@FastGTPSA!(out = acos(t) - acos(v))) == 0
  @test @allocations(@FastGTPSA!(out = atan(t) - atan(v))) == 0
  @test @allocations(@FastGTPSA!(out = acsc(t) - acsc(v))) == 0
  @test @allocations(@FastGTPSA!(out = asec(t) - asec(v))) == 0
  @test @allocations(@FastGTPSA!(out = acot(t) - acot(v))) == 0
  @test @allocations(@FastGTPSA!(out = asinh(t) - asinh(v))) == 0
  @test @allocations(@FastGTPSA!(out = acosh(t) - acosh(v))) == 0
  @test @allocations(@FastGTPSA!(out = atanh(t) - atanh(v))) == 0
  @test @allocations(@FastGTPSA!(out = acsch(t) - acsch(v))) == 0
  @test @allocations(@FastGTPSA!(out = asech(t) - asech(v))) == 0
  @test @allocations(@FastGTPSA!(out = acoth(t) - acoth(v))) == 0
  @test @allocations(@FastGTPSA!(out = asinc(t/pi) - asin(v)/v)) == 0
  @test @allocations(@FastGTPSA!(out = asinhc(t/pi) - asinh(v)/v)) == 0
  @test @allocations(@FastGTPSA!(out = zero(t) - zero(v))) == 0
  @test @allocations(@FastGTPSA!(out = real(t) - real(v))) == 0
  @test @allocations(@FastGTPSA!(out = imag(t) - imag(v))) == 0
  @test @allocations(@FastGTPSA!(out = conj(t) - conj(v))) == 0
  @test @allocations(@FastGTPSA!(out = sinhc(t/pi) - sinh(v)/v)) == 0
  @test @allocations(@FastGTPSA!(out = erf(t) - erf(v))) == 0
  @test @allocations(@FastGTPSA!(out = erfc(t) - erfc(v))) == 0
  @test @allocations(@FastGTPSA!(out = -im*erf(t*im) - erfi(v))) == 0
  @test @allocations(@FastGTPSA!(out = angle(t2+im*t3) - angle(2+3im))) == 0
  @test @allocations(@FastGTPSA!(out = angle(t2-im*t3) - angle(2-3im))) == 0
  @test @allocations(@FastGTPSA!(out = angle(-t2-im*t3) - angle(-2-3im))) == 0
  @test @allocations(@FastGTPSA!(out = angle(-t2+im*t3) - angle(-2+3im))) == 0
  @test @allocations(@FastGTPSA!(out = angle(ct2) - angle(2+2im))) == 0
  @test @allocations(@FastGTPSA!(out = angle(-ct2) - angle(-2-2im))) == 0
  @test @allocations(@FastGTPSA!(out = complex(ct3) - complex(3+3im))) == 0
  @test @allocations(@FastGTPSA!(out = polar(ct2) - (abs(2+2im)+im*angle(2+2im)))) == 0
  @test @allocations(@FastGTPSA!(out = polar(-ct1) - (abs(-1-im)+im*angle(-1-im)))) == 0
  @test @allocations(@FastGTPSA!(out = rect(ct2) - (2*cos(2) + im*2*sin(2)))) == 0
  @test @allocations(@FastGTPSA!(out = rect(-ct1) - (-1*cos(-1) + im*-1*sin(-1)))) == 0

  d = Descriptor(1, 5)
  t = TPS(use=d)
  t[0] = 0.5; t[[1]] = 2; t[[2]] = 3; t[[3]] = 4; t[[4]] = 5; t[[5]] = 6

  tol = 1e-10

  @test @allocations(@FastGTPSA(normTPS(sin(t)^2+cos(t)^2 - 1))) == 0
  @test @allocations(@FastGTPSA(normTPS(1/sin(t) - csc(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(1/cos(t) - sec(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(1/tan(t) - cot(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sin(t)/cos(t) - tan(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(cos(2*t) - cos(t)^2 + sin(t)^2))) == 0
  @test @allocations(@FastGTPSA(normTPS(sec(t)^2 - 1 - tan(t)^2))) == 0
  @test @allocations(@FastGTPSA(normTPS(sin(t/2) - sqrt((1-cos(t))/2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(cos(t/2) - sqrt((1+cos(t))/2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sqrt(t^2) - abs(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(csc(t)^2 - cot(t)^2 - 1))) == 0
  @test @allocations(@FastGTPSA(normTPS(exp(log(t)) - t))) == 0
  @test @allocations(@FastGTPSA(normTPS(log(exp(t)) - t))) == 0
  @test @allocations(@FastGTPSA(normTPS(log(exp(t)) - exp(log(t))))) == 0
  @test @allocations(@FastGTPSA(normTPS(log(t^2) - 2*log(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(5*log(t) - log(t^5)))) == 0
  @test @allocations(@FastGTPSA(normTPS(t*log(5) - log(5^t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sinc(t) - sin(pi*t)/(pi*t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sinhc(t/pi) - sinh(t)/t))) == 0
  @test @allocations(@FastGTPSA(normTPS(exp(im*t) - cos(t) - im*sin(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(real(exp(im*t)) - cos(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(imag(exp(im*t)) - sin(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sinh(t) - (exp(t) - exp(-t))/2))) == 0
  @test @allocations(@FastGTPSA(normTPS(cosh(t) - (exp(t) + exp(-t))/2))) == 0
  @test @allocations(@FastGTPSA(normTPS(tanh(t) - sinh(t)/cosh(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(csch(t) - 1/sinh(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sech(t) - 1/cosh(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(coth(t) - cosh(t)/sinh(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(coth(t) - 1/tanh(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(cosh(t)^2 - sinh(t)^2 - 1))) == 0
  @test @allocations(@FastGTPSA(normTPS(1 - tanh(t)^2 - sech(t)^2))) == 0
  @test @allocations(@FastGTPSA(normTPS(coth(t)^2 - 1 - csch(t)^2))) == 0
  @test @allocations(@FastGTPSA(normTPS(asin(sin(t)) - t))) == 0
  @test @allocations(@FastGTPSA(normTPS(acos(cos(t)) - t))) == 0
  @test @allocations(@FastGTPSA(normTPS(atan(tan(t)) - t))) == 0
  @test @allocations(@FastGTPSA(normTPS(acsc(1/t) - asin(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(asec(1/t) - acos(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(acot(1/t) - atan(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(asinh(sinh(t)) - t))) == 0
  @test @allocations(@FastGTPSA(normTPS(acosh(cosh(t)) - t))) == 0
  @test @allocations(@FastGTPSA(normTPS(atanh(tanh(t)) - t))) == 0
  @test @allocations(@FastGTPSA(normTPS(acsch(t) - asinh(1/t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(asech(t) - acosh(1/t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(acoth(1/t) - atanh(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(asinc(t/pi) - asin(t)/t))) == 0
  @test @allocations(@FastGTPSA(normTPS(asinhc(t/pi) - asinh(t)/t))) == 0
  @test @allocations(@FastGTPSA(normTPS(erfc(t) - 1 + erf(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(erf(-t) + erf(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(angle(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(complex(t) - t))) == 0
  @test @allocations(@FastGTPSA(normTPS(complex(t,t) - (t+im*t)))) == 0

  @test @allocations(@FastGTPSA!(out = sin(t)^2+cos(t)^2 - 1)) == 0
  @test @allocations(@FastGTPSA!(out = 1/sin(t) - csc(t))) == 0
  @test @allocations(@FastGTPSA!(out = 1/cos(t) - sec(t))) == 0
  @test @allocations(@FastGTPSA!(out = 1/tan(t) - cot(t))) == 0
  @test @allocations(@FastGTPSA!(out = sin(t)/cos(t) - tan(t))) == 0
  @test @allocations(@FastGTPSA!(out = cos(2*t) - cos(t)^2 + sin(t)^2)) == 0
  @test @allocations(@FastGTPSA!(out = sec(t)^2 - 1 - tan(t)^2)) == 0
  @test @allocations(@FastGTPSA!(out = sin(t/2) - sqrt((1-cos(t))/2))) == 0
  @test @allocations(@FastGTPSA!(out = cos(t/2) - sqrt((1+cos(t))/2))) == 0
  @test @allocations(@FastGTPSA!(out = sqrt(t^2) - abs(t))) == 0
  @test @allocations(@FastGTPSA!(out = csc(t)^2 - cot(t)^2 - 1)) == 0
  @test @allocations(@FastGTPSA!(out = exp(log(t)) - t)) == 0
  @test @allocations(@FastGTPSA!(out = log(exp(t)) - t)) == 0
  @test @allocations(@FastGTPSA!(out = log(exp(t)) - exp(log(t)))) == 0
  @test @allocations(@FastGTPSA!(out = log(t^2) - 2*log(t))) == 0
  @test @allocations(@FastGTPSA!(out = 5*log(t) - log(t^5))) == 0
  @test @allocations(@FastGTPSA!(out = t*log(5) - log(5^t))) == 0
  @test @allocations(@FastGTPSA!(out = sinc(t) - sin(pi*t)/(pi*t))) == 0
  @test @allocations(@FastGTPSA!(out = sinhc(t/pi) - sinh(t)/t)) == 0
  @test @allocations(@FastGTPSA!(out = exp(im*t) - cos(t) - im*sin(t))) == 0
  @test @allocations(@FastGTPSA!(out = real(exp(im*t)) - cos(t))) == 0
  @test @allocations(@FastGTPSA!(out = imag(exp(im*t)) - sin(t))) == 0
  @test @allocations(@FastGTPSA!(out = sinh(t) - (exp(t) - exp(-t))/2)) == 0
  @test @allocations(@FastGTPSA!(out = cosh(t) - (exp(t) + exp(-t))/2)) == 0
  @test @allocations(@FastGTPSA!(out = tanh(t) - sinh(t)/cosh(t))) == 0
  @test @allocations(@FastGTPSA!(out = csch(t) - 1/sinh(t))) == 0
  @test @allocations(@FastGTPSA!(out = sech(t) - 1/cosh(t))) == 0
  @test @allocations(@FastGTPSA!(out = coth(t) - cosh(t)/sinh(t))) == 0
  @test @allocations(@FastGTPSA!(out = coth(t) - 1/tanh(t))) == 0
  @test @allocations(@FastGTPSA!(out = cosh(t)^2 - sinh(t)^2 - 1)) == 0
  @test @allocations(@FastGTPSA!(out = 1 - tanh(t)^2 - sech(t)^2)) == 0
  @test @allocations(@FastGTPSA!(out = coth(t)^2 - 1 - csch(t)^2)) == 0
  @test @allocations(@FastGTPSA!(out = asin(sin(t)) - t)) == 0
  @test @allocations(@FastGTPSA!(out = acos(cos(t)) - t)) == 0
  @test @allocations(@FastGTPSA!(out = atan(tan(t)) - t)) == 0
  @test @allocations(@FastGTPSA!(out = acsc(1/t) - asin(t))) == 0
  @test @allocations(@FastGTPSA!(out = asec(1/t) - acos(t))) == 0
  @test @allocations(@FastGTPSA!(out = acot(1/t) - atan(t))) == 0
  @test @allocations(@FastGTPSA!(out = asinh(sinh(t)) - t)) == 0
  @test @allocations(@FastGTPSA!(out = acosh(cosh(t)) - t)) == 0
  @test @allocations(@FastGTPSA!(out = atanh(tanh(t)) - t)) == 0
  @test @allocations(@FastGTPSA!(out = acsch(t) - asinh(1/t))) == 0
  @test @allocations(@FastGTPSA!(out = asech(t) - acosh(1/t))) == 0
  @test @allocations(@FastGTPSA!(out = acoth(1/t) - atanh(t))) == 0
  @test @allocations(@FastGTPSA!(out = asinc(t/pi) - asin(t)/t)) == 0
  @test @allocations(@FastGTPSA!(out = asinhc(t/pi) - asinh(t)/t)) == 0
  @test @allocations(@FastGTPSA!(out = erfc(t) - 1 + erf(t))) == 0
  @test @allocations(@FastGTPSA!(out = erf(-t) + erf(t))) == 0
  @test @allocations(@FastGTPSA!(out = angle(t))) == 0
  @test @allocations(@FastGTPSA!(out = complex(t) - t)) == 0
  @test @allocations(@FastGTPSA!(out = complex(t,t) - (t+im*t))) == 0

  t = ComplexTPS64(t)
  t[0] = 0.5+0.5im; t[[1]] = 2+2im; t[[2]] = 3+3im; t[[3]] = 4+4im; t[[4]] = 5+5im; t[[5]] = 6+6im
  @test @allocations(@FastGTPSA(normTPS(sin(t)^2+cos(t)^2 - 1))) == 0
  @test @allocations(@FastGTPSA(normTPS(1/sin(t) - csc(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(1/cos(t) - sec(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(1/tan(t) - cot(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sin(t)/cos(t) - tan(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(cos(2*t) - cos(t)^2 + sin(t)^2))) == 0
  @test @allocations(@FastGTPSA(normTPS(sec(t)^2 - 1 - tan(t)^2))) == 0
  @test @allocations(@FastGTPSA(normTPS(sin(t/2) - sqrt((1-cos(t))/2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(cos(t/2) - sqrt((1+cos(t))/2)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sqrt(t^2) - t))) == 0
  @test @allocations(@FastGTPSA(normTPS(csc(t)^2 - cot(t)^2 - 1))) == 0
  @test @allocations(@FastGTPSA(normTPS(exp(log(t)) - t))) == 0
  @test @allocations(@FastGTPSA(normTPS(log(exp(t)) - t))) == 0
  @test @allocations(@FastGTPSA(normTPS(log(exp(t)) - exp(log(t))))) == 0
  @test @allocations(@FastGTPSA(normTPS(log(t^2) - 2*log(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(5*log(t) - log(t^5) - 2*pi*im))) == 0
  @test @allocations(@FastGTPSA(normTPS(t*log(5) - log(5^t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sinc(t/pi) - sin(t)/t))) == 0
  @test @allocations(@FastGTPSA(normTPS(sinhc(t/pi) - sinh(t)/t))) == 0
  @test @allocations(@FastGTPSA(normTPS(exp(im*t) - cos(t) - im*sin(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sinh(t) - (exp(t) - exp(-t))/2))) == 0
  @test @allocations(@FastGTPSA(normTPS(cosh(t) - (exp(t) + exp(-t))/2))) == 0
  @test @allocations(@FastGTPSA(normTPS(tanh(t) - sinh(t)/cosh(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(csch(t) - 1/sinh(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(sech(t) - 1/cosh(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(coth(t) - cosh(t)/sinh(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(coth(t) - 1/tanh(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(cosh(t)^2 - sinh(t)^2 - 1))) == 0
  @test @allocations(@FastGTPSA(normTPS(1 - tanh(t)^2 - sech(t)^2))) == 0
  @test @allocations(@FastGTPSA(normTPS(coth(t)^2 - 1 - csch(t)^2))) == 0
  @test @allocations(@FastGTPSA(normTPS(asin(sin(t)) - t))) == 0
  @test @allocations(@FastGTPSA(normTPS(acos(cos(t)) - t))) == 0
  @test @allocations(@FastGTPSA(normTPS(atan(tan(t)) - t))) == 0
  @test @allocations(@FastGTPSA(normTPS(acsc(t) - asin(1/t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(asec(t) - acos(1/t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(acot(t) - atan(1/t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(asinh(sinh(t)) - t))) == 0
  @test @allocations(@FastGTPSA(normTPS(acosh(cosh(t)) - t))) == 0
  @test @allocations(@FastGTPSA(normTPS(atanh(tanh(t)) - t))) == 0
  @test @allocations(@FastGTPSA(normTPS(acsch(t) - asinh(1/t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(asech(t) - acosh(1/t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(acoth(t) - atanh(1/t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(asinc(t/pi) - asin(t)/t))) == 0
  @test @allocations(@FastGTPSA(normTPS(asinhc(t/pi) - asinh(t)/t))) == 0
  @test @allocations(@FastGTPSA(normTPS(erfc(t) - 1 + erf(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(erf(-t) + erf(t)))) == 0
  @test @allocations(@FastGTPSA(normTPS(angle(t) - atan(imag(t),real(t))))) == 0
  @test @allocations(@FastGTPSA(normTPS(complex(t) - t))) == 0

  @test @allocations(@FastGTPSA!(out = sin(t)^2+cos(t)^2 - 1)) == 0
  @test @allocations(@FastGTPSA!(out = 1/sin(t) - csc(t))) == 0
  @test @allocations(@FastGTPSA!(out = 1/cos(t) - sec(t))) == 0
  @test @allocations(@FastGTPSA!(out = 1/tan(t) - cot(t))) == 0
  @test @allocations(@FastGTPSA!(out = sin(t)/cos(t) - tan(t))) == 0
  @test @allocations(@FastGTPSA!(out = cos(2*t) - cos(t)^2 + sin(t)^2)) == 0
  @test @allocations(@FastGTPSA!(out = sec(t)^2 - 1 - tan(t)^2)) == 0
  @test @allocations(@FastGTPSA!(out = sin(t/2) - sqrt((1-cos(t))/2))) == 0
  @test @allocations(@FastGTPSA!(out = cos(t/2) - sqrt((1+cos(t))/2))) == 0
  @test @allocations(@FastGTPSA!(out = sqrt(t^2) - t)) == 0
  @test @allocations(@FastGTPSA!(out = csc(t)^2 - cot(t)^2 - 1)) == 0
  @test @allocations(@FastGTPSA!(out = exp(log(t)) - t)) == 0
  @test @allocations(@FastGTPSA!(out = log(exp(t)) - t)) == 0
  @test @allocations(@FastGTPSA!(out = log(exp(t)) - exp(log(t)))) == 0
  @test @allocations(@FastGTPSA!(out = log(t^2) - 2*log(t))) == 0
  @test @allocations(@FastGTPSA!(out = 5*log(t) - log(t^5) - 2*pi*im)) == 0
  @test @allocations(@FastGTPSA!(out = t*log(5) - log(5^t))) == 0
  @test @allocations(@FastGTPSA!(out = sinc(t/pi) - sin(t)/t)) == 0
  @test @allocations(@FastGTPSA!(out = sinhc(t/pi) - sinh(t)/t)) == 0
  @test @allocations(@FastGTPSA!(out = exp(im*t) - cos(t) - im*sin(t))) == 0
  @test @allocations(@FastGTPSA!(out = sinh(t) - (exp(t) - exp(-t))/2)) == 0
  @test @allocations(@FastGTPSA!(out = cosh(t) - (exp(t) + exp(-t))/2)) == 0
  @test @allocations(@FastGTPSA!(out = tanh(t) - sinh(t)/cosh(t))) == 0
  @test @allocations(@FastGTPSA!(out = csch(t) - 1/sinh(t))) == 0
  @test @allocations(@FastGTPSA!(out = sech(t) - 1/cosh(t))) == 0
  @test @allocations(@FastGTPSA!(out = coth(t) - cosh(t)/sinh(t))) == 0
  @test @allocations(@FastGTPSA!(out = coth(t) - 1/tanh(t))) == 0
  @test @allocations(@FastGTPSA!(out = cosh(t)^2 - sinh(t)^2 - 1)) == 0
  @test @allocations(@FastGTPSA!(out = 1 - tanh(t)^2 - sech(t)^2)) == 0
  @test @allocations(@FastGTPSA!(out = coth(t)^2 - 1 - csch(t)^2)) == 0
  @test @allocations(@FastGTPSA!(out = asin(sin(t)) - t)) == 0
  @test @allocations(@FastGTPSA!(out = acos(cos(t)) - t)) == 0
  @test @allocations(@FastGTPSA!(out = atan(tan(t)) - t)) == 0
  @test @allocations(@FastGTPSA!(out = acsc(t) - asin(1/t))) == 0
  @test @allocations(@FastGTPSA!(out = asec(t) - acos(1/t))) == 0
  @test @allocations(@FastGTPSA!(out = acot(t) - atan(1/t))) == 0
  @test @allocations(@FastGTPSA!(out = asinh(sinh(t)) - t)) == 0
  @test @allocations(@FastGTPSA!(out = acosh(cosh(t)) - t)) == 0
  @test @allocations(@FastGTPSA!(out = atanh(tanh(t)) - t)) == 0
  @test @allocations(@FastGTPSA!(out = acsch(t) - asinh(1/t))) == 0
  @test @allocations(@FastGTPSA!(out = asech(t) - acosh(1/t))) == 0
  @test @allocations(@FastGTPSA!(out = acoth(t) - atanh(1/t))) == 0
  @test @allocations(@FastGTPSA!(out = asinc(t/pi) - asin(t)/t)) == 0
  @test @allocations(@FastGTPSA!(out = asinhc(t/pi) - asinh(t)/t)) == 0
  @test @allocations(@FastGTPSA!(out = erfc(t) - 1 + erf(t))) == 0
  @test @allocations(@FastGTPSA!(out = erf(-t) + erf(t))) == 0
  @test @allocations(@FastGTPSA!(out = angle(t) - atan(imag(t),real(t)))) == 0
  @test @allocations(@FastGTPSA!(out = complex(t) - t)) == 0

  @test GTPSA.checktemps()
end


@testset "Taylor map benchmark against ForwardDiff" begin
  include("../benchmark/track.jl")
  map = benchmark_GTPSA2()
  jFD, hFD = benchmark_ForwardDiff2()
  tol = 1e-10
  
  h1FD = reshape(hFD,6,58,58)[1,:,:]
  h2FD = reshape(hFD,6,58,58)[2,:,:]
  h3FD = reshape(hFD,6,58,58)[3,:,:]
  h4FD = reshape(hFD,6,58,58)[4,:,:]
  h5FD = reshape(hFD,6,58,58)[5,:,:]
  h6FD = reshape(hFD,6,58,58)[6,:,:]

  j = GTPSA.jacobian(map,include_params=true)
  h1 = GTPSA.hessian(map[1],include_params=true)
  h2 = GTPSA.hessian(map[2],include_params=true)
  h3 = GTPSA.hessian(map[3],include_params=true)
  h4 = GTPSA.hessian(map[4],include_params=true)
  h5 = GTPSA.hessian(map[5],include_params=true)
  h6 = GTPSA.hessian(map[6],include_params=true)


  @test all(abs.(j - jFD) .< tol)
  @test all(abs.(h1 - h1FD) .< tol)
  @test all(abs.(h2 - h2FD) .< tol)
  @test all(abs.(h3 - h3FD) .< tol)
  @test all(abs.(h4 - h4FD) .< tol)
  @test all(abs.(h5 - h5FD) .< tol)
  @test all(abs.(h6 - h6FD) .< tol)
end

@testset "Compare with MAD" begin
  include("compare_MAD.jl")
  expected_out = """mad_mono.h downloaded.
  Comparing mad_mono.h to mono.jl...
  mad_desc.h downloaded.
  Comparing mad_desc.h to desc.jl...
  mad_tpsa.h downloaded.
  Comparing mad_tpsa.h to rtpsa.jl...
  mad_ctpsa.h downloaded.
  Comparing mad_ctpsa.h to ctpsa.jl...
  """
  @test compare_MAD() == expected_out
end
