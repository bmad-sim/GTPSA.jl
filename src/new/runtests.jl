using Test, JET
using SpecialFunctions
using GTPSA
import GTPSA: Desc, RTPSA, CTPSA

@testset "Arithmetic operators" begin
  d = Descriptor(1, 5)
  t = NewTPS(use=d)
  ct = ComplexNewTPS(t)

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
  @test !(t === +t)
  @test isequal(-1, -t)
  @test -1 == -t
  @test !(t === -t)

  @test isequal(ct, +ct)
  @test ct == +ct
  @test !(ct === +ct)
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

  @test norm(tn) == sum([i for i in 1:6])
  @test norm(tcn) == sum([abs(i+i*im) for i in 1:6])

  # NewTPS:
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

  # Promotion of NewTPS to ComplexTPS
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
  d = Descriptor(1, 5)
  t = NewTPS(use=d)
  v = 0.5
  t[0] = v
  tol = 1e-14
  t1 = NewTPS(t)
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
  @test norm(erf(t) - erf(v)) < tol
  @test norm(erfc(t) - erfc(v)) < tol
  @test norm(-im*erf(t*im) - erfi(v)) < tol
  
  @test norm(atan(t3,t2) - atan(3,2)) < tol
  @test norm(atan(t3,2) - atan(3,2)) < tol
  @test norm(atan(3,t2) - atan(3,2)) < tol
  @test norm(atan(t3,-t2) - atan(3,-2)) < tol
  @test norm(atan(t3,-2) - atan(3,-2)) < tol
  @test norm(atan(3,-t2) - atan(3,-2)) < tol
  @test norm(atan(-t3,-t2) - atan(-3,-2)) < tol
  @test norm(atan(-t3,-2) - atan(-3,-2)) < tol
  @test norm(atan(-3,-t2) - atan(-3,-2)) < tol
  @test norm(atan(-t3,t2) - atan(-3,2)) < tol
  @test norm(atan(-t3,2) - atan(-3,2)) < tol
  @test norm(atan(-3,t2) - atan(-3,2)) < tol
  
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
  @test norm(angle(-t2) - angle(-2)) < tol
  @test norm(complex(t3) - complex(3)) < tol
  @test norm(complex(t2,t3) - complex(2,3)) < tol
  @test norm(polar(t2) - (abs(2)+im*atan(0,2))) < tol
  @test norm(polar(-t1) - (abs(-1)+im*atan(0,-1))) < tol
  @test norm(rect(t2) - (2*cos(0) + im*2*sin(0))) < tol
  @test norm(rect(-t1) - (-1*cos(0) + im*-1*sin(0))) < tol
  

  v = 0.5+0.5im
  t = ComplexNewTPS(t)
  t[0] = v
  ct1 = ComplexNewTPS(t)
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
  @test norm(asinc(t/pi) - asin(v)/v) < tol
  @test norm(asinhc(t/pi) - asinh(v)/v) < tol

  @test norm(zero(t) - zero(v)) < tol
  @test norm(real(t) - real(v)) < tol
  @test norm(imag(t) - imag(v)) < tol
  @test norm(conj(t) - conj(v)) < tol
  @test norm(sinhc(t/pi) - sinh(v)/v) < tol
  @test norm(erf(t) - erf(v)) < tol
  @test norm(erfc(t) - erfc(v)) < tol
  @test norm(-im*erf(t*im) - erfi(v)) < tol
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
  
  @test norm(angle(t2+im*t3) - angle(2+3im)) < tol
  @test norm(angle(t2-im*t3) - angle(2-3im)) < tol
  @test norm(angle(-t2-im*t3) - angle(-2-3im)) < tol
  @test norm(angle(-t2+im*t3) - angle(-2+3im)) < tol
  @test norm(angle(ct2) - angle(2+2im)) < tol
  @test norm(angle(-ct2) - angle(-2-2im)) < tol
  @test norm(complex(ct3) - complex(3+3im)) < tol
  @test norm(polar(ct2) - (abs(2+2im)+im*angle(2+2im))) < tol
  @test norm(polar(-ct1) - (abs(-1-im)+im*angle(-1-im))) < tol
  @test norm(rect(ct2) - (2*cos(2) + im*2*sin(2))) < tol
  @test norm(rect(-ct1) - (-1*cos(-1) + im*-1*sin(-1))) < tol
  
  # Hypot, mixing NewTPS with ComplexTPS
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
  t = NewTPS(use=d)
  t[0] = 0.5; t[[1]] = 2; t[[2]] = 3; t[[3]] = 4; t[[4]] = 5; t[[5]] = 6

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
  @test norm(erfc(t) - 1 + erf(t)) < tol
  @test norm(erf(-t) + erf(t)) < tol
  @test norm(angle(t)) < tol
  @test norm(complex(t) - t) < tol
  @test norm(complex(t,t) - (t+im*t)) < tol

  t = ComplexNewTPS(t)
  t[0] = 0.5+0.5im; t[[1]] = 2+2im; t[[2]] = 3+3im; t[[3]] = 4+4im; t[[4]] = 5+5im; t[[5]] = 6+6im
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
  
  @test norm(asin(sin(t)) - t) < tol
  @test norm(acos(cos(t)) - t) < tol
  @test norm(atan(tan(t)) - t) < tol
  @test norm(acsc(t) - asin(1/t)) < tol
  @test norm(asec(t) - acos(1/t)) < tol
  @test norm(acot(t) - atan(1/t)) < tol
  @test norm(asinh(sinh(t)) - t) < tol
  @test norm(acosh(cosh(t)) - t) < tol
  @test norm(atanh(tanh(t)) - t) < tol
  @test norm(acsch(t) - asinh(1/t)) < tol
  @test norm(asech(t) - acosh(1/t)) < tol
  @test norm(acoth(t) - atanh(1/t)) < tol
  @test norm(asinc(t/pi) - asin(t)/t) < tol
  @test norm(asinhc(t/pi) - asinh(t)/t) < tol
  
  @test norm(erfc(t) - 1 + erf(t)) < tol
  @test norm(erf(-t) + erf(t)) < tol
  @test norm(angle(t) - atan(imag(t),real(t))) < tol
  @test norm(complex(t) - t) < tol
end

@testset "Indexing" begin
  d = Descriptor(3,10,2,10)
  v = GTPSA.newvars(d)
  p = GTPSA.newparams(d)
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

@testset "@FastGTPSA - Arithmetic operators" begin
  d = Descriptor(1, 5)
  t = NewTPS(use=d)
  ct = ComplexNewTPS(t)
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

  # NewTPS:
  @test @FastGTPSA(norm(t1 + t2 - t3)) < tol
  @test @FastGTPSA(norm(t2 + t1 - t3)) < tol
  @test @FastGTPSA(norm(t1 + 2 - t3)) < tol
  @test @FastGTPSA(norm(2 + t1 - t3)) < tol
  @test @FastGTPSA(norm(t3 - t2 - t1)) < tol
  @test @FastGTPSA(norm(t2 - t3 - -t1)) < tol
  @test @FastGTPSA(norm(t3 - 2 - t1)) < tol
  @test @FastGTPSA(norm(2 - t3 - -t1)) < tol
  @test @FastGTPSA(norm(t2 * t3 - 6)) < tol
  @test @FastGTPSA(norm(t3 * t2 - 6)) < tol
  @test @FastGTPSA(norm(t2 * 5 - 10)) < tol
  @test @FastGTPSA(norm(5 * t2 - 10 * t1)) < tol
  @test @FastGTPSA(norm(t1 / t2 - 1/2)) < tol
  @test @FastGTPSA(norm(t2 / t1 - 2)) < tol
  @test @FastGTPSA(norm(1 / t2 - 1/2)) < tol
  @test @FastGTPSA(norm(t2 / 3 - 2/3)) < tol
  @test @FastGTPSA(norm(t2 / t2 - t1)) < tol
  @test @FastGTPSA(norm(t2 / t2 - 1)) < tol
  @test @FastGTPSA(norm(t2 ^ t3 - 8)) < tol
  @test @FastGTPSA(norm(t3 ^ t2 - 9)) < tol
  @test @FastGTPSA(norm(t2 ^ 3 - 8)) < tol
  @test @FastGTPSA(norm(t2 ^ (1/2) - sqrt(2))) < tol
  @test @FastGTPSA(norm(t2 ^ (1/2) - sqrt(t2))) < tol
  @test @FastGTPSA(norm(2 ^ t3 - 8)) < tol
  @test @FastGTPSA(norm(inv(t3) - 1/t3)) < tol
  @test @FastGTPSA(norm(inv(t3) - 1/3)) < tol

  # ComplexTPS:
  @test @FastGTPSA(norm(ct1 + ct2 - ct3)) < tol
  @test @FastGTPSA(norm(ct2 + ct1 - ct3)) < tol
  @test @FastGTPSA(norm(ct1 + (2+2im) - ct3)) < tol
  @test @FastGTPSA(norm((2+2im) + ct1 - ct3)) < tol
  @test @FastGTPSA(norm(ct3 - ct2 - ct1)) < tol
  @test @FastGTPSA(norm(ct2 - ct3 - -ct1)) < tol
  @test @FastGTPSA(norm(ct3 - (2+2im) - ct1)) < tol
  @test @FastGTPSA(norm((2+2im) - ct3 - -ct1)) < tol
  @test @FastGTPSA(norm(ct2 * ct3 - (2+2im)*(3+3im))) < tol
  @test @FastGTPSA(norm(ct3 * ct2 - (2+2im)*(3+3im))) < tol
  @test @FastGTPSA(norm(ct2 * 5 - (10+10im))) < tol
  @test @FastGTPSA(norm(5 * ct2 - (10 * ct1))) < tol
  @test @FastGTPSA(norm(ct1 / ct2 - (1+im)/(2+2im))) < tol
  @test @FastGTPSA(norm(ct2 / ct1 - 2)) < tol
  @test @FastGTPSA(norm(1 / ct2 - 1/(2+2im))) < tol
  @test @FastGTPSA(norm(ct2 / 3 - (2+2im)/3)) < tol
  @test @FastGTPSA(norm(ct2 / ct2 - 1)) < tol
  @test @FastGTPSA(norm(ct2 ^ ct3 - (2+2im)^(3+3im))) < tol
  @test @FastGTPSA(norm(ct3 ^ ct2 - (3+3im)^(2+2im))) < tol
  @test @FastGTPSA(norm(ct2 ^ 3 - (2+2im)^3)) < tol
  @test @FastGTPSA(norm(ct2 ^ (1/2) - sqrt(2+2im))) < tol
  @test @FastGTPSA(norm(ct2 ^ (1/2) - sqrt(ct2))) < tol
  @test @FastGTPSA(norm(2 ^ ct3 - 2^(3+3im))) < tol
  @test @FastGTPSA(norm(inv(ct3) - 1/ct3)) < tol
  @test @FastGTPSA(norm(inv(ct3) - 1/(3+3im))) < tol

  # Promotion of NewTPS to ComplexTPS
  @test @FastGTPSA(norm(t1 + ct2 - (1 + (2+2im)))) < tol
  @test @FastGTPSA(norm(ct2 + t1 - (1 + (2+2im)))) < tol
  @test @FastGTPSA(norm(t1 + (2+2im) - (1 + (2+2im)))) < tol
  @test @FastGTPSA(norm((2+2im) + t1 - (1 + (2+2im)))) < tol
  @test @FastGTPSA(norm(t3 - ct2 - (3 - (2+2im)))) < tol
  @test @FastGTPSA(norm(ct2 - t3 - ((2+2im) - 3))) < tol
  @test @FastGTPSA(norm(t3 - (2+2im) - (3 - (2+2im)))) < tol
  @test @FastGTPSA(norm((2+2im) - t3 - ((2+2im) - 3))) < tol
  @test @FastGTPSA(norm(t2 * ct3 - 2 * (3+3im))) < tol
  @test @FastGTPSA(norm(ct3 * t2 - 2 * (3+3im))) < tol
  @test @FastGTPSA(norm(t2 * (3+3im) - 2 * (3+3im))) < tol
  @test @FastGTPSA(norm((3+3im) * t2 - 2 * (3+3im))) < tol
  @test @FastGTPSA(norm(t2 / ct3 - 2/(3+3im))) < tol
  @test @FastGTPSA(norm(ct3 / t2 - (3+3im)/2)) < tol
  @test @FastGTPSA(norm(t2 / (3+3im) - 2/(3+3im))) < tol
  @test @FastGTPSA(norm((3+3im) / t2 - (3+3im)/2)) < tol
  @test @FastGTPSA(norm(t2 ^ ct3 - 2^(3+3im))) < tol
  @test @FastGTPSA(norm(ct3 ^ t2 - (3+3im)^2)) < tol
  @test @FastGTPSA(norm(t2 ^ (3+3im) - 2^(3+3im))) < tol
  @test @FastGTPSA(norm((3+3im)^t2 - (3+3im)^2)) < tol

  # Make sure stack is 0:
  desc = unsafe_load(GTPSA.getdesc(t1).desc)
  tmpidx = unsafe_load(desc.ti)
  ctmpidx = unsafe_load(desc.cti)
  @test ctmpidx == 0
  @test tmpidx == 0
end

@testset "@FastGTPSA - Functions: scalar TPSs vs. Julia scalars" begin
  d = Descriptor(1, 5)
  t = NewTPS(use=d)
  v = 0.5
  t[0] = v
  tol = 1e-14
  t1 = NewTPS(t)
  t1[0] = 1
  t2 = zero(t1)
  t2[0] = 2
  t3 = zero(t1)
  t3[0] = 3


  @test @FastGTPSA(norm(abs(-t) - abs(-v) )) < tol
  @test @FastGTPSA(norm(sqrt(t) - sqrt(v))) < tol
  @test @FastGTPSA(norm(exp(t) - exp(v))) < tol
  @test @FastGTPSA(norm(log(t) - log(v))) < tol
  @test @FastGTPSA(norm(sin(t) - sin(v))) < tol
  @test @FastGTPSA(norm(cos(t) - cos(v))) < tol
  @test @FastGTPSA(norm(tan(t) - tan(v))) < tol
  @test @FastGTPSA(norm(csc(t) - csc(v))) < tol
  @test @FastGTPSA(norm(sec(t) - sec(v))) < tol
  @test @FastGTPSA(norm(cot(t) - cot(v))) < tol
  @test @FastGTPSA(norm(sinc(t) - sinc(v))) < tol
  @test @FastGTPSA(norm(sinh(t) - sinh(v))) < tol
  @test @FastGTPSA(norm(cosh(t) - cosh(v))) < tol
  @test @FastGTPSA(norm(tanh(t) - tanh(v))) < tol
  @test @FastGTPSA(norm(csch(t) - csch(v))) < tol
  @test @FastGTPSA(norm(sech(t) - sech(v))) < tol
  @test @FastGTPSA(norm(coth(t) - coth(v))) < tol
  @test @FastGTPSA(norm(asin(t) - asin(v))) < tol
  @test @FastGTPSA(norm(acos(t) - acos(v))) < tol
  @test @FastGTPSA(norm(atan(t) - atan(v))) < tol
  @test @FastGTPSA(norm(acsc(1/t) - acsc(1/v))) < tol
  @test @FastGTPSA(norm(asec(1/t) - asec(1/v))) < tol
  @test @FastGTPSA(norm(acot(1/t) - acot(1/v))) < tol
  @test @FastGTPSA(norm(asinh(t) - asinh(v))) < tol
  @test @FastGTPSA(norm(acosh(1/t) - acosh(1/v))) < tol
  @test @FastGTPSA(norm(atanh(t) - atanh(v))) < tol
  @test @FastGTPSA(norm(acsch(1/t) - acsch(1/v))) < tol
  @test @FastGTPSA(norm(asech(t) - asech(v))) < tol
  @test @FastGTPSA(norm(acoth(1/t) - acoth(1/v))) < tol
  @test @FastGTPSA(norm(asinc(t/pi) - asin(v)/(v))) < tol
  @test @FastGTPSA(norm(asinhc(t/pi) - asinh(v)/(v))) < tol
  @test @FastGTPSA(norm(zero(t) - zero(v))) < tol
  @test @FastGTPSA(norm(real(t) - real(v))) < tol
  @test @FastGTPSA(norm(imag(t) - imag(v))) < tol
  @test @FastGTPSA(norm(conj(t) - conj(v))) < tol
  @test @FastGTPSA(norm(sinhc(t/pi) - sinh(v)/v)) < tol
  @test @FastGTPSA(norm(erf(t) - erf(v))) < tol
  @test @FastGTPSA(norm(erfc(t) - erfc(v))) < tol
  @test @FastGTPSA(norm(-im*erf(t*im) - erfi(v))) < tol
  @test @FastGTPSA(norm(atan(t3,t2) - atan(3,2))) < tol
  @test @FastGTPSA(norm(atan(t3,2) - atan(3,2))) < tol
  @test @FastGTPSA(norm(atan(3,t2) - atan(3,2))) < tol
  @test @FastGTPSA(norm(atan(t3,-t2) - atan(3,-2))) < tol
  @test @FastGTPSA(norm(atan(t3,-2) - atan(3,-2))) < tol
  @test @FastGTPSA(norm(atan(3,-t2) - atan(3,-2))) < tol
  @test @FastGTPSA(norm(atan(-t3,-t2) - atan(-3,-2))) < tol
  @test @FastGTPSA(norm(atan(-t3,-2) - atan(-3,-2))) < tol
  @test @FastGTPSA(norm(atan(-3,-t2) - atan(-3,-2))) < tol
  @test @FastGTPSA(norm(atan(-t3,t2) - atan(-3,2))) < tol
  @test @FastGTPSA(norm(atan(-t3,2) - atan(-3,2))) < tol
  @test @FastGTPSA(norm(atan(-3,t2) - atan(-3,2))) < tol
  
  @test @FastGTPSA(norm(hypot(t2,t3) - hypot(2,3))) < tol
  @test @FastGTPSA(norm(hypot(2,t3) - hypot(2,3))) < tol
  @test @FastGTPSA(norm(hypot(t2,3) - hypot(2,3))) < tol
  @test @FastGTPSA(norm(hypot(t1,t2,t3) - hypot(1,2,3))) < tol
  @test @FastGTPSA(norm(hypot(1, t2, t3) - hypot(1,2,3))) < tol
  @test @FastGTPSA(norm(hypot(t1, 2, t3) - hypot(1,2,3))) < tol
  @test @FastGTPSA(norm(hypot(t1, t2, 3) - hypot(1,2,3))) < tol
  @test @FastGTPSA(norm(hypot(1, 2, t3) - hypot(1,2,3))) < tol
  @test @FastGTPSA(norm(hypot(1, t2, 3) - hypot(1,2,3))) < tol
  @test @FastGTPSA(norm(hypot(t1, 2, 3) - hypot(1,2,3))) < tol
  
  @test @FastGTPSA(norm(angle(t2) - angle(2))) < tol
  @test @FastGTPSA(norm(angle(-t2) - angle(-2))) < tol
  @test @FastGTPSA(norm(complex(t3) - complex(3))) < tol
  @test @FastGTPSA(norm(complex(t2,t3) - complex(2,3))) < tol
  @test @FastGTPSA(norm(polar(t2) - (abs(2)+im*atan(0,2)))) < tol
  @test @FastGTPSA(norm(polar(-t1) - (abs(-1)+im*atan(0,-1)))) < tol
  @test @FastGTPSA(norm(rect(t2) - (2*cos(0) + im*2*sin(0)))) < tol
  @test @FastGTPSA(norm(rect(-t1) - (-1*cos(0) + im*-1*sin(0)))) < tol
  

  v = 0.5+0.5im
  t = ComplexNewTPS(t)
  t[0] = v
  ct1 = ComplexNewTPS(t)
  ct1[0] = 1 + 1im
  ct2 = zero(ct1)
  ct2[0] = 2 + 2im
  ct3 = zero(ct1)
  ct3[0] = 3 + 3im
  @test @FastGTPSA(norm(abs(-t) - abs(-v) )) < tol
  @test @FastGTPSA(norm(sqrt(t) - sqrt(v))) < tol
  @test @FastGTPSA(norm(exp(t) - exp(v))) < tol
  @test @FastGTPSA(norm(log(t) - log(v))) < tol
  @test @FastGTPSA(norm(sin(t) - sin(v))) < tol
  @test @FastGTPSA(norm(cos(t) - cos(v))) < tol
  @test @FastGTPSA(norm(tan(t) - tan(v))) < tol
  @test @FastGTPSA(norm(csc(t) - csc(v))) < tol
  @test @FastGTPSA(norm(sec(t) - sec(v))) < tol
  @test @FastGTPSA(norm(cot(t) - cot(v))) < tol
  @test @FastGTPSA(norm(sinc(t) - sinc(v))) < tol
  @test @FastGTPSA(norm(sinh(t) - sinh(v))) < tol
  @test @FastGTPSA(norm(cosh(t) - cosh(v))) < tol
  @test @FastGTPSA(norm(tanh(t) - tanh(v))) < tol
  @test @FastGTPSA(norm(csch(t) - csch(v))) < tol
  @test @FastGTPSA(norm(sech(t) - sech(v))) < tol
  @test @FastGTPSA(norm(coth(t) - coth(v))) < tol

  @test @FastGTPSA(norm(asin(t) - asin(v))) < tol
  @test @FastGTPSA(norm(acos(t) - acos(v))) < tol
  @test @FastGTPSA(norm(atan(t) - atan(v))) < tol
  @test @FastGTPSA(norm(acsc(t) - acsc(v))) < tol
  @test @FastGTPSA(norm(asec(t) - asec(v))) < tol
  @test @FastGTPSA(norm(acot(t) - acot(v))) < tol
  @test @FastGTPSA(norm(asinh(t) - asinh(v))) < tol
  @test @FastGTPSA(norm(acosh(t) - acosh(v))) < tol
  @test @FastGTPSA(norm(atanh(t) - atanh(v))) < tol
  @test @FastGTPSA(norm(acsch(t) - acsch(v))) < tol
  @test @FastGTPSA(norm(asech(t) - asech(v))) < tol
  @test @FastGTPSA(norm(acoth(t) - acoth(v))) < tol
  @test @FastGTPSA(norm(asinc(t/pi) - asin(v)/v)) < tol
  @test @FastGTPSA(norm(asinhc(t/pi) - asinh(v)/v)) < tol
  
  @test @FastGTPSA(norm(zero(t) - zero(v))) < tol
  @test @FastGTPSA(norm(real(t) - real(v))) < tol
  @test @FastGTPSA(norm(imag(t) - imag(v))) < tol
  @test @FastGTPSA(norm(conj(t) - conj(v))) < tol
  @test @FastGTPSA(norm(sinhc(t/pi) - sinh(v)/v)) < tol
  @test @FastGTPSA(norm(erf(t) - erf(v))) < tol
  @test @FastGTPSA(norm(erfc(t) - erfc(v))) < tol
  @test @FastGTPSA(norm(-im*erf(t*im) - erfi(v))) < tol
  @test @FastGTPSA(norm(hypot(ct2,ct3) - hypot(2+2im,3+3im))) < tol
  @test @FastGTPSA(norm(hypot(2+2im,ct3) - hypot(2+2im,3+3im))) < tol
  @test @FastGTPSA(norm(hypot(ct2,3+3im) - hypot(2+2im,3+3im))) < tol
  @test @FastGTPSA(norm(hypot(ct1,ct2,ct3) - hypot(1+1im,2+2im,3+3im))) < tol
  @test @FastGTPSA(norm(hypot(1+1im, ct2, ct3) - hypot(1+1im,2+2im,3+3im))) < tol
  @test @FastGTPSA(norm(hypot(ct1, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im))) < tol
  @test @FastGTPSA(norm(hypot(ct1, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im))) < tol
  @test @FastGTPSA(norm(hypot(1+1im, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im))) < tol
  @test @FastGTPSA(norm(hypot(1+1im, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im))) < tol
  @test @FastGTPSA(norm(hypot(ct1, 2+2im, 3+3im) - hypot(1+1im,2+2im,3+3im))) < tol
  
  @test @FastGTPSA(norm(angle(t2+im*t3) - angle(2+3im))) < tol
  @test @FastGTPSA(norm(angle(t2-im*t3) - angle(2-3im))) < tol
  @test @FastGTPSA(norm(angle(-t2-im*t3) - angle(-2-3im))) < tol
  @test @FastGTPSA(norm(angle(-t2+im*t3) - angle(-2+3im))) < tol
  @test @FastGTPSA(norm(angle(ct2) - angle(2+2im))) < tol
  @test @FastGTPSA(norm(angle(-ct2) - angle(-2-2im))) < tol
  @test @FastGTPSA(norm(complex(ct3) - complex(3+3im))) < tol
  @test @FastGTPSA(norm(polar(ct2) - (abs(2+2im)+im*angle(2+2im)))) < tol
  @test @FastGTPSA(norm(polar(-ct1) - (abs(-1-im)+im*angle(-1-im)))) < tol
  @test @FastGTPSA(norm(rect(ct2) - (2*cos(2) + im*2*sin(2)))) < tol
  @test @FastGTPSA(norm(rect(-ct1) - (-1*cos(-1) + im*-1*sin(-1)))) < tol
  
  # Hypot, mixing NewTPS with ComplexTPS
  @test @FastGTPSA(norm(hypot(ct1, ct2, t3) - hypot(1+1im,2+2im,3))) < tol
  @test @FastGTPSA(norm(hypot(ct1, t2, ct3) - hypot(1+1im,2,3+3im))) < tol
  @test @FastGTPSA(norm(hypot(t1, ct2, ct3) - hypot(1,2+2im,3+3im))) < tol
  @test @FastGTPSA(norm(hypot(ct1, t2, t3) - hypot(1+1im,2,3))) < tol
  @test @FastGTPSA(norm(hypot(t1, ct2, t3) - hypot(1,2+2im,3))) < tol
  @test @FastGTPSA(norm(hypot(t1, t2, ct3) - hypot(1,2,3+3im))) < tol
  @test @FastGTPSA(norm(hypot(ct1,t2,3+3im) - hypot(1+1im,2,3+3im))) < tol
  @test @FastGTPSA(norm(hypot(t1, ct2, 3+3im) - hypot(1,2+2im,3+3im))) < tol
  @test @FastGTPSA(norm(hypot(ct1,2+2im,t3) - hypot(1+1im,2+2im,3))) < tol
  @test @FastGTPSA(norm(hypot(t1,2+2im,ct3) - hypot(1,2+2im,3+3im))) < tol
  @test @FastGTPSA(norm(hypot(1+1im,ct2,t3) - hypot(1+1im,2+2im,3))) < tol
  @test @FastGTPSA(norm(hypot(1+1im, t2, ct3) - hypot(1+1im,2,3+3im))) < tol
  @test @FastGTPSA(norm(hypot(t1,t2,3+3im) - hypot(1,2,3+3im))) < tol
  @test @FastGTPSA(norm(hypot(t1,2+2im,t3) - hypot(1,2+2im,3))) < tol
  @test @FastGTPSA(norm(hypot(1+1im,t2,t3) - hypot(1+1im,2,3))) < tol
  @test @FastGTPSA(norm(hypot(t1,2,3+3im) - hypot(1,2,3+3im))) < tol
  @test @FastGTPSA(norm(hypot(1,t2,3+3im) - hypot(1,2,3+3im))) < tol
  @test @FastGTPSA(norm(hypot(1+1im,2,t3) - hypot(1+1im,2,3))) < tol

  # Make sure stack is 0:
  desc = unsafe_load(GTPSA.getdesc(t1).desc)
  tmpidx = unsafe_load(desc.ti)
  ctmpidx = unsafe_load(desc.cti)
  @test ctmpidx == 0
  @test tmpidx == 0
end

@testset "@FastGTPSA - Functions: identities, using TPSs" begin
  d = Descriptor(1, 5)
  t = NewTPS(use=d)
  t[0] = 0.5; t[[1]] = 2; t[[2]] = 3; t[[3]] = 4; t[[4]] = 5; t[[5]] = 6

  tol = 1e-10

  @test @FastGTPSA(norm(sin(t)^2+cos(t)^2 - 1)) < tol
  @test @FastGTPSA(norm(1/sin(t) - csc(t))) < tol
  @test @FastGTPSA(norm(1/cos(t) - sec(t))) < tol
  @test @FastGTPSA(norm(1/tan(t) - cot(t))) < tol
  @test @FastGTPSA(norm(sin(t)/cos(t) - tan(t))) < tol
  @test @FastGTPSA(norm(cos(2*t) - cos(t)^2 + sin(t)^2)) < tol
  @test @FastGTPSA(norm(sec(t)^2 - 1 - tan(t)^2)) < tol
  @test @FastGTPSA(norm(sin(t/2) - sqrt((1-cos(t))/2))) < tol
  @test @FastGTPSA(norm(cos(t/2) - sqrt((1+cos(t))/2))) < tol
  @test @FastGTPSA(norm(sqrt(t^2) - abs(t))) < tol
  @test @FastGTPSA(norm(csc(t)^2 - cot(t)^2 - 1)) < tol
  @test @FastGTPSA(norm(exp(log(t)) - t)) < tol
  @test @FastGTPSA(norm(log(exp(t)) - t)) < tol
  @test @FastGTPSA(norm(log(exp(t)) - exp(log(t)))) < tol
  @test @FastGTPSA(norm(log(t^2) - 2*log(t))) < tol
  @test @FastGTPSA(norm(5*log(t) - log(t^5))) < tol
  @test @FastGTPSA(norm(t*log(5) - log(5^t))) < tol
  @test @FastGTPSA(norm(sinc(t) - sin(pi*t)/(pi*t))) < tol
  @test @FastGTPSA(norm(sinhc(t/pi) - sinh(t)/t)) < tol
  @test @FastGTPSA(norm(exp(im*t) - cos(t) - im*sin(t))) < tol
  @test @FastGTPSA(norm(real(exp(im*t)) - cos(t))) < tol
  @test @FastGTPSA(norm(imag(exp(im*t)) - sin(t))) < tol
  @test @FastGTPSA(norm(sinh(t) - (exp(t) - exp(-t))/2)) < tol
  @test @FastGTPSA(norm(cosh(t) - (exp(t) + exp(-t))/2)) < tol
  @test @FastGTPSA(norm(tanh(t) - sinh(t)/cosh(t))) < tol
  @test @FastGTPSA(norm(csch(t) - 1/sinh(t))) < tol
  @test @FastGTPSA(norm(sech(t) - 1/cosh(t))) < tol
  @test @FastGTPSA(norm(coth(t) - cosh(t)/sinh(t))) < tol
  @test @FastGTPSA(norm(coth(t) - 1/tanh(t))) < tol
  @test @FastGTPSA(norm(cosh(t)^2 - sinh(t)^2 - 1)) < tol
  @test @FastGTPSA(norm(1 - tanh(t)^2 - sech(t)^2)) < tol
  @test @FastGTPSA(norm(coth(t)^2 - 1 - csch(t)^2)) < tol
  @test @FastGTPSA(norm(asin(sin(t)) - t)) < tol
  @test @FastGTPSA(norm(acos(cos(t)) - t)) < tol
  @test @FastGTPSA(norm(atan(tan(t)) - t)) < tol
  @test @FastGTPSA(norm(acsc(1/t) - asin(t))) < tol
  @test @FastGTPSA(norm(asec(1/t) - acos(t))) < tol
  @test @FastGTPSA(norm(acot(1/t) - atan(t))) < tol
  @test @FastGTPSA(norm(asinh(sinh(t)) - t)) < tol
  @test @FastGTPSA(norm(acosh(cosh(t)) - t)) < tol
  @test @FastGTPSA(norm(atanh(tanh(t)) - t)) < tol
  @test @FastGTPSA(norm(acsch(t) - asinh(1/t))) < tol
  @test @FastGTPSA(norm(asech(t) - acosh(1/t))) < tol
  @test @FastGTPSA(norm(acoth(1/t) - atanh(t))) < tol
  @test @FastGTPSA(norm(asinc(t/pi) - asin(t)/t)) < tol
  @test @FastGTPSA(norm(asinhc(t/pi) - asinh(t)/t)) < tol
  @test @FastGTPSA(norm(erfc(t) - 1 + erf(t))) < tol
  @test @FastGTPSA(norm(erf(-t) + erf(t))) < tol
  @test @FastGTPSA(norm(angle(t))) < tol
  @test @FastGTPSA(norm(complex(t) - t)) < tol
  @test @FastGTPSA(norm(complex(t,t) - (t+im*t))) < tol

  t = ComplexNewTPS(t)
  t[0] = 0.5+0.5im; t[[1]] = 2+2im; t[[2]] = 3+3im; t[[3]] = 4+4im; t[[4]] = 5+5im; t[[5]] = 6+6im
  @test @FastGTPSA(norm(sin(t)^2+cos(t)^2 - 1)) < tol
  @test @FastGTPSA(norm(1/sin(t) - csc(t))) < tol
  @test @FastGTPSA(norm(1/cos(t) - sec(t))) < tol
  @test @FastGTPSA(norm(1/tan(t) - cot(t))) < tol
  @test @FastGTPSA(norm(sin(t)/cos(t) - tan(t))) < tol
  @test @FastGTPSA(norm(cos(2*t) - cos(t)^2 + sin(t)^2)) < tol
  @test @FastGTPSA(norm(sec(t)^2 - 1 - tan(t)^2)) < tol
  @test @FastGTPSA(norm(sin(t/2) - sqrt((1-cos(t))/2))) < tol
  @test @FastGTPSA(norm(cos(t/2) - sqrt((1+cos(t))/2))) < tol
  @test @FastGTPSA(norm(sqrt(t^2) - t)) < tol
  @test @FastGTPSA(norm(csc(t)^2 - cot(t)^2 - 1)) < tol
  @test @FastGTPSA(norm(exp(log(t)) - t)) < tol
  @test @FastGTPSA(norm(log(exp(t)) - t)) < tol
  @test @FastGTPSA(norm(log(exp(t)) - exp(log(t)))) < tol
  @test @FastGTPSA(norm(log(t^2) - 2*log(t))) < tol
  @test @FastGTPSA(norm(5*log(t) - log(t^5) - 2*pi*im)) < tol
  @test @FastGTPSA(norm(t*log(5) - log(5^t))) < tol
  @test @FastGTPSA(norm(sinc(t/pi) - sin(t)/t)) < tol
  @test @FastGTPSA(norm(sinhc(t/pi) - sinh(t)/t)) < tol
  @test @FastGTPSA(norm(exp(im*t) - cos(t) - im*sin(t))) < tol
  @test @FastGTPSA(norm(sinh(t) - (exp(t) - exp(-t))/2)) < tol
  @test @FastGTPSA(norm(cosh(t) - (exp(t) + exp(-t))/2)) < tol
  @test @FastGTPSA(norm(tanh(t) - sinh(t)/cosh(t))) < tol
  @test @FastGTPSA(norm(csch(t) - 1/sinh(t))) < tol
  @test @FastGTPSA(norm(sech(t) - 1/cosh(t))) < tol
  @test @FastGTPSA(norm(coth(t) - cosh(t)/sinh(t))) < tol
  @test @FastGTPSA(norm(coth(t) - 1/tanh(t))) < tol
  @test @FastGTPSA(norm(cosh(t)^2 - sinh(t)^2 - 1)) < tol
  @test @FastGTPSA(norm(1 - tanh(t)^2 - sech(t)^2)) < tol
  @test @FastGTPSA(norm(coth(t)^2 - 1 - csch(t)^2)) < tol
  
  @test @FastGTPSA(norm(asin(sin(t)) - t)) < tol
  @test @FastGTPSA(norm(acos(cos(t)) - t)) < tol
  @test @FastGTPSA(norm(atan(tan(t)) - t)) < tol
  @test @FastGTPSA(norm(acsc(t) - asin(1/t))) < tol
  @test @FastGTPSA(norm(asec(t) - acos(1/t))) < tol
  @test @FastGTPSA(norm(acot(t) - atan(1/t))) < tol
  @test @FastGTPSA(norm(asinh(sinh(t)) - t)) < tol
  @test @FastGTPSA(norm(acosh(cosh(t)) - t)) < tol
  @test @FastGTPSA(norm(atanh(tanh(t)) - t)) < tol
  @test @FastGTPSA(norm(acsch(t) - asinh(1/t))) < tol
  @test @FastGTPSA(norm(asech(t) - acosh(1/t))) < tol
  @test @FastGTPSA(norm(acoth(t) - atanh(1/t))) < tol
  @test @FastGTPSA(norm(asinc(t/pi) - asin(t)/t)) < tol
  @test @FastGTPSA(norm(asinhc(t/pi) - asinh(t)/t)) < tol
  
  @test @FastGTPSA(norm(erfc(t) - 1 + erf(t))) < tol
  @test @FastGTPSA(norm(erf(-t) + erf(t))) < tol
  @test @FastGTPSA(norm(angle(t) - atan(imag(t),real(t)))) < tol
  @test @FastGTPSA(norm(complex(t) - t)) < tol

  # Make sure stack is 0:
  desc = unsafe_load(GTPSA.getdesc(t).desc)
  tmpidx = unsafe_load(desc.ti)
  ctmpidx = unsafe_load(desc.cti)
  @test ctmpidx == 0
  @test tmpidx == 0
end

@testset "Type stability" begin
  include("type_stable.jl")
  include("../benchmark/track.jl")
  @test_opt type_stable_test()
  @test_opt benchmark_GTPSA()
end

@testset "Taylor map benchmark against ForwardDiff" begin
  include("track.jl")
  map = benchmark_newGTPSA()
  jFD, hFD = benchmark_ForwardDiff()
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
