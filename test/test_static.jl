@testset "Static: Arithmetic operators" begin
  d = Descriptor(1, 5)
  t = TPS{d}()
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

@testset "Static: Functions: scalar TPSs vs. Julia scalars" begin
  d = Descriptor(1, 5)
  t = TPS{d}()
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

@testset "Static: Functions: identities, using TPSs" begin
  d = Descriptor(1, 5)
  t = TPS{d}()
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

@testset "Static: Indexing" begin
  d = Descriptor(3,10,2,10)
  v = @vars(d)
  p = @params(d)
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

@testset "Static: FastGTPSA - Arithmetic operators" begin
  d = Descriptor(1, 5)
  t = TPS{d}()
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

  out = ComplexTPS64{d}()
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

@testset "Static: FastGTPSA - Functions: scalar TPSs vs. Julia scalars" begin
  d = Descriptor(1, 5)
  t = TPS{d}()
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
  t = TPS{d}()
  v = 0.5
  t[0] = v
  tol = 1e-14
  t1 = TPS(t)
  t1[0] = 1
  t2 = zero(t1)
  t2[0] = 2
  t3 = zero(t1)
  t3[0] = 3
  out = ComplexTPS64{d}()

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

@testset "Static: FastGTPSA - Functions: identities, using TPSs" begin
  d = Descriptor(1, 5)
  t = TPS{d}()
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
  t = TPS{d}()
  t[0] = 0.5; t[[1]] = 2; t[[2]] = 3; t[[3]] = 4; t[[4]] = 5; t[[5]] = 6

  tol = 1e-10
  out = ComplexTPS64{d}()
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

# NOTE: for some reason merely overloading Base.setindex!(A::Array{T}, x::TempTPS, i1::Int) where {T<:TPS},
# in Julia 1.9 causes allocations. This is not the case in 1.10, so presumably this is a bug.
# Therefore, allocation tests only are performed on >=1.10
if VERSION >= v"1.10"

@testset "Static: FastGTPSA - Allocations" begin
  d = Descriptor(1, 5)
  t = TPS{d}()
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
  out = ComplexTPS64{d}()

  # TPS:
  @test @benchmark(@FastGTPSA(normTPS($t1 + $t2 - $t3))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t2 + $t1 - $t3))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t1 + 2 - $t3))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(2 + $t1 - $t3))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t3 - $t2 - $t1))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t2 - $t3 - -$t1))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t3 - 2 - $t1))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(2 - $t3 - -$t1))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t2 * $t3 - 6))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t3 * $t2 - 6))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t2 * 5 - 10))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(5 * $t2 - 10 * $t1))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t1 / $t2 - 1/2))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t2 / $t1 - 2))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(1 / $t2 - 1/2))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t2 / 3 - 2/3))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t2 / $t2 - $t1))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t2 / $t2 - 1))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t2 ^ $t3 - 8))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t3 ^ $t2 - 9))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t2 ^ 3 - 8))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t2 ^ (1/2) - sqrt(2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t2 ^ (1/2) - sqrt($t2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(2 ^ $t3 - 8))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(inv($t3) - 1/$t3))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(inv($t3) - 1/3))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct1 + $ct2 - $ct3))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct2 + $ct1 - $ct3))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct1 + (2+2im) - $ct3))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS((2+2im) + $ct1 - $ct3))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct3 - $ct2 - $ct1))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct2 - $ct3 - -$ct1))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct3 - (2+2im) - $ct1))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS((2+2im) - $ct3 - -$ct1))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct2 * $ct3 - (2+2im)*(3+3im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct3 * $ct2 - (2+2im)*(3+3im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct2 * 5 - (10+10im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(5 * $ct2 - (10 * $ct1)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct1 / $ct2 - (1+im)/(2+2im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct2 / $ct1 - 2))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(1 / $ct2 - 1/(2+2im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct2 / 3 - (2+2im)/3))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct2 / $ct2 - 1))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct2 ^ $ct3 - (2+2im)^(3+3im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct3 ^ $ct2 - (3+3im)^(2+2im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct2 ^ 3 - (2+2im)^3))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct2 ^ (1/2) - sqrt(2+2im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct2 ^ (1/2) - sqrt($ct2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(2 ^ $ct3 - 2^(3+3im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(inv($ct3) - 1/$ct3))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(inv($ct3) - 1/(3+3im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t1 + $ct2 - (1 + (2+2im))))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct2 + $t1 - (1 + (2+2im))))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t1 + (2+2im) - (1 + (2+2im))))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS((2+2im) + $t1 - (1 + (2+2im))))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t3 - $ct2 - (3 - (2+2im))))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct2 - $t3 - ((2+2im) - 3)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t3 - (2+2im) - (3 - (2+2im))))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS((2+2im) - $t3 - ((2+2im) - 3)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t2 * $ct3 - 2 * (3+3im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct3 * $t2 - 2 * (3+3im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t2 * (3+3im) - 2 * (3+3im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS((3+3im) * $t2 - 2 * (3+3im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t2 / $ct3 - 2/(3+3im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct3 / $t2 - (3+3im)/2))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t2 / (3+3im) - 2/(3+3im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS((3+3im) / $t2 - (3+3im)/2))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t2 ^ $ct3 - 2^(3+3im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($ct3 ^ $t2 - (3+3im)^2))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t2 ^ (3+3im) - 2^(3+3im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS((3+3im)^$t2 - (3+3im)^2))).allocs == 0

  @test @benchmark(@FastGTPSA!($out = $t1 + $t2 - $t3)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t2 + $t1 - $t3)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t1 + 2 - $t3)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = 2 + $t1 - $t3)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t3 - $t2 - $t1)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t2 - $t3 - -$t1)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t3 - 2 - $t1)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = 2 - $t3 - -$t1)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t2 * $t3 - 6)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t3 * $t2 - 6)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t2 * 5 - 10)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = 5 * $t2 - 10 * $t1)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t1 / $t2 - 1/2)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t2 / $t1 - 2)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = 1 / $t2 - 1/2)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t2 / 3 - 2/3)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t2 / $t2 - $t1)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t2 / $t2 - 1)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t2 ^ $t3 - 8)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t3 ^ $t2 - 9)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t2 ^ 3 - 8)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t2 ^ (1/2) - sqrt(2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t2 ^ (1/2) - sqrt($t2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = 2 ^ $t3 - 8)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = inv($t3) - 1/$t3)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = inv($t3) - 1/3)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct1 + $ct2 - $ct3)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct2 + $ct1 - $ct3)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct1 + (2+2im) - $ct3)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = (2+2im) + $ct1 - $ct3)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct3 - $ct2 - $ct1)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct2 - $ct3 - -$ct1)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct3 - (2+2im) - $ct1)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = (2+2im) - $ct3 - -$ct1)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct2 * $ct3 - (2+2im)*(3+3im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct3 * $ct2 - (2+2im)*(3+3im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct2 * 5 - (10+10im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = 5 * $ct2 - (10 * $ct1))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct1 / $ct2 - (1+im)/(2+2im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct2 / $ct1 - 2)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = 1 / $ct2 - 1/(2+2im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct2 / 3 - (2+2im)/3)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct2 / $ct2 - 1)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct2 ^ $ct3 - (2+2im)^(3+3im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct3 ^ $ct2 - (3+3im)^(2+2im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct2 ^ 3 - (2+2im)^3)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct2 ^ (1/2) - sqrt(2+2im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct2 ^ (1/2) - sqrt($ct2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = 2 ^ $ct3 - 2^(3+3im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = inv($ct3) - 1/$ct3)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = inv($ct3) - 1/(3+3im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t1 + $ct2 - (1 + (2+2im)))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct2 + $t1 - (1 + (2+2im)))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t1 + (2+2im) - (1 + (2+2im)))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = (2+2im) + $t1 - (1 + (2+2im)))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t3 - $ct2 - (3 - (2+2im)))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct2 - $t3 - ((2+2im) - 3))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t3 - (2+2im) - (3 - (2+2im)))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = (2+2im) - $t3 - ((2+2im) - 3))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t2 * $ct3 - 2 * (3+3im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct3 * $t2 - 2 * (3+3im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t2 * (3+3im) - 2 * (3+3im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = (3+3im) * $t2 - 2 * (3+3im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t2 / $ct3 - 2/(3+3im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct3 / $t2 - (3+3im)/2)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t2 / (3+3im) - 2/(3+3im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = (3+3im) / $t2 - (3+3im)/2)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t2 ^ $ct3 - 2^(3+3im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $ct3 ^ $t2 - (3+3im)^2)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t2 ^ (3+3im) - 2^(3+3im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = (3+3im)^$t2 - (3+3im)^2)).allocs == 0

  d = Descriptor(1, 5)
  t = TPS{d}()
  v = 0.5
  t[0] = v
  tol = 1e-14
  t1 = TPS(t)
  t1[0] = 1
  t2 = zero(t1)
  t2[0] = 2
  t3 = zero(t1)
  t3[0] = 3


  @test @benchmark(@FastGTPSA(normTPS(abs(-$t) - abs(-$v) ))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sqrt($t) - sqrt($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(exp($t) - exp($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(log($t) - log($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sin($t) - sin($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(cos($t) - cos($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(tan($t) - tan($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(csc($t) - csc($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sec($t) - sec($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(cot($t) - cot($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sinc($t) - sinc($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sinh($t) - sinh($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(cosh($t) - cosh($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(tanh($t) - tanh($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(csch($t) - csch($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sech($t) - sech($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(coth($t) - coth($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asin($t) - asin($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acos($t) - acos($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(atan($t) - atan($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acsc(1/$t) - acsc(1/$v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asec(1/$t) - asec(1/$v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acot(1/$t) - acot(1/$v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asinh($t) - asinh($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acosh(1/$t) - acosh(1/$v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(atanh($t) - atanh($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acsch(1/$t) - acsch(1/$v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asech($t) - asech($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acoth(1/$t) - acoth(1/$v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asinc($t/pi) - asin($v)/($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asinhc($t/pi) - asinh($v)/($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(zero($t) - zero($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(one($t) + one($t) - zero($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(real($t) - real($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(imag($t) - imag($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(conj($t) - conj($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sinhc($t/pi) - sinh($v)/$v))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(erf($t) - erf($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(erfc($t) - erfc($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(-im*erf($t*im) - erfi($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(atan($t3,$t2) - atan(3,2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(atan($t3,2) - atan(3,2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(atan(3,$t2) - atan(3,2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(atan($t3,-$t2) - atan(3,-2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(atan($t3,-2) - atan(3,-2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(atan(3,-$t2) - atan(3,-2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(atan(-$t3,-$t2) - atan(-3,-2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(atan(-$t3,-2) - atan(-3,-2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(atan(-3,-$t2) - atan(-3,-2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(atan(-$t3,$t2) - atan(-3,2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(atan(-$t3,2) - atan(-3,2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(atan(-3,$t2) - atan(-3,2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(angle($t2) - angle(2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(angle(-$t2) - angle(-2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(complex($t3) - complex(3)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(complex($t2,$t3) - complex(2,3)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(polar($t2) - (abs(2)+im*atan(0,2))))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(polar(-$t1) - (abs(-1)+im*atan(0,-1))))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(rect($t2) - (2*cos(0) + im*2*sin(0))))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(rect(-$t1) - (-1*cos(0) + im*-1*sin(0))))).allocs == 0

  
  @test @benchmark(@FastGTPSA!($out = abs(-$t) - abs(-$v) )).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sqrt($t) - sqrt($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = exp($t) - exp($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = log($t) - log($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sin($t) - sin($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = cos($t) - cos($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = tan($t) - tan($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = csc($t) - csc($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sec($t) - sec($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = cot($t) - cot($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sinc($t) - sinc($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sinh($t) - sinh($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = cosh($t) - cosh($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = tanh($t) - tanh($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = csch($t) - csch($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sech($t) - sech($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = coth($t) - coth($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asin($t) - asin($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acos($t) - acos($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = atan($t) - atan($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acsc(1/$t) - acsc(1/$v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asec(1/$t) - asec(1/$v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acot(1/$t) - acot(1/$v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asinh($t) - asinh($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acosh(1/$t) - acosh(1/$v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = atanh($t) - atanh($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acsch(1/$t) - acsch(1/$v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asech($t) - asech($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acoth(1/$t) - acoth(1/$v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asinc($t/pi) - asin($v)/($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asinhc($t/pi) - asinh($v)/($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = zero($t) - zero($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = one($t) + one($t) - zero($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = real($t) - real($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = imag($t) - imag($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = conj($t) - conj($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sinhc($t/pi) - sinh($v)/$v)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = erf($t) - erf($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = erfc($t) - erfc($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = -im*erf($t*im) - erfi($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = atan($t3,$t2) - atan(3,2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = atan($t3,2) - atan(3,2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = atan(3,$t2) - atan(3,2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = atan($t3,-$t2) - atan(3,-2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = atan($t3,-2) - atan(3,-2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = atan(3,-$t2) - atan(3,-2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = atan(-$t3,-$t2) - atan(-3,-2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = atan(-$t3,-2) - atan(-3,-2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = atan(-3,-$t2) - atan(-3,-2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = atan(-$t3,$t2) - atan(-3,2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = atan(-$t3,2) - atan(-3,2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = atan(-3,$t2) - atan(-3,2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = angle($t2) - angle(2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = angle(-$t2) - angle(-2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = complex($t3) - complex(3))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = complex($t2,$t3) - complex(2,3))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = polar($t2) - (abs(2)+im*atan(0,2)))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = polar(-$t1) - (abs(-1)+im*atan(0,-1)))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = rect($t2) - (2*cos(0) + im*2*sin(0)))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = rect(-$t1) - (-1*cos(0) + im*-1*sin(0)))).allocs == 0
  

  v = 0.5+0.5im
  t = ComplexTPS64(t)
  t[0] = v
  ct1 = ComplexTPS64(t)
  ct1[0] = 1 + 1im
  ct2 = zero(ct1)
  ct2[0] = 2 + 2im
  ct3 = zero(ct1)
  ct3[0] = 3 + 3im
  @test @benchmark(@FastGTPSA(normTPS(abs(-$t) - abs(-$v) ))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sqrt($t) - sqrt($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(exp($t) - exp($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(log($t) - log($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sin($t) - sin($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(cos($t) - cos($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(tan($t) - tan($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(csc($t) - csc($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sec($t) - sec($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(cot($t) - cot($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sinc($t) - sinc($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sinh($t) - sinh($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(cosh($t) - cosh($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(tanh($t) - tanh($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(csch($t) - csch($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sech($t) - sech($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(coth($t) - coth($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asin($t) - asin($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acos($t) - acos($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(atan($t) - atan($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acsc($t) - acsc($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asec($t) - asec($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acot($t) - acot($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asinh($t) - asinh($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acosh($t) - acosh($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(atanh($t) - atanh($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acsch($t) - acsch($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asech($t) - asech($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acoth($t) - acoth($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asinc($t/pi) - asin($v)/$v))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asinhc($t/pi) - asinh($v)/$v))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(zero($t) - zero($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(real($t) - real($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(imag($t) - imag($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(conj($t) - conj($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sinhc($t/pi) - sinh($v)/$v))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(erf($t) - erf($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(erfc($t) - erfc($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(-im*erf($t*im) - erfi($v)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(angle($t2+im*$t3) - angle(2+3im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(angle($t2-im*$t3) - angle(2-3im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(angle(-$t2-im*$t3) - angle(-2-3im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(angle(-$t2+im*$t3) - angle(-2+3im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(angle($ct2) - angle(2+2im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(angle(-$ct2) - angle(-2-2im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(complex($ct3) - complex(3+3im)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(polar($ct2) - (abs(2+2im)+im*angle(2+2im))))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(polar(-$ct1) - (abs(-1-im)+im*angle(-1-im))))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(rect($ct2) - (2*cos(2) + im*2*sin(2))))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(rect(-$ct1) - (-1*cos(-1) + im*-1*sin(-1))))).allocs == 0

  @test @benchmark(@FastGTPSA!($out = abs(-$t) - abs(-$v) )).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sqrt($t) - sqrt($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = exp($t) - exp($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = log($t) - log($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sin($t) - sin($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = cos($t) - cos($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = tan($t) - tan($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = csc($t) - csc($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sec($t) - sec($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = cot($t) - cot($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sinc($t) - sinc($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sinh($t) - sinh($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = cosh($t) - cosh($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = tanh($t) - tanh($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = csch($t) - csch($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sech($t) - sech($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = coth($t) - coth($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asin($t) - asin($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acos($t) - acos($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = atan($t) - atan($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acsc($t) - acsc($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asec($t) - asec($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acot($t) - acot($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asinh($t) - asinh($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acosh($t) - acosh($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = atanh($t) - atanh($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acsch($t) - acsch($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asech($t) - asech($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acoth($t) - acoth($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asinc($t/pi) - asin($v)/$v)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asinhc($t/pi) - asinh($v)/$v)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = zero($t) - zero($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = real($t) - real($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = imag($t) - imag($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = conj($t) - conj($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sinhc($t/pi) - sinh($v)/$v)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = erf($t) - erf($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = erfc($t) - erfc($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = -im*erf($t*im) - erfi($v))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = angle($t2+im*$t3) - angle(2+3im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = angle($t2-im*$t3) - angle(2-3im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = angle(-$t2-im*$t3) - angle(-2-3im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = angle(-$t2+im*$t3) - angle(-2+3im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = angle($ct2) - angle(2+2im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = angle(-$ct2) - angle(-2-2im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = complex($ct3) - complex(3+3im))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = polar($ct2) - (abs(2+2im)+im*angle(2+2im)))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = polar(-$ct1) - (abs(-1-im)+im*angle(-1-im)))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = rect($ct2) - (2*cos(2) + im*2*sin(2)))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = rect(-$ct1) - (-1*cos(-1) + im*-1*sin(-1)))).allocs == 0

  d = Descriptor(1, 5)
  t = TPS{d}()
  t[0] = 0.5; t[[1]] = 2; t[[2]] = 3; t[[3]] = 4; t[[4]] = 5; t[[5]] = 6

  tol = 1e-10

  @test @benchmark(@FastGTPSA(normTPS(sin($t)^2+cos($t)^2 - 1))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(1/sin($t) - csc($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(1/cos($t) - sec($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(1/tan($t) - cot($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sin($t)/cos($t) - tan($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(cos(2*$t) - cos($t)^2 + sin($t)^2))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sec($t)^2 - 1 - tan($t)^2))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sin($t/2) - sqrt((1-cos($t))/2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(cos($t/2) - sqrt((1+cos($t))/2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sqrt($t^2) - abs($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(csc($t)^2 - cot($t)^2 - 1))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(exp(log($t)) - $t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(log(exp($t)) - $t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(log(exp($t)) - exp(log($t))))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(log($t^2) - 2*log($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(5*log($t) - log($t^5)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t*log(5) - log(5^$t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sinc($t) - sin(pi*$t)/(pi*$t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sinhc($t/pi) - sinh($t)/$t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(exp(im*$t) - cos($t) - im*sin($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(real(exp(im*$t)) - cos($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(imag(exp(im*$t)) - sin($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sinh($t) - (exp($t) - exp(-$t))/2))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(cosh($t) - (exp($t) + exp(-$t))/2))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(tanh($t) - sinh($t)/cosh($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(csch($t) - 1/sinh($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sech($t) - 1/cosh($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(coth($t) - cosh($t)/sinh($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(coth($t) - 1/tanh($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(cosh($t)^2 - sinh($t)^2 - 1))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(1 - tanh($t)^2 - sech($t)^2))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(coth($t)^2 - 1 - csch($t)^2))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asin(sin($t)) - $t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acos(cos($t)) - $t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(atan(tan($t)) - $t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acsc(1/$t) - asin($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asec(1/$t) - acos($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acot(1/$t) - atan($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asinh(sinh($t)) - $t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acosh(cosh($t)) - $t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(atanh(tanh($t)) - $t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acsch($t) - asinh(1/$t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asech($t) - acosh(1/$t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acoth(1/$t) - atanh($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asinc($t/pi) - asin($t)/$t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asinhc($t/pi) - asinh($t)/$t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(erfc($t) - 1 + erf($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(erf(-$t) + erf($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(angle($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(complex($t) - $t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(complex($t,$t) - ($t+im*$t)))).allocs == 0

  @test @benchmark(@FastGTPSA!($out = sin($t)^2+cos($t)^2 - 1)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = 1/sin($t) - csc($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = 1/cos($t) - sec($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = 1/tan($t) - cot($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sin($t)/cos($t) - tan($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = cos(2*$t) - cos($t)^2 + sin($t)^2)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sec($t)^2 - 1 - tan($t)^2)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sin($t/2) - sqrt((1-cos($t))/2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = cos($t/2) - sqrt((1+cos($t))/2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sqrt($t^2) - abs($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = csc($t)^2 - cot($t)^2 - 1)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = exp(log($t)) - $t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = log(exp($t)) - $t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = log(exp($t)) - exp(log($t)))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = log($t^2) - 2*log($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = 5*log($t) - log($t^5))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t*log(5) - log(5^$t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sinc($t) - sin(pi*$t)/(pi*$t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sinhc($t/pi) - sinh($t)/$t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = exp(im*$t) - cos($t) - im*sin($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = real(exp(im*$t)) - cos($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = imag(exp(im*$t)) - sin($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sinh($t) - (exp($t) - exp(-$t))/2)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = cosh($t) - (exp($t) + exp(-$t))/2)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = tanh($t) - sinh($t)/cosh($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = csch($t) - 1/sinh($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sech($t) - 1/cosh($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = coth($t) - cosh($t)/sinh($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = coth($t) - 1/tanh($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = cosh($t)^2 - sinh($t)^2 - 1)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = 1 - tanh($t)^2 - sech($t)^2)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = coth($t)^2 - 1 - csch($t)^2)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asin(sin($t)) - $t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acos(cos($t)) - $t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = atan(tan($t)) - $t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acsc(1/$t) - asin($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asec(1/$t) - acos($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acot(1/$t) - atan($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asinh(sinh($t)) - $t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acosh(cosh($t)) - $t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = atanh(tanh($t)) - $t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acsch($t) - asinh(1/$t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asech($t) - acosh(1/$t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acoth(1/$t) - atanh($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asinc($t/pi) - asin($t)/$t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asinhc($t/pi) - asinh($t)/$t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = erfc($t) - 1 + erf($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = erf(-$t) + erf($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = angle($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = complex($t) - $t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = complex($t,$t) - ($t+im*$t))).allocs == 0

  t = ComplexTPS64(t)
  t[0] = 0.5+0.5im; t[[1]] = 2+2im; t[[2]] = 3+3im; t[[3]] = 4+4im; t[[4]] = 5+5im; t[[5]] = 6+6im
  @test @benchmark(@FastGTPSA(normTPS(sin($t)^2+cos($t)^2 - 1))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(1/sin($t) - csc($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(1/cos($t) - sec($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(1/tan($t) - cot($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sin($t)/cos($t) - tan($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(cos(2*$t) - cos($t)^2 + sin($t)^2))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sec($t)^2 - 1 - tan($t)^2))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sin($t/2) - sqrt((1-cos($t))/2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(cos($t/2) - sqrt((1+cos($t))/2)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sqrt($t^2) - $t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(csc($t)^2 - cot($t)^2 - 1))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(exp(log($t)) - $t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(log(exp($t)) - $t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(log(exp($t)) - exp(log($t))))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(log($t^2) - 2*log($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(5*log($t) - log($t^5) - 2*pi*im))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS($t*log(5) - log(5^$t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sinc($t/pi) - sin($t)/$t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sinhc($t/pi) - sinh($t)/$t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(exp(im*$t) - cos($t) - im*sin($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sinh($t) - (exp($t) - exp(-$t))/2))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(cosh($t) - (exp($t) + exp(-$t))/2))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(tanh($t) - sinh($t)/cosh($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(csch($t) - 1/sinh($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(sech($t) - 1/cosh($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(coth($t) - cosh($t)/sinh($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(coth($t) - 1/tanh($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(cosh($t)^2 - sinh($t)^2 - 1))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(1 - tanh($t)^2 - sech($t)^2))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(coth($t)^2 - 1 - csch($t)^2))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asin(sin($t)) - $t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acos(cos($t)) - $t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(atan(tan($t)) - $t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acsc($t) - asin(1/$t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asec($t) - acos(1/$t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acot($t) - atan(1/$t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asinh(sinh($t)) - $t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acosh(cosh($t)) - $t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(atanh(tanh($t)) - $t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acsch($t) - asinh(1/$t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asech($t) - acosh(1/$t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(acoth($t) - atanh(1/$t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asinc($t/pi) - asin($t)/$t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(asinhc($t/pi) - asinh($t)/$t))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(erfc($t) - 1 + erf($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(erf(-$t) + erf($t)))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(angle($t) - atan(imag($t),real($t))))).allocs == 0
  @test @benchmark(@FastGTPSA(normTPS(complex($t) - $t))).allocs == 0

  @test @benchmark(@FastGTPSA!($out = sin($t)^2+cos($t)^2 - 1)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = 1/sin($t) - csc($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = 1/cos($t) - sec($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = 1/tan($t) - cot($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sin($t)/cos($t) - tan($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = cos(2*$t) - cos($t)^2 + sin($t)^2)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sec($t)^2 - 1 - tan($t)^2)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sin($t/2) - sqrt((1-cos($t))/2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = cos($t/2) - sqrt((1+cos($t))/2))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sqrt($t^2) - $t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = csc($t)^2 - cot($t)^2 - 1)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = exp(log($t)) - $t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = log(exp($t)) - $t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = log(exp($t)) - exp(log($t)))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = log($t^2) - 2*log($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = 5*log($t) - log($t^5) - 2*pi*im)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = $t*log(5) - log(5^$t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sinc($t/pi) - sin($t)/$t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sinhc($t/pi) - sinh($t)/$t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = exp(im*$t) - cos($t) - im*sin($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sinh($t) - (exp($t) - exp(-$t))/2)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = cosh($t) - (exp($t) + exp(-$t))/2)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = tanh($t) - sinh($t)/cosh($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = csch($t) - 1/sinh($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = sech($t) - 1/cosh($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = coth($t) - cosh($t)/sinh($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = coth($t) - 1/tanh($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = cosh($t)^2 - sinh($t)^2 - 1)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = 1 - tanh($t)^2 - sech($t)^2)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = coth($t)^2 - 1 - csch($t)^2)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asin(sin($t)) - $t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acos(cos($t)) - $t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = atan(tan($t)) - $t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acsc($t) - asin(1/$t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asec($t) - acos(1/$t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acot($t) - atan(1/$t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asinh(sinh($t)) - $t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acosh(cosh($t)) - $t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = atanh(tanh($t)) - $t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acsch($t) - asinh(1/$t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asech($t) - acosh(1/$t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = acoth($t) - atanh(1/$t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asinc($t/pi) - asin($t)/$t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = asinhc($t/pi) - asinh($t)/$t)).allocs == 0
  @test @benchmark(@FastGTPSA!($out = erfc($t) - 1 + erf($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = erf(-$t) + erf($t))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = angle($t) - atan(imag($t),real($t)))).allocs == 0
  @test @benchmark(@FastGTPSA!($out = complex($t) - $t)).allocs == 0

  @test GTPSA.checktemps()
end
end

@testset "Static: FastGTPSA - Broadcasting" begin
  d = Descriptor(1, 5)
  t = TPS{d}()
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

  out = ComplexTPS64{d}()

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
  t = TPS{d}()
  v = 0.5
  t[0] = v
  tol = 1e-14
  t1 = TPS(t)
  t1[0] = 1
  t2 = zero(t1)
  t2[0] = 2
  t3 = zero(t1)
  t3[0] = 3
  out = ComplexTPS64{d}()

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
  t = TPS{d}()
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
  t = TPS{d}()
  t[0] = 0.5; t[[1]] = 2; t[[2]] = 3; t[[3]] = 4; t[[4]] = 5; t[[5]] = 6
  tol = 1e-10
  out = ComplexTPS64{d}()
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
  t = TPS{d}()
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

@testset "Static: FastGTPSA - Block + Block Allocations" begin
  d = Descriptor(3, 7); x = @vars(d); y= rand(3)
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

  y = TPS{d}.(y)
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
  t = TPS{d}()
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
  v = 0.5+0.5im
  tol = 1e-10

  x = zeros(ComplexTPS64,248)

  @FastGTPSA begin
    y1 =  t1 + t2 - t3
    y2 =  t2 + t1 - t3
    y3 =  t1 + 2 - t3
    y4 =  2 + t1 - t3
    y5 =  t3 - t2 - t1
    y6 =  t2 - t3 - -t1
    y7 =  t3 - 2 - t1
    y8 =  2 - t3 - -t1
    y9 =  t2 * t3 - 6
    y10 =  t3 * t2 - 6
    y11 =  t2 * 5 - 10
    y12 =  5 * t2 - 10 * t1
    y13 =  t1 / t2 - 1/2
    y14 =  t2 / t1 - 2
    y15 =  1 / t2 - 1/2
    y16 =  t2 / 3 - 2/3
    y17 =  t2 / t2 - t1
    y18 =  t2 / t2 - 1
    y19 =  t2 ^ t3 - 8
    y20 =  t3 ^ t2 - 9
    y21 =  t2 ^ 3 - 8
    y22 =  t2 ^ (1/2) - sqrt(2)
    y23 =  t2 ^ (1/2) - sqrt(t2)
    y24 =  2 ^ t3 - 8
    y25 =  inv(t3) - 1/t3
    y26 =  inv(t3) - 1/3
    y27 =  ct1 + ct2 - ct3
    y28 =  ct2 + ct1 - ct3
    y29 =  ct1 + (2+2im) - ct3
    y30 =  (2+2im) + ct1 - ct3
    y31 =  ct3 - ct2 - ct1
    y32 =  ct2 - ct3 - -ct1
    y33 =  ct3 - (2+2im) - ct1
    y34 =  (2+2im) - ct3 - -ct1
    y35 =  ct2 * ct3 - (2+2im)*(3+3im)
    y36 =  ct3 * ct2 - (2+2im)*(3+3im)
    y37 =  ct2 * 5 - (10+10im)
    y38 =  5 * ct2 - (10 * ct1)
    y39 =  ct1 / ct2 - (1+im)/(2+2im)
    y40 =  ct2 / ct1 - 2
    y41 =  1 / ct2 - 1/(2+2im)
    y42 =  ct2 / 3 - (2+2im)/3
    y43 =  ct2 / ct2 - 1
    y44 =  ct2 ^ ct3 - (2+2im)^(3+3im)
    y45 =  ct3 ^ ct2 - (3+3im)^(2+2im)
    y46 =  ct2 ^ 3 - (2+2im)^3
    y47 =  ct2 ^ (1/2) - sqrt(2+2im)
    y48 =  ct2 ^ (1/2) - sqrt(ct2)
    y49 =  2 ^ ct3 - 2^(3+3im)
    y50 =  inv(ct3) - 1/ct3
    y51 =  inv(ct3) - 1/(3+3im)
    y52 =  t1 + ct2 - (1 + (2+2im))
    y53 =  ct2 + t1 - (1 + (2+2im))
    y54 =  t1 + (2+2im) - (1 + (2+2im))
    y55 =  (2+2im) + t1 - (1 + (2+2im))
    y56 =  t3 - ct2 - (3 - (2+2im))
    y57 =  ct2 - t3 - ((2+2im) - 3)
    y58 =  t3 - (2+2im) - (3 - (2+2im))
    y59 =  (2+2im) - t3 - ((2+2im) - 3)
    y60 =  t2 * ct3 - 2 * (3+3im)
    y61 =  ct3 * t2 - 2 * (3+3im)
    y62 =  t2 * (3+3im) - 2 * (3+3im)
    y63 =  (3+3im) * t2 - 2 * (3+3im)
    y64 =  t2 / ct3 - 2/(3+3im)
    y65 =  ct3 / t2 - (3+3im)/2
    y66 =  t2 / (3+3im) - 2/(3+3im)
    y67 =  (3+3im) / t2 - (3+3im)/2
    y68 =  t2 ^ ct3 - 2^(3+3im)
    y69 =  ct3 ^ t2 - (3+3im)^2
    y70 =  t2 ^ (3+3im) - 2^(3+3im)
    y71 =  (3+3im)^t2 - (3+3im)^2
    y72 =  sin(t)^2+cos(t)^2 - 1
    y73 =  1/sin(t) - csc(t)
    y74 =  1/cos(t) - sec(t)
    y75 =  1/tan(t) - cot(t)
    y76 =  sin(t)/cos(t) - tan(t)
    y77 =  cos(2*t) - cos(t)^2 + sin(t)^2
    y78 =  sec(t)^2 - 1 - tan(t)^2
    y79 =  sin(t/2) - sqrt((1-cos(t))/2)
    y80 =  cos(t/2) - sqrt((1+cos(t))/2)
    y81 =  sqrt(real(t)^2) - abs(real(t))
    y82 =  csc(t)^2 - cot(t)^2 - 1
    y83 =  exp(log(t)) - t
    y84 =  log(exp(t)) - t
    y85 =  log(exp(t)) - exp(log(t))
    y86 =  log(t^2) - 2*log(t)
    y87 =  5*log(real(t)) - log(real(t)^5)
    y88 =  t*log(5) - log(5^t)
    y89 =  sinc(t) - sin(pi*t)/(pi*t)
    y90 =  sinhc(t/pi) - sinh(t)/t
    y91 =  exp(im*t) - cos(t) - im*sin(t)
    y92 =  real(exp(im*real(t))) - cos(real(t))
    y93 =  imag(exp(im*real(t))) - sin(real(t))
    y94 =  sinh(t) - (exp(t) - exp(-t))/2
    y95 =  cosh(t) - (exp(t) + exp(-t))/2
    y96 =  tanh(t) - sinh(t)/cosh(t)
    y97 =  csch(t) - 1/sinh(t)
    y98 =  sech(t) - 1/cosh(t)
    y99 =  coth(t) - cosh(t)/sinh(t)
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
    y120 = angle(real(t))
    y121 = complex(t) - t
    y122 = complex(real(t),real(t)) - (real(t)+im*real(t))
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

  a1 = @benchmark @FastGTPSA begin
    y1 =  $t1 + $t2 - $t3
    y2 =  $t2 + $t1 - $t3
    y3 =  $t1 + 2 - $t3
    y4 =  2 + $t1 - $t3
    y5 =  $t3 - $t2 - $t1
    y6 =  $t2 - $t3 - -$t1
    y7 =  $t3 - 2 - $t1
    y8 =  2 - $t3 - -$t1
    y9 =  $t2 * $t3 - 6
    y10 =  $t3 * $t2 - 6
    y11 =  $t2 * 5 - 10
    y12 =  5 * $t2 - 10 * $t1
    y13 =  $t1 / $t2 - 1/2
    y14 =  $t2 / $t1 - 2
    y15 =  1 / $t2 - 1/2
    y16 =  $t2 / 3 - 2/3
    y17 =  $t2 / $t2 - $t1
    y18 =  $t2 / $t2 - 1
    y19 =  $t2 ^ $t3 - 8
    y20 =  $t3 ^ $t2 - 9
    y21 =  $t2 ^ 3 - 8
    y22 =  $t2 ^ (1/2) - sqrt(2)
    y23 =  $t2 ^ (1/2) - sqrt($t2)
    y24 =  2 ^ $t3 - 8
    y25 =  inv($t3) - 1/$t3
    y26 =  inv($t3) - 1/3
    y27 =  $ct1 + $ct2 - $ct3
    y28 =  $ct2 + $ct1 - $ct3
    y29 =  $ct1 + (2+2im) - $ct3
    y30 =  (2+2im) + $ct1 - $ct3
    y31 =  $ct3 - $ct2 - $ct1
    y32 =  $ct2 - $ct3 - -$ct1
    y33 =  $ct3 - (2+2im) - $ct1
    y34 =  (2+2im) - $ct3 - -$ct1
    y35 =  $ct2 * $ct3 - (2+2im)*(3+3im)
    y36 =  $ct3 * $ct2 - (2+2im)*(3+3im)
    y37 =  $ct2 * 5 - (10+10im)
    y38 =  5 * $ct2 - (10 * $ct1)
    y39 =  $ct1 / $ct2 - (1+im)/(2+2im)
    y40 =  $ct2 / $ct1 - 2
    y41 =  1 / $ct2 - 1/(2+2im)
    y42 =  $ct2 / 3 - (2+2im)/3
    y43 =  $ct2 / $ct2 - 1
    y44 =  $ct2 ^ $ct3 - (2+2im)^(3+3im)
    y45 =  $ct3 ^ $ct2 - (3+3im)^(2+2im)
    y46 =  $ct2 ^ 3 - (2+2im)^3
    y47 =  $ct2 ^ (1/2) - sqrt(2+2im)
    y48 =  $ct2 ^ (1/2) - sqrt($ct2)
    y49 =  2 ^ $ct3 - 2^(3+3im)
    y50 =  inv($ct3) - 1/$ct3
    y51 =  inv($ct3) - 1/(3+3im)
    y52 =  $t1 + $ct2 - (1 + (2+2im))
    y53 =  $ct2 + $t1 - (1 + (2+2im))
    y54 =  $t1 + (2+2im) - (1 + (2+2im))
    y55 =  (2+2im) + $t1 - (1 + (2+2im))
    y56 =  $t3 - $ct2 - (3 - (2+2im))
    y57 =  $ct2 - $t3 - ((2+2im) - 3)
    y58 =  $t3 - (2+2im) - (3 - (2+2im))
    y59 =  (2+2im) - $t3 - ((2+2im) - 3)
    y60 =  $t2 * $ct3 - 2 * (3+3im)
    y61 =  $ct3 * $t2 - 2 * (3+3im)
    y62 =  $t2 * (3+3im) - 2 * (3+3im)
    y63 =  (3+3im) * $t2 - 2 * (3+3im)
    y64 =  $t2 / $ct3 - 2/(3+3im)
    y65 =  $ct3 / $t2 - (3+3im)/2
    y66 =  $t2 / (3+3im) - 2/(3+3im)
    y67 =  (3+3im) / $t2 - (3+3im)/2
    y68 =  $t2 ^ $ct3 - 2^(3+3im)
    y69 =  $ct3 ^ $t2 - (3+3im)^2
    y70 =  $t2 ^ (3+3im) - 2^(3+3im)
    y71 =  (3+3im)^$t2 - (3+3im)^2
    y72 =  sin($t)^2+cos($t)^2 - 1
    y73 =  1/sin($t) - csc($t)
    y74 =  1/cos($t) - sec($t)
    y75 =  1/tan($t) - cot($t)
    y76 =  sin($t)/cos($t) - tan($t)
    y77 =  cos(2*$t) - cos($t)^2 + sin($t)^2
    y78 =  sec($t)^2 - 1 - tan($t)^2
    y79 =  sin($t/2) - sqrt((1-cos($t))/2)
    y80 =  cos($t/2) - sqrt((1+cos($t))/2)
    y81 =  sqrt(real($t)^2) - abs(real($t))
    y82 =  csc($t)^2 - cot($t)^2 - 1
    y83 =  exp(log($t)) - $t
    y84 =  log(exp($t)) - $t
    y85 =  log(exp($t)) - exp(log($t))
    y86 =  log($t^2) - 2*log($t)
    y87 =  5*log(real($t)) - log(real($t)^5)
    y88 =  $t*log(5) - log(5^$t)
    y89 =  sinc($t) - sin(pi*$t)/(pi*$t)
    y90 =  sinhc($t/pi) - sinh($t)/$t
    y91 =  exp(im*$t) - cos($t) - im*sin($t)
    y92 =  real(exp(im*real($t))) - cos(real($t))
    y93 =  imag(exp(im*real($t))) - sin(real($t))
    y94 =  sinh($t) - (exp($t) - exp(-$t))/2
    y95 =  cosh($t) - (exp($t) + exp(-$t))/2
    y96 =  tanh($t) - sinh($t)/cosh($t)
    y97 =  csch($t) - 1/sinh($t)
    y98 =  sech($t) - 1/cosh($t)
    y99 =  coth($t) - cosh($t)/sinh($t)
    y100 = coth($t) - 1/tanh($t)
    y101 = cosh($t)^2 - sinh($t)^2 - 1
    y102 = 1 - tanh($t)^2 - sech($t)^2
    y103 = coth($t)^2 - 1 - csch($t)^2
    y104 = asin(sin($t)) - $t
    y105 = acos(cos($t)) - $t
    y106 = atan(tan($t)) - $t
    y107 = acsc(1/$t) - asin($t)
    y108 = asec(1/$t) - acos($t)
    y109 = acot(1/$t) - atan($t)
    y110 = asinh(sinh($t)) - $t
    y111 = acosh(cosh($t)) - $t
    y112 = atanh(tanh($t)) - $t
    y113 = acsch($t) - asinh(1/$t)
    y114 = asech($t) - acosh(1/$t)
    y115 = acoth(1/$t) - atanh($t)
    y116 = asinc($t/pi) - asin($t)/$t
    y117 = asinhc($t/pi) - asinh($t)/$t
    y118 = erfc($t) - 1 + erf($t)
    y119 = erf(-$t) + erf($t)
    y120 = angle(real($t))
    y121 = complex($t) - $t
    y122 = complex(real($t),real($t)) - (real($t)+im*real($t))
    y123 = sin($t)^2+cos($t)^2 - 1
    y124 = 1/sin($t) - csc($t)
    y125 = 1/cos($t) - sec($t)
    y126 = 1/tan($t) - cot($t)
    y127 = sin($t)/cos($t) - tan($t)
    y128 = cos(2*$t) - cos($t)^2 + sin($t)^2
    y129 = sec($t)^2 - 1 - tan($t)^2
    y130 = sin($t/2) - sqrt((1-cos($t))/2)
    y131 = cos($t/2) - sqrt((1+cos($t))/2)
    y132 = sqrt($t^2) - $t
    y133 = csc($t)^2 - cot($t)^2 - 1
    y134 = exp(log($t)) - $t
    y135 = log(exp($t)) - $t
    y136 = log(exp($t)) - exp(log($t))
    y137 = log($t^2) - 2*log($t)
    y138 = 5*log($t) - log($t^5) - 2*pi*im
    y139 = $t*log(5) - log(5^$t)
    y140 = sinc($t/pi) - sin($t)/$t
    y141 = sinhc($t/pi) - sinh($t)/$t
    y142 = exp(im*$t) - cos($t) - im*sin($t)
    y143 = sinh($t) - (exp($t) - exp(-$t))/2
    y144 = cosh($t) - (exp($t) + exp(-$t))/2
    y145 = tanh($t) - sinh($t)/cosh($t)
    y146 = csch($t) - 1/sinh($t)
    y147 = sech($t) - 1/cosh($t)
    y148 = coth($t) - cosh($t)/sinh($t)
    y149 = coth($t) - 1/tanh($t)
    y150 = cosh($t)^2 - sinh($t)^2 - 1
    y151 = 1 - tanh($t)^2 - sech($t)^2
    y152 = coth($t)^2 - 1 - csch($t)^2
    y153 = asin(sin($t)) - $t
    y154 = acos(cos($t)) - $t
    y155 = atan(tan($t)) - $t
    y156 = acsc($t) - asin(1/$t)
    y157 = asec($t) - acos(1/$t)
    y158 = acot($t) - atan(1/$t)
    y159 = asinh(sinh($t)) - $t
    y160 = acosh(cosh($t)) - $t
    y161 = atanh(tanh($t)) - $t
    y162 = acsch($t) - asinh(1/$t)
    y163 = asech($t) - acosh(1/$t)
    y164 = acoth($t) - atanh(1/$t)
    y165 = asinc($t/pi) - asin($t)/$t
    y166 = asinhc($t/pi) - asinh($t)/$t
    y167 = erfc($t) - 1 + erf($t)
    y168 = erf(-$t) + erf($t)
    y169 = angle($t) - atan(imag($t),real($t))
    y170 = complex($t) - $t
  end

  @test VERSION < v"1.10" || a1.allocs == 170*2
  a2 = @benchmark @FastGTPSA! begin
    $x[1  ]= $t1 + $t2 - $t3
    $x[2  ]= $t2 + $t1 - $t3
    $x[3  ]= $t1 + 2 - $t3
    $x[4  ]= 2 + $t1 - $t3
    $x[5  ]= $t3 - $t2 - $t1
    $x[6  ]= $t2 - $t3 - -$t1
    $x[7  ]= $t3 - 2 - $t1
    $x[8  ]= 2 - $t3 - -$t1
    $x[9  ]=$t2 * $t3 - 6
    $x[10 ]= $t3 * $t2 - 6
    $x[11 ]= $t2 * 5 - 10
    $x[12 ]= 5 * $t2 - 10 * $t1
    $x[13 ]= $t1 / $t2 - 1/2
    $x[14 ]= $t2 / $t1 - 2
    $x[15 ]= 1 / $t2 - 1/2
    $x[16 ]= $t2 / 3 - 2/3
    $x[17 ]= $t2 / $t2 - $t1
    $x[18 ]= $t2 / $t2 - 1
    $x[19 ]= $t2 ^ $t3 - 8
    $x[20 ]= $t3 ^ $t2 - 9
    $x[21 ]= $t2 ^ 3 - 8
    $x[22 ]= $t2 ^ (1/2) - sqrt(2)
    $x[23 ]= $t2 ^ (1/2) - sqrt($t2)
    $x[24 ]= 2 ^ $t3 - 8
    $x[25 ]= inv($t3) - 1/$t3
    $x[26 ]= inv($t3) - 1/3
    $x[27 ]= $ct1 + $ct2 - $ct3
    $x[28 ]= $ct2 + $ct1 - $ct3
    $x[29 ]= $ct1 + (2+2im) - $ct3
    $x[30 ]= (2+2im) + $ct1 - $ct3
    $x[31 ]= $ct3 - $ct2 - $ct1
    $x[32 ]= $ct2 - $ct3 - -$ct1
    $x[33 ]= $ct3 - (2+2im) - $ct1
    $x[34 ]= (2+2im) - $ct3 - -$ct1
    $x[35 ]= $ct2 * $ct3 - (2+2im)*(3+3im)
    $x[36 ]= $ct3 * $ct2 - (2+2im)*(3+3im)
    $x[37 ]= $ct2 * 5 - (10+10im)
    $x[38 ]= 5 * $ct2 - (10 * $ct1)
    $x[39 ]= $ct1 / $ct2 - (1+im)/(2+2im)
    $x[40 ]= $ct2 / $ct1 - 2
    $x[41 ]= 1 / $ct2 - 1/(2+2im)
    $x[42 ]= $ct2 / 3 - (2+2im)/3
    $x[43 ]= $ct2 / $ct2 - 1
    $x[44 ]= $ct2 ^ $ct3 - (2+2im)^(3+3im)
    $x[45 ]= $ct3 ^ $ct2 - (3+3im)^(2+2im)
    $x[46 ]= $ct2 ^ 3 - (2+2im)^3
    $x[47 ]= $ct2 ^ (1/2) - sqrt(2+2im)
    $x[48 ]= $ct2 ^ (1/2) - sqrt($ct2)
    $x[49 ]= 2 ^ $ct3 - 2^(3+3im)
    $x[50 ]= inv($ct3) - 1/$ct3
    $x[51 ]= inv($ct3) - 1/(3+3im)
    $x[52 ]= $t1 + $ct2 - (1 + (2+2im))
    $x[53 ]= $ct2 + $t1 - (1 + (2+2im))
    $x[54 ]= $t1 + (2+2im) - (1 + (2+2im))
    $x[55 ]= (2+2im) + $t1 - (1 + (2+2im))
    $x[56 ]= $t3 - $ct2 - (3 - (2+2im))
    $x[57 ]= $ct2 - $t3 - ((2+2im) - 3)
    $x[58 ]= $t3 - (2+2im) - (3 - (2+2im))
    $x[59 ]= (2+2im) - $t3 - ((2+2im) - 3)
    $x[60 ]= $t2 * $ct3 - 2 * (3+3im)
    $x[61 ]= $ct3 * $t2 - 2 * (3+3im)
    $x[62 ]= $t2 * (3+3im) - 2 * (3+3im)
    $x[63 ]= (3+3im) * $t2 - 2 * (3+3im)
    $x[64 ]= $t2 / $ct3 - 2/(3+3im)
    $x[65 ]= $ct3 / $t2 - (3+3im)/2
    $x[66 ]= $t2 / (3+3im) - 2/(3+3im)
    $x[67 ]= (3+3im) / $t2 - (3+3im)/2
    $x[68 ]= $t2 ^ $ct3 - 2^(3+3im)
    $x[69 ]= $ct3 ^ $t2 - (3+3im)^2
    $x[70 ]= $t2 ^ (3+3im) - 2^(3+3im)
    $x[71 ]= (3+3im)^$t2 - (3+3im)^2
    $x[72 ]= sin($t)^2+cos($t)^2 - 1
    $x[73 ]= 1/sin($t) - csc($t)
    $x[74 ]= 1/cos($t) - sec($t)
    $x[75 ]= 1/tan($t) - cot($t)
    $x[76 ]= sin($t)/cos($t) - tan($t)
    $x[77 ]= cos(2*$t) - cos($t)^2 + sin($t)^2
    $x[78 ]= sec($t)^2 - 1 - tan($t)^2
    $x[79 ]= sin($t/2) - sqrt((1-cos($t))/2)
    $x[80 ]= cos($t/2) - sqrt((1+cos($t))/2)
    $x[81 ]= sqrt(real($t)^2) - abs(real($t))
    $x[82 ]= csc($t)^2 - cot($t)^2 - 1
    $x[83 ]= exp(log($t)) - $t
    $x[84 ]= log(exp($t)) - $t
    $x[85 ]= log(exp($t)) - exp(log($t))
    $x[86 ]= log($t^2) - 2*log($t)
    $x[87 ]= 5*log(real($t)) - log(real($t)^5)
    $x[88 ]= $t*log(5) - log(5^$t)
    $x[89 ]= sinc($t) - sin(pi*$t)/(pi*$t)
    $x[90 ]= sinhc($t/pi) - sinh($t)/$t
    $x[91 ]= exp(im*$t) - cos($t) - im*sin($t)
    $x[92 ]= real(exp(im*real($t))) - cos(real($t))
    $x[93 ]= imag(exp(im*real($t))) - sin(real($t))
    $x[94 ]= sinh($t) - (exp($t) - exp(-$t))/2
    $x[95 ]= cosh($t) - (exp($t) + exp(-$t))/2
    $x[96 ]= tanh($t) - sinh($t)/cosh($t)
    $x[97 ]= csch($t) - 1/sinh($t)
    $x[98 ]= sech($t) - 1/cosh($t)
    $x[99 ]= coth($t) - cosh($t)/sinh($t)
    $x[100] = coth($t) - 1/tanh($t)
    $x[101] = cosh($t)^2 - sinh($t)^2 - 1
    $x[102] = 1 - tanh($t)^2 - sech($t)^2
    $x[103] = coth($t)^2 - 1 - csch($t)^2
    $x[104] = asin(sin($t)) - $t
    $x[105] = acos(cos($t)) - $t
    $x[106] = atan(tan($t)) - $t
    $x[107] = acsc(1/$t) - asin($t)
    $x[108] = asec(1/$t) - acos($t)
    $x[109] = acot(1/$t) - atan($t)
    $x[110] = asinh(sinh($t)) - $t
    $x[111] = acosh(cosh($t)) - $t
    $x[112] = atanh(tanh($t)) - $t
    $x[113] = acsch($t) - asinh(1/$t)
    $x[114] = asech($t) - acosh(1/$t)
    $x[115] = acoth(1/$t) - atanh($t)
    $x[116] = asinc($t/pi) - asin($t)/$t
    $x[117] = asinhc($t/pi) - asinh($t)/$t
    $x[118] = erfc($t) - 1 + erf($t)
    $x[119] = erf(-$t) + erf($t)
    $x[120] = angle(real($t))
    $x[121] = complex($t) - $t
    $x[122] = complex(real($t),real($t)) - (real($t)+im*real($t))
    $x[123] = sin($t)^2+cos($t)^2 - 1
    $x[124] = 1/sin($t) - csc($t)
    $x[125] = 1/cos($t) - sec($t)
    $x[126] = 1/tan($t) - cot($t)
    $x[127] = sin($t)/cos($t) - tan($t)
    $x[128] = cos(2*$t) - cos($t)^2 + sin($t)^2
    $x[129] = sec($t)^2 - 1 - tan($t)^2
    $x[130] = sin($t/2) - sqrt((1-cos($t))/2)
    $x[131] = cos($t/2) - sqrt((1+cos($t))/2)
    $x[132] = sqrt($t^2) - $t
    $x[133] = csc($t)^2 - cot($t)^2 - 1
    $x[134] = exp(log($t)) - $t
    $x[135] = log(exp($t)) - $t
    $x[136] = log(exp($t)) - exp(log($t))
    $x[137] = log($t^2) - 2*log($t)
    $x[138] = 5*log($t) - log($t^5) - 2*pi*im
    $x[139] = $t*log(5) - log(5^$t)
    $x[140] = sinc($t/pi) - sin($t)/$t
    $x[141] = sinhc($t/pi) - sinh($t)/$t
    $x[142] = exp(im*$t) - cos($t) - im*sin($t)
    $x[143] = sinh($t) - (exp($t) - exp(-$t))/2
    $x[144] = cosh($t) - (exp($t) + exp(-$t))/2
    $x[145] = tanh($t) - sinh($t)/cosh($t)
    $x[146] = csch($t) - 1/sinh($t)
    $x[147] = sech($t) - 1/cosh($t)
    $x[148] = coth($t) - cosh($t)/sinh($t)
    $x[149] = coth($t) - 1/tanh($t)
    $x[150] = cosh($t)^2 - sinh($t)^2 - 1
    $x[151] = 1 - tanh($t)^2 - sech($t)^2
    $x[152] = coth($t)^2 - 1 - csch($t)^2
    $x[153] = asin(sin($t)) - $t
    $x[154] = acos(cos($t)) - $t
    $x[155] = atan(tan($t)) - $t
    $x[156] = acsc($t) - asin(1/$t)
    $x[157] = asec($t) - acos(1/$t)
    $x[158] = acot($t) - atan(1/$t)
    $x[159] = asinh(sinh($t)) - $t
    $x[160] = acosh(cosh($t)) - $t
    $x[161] = atanh(tanh($t)) - $t
    $x[162] = acsch($t) - asinh(1/$t)
    $x[163] = asech($t) - acosh(1/$t)
    $x[164] = acoth($t) - atanh(1/$t)
    $x[165] = asinc($t/pi) - asin($t)/$t
    $x[166] = asinhc($t/pi) - asinh($t)/$t
    $x[167] = erfc($t) - 1 + erf($t)
    $x[168] = erf(-$t) + erf($t)
    $x[169] = angle($t) - atan(imag($t),real($t))
    $x[170] = complex($t) - $t
  end
  @test VERSION < v"1.10" || a2.allocs == 0

  t = ComplexTPS64{d}()
  t[0] = 0.5+0.5im
  v = 0.5+0.5im
  @FastGTPSA begin
    y171 = sqrt(t) - sqrt(v)
    y172 = exp(t) - exp(v)
    y173 = log(t) - log(v)
    y174 = sin(t) - sin(v)
    y175 = cos(t) - cos(v)
    y176 = tan(t) - tan(v)
    y177 = csc(t) - csc(v)
    y178 = sec(t) - sec(v)
    y179 = cot(t) - cot(v)
    y180 = sinc(t) - sinc(v)
    y181 = sinh(t) - sinh(v)
    y182 = cosh(t) - cosh(v)
    y183 = tanh(t) - tanh(v)
    y184 = csch(t) - csch(v)
    y185 = sech(t) - sech(v)
    y186 = coth(t) - coth(v)
    y187 = asin(t) - asin(v)
    y188 = acos(t) - acos(v)
    y189 = atan(t) - atan(v)
    y190 = acsc(t) - acsc(v)
    y191 = asec(t) - asec(v)
    y192 = acot(t) - acot(v)
    y193 = asinh(t) - asinh(v)
    y194 = acosh(t) - acosh(v)
    y195 = atanh(t) - atanh(v)
    y196 = acsch(t) - acsch(v)
    y197 = asech(t) - asech(v)
    y198 = acoth(t) - acoth(v)
    y199 = asinc(t/pi) - asin(v)/v
    y200 = asinhc(t/pi) - asinh(v)/v
    y201 = zero(t) - zero(v)
    y202 = real(t) - real(v)
    y203 = imag(t) - imag(v)
    y204 = conj(t) - conj(v)
    y205 = sinhc(t/pi) - sinh(v)/v
    y206 = erf(t) - erf(v)
    y207 = erfc(t) - erfc(v)
    y208 = -im*erf(t*im) - erfi(v)
    y219 = angle(t2+im*t3) - angle(2+3im)
    y220 = angle(t2-im*t3) - angle(2-3im)
    y221 = angle(-t2-im*t3) - angle(-2-3im)
    y222 = angle(-t2+im*t3) - angle(-2+3im)
    y223 = angle(ct2) - angle(2+2im)
    y224 = angle(-ct2) - angle(-2-2im)
    y225 = complex(ct3) - complex(3+3im)
    y226 = polar(ct2) - (abs(2+2im)+im*angle(2+2im))
    y227 = polar(-ct1) - (abs(-1-im)+im*angle(-1-im))
    y228 = rect(ct2) - (2*cos(2) + im*2*sin(2))
    y229 = rect(-ct1) - (-1*cos(-1) + im*-1*sin(-1))
    y248 = abs(-t) - abs(-v) 
  end
  a1 = @benchmark @FastGTPSA begin
    y171 = sqrt($t) - sqrt($v)
    y172 = exp($t) - exp($v)
    y173 = log($t) - log($v)
    y174 = sin($t) - sin($v)
    y175 = cos($t) - cos($v)
    y176 = tan($t) - tan($v)
    y177 = csc($t) - csc($v)
    y178 = sec($t) - sec($v)
    y179 = cot($t) - cot($v)
    y180 = sinc($t) - sinc($v)
    y181 = sinh($t) - sinh($v)
    y182 = cosh($t) - cosh($v)
    y183 = tanh($t) - tanh($v)
    y184 = csch($t) - csch($v)
    y185 = sech($t) - sech($v)
    y186 = coth($t) - coth($v)
    y187 = asin($t) - asin($v)
    y188 = acos($t) - acos($v)
    y189 = atan($t) - atan($v)
    y190 = acsc($t) - acsc($v)
    y191 = asec($t) - asec($v)
    y192 = acot($t) - acot($v)
    y193 = asinh($t) - asinh($v)
    y194 = acosh($t) - acosh($v)
    y195 = atanh($t) - atanh($v)
    y196 = acsch($t) - acsch($v)
    y197 = asech($t) - asech($v)
    y198 = acoth($t) - acoth($v)
    y199 = asinc($t/pi) - asin($v)/$v
    y200 = asinhc($t/pi) - asinh($v)/$v
    y201 = zero($t) - zero($v)
    y202 = real($t) - real($v)
    y203 = imag($t) - imag($v)
    y204 = conj($t) - conj($v)
    y205 = sinhc($t/pi) - sinh($v)/$v
    y206 = erf($t) - erf($v)
    y207 = erfc($t) - erfc($v)
    y208 = -im*erf($t*im) - erfi($v)
    y219 = angle($t2+im*$t3) - angle(2+3im)
    y220 = angle($t2-im*$t3) - angle(2-3im)
    y221 = angle(-$t2-im*$t3) - angle(-2-3im)
    y222 = angle(-$t2+im*$t3) - angle(-2+3im)
    y223 = angle($ct2) - angle(2+2im)
    y224 = angle(-$ct2) - angle(-2-2im)
    y225 = complex($ct3) - complex(3+3im)
    y226 = polar($ct2) - (abs(2+2im)+im*angle(2+2im))
    y227 = polar(-$ct1) - (abs(-1-im)+im*angle(-1-im))
    y228 = rect($ct2) - (2*cos(2) + im*2*sin(2))
    y229 = rect(-$ct1) - (-1*cos(-1) + im*-1*sin(-1))
    y248 = abs(-$t) - abs(-$v) 
  end
  @FastGTPSA begin
    y209 = hypot(ct2,ct3) - hypot(2+2im,3+3im)
    y210 = hypot(2+2im,ct3) - hypot(2+2im,3+3im)
    y211 = hypot(ct2,3+3im) - hypot(2+2im,3+3im)
    y212 = hypot(ct1,ct2,ct3) - hypot(1+1im,2+2im,3+3im)
    y213 = hypot(1+1im, ct2, ct3) - hypot(1+1im,2+2im,3+3im)
    y214 = hypot(ct1, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)
    y215 = hypot(ct1, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)
    y216 = hypot(1+1im, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)
    y217 = hypot(1+1im, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)
    y218 = hypot(ct1, 2+2im, 3+3im) - hypot(1+1im,2+2im,3+3im)
    y230 = hypot(ct1, ct2, t3) - hypot(1+1im,2+2im,3)
    y231 = hypot(ct1, t2, ct3) - hypot(1+1im,2,3+3im)
    y232 = hypot(t1, ct2, ct3) - hypot(1,2+2im,3+3im)
    y233 = hypot(ct1, t2, t3) - hypot(1+1im,2,3)
    y234 = hypot(t1, ct2, t3) - hypot(1,2+2im,3)
    y235 = hypot(t1, t2, ct3) - hypot(1,2,3+3im)
    y236 = hypot(ct1,t2,3+3im) - hypot(1+1im,2,3+3im)
    y237 = hypot(t1, ct2, 3+3im) - hypot(1,2+2im,3+3im)
    y238 = hypot(ct1,2+2im,t3) - hypot(1+1im,2+2im,3)
    y239 = hypot(t1,2+2im,ct3) - hypot(1,2+2im,3+3im)
    y240 = hypot(1+1im,ct2,t3) - hypot(1+1im,2+2im,3)
    y241 = hypot(1+1im, t2, ct3) - hypot(1+1im,2,3+3im)
    y242 = hypot(t1,t2,3+3im) - hypot(1,2,3+3im)
    y243 = hypot(t1,2+2im,t3) - hypot(1,2+2im,3)
    y244 = hypot(1+1im,t2,t3) - hypot(1+1im,2,3)
    y245 = hypot(t1,2,3+3im) - hypot(1,2,3+3im)
    y246 = hypot(1,t2,3+3im) - hypot(1,2,3+3im)
    y247 = hypot(1+1im,2,t3) - hypot(1+1im,2,3)
  end
  @test VERSION < v"1.10" || a1.allocs == 50*2

  a2 = @benchmark @FastGTPSA! begin
    $x[171] = sqrt($t) - sqrt($v)
    $x[172] = exp($t) - exp($v)
    $x[173] = log($t) - log($v)
    $x[174] = sin($t) - sin($v)
    $x[175] = cos($t) - cos($v)
    $x[176] = tan($t) - tan($v)
    $x[177] = csc($t) - csc($v)
    $x[178] = sec($t) - sec($v)
    $x[179] = cot($t) - cot($v)
    $x[180] = sinc($t) - sinc($v)
    $x[181] = sinh($t) - sinh($v)
    $x[182] = cosh($t) - cosh($v)
    $x[183] = tanh($t) - tanh($v)
    $x[184] = csch($t) - csch($v)
    $x[185] = sech($t) - sech($v)
    $x[186] = coth($t) - coth($v)
    $x[187] = asin($t) - asin($v)
    $x[188] = acos($t) - acos($v)
    $x[189] = atan($t) - atan($v)
    $x[190] = acsc($t) - acsc($v)
    $x[191] = asec($t) - asec($v)
    $x[192] = acot($t) - acot($v)
    $x[193] = asinh($t) - asinh($v)
    $x[194] = acosh($t) - acosh($v)
    $x[195] = atanh($t) - atanh($v)
    $x[196] = acsch($t) - acsch($v)
    $x[197] = asech($t) - asech($v)
    $x[198] = acoth($t) - acoth($v)
    $x[199] = asinc($t/pi) - asin($v)/$v
    $x[200] = asinhc($t/pi) - asinh($v)/$v
    $x[201] = zero($t) - zero($v)
    $x[202] = real($t) - real($v)
    $x[203] = imag($t) - imag($v)
    $x[204] = conj($t) - conj($v)
    $x[205] = sinhc($t/pi) - sinh($v)/$v
    $x[206] = erf($t) - erf($v)
    $x[207] = erfc($t) - erfc($v)
    $x[208] = -im*erf($t*im) - erfi($v)
    $x[219] = angle($t2+im*$t3) - angle(2+3im)
    $x[220] = angle($t2-im*$t3) - angle(2-3im)
    $x[221] = angle(-$t2-im*$t3) - angle(-2-3im)
    $x[222] = angle(-$t2+im*$t3) - angle(-2+3im)
    $x[223] = angle($ct2) - angle(2+2im)
    $x[224] = angle(-$ct2) - angle(-2-2im)
    $x[225] = complex($ct3) - complex(3+3im)
    $x[226] = polar($ct2) - (abs(2+2im)+im*angle(2+2im))
    $x[227] = polar(-$ct1) - (abs(-1-im)+im*angle(-1-im))
    $x[228] = rect($ct2) - (2*cos(2) + im*2*sin(2))
    $x[229] = rect(-$ct1) - (-1*cos(-1) + im*-1*sin(-1))
    $x[248] = abs(-$t) - abs(-$v) 
  end
  @FastGTPSA! begin
    x[209] = hypot(ct2,ct3) - hypot(2+2im,3+3im)
    x[210] = hypot(2+2im,ct3) - hypot(2+2im,3+3im)
    x[211] = hypot(ct2,3+3im) - hypot(2+2im,3+3im)
    x[212] = hypot(ct1,ct2,ct3) - hypot(1+1im,2+2im,3+3im)
    x[213] = hypot(1+1im, ct2, ct3) - hypot(1+1im,2+2im,3+3im)
    x[214] = hypot(ct1, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)
    x[215] = hypot(ct1, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)
    x[216] = hypot(1+1im, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)
    x[217] = hypot(1+1im, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)
    x[218] = hypot(ct1, 2+2im, 3+3im) - hypot(1+1im,2+2im,3+3im)
    x[230] = hypot(ct1, ct2, t3) - hypot(1+1im,2+2im,3)
    x[231] = hypot(ct1, t2, ct3) - hypot(1+1im,2,3+3im)
    x[232] = hypot(t1, ct2, ct3) - hypot(1,2+2im,3+3im)
    x[233] = hypot(ct1, t2, t3) - hypot(1+1im,2,3)
    x[234] = hypot(t1, ct2, t3) - hypot(1,2+2im,3)
    x[235] = hypot(t1, t2, ct3) - hypot(1,2,3+3im)
    x[236] = hypot(ct1,t2,3+3im) - hypot(1+1im,2,3+3im)
    x[237] = hypot(t1, ct2, 3+3im) - hypot(1,2+2im,3+3im)
    x[238] = hypot(ct1,2+2im,t3) - hypot(1+1im,2+2im,3)
    x[239] = hypot(t1,2+2im,ct3) - hypot(1,2+2im,3+3im)
    x[240] = hypot(1+1im,ct2,t3) - hypot(1+1im,2+2im,3)
    x[241] = hypot(1+1im, t2, ct3) - hypot(1+1im,2,3+3im)
    x[242] = hypot(t1,t2,3+3im) - hypot(1,2,3+3im)
    x[243] = hypot(t1,2+2im,t3) - hypot(1,2+2im,3)
    x[244] = hypot(1+1im,t2,t3) - hypot(1+1im,2,3)
    x[245] = hypot(t1,2,3+3im) - hypot(1,2,3+3im)
    x[246] = hypot(1,t2,3+3im) - hypot(1,2,3+3im)
    x[247] = hypot(1+1im,2,t3) - hypot(1+1im,2,3)
  end
  @test VERSION < v"1.10" || a2.allocs == 0

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
  @test normTPS(y72 ) < tol
  @test normTPS(y73 ) < tol
  @test normTPS(y74 ) < tol
  @test normTPS(y75 ) < tol
  @test normTPS(y76 ) < tol
  @test normTPS(y77 ) < tol
  @test normTPS(y78 ) < tol
  @test normTPS(y79 ) < tol
  @test normTPS(y80 ) < tol
  @test normTPS(y81 ) < tol
  @test normTPS(y82 ) < tol
  @test normTPS(y83 ) < tol
  @test normTPS(y84 ) < tol
  @test normTPS(y85 ) < tol
  @test normTPS(y86 ) < tol
  @test normTPS(y87 ) < tol
  @test normTPS(y88 ) < tol
  @test normTPS(y89 ) < tol
  @test normTPS(y90 ) < tol
  @test normTPS(y91 ) < tol
  @test normTPS(y92 ) < tol
  @test normTPS(y93 ) < tol
  @test normTPS(y94 ) < tol
  @test normTPS(y95 ) < tol
  @test normTPS(y96 ) < tol
  @test normTPS(y97 ) < tol
  @test normTPS(y98 ) < tol
  @test normTPS(y99 ) < tol
  @test normTPS(y100) < tol
  @test normTPS(y101) < tol
  @test normTPS(y102) < tol
  @test normTPS(y103) < tol
  @test normTPS(y104) < tol
  @test normTPS(y105) < tol
  @test normTPS(y106) < tol
  @test normTPS(y107) < tol
  @test normTPS(y108) < tol
  @test normTPS(y109) < tol
  @test normTPS(y110) < tol
  @test normTPS(y111) < tol
  @test normTPS(y112) < tol
  @test normTPS(y113) < tol
  @test normTPS(y114) < tol
  @test normTPS(y115) < tol
  @test normTPS(y116) < tol
  @test normTPS(y117) < tol
  @test normTPS(y118) < tol
  @test normTPS(y119) < tol
  @test normTPS(y120) < tol
  @test normTPS(y121) < tol
  @test normTPS(y122) < tol
  @test normTPS(y123) < tol
  @test normTPS(y124) < tol
  @test normTPS(y125) < tol
  @test normTPS(y126) < tol
  @test normTPS(y127) < tol
  @test normTPS(y128) < tol
  @test normTPS(y129) < tol
  @test normTPS(y130) < tol
  @test normTPS(y131) < tol
  @test normTPS(y132) < tol
  @test normTPS(y133) < tol
  @test normTPS(y134) < tol
  @test normTPS(y135) < tol
  @test normTPS(y136) < tol
  @test normTPS(y137) < tol
  @test normTPS(y138) < tol
  @test normTPS(y139) < tol
  @test normTPS(y140) < tol
  @test normTPS(y141) < tol
  @test normTPS(y142) < tol
  @test normTPS(y143) < tol
  @test normTPS(y144) < tol
  @test normTPS(y145) < tol
  @test normTPS(y146) < tol
  @test normTPS(y147) < tol
  @test normTPS(y148) < tol
  @test normTPS(y149) < tol
  @test normTPS(y150) < tol
  @test normTPS(y151) < tol
  @test normTPS(y152) < tol
  @test normTPS(y153) < tol
  @test normTPS(y154) < tol
  @test normTPS(y155) < tol
  @test normTPS(y156) < tol
  @test normTPS(y157) < tol
  @test normTPS(y158) < tol
  @test normTPS(y159) < tol
  @test normTPS(y160) < tol
  @test normTPS(y161) < tol
  @test normTPS(y162) < tol
  @test normTPS(y163) < tol
  @test normTPS(y164) < tol
  @test normTPS(y165) < tol
  @test normTPS(y166) < tol
  @test normTPS(y167) < tol
  @test normTPS(y168) < tol
  @test normTPS(y169) < tol
  @test normTPS(y170) < tol
  @test normTPS(y171) < tol
  @test normTPS(y172) < tol
  @test normTPS(y173) < tol
  @test normTPS(y174) < tol
  @test normTPS(y175) < tol
  @test normTPS(y176) < tol
  @test normTPS(y177) < tol
  @test normTPS(y178) < tol
  @test normTPS(y179) < tol
  @test normTPS(y180) < tol
  @test normTPS(y181) < tol
  @test normTPS(y182) < tol
  @test normTPS(y183) < tol
  @test normTPS(y184) < tol
  @test normTPS(y185) < tol
  @test normTPS(y186) < tol
  @test normTPS(y187) < tol
  @test normTPS(y188) < tol
  @test normTPS(y189) < tol
  @test normTPS(y190) < tol
  @test normTPS(y191) < tol
  @test normTPS(y192) < tol
  @test normTPS(y193) < tol
  @test normTPS(y194) < tol
  @test normTPS(y195) < tol
  @test normTPS(y196) < tol
  @test normTPS(y197) < tol
  @test normTPS(y198) < tol
  @test normTPS(y199) < tol
  @test normTPS(y200) < tol
  @test normTPS(y201) < tol
  @test normTPS(y202) < tol
  @test normTPS(y203) < tol
  @test normTPS(y204) < tol
  @test normTPS(y205) < tol
  @test normTPS(y206) < tol
  @test normTPS(y207) < tol
  @test normTPS(y208) < tol
  @test normTPS(y209) < tol
  @test normTPS(y210) < tol
  @test normTPS(y211) < tol
  @test normTPS(y212) < tol
  @test normTPS(y213) < tol
  @test normTPS(y214) < tol
  @test normTPS(y215) < tol
  @test normTPS(y216) < tol
  @test normTPS(y217) < tol
  @test normTPS(y218) < tol
  @test normTPS(y219) < tol
  @test normTPS(y220) < tol
  @test normTPS(y221) < tol
  @test normTPS(y222) < tol
  @test normTPS(y223) < tol
  @test normTPS(y224) < tol
  @test normTPS(y225) < tol
  @test normTPS(y226) < tol
  @test normTPS(y227) < tol
  @test normTPS(y228) < tol
  @test normTPS(y229) < tol
  @test normTPS(y230) < tol
  @test normTPS(y231) < tol
  @test normTPS(y232) < tol
  @test normTPS(y233) < tol
  @test normTPS(y234) < tol
  @test normTPS(y235) < tol
  @test normTPS(y236) < tol
  @test normTPS(y237) < tol
  @test normTPS(y238) < tol
  @test normTPS(y239) < tol
  @test normTPS(y240) < tol
  @test normTPS(y241) < tol
  @test normTPS(y242) < tol
  @test normTPS(y243) < tol
  @test normTPS(y244) < tol
  @test normTPS(y245) < tol
  @test normTPS(y246) < tol
  @test normTPS(y247) < tol
  @test normTPS(y248) < tol


  @test normTPS(x[1  ]) < tol
  @test normTPS(x[2  ]) < tol
  @test normTPS(x[3  ]) < tol
  @test normTPS(x[4  ]) < tol
  @test normTPS(x[5  ]) < tol
  @test normTPS(x[6  ]) < tol
  @test normTPS(x[7  ]) < tol
  @test normTPS(x[8  ]) < tol
  @test normTPS(x[9  ]) < tol
  @test normTPS(x[10 ]) < tol
  @test normTPS(x[11 ]) < tol
  @test normTPS(x[12 ]) < tol
  @test normTPS(x[13 ]) < tol
  @test normTPS(x[14 ]) < tol
  @test normTPS(x[15 ]) < tol
  @test normTPS(x[16 ]) < tol
  @test normTPS(x[17 ]) < tol
  @test normTPS(x[18 ]) < tol
  @test normTPS(x[19 ]) < tol
  @test normTPS(x[20 ]) < tol
  @test normTPS(x[21 ]) < tol
  @test normTPS(x[22 ]) < tol
  @test normTPS(x[23 ]) < tol
  @test normTPS(x[24 ]) < tol
  @test normTPS(x[25 ]) < tol
  @test normTPS(x[26 ]) < tol
  @test normTPS(x[27 ]) < tol
  @test normTPS(x[28 ]) < tol
  @test normTPS(x[29 ]) < tol
  @test normTPS(x[30 ]) < tol
  @test normTPS(x[31 ]) < tol
  @test normTPS(x[32 ]) < tol
  @test normTPS(x[33 ]) < tol
  @test normTPS(x[34 ]) < tol
  @test normTPS(x[35 ]) < tol
  @test normTPS(x[36 ]) < tol
  @test normTPS(x[37 ]) < tol
  @test normTPS(x[38 ]) < tol
  @test normTPS(x[39 ]) < tol
  @test normTPS(x[40 ]) < tol
  @test normTPS(x[41 ]) < tol
  @test normTPS(x[42 ]) < tol
  @test normTPS(x[43 ]) < tol
  @test normTPS(x[44 ]) < tol
  @test normTPS(x[45 ]) < tol
  @test normTPS(x[46 ]) < tol
  @test normTPS(x[47 ]) < tol
  @test normTPS(x[48 ]) < tol
  @test normTPS(x[49 ]) < tol
  @test normTPS(x[50 ]) < tol
  @test normTPS(x[51 ]) < tol
  @test normTPS(x[52 ]) < tol
  @test normTPS(x[53 ]) < tol
  @test normTPS(x[54 ]) < tol
  @test normTPS(x[55 ]) < tol
  @test normTPS(x[56 ]) < tol
  @test normTPS(x[57 ]) < tol
  @test normTPS(x[58 ]) < tol
  @test normTPS(x[59 ]) < tol
  @test normTPS(x[60 ]) < tol
  @test normTPS(x[61 ]) < tol
  @test normTPS(x[62 ]) < tol
  @test normTPS(x[63 ]) < tol
  @test normTPS(x[64 ]) < tol
  @test normTPS(x[65 ]) < tol
  @test normTPS(x[66 ]) < tol
  @test normTPS(x[67 ]) < tol
  @test normTPS(x[68 ]) < tol
  @test normTPS(x[69 ]) < tol
  @test normTPS(x[70 ]) < tol
  @test normTPS(x[71 ]) < tol
  @test normTPS(x[72 ]) < tol
  @test normTPS(x[73 ]) < tol
  @test normTPS(x[74 ]) < tol
  @test normTPS(x[75 ]) < tol
  @test normTPS(x[76 ]) < tol
  @test normTPS(x[77 ]) < tol
  @test normTPS(x[78 ]) < tol
  @test normTPS(x[79 ]) < tol
  @test normTPS(x[80 ]) < tol
  @test normTPS(x[81 ]) < tol
  @test normTPS(x[82 ]) < tol
  @test normTPS(x[83 ]) < tol
  @test normTPS(x[84 ]) < tol
  @test normTPS(x[85 ]) < tol
  @test normTPS(x[86 ]) < tol
  @test normTPS(x[87 ]) < tol
  @test normTPS(x[88 ]) < tol
  @test normTPS(x[89 ]) < tol
  @test normTPS(x[90 ]) < tol
  @test normTPS(x[91 ]) < tol
  @test normTPS(x[92 ]) < tol
  @test normTPS(x[93 ]) < tol
  @test normTPS(x[94 ]) < tol
  @test normTPS(x[95 ]) < tol
  @test normTPS(x[96 ]) < tol
  @test normTPS(x[97 ]) < tol
  @test normTPS(x[98 ]) < tol
  @test normTPS(x[99 ]) < tol
  @test normTPS(x[100]) < tol
  @test normTPS(x[101]) < tol
  @test normTPS(x[102]) < tol
  @test normTPS(x[103]) < tol
  @test normTPS(x[104]) < tol
  @test normTPS(x[105]) < tol
  @test normTPS(x[106]) < tol
  @test normTPS(x[107]) < tol
  @test normTPS(x[108]) < tol
  @test normTPS(x[109]) < tol
  @test normTPS(x[110]) < tol
  @test normTPS(x[111]) < tol
  @test normTPS(x[112]) < tol
  @test normTPS(x[113]) < tol
  @test normTPS(x[114]) < tol
  @test normTPS(x[115]) < tol
  @test normTPS(x[116]) < tol
  @test normTPS(x[117]) < tol
  @test normTPS(x[118]) < tol
  @test normTPS(x[119]) < tol
  @test normTPS(x[120]) < tol
  @test normTPS(x[121]) < tol
  @test normTPS(x[122]) < tol
  @test normTPS(x[123]) < tol
  @test normTPS(x[124]) < tol
  @test normTPS(x[125]) < tol
  @test normTPS(x[126]) < tol
  @test normTPS(x[127]) < tol
  @test normTPS(x[128]) < tol
  @test normTPS(x[129]) < tol
  @test normTPS(x[130]) < tol
  @test normTPS(x[131]) < tol
  @test normTPS(x[132]) < tol
  @test normTPS(x[133]) < tol
  @test normTPS(x[134]) < tol
  @test normTPS(x[135]) < tol
  @test normTPS(x[136]) < tol
  @test normTPS(x[137]) < tol
  @test normTPS(x[138]) < tol
  @test normTPS(x[139]) < tol
  @test normTPS(x[140]) < tol
  @test normTPS(x[141]) < tol
  @test normTPS(x[142]) < tol
  @test normTPS(x[143]) < tol
  @test normTPS(x[144]) < tol
  @test normTPS(x[145]) < tol
  @test normTPS(x[146]) < tol
  @test normTPS(x[147]) < tol
  @test normTPS(x[148]) < tol
  @test normTPS(x[149]) < tol
  @test normTPS(x[150]) < tol
  @test normTPS(x[151]) < tol
  @test normTPS(x[152]) < tol
  @test normTPS(x[153]) < tol
  @test normTPS(x[154]) < tol
  @test normTPS(x[155]) < tol
  @test normTPS(x[156]) < tol
  @test normTPS(x[157]) < tol
  @test normTPS(x[158]) < tol
  @test normTPS(x[159]) < tol
  @test normTPS(x[160]) < tol
  @test normTPS(x[161]) < tol
  @test normTPS(x[162]) < tol
  @test normTPS(x[163]) < tol
  @test normTPS(x[164]) < tol
  @test normTPS(x[165]) < tol
  @test normTPS(x[166]) < tol
  @test normTPS(x[167]) < tol
  @test normTPS(x[168]) < tol
  @test normTPS(x[169]) < tol
  @test normTPS(x[170]) < tol
  @test normTPS(x[171]) < tol
  @test normTPS(x[172]) < tol
  @test normTPS(x[173]) < tol
  @test normTPS(x[174]) < tol
  @test normTPS(x[175]) < tol
  @test normTPS(x[176]) < tol
  @test normTPS(x[177]) < tol
  @test normTPS(x[178]) < tol
  @test normTPS(x[179]) < tol
  @test normTPS(x[180]) < tol
  @test normTPS(x[181]) < tol
  @test normTPS(x[182]) < tol
  @test normTPS(x[183]) < tol
  @test normTPS(x[184]) < tol
  @test normTPS(x[185]) < tol
  @test normTPS(x[186]) < tol
  @test normTPS(x[187]) < tol
  @test normTPS(x[188]) < tol
  @test normTPS(x[189]) < tol
  @test normTPS(x[190]) < tol
  @test normTPS(x[191]) < tol
  @test normTPS(x[192]) < tol
  @test normTPS(x[193]) < tol
  @test normTPS(x[194]) < tol
  @test normTPS(x[195]) < tol
  @test normTPS(x[196]) < tol
  @test normTPS(x[197]) < tol
  @test normTPS(x[198]) < tol
  @test normTPS(x[199]) < tol
  @test normTPS(x[200]) < tol
  @test normTPS(x[201]) < tol
  @test normTPS(x[202]) < tol
  @test normTPS(x[203]) < tol
  @test normTPS(x[204]) < tol
  @test normTPS(x[205]) < tol
  @test normTPS(x[206]) < tol
  @test normTPS(x[207]) < tol
  @test normTPS(x[208]) < tol
  @test normTPS(x[209]) < tol
  @test normTPS(x[210]) < tol
  @test normTPS(x[211]) < tol
  @test normTPS(x[212]) < tol
  @test normTPS(x[213]) < tol
  @test normTPS(x[214]) < tol
  @test normTPS(x[215]) < tol
  @test normTPS(x[216]) < tol
  @test normTPS(x[217]) < tol
  @test normTPS(x[218]) < tol
  @test normTPS(x[219]) < tol
  @test normTPS(x[220]) < tol
  @test normTPS(x[221]) < tol
  @test normTPS(x[222]) < tol
  @test normTPS(x[223]) < tol
  @test normTPS(x[224]) < tol
  @test normTPS(x[225]) < tol
  @test normTPS(x[226]) < tol
  @test normTPS(x[227]) < tol
  @test normTPS(x[228]) < tol
  @test normTPS(x[229]) < tol
  @test normTPS(x[230]) < tol
  @test normTPS(x[231]) < tol
  @test normTPS(x[232]) < tol
  @test normTPS(x[233]) < tol
  @test normTPS(x[234]) < tol
  @test normTPS(x[235]) < tol
  @test normTPS(x[236]) < tol
  @test normTPS(x[237]) < tol
  @test normTPS(x[238]) < tol
  @test normTPS(x[239]) < tol
  @test normTPS(x[240]) < tol
  @test normTPS(x[241]) < tol
  @test normTPS(x[242]) < tol
  @test normTPS(x[243]) < tol
  @test normTPS(x[244]) < tol
  @test normTPS(x[245]) < tol
  @test normTPS(x[246]) < tol
  @test normTPS(x[247]) < tol
  @test normTPS(x[248]) < tol
end

@testset "Static: FastGTPSA - Block Broadcasting" begin
  d = Descriptor(1,5)
  w = [[ComplexTPS64{d}()] for i in 1:248]

  t = [TPS{d}()]
  ct = [ComplexTPS64{d}()]
  # Set scalar part so both TPSs are 1
  t[1][0] = 1
  ct[1][0] = 1
  # Now do operators
  t1 = TPS{d}.(t)
  t1[1][0] = 1
  t2 = TPS{d}.(t)
  t2[1][0] = 2
  t3 = TPS{d}.(t)
  t3[1][0] = 3

  ct1 = ComplexTPS64{d}.(ct)
  ct1[1][0] = 1 + 1im
  ct2 = ComplexTPS64{d}.(ct)
  ct2[1][0] = 2 + 2im
  ct3 = ComplexTPS64{d}.(ct)
  ct3[1][0] = 3 + 3im

  t = ComplexTPS64{d}.(t)
  t[1][0] = 0.5+0.5im; t[1][[1]] = 2+2im; t[1][[2]] = 3+3im; t[1][[3]] = 4+4im; t[1][[4]] = 5+5im; t[1][[5]] = 6+6im
  v = [0.5+0.5im]
  tol = 1e-10

  @FastGTPSA begin
    y1 = @. t1 + t2 - t3
    y2 = @. t2 + t1 - t3
    y3 = @. t1 + 2 - t3
    y4 = @. 2 + t1 - t3
    y5 = @. t3 - t2 - t1
    y6 = @. t2 - t3 - -t1
    y7 = @. t3 - 2 - t1
    y8 = @. 2 - t3 - -t1
    y9 = @. t2 * t3 - 6
    y10 =@.  t3 * t2 - 6
    y11 =@.  t2 * 5 - 10
    y12 =@.  5 * t2 - 10 * t1
    y13 =@.  t1 / t2 - 1/2
    y14 =@.  t2 / t1 - 2
    y15 =@.  1 / t2 - 1/2
    y16 =@.  t2 / 3 - 2/3
    y17 =@.  t2 / t2 - t1
    y18 =@.  t2 / t2 - 1
    y19 =@.  t2 ^ t3 - 8
    y20 =@.  t3 ^ t2 - 9
    y21 =@.  t2 ^ 3 - 8
    y22 =@.  t2 ^ (1/2) - sqrt(2)
    y23 =@.  t2 ^ (1/2) - sqrt(t2)
    y24 =@.  2 ^ t3 - 8
    y25 =@.  inv(t3) - 1/t3
    y26 =@.  inv(t3) - 1/3
    y27 =@.  ct1 + ct2 - ct3
    y28 =@.  ct2 + ct1 - ct3
    y29 =@.  ct1 + (2+2im) - ct3
    y30 =@.  (2+2im) + ct1 - ct3
    y31 =@.  ct3 - ct2 - ct1
    y32 =@.  ct2 - ct3 - -ct1
    y33 =@.  ct3 - (2+2im) - ct1
    y34 =@.  (2+2im) - ct3 - -ct1
    y35 =@.  ct2 * ct3 - (2+2im)*(3+3im)
    y36 =@.  ct3 * ct2 - (2+2im)*(3+3im)
    y37 =@.  ct2 * 5 - (10+10im)
    y38 =@.  5 * ct2 - (10 * ct1)
    y39 =@.  ct1 / ct2 - (1+im)/(2+2im)
    y40 =@.  ct2 / ct1 - 2
    y41 =@.  1 / ct2 - 1/(2+2im)
    y42 =@.  ct2 / 3 - (2+2im)/3
    y43 =@.  ct2 / ct2 - 1
    y44 =@.  ct2 ^ ct3 - (2+2im)^(3+3im)
    y45 =@.  ct3 ^ ct2 - (3+3im)^(2+2im)
    y46 =@.  ct2 ^ 3 - (2+2im)^3
    y47 =@.  ct2 ^ (1/2) - sqrt(2+2im)
    y48 =@.  ct2 ^ (1/2) - sqrt(ct2)
    y49 =@.  2 ^ ct3 - 2^(3+3im)
    y50 =@.  inv(ct3) - 1/ct3
    y51 =@.  inv(ct3) - 1/(3+3im)
    y52 =@.  t1 + ct2 - (1 + (2+2im))
    y53 =@.  ct2 + t1 - (1 + (2+2im))
    y54 =@.  t1 + (2+2im) - (1 + (2+2im))
    y55 =@.  (2+2im) + t1 - (1 + (2+2im))
    y56 =@.  t3 - ct2 - (3 - (2+2im))
    y57 =@.  ct2 - t3 - ((2+2im) - 3)
    y58 =@.  t3 - (2+2im) - (3 - (2+2im))
    y59 =@.  (2+2im) - t3 - ((2+2im) - 3)
    y60 =@.  t2 * ct3 - 2 * (3+3im)
    y61 =@.  ct3 * t2 - 2 * (3+3im)
    y62 =@.  t2 * (3+3im) - 2 * (3+3im)
    y63 =@.  (3+3im) * t2 - 2 * (3+3im)
    y64 =@.  t2 / ct3 - 2/(3+3im)
    y65 =@.  ct3 / t2 - (3+3im)/2
    y66 =@.  t2 / (3+3im) - 2/(3+3im)
    y67 =@.  (3+3im) / t2 - (3+3im)/2
    y68 =@.  t2 ^ ct3 - 2^(3+3im)
    y69 =@.  ct3 ^ t2 - (3+3im)^2
    y70 =@.  t2 ^ (3+3im) - 2^(3+3im)
    y71 =@.  (3+3im)^t2 - (3+3im)^2
    y72 =@.  sin(t)^2+cos(t)^2 - 1
    y73 =@.  1/sin(t) - csc(t)
    y74 =@.  1/cos(t) - sec(t)
    y75 =@.  1/tan(t) - cot(t)
    y76 =@.  sin(t)/cos(t) - tan(t)
    y77 =@.  cos(2*t) - cos(t)^2 + sin(t)^2
    y78 =@.  sec(t)^2 - 1 - tan(t)^2
    y79 =@.  sin(t/2) - sqrt((1-cos(t))/2)
    y80 =@.  cos(t/2) - sqrt((1+cos(t))/2)
    y81 =@.  sqrt(real(t)^2) - abs(real(t))
    y82 =@.  csc(t)^2 - cot(t)^2 - 1
    y83 =@.  exp(log(t)) - t
    y84 =@.  log(exp(t)) - t
    y85 =@.  log(exp(t)) - exp(log(t))
    y86 =@.  log(t^2) - 2*log(t)
    y87 =@.  5*log(real(t)) - log(real(t)^5)
    y88 =@.  t*log(5) - log(5^t)
    y89 =@.  sinc(t) - sin(pi*t)/(pi*t)
    y90 =@.  sinhc(t/pi) - sinh(t)/t
    y91 =@.  exp(im*t) - cos(t) - im*sin(t)
    y92 =@.  real(exp(im*real(t))) - cos(real(t))
    y93 =@.  imag(exp(im*real(t))) - sin(real(t))
    y94 =@.  sinh(t) - (exp(t) - exp(-t))/2
    y95 =@.  cosh(t) - (exp(t) + exp(-t))/2
    y96 =@.  tanh(t) - sinh(t)/cosh(t)
    y97 =@.  csch(t) - 1/sinh(t)
    y98 =@.  sech(t) - 1/cosh(t)
    y99 =@.  coth(t) - cosh(t)/sinh(t)
    y100 =@. coth(t) - 1/tanh(t)
    y101 =@. cosh(t)^2 - sinh(t)^2 - 1
    y102 =@. 1 - tanh(t)^2 - sech(t)^2
    y103 =@. coth(t)^2 - 1 - csch(t)^2
    y104 =@. asin(sin(t)) - t
    y105 =@. acos(cos(t)) - t
    y106 =@. atan(tan(t)) - t
    y107 =@. acsc(1/t) - asin(t)
    y108 =@. asec(1/t) - acos(t)
    y109 =@. acot(1/t) - atan(t)
    y110 =@. asinh(sinh(t)) - t
    y111 =@. acosh(cosh(t)) - t
    y112 =@. atanh(tanh(t)) - t
    y113 =@. acsch(t) - asinh(1/t)
    y114 =@. asech(t) - acosh(1/t)
    y115 =@. acoth(1/t) - atanh(t)
    y116 =@. asinc(t/pi) - asin(t)/t
    y117 =@. asinhc(t/pi) - asinh(t)/t
    y118 =@. erfc(t) - 1 + erf(t)
    y119 =@. erf(-t) + erf(t)
    y120 =@. angle(real(t))
    y121 =@. complex(t) - t
    y122 =@. complex(real(t),real(t)) - (real(t)+im*real(t))
    y123 =@. sin(t)^2+cos(t)^2 - 1
    y124 =@. 1/sin(t) - csc(t)
    y125 =@. 1/cos(t) - sec(t)
    y126 =@. 1/tan(t) - cot(t)
    y127 =@. sin(t)/cos(t) - tan(t)
    y128 =@. cos(2*t) - cos(t)^2 + sin(t)^2
    y129 =@. sec(t)^2 - 1 - tan(t)^2
    y130 =@. sin(t/2) - sqrt((1-cos(t))/2)
    y131 =@. cos(t/2) - sqrt((1+cos(t))/2)
    y132 =@. sqrt(t^2) - t
    y133 =@. csc(t)^2 - cot(t)^2 - 1
    y134 =@. exp(log(t)) - t
    y135 =@. log(exp(t)) - t
    y136 =@. log(exp(t)) - exp(log(t))
    y137 =@. log(t^2) - 2*log(t)
    y138 =@. 5*log(t) - log(t^5) - 2*pi*im
    y139 =@. t*log(5) - log(5^t)
    y140 =@. sinc(t/pi) - sin(t)/t
    y141 =@. sinhc(t/pi) - sinh(t)/t
    y142 =@. exp(im*t) - cos(t) - im*sin(t)
    y143 =@. sinh(t) - (exp(t) - exp(-t))/2
    y144 =@. cosh(t) - (exp(t) + exp(-t))/2
    y145 =@. tanh(t) - sinh(t)/cosh(t)
    y146 =@. csch(t) - 1/sinh(t)
    y147 =@. sech(t) - 1/cosh(t)
    y148 =@. coth(t) - cosh(t)/sinh(t)
    y149 =@. coth(t) - 1/tanh(t)
    y150 =@. cosh(t)^2 - sinh(t)^2 - 1
    y151 =@. 1 - tanh(t)^2 - sech(t)^2
    y152 =@. coth(t)^2 - 1 - csch(t)^2
    y153 =@. asin(sin(t)) - t
    y154 =@. acos(cos(t)) - t
    y155 =@. atan(tan(t)) - t
    y156 =@. acsc(t) - asin(1/t)
    y157 =@. asec(t) - acos(1/t)
    y158 =@. acot(t) - atan(1/t)
    y159 =@. asinh(sinh(t)) - t
    y160 =@. acosh(cosh(t)) - t
    y161 =@. atanh(tanh(t)) - t
    y162 =@. acsch(t) - asinh(1/t)
    y163 =@. asech(t) - acosh(1/t)
    y164 =@. acoth(t) - atanh(1/t)
    y165 =@. asinc(t/pi) - asin(t)/t
    y166 =@. asinhc(t/pi) - asinh(t)/t
    y167 =@. erfc(t) - 1 + erf(t)
    y168 =@. erf(-t) + erf(t)
    y169 =@. angle(t) - atan(imag(t),real(t))
    y170 =@. complex(t) - t
  end

  @FastGTPSA! begin
    @. w[1  ]= t1 + t2 - t3
    @. w[2  ]= t2 + t1 - t3
    @. w[3  ]= t1 + 2 - t3
    @. w[4  ]= 2 + t1 - t3
    @. w[5  ]= t3 - t2 - t1
    @. w[6  ]= t2 - t3 - -t1
    @. w[7  ]= t3 - 2 - t1
    @. w[8  ]= 2 - t3 - -t1
    @. w[9  ]=t2 * t3 - 6
    @. w[10 ]= t3 * t2 - 6
    @. w[11 ]= t2 * 5 - 10
    @. w[12 ]= 5 * t2 - 10 * t1
    @. w[13 ]= t1 / t2 - 1/2
    @. w[14 ]= t2 / t1 - 2
    @. w[15 ]= 1 / t2 - 1/2
    @. w[16 ]= t2 / 3 - 2/3
    @. w[17 ]= t2 / t2 - t1
    @. w[18 ]= t2 / t2 - 1
    @. w[19 ]= t2 ^ t3 - 8
    @. w[20 ]= t3 ^ t2 - 9
    @. w[21 ]= t2 ^ 3 - 8
    @. w[22 ]= t2 ^ (1/2) - sqrt(2)
    @. w[23 ]= t2 ^ (1/2) - sqrt(t2)
    @. w[24 ]= 2 ^ t3 - 8
    @. w[25 ]= inv(t3) - 1/t3
    @. w[26 ]= inv(t3) - 1/3
    @. w[27 ]= ct1 + ct2 - ct3
    @. w[28 ]= ct2 + ct1 - ct3
    @. w[29 ]= ct1 + (2+2im) - ct3
    @. w[30 ]= (2+2im) + ct1 - ct3
    @. w[31 ]= ct3 - ct2 - ct1
    @. w[32 ]= ct2 - ct3 - -ct1
    @. w[33 ]= ct3 - (2+2im) - ct1
    @. w[34 ]= (2+2im) - ct3 - -ct1
    @. w[35 ]= ct2 * ct3 - (2+2im)*(3+3im)
    @. w[36 ]= ct3 * ct2 - (2+2im)*(3+3im)
    @. w[37 ]= ct2 * 5 - (10+10im)
    @. w[38 ]= 5 * ct2 - (10 * ct1)
    @. w[39 ]= ct1 / ct2 - (1+im)/(2+2im)
    @. w[40 ]= ct2 / ct1 - 2
    @. w[41 ]= 1 / ct2 - 1/(2+2im)
    @. w[42 ]= ct2 / 3 - (2+2im)/3
    @. w[43 ]= ct2 / ct2 - 1
    @. w[44 ]= ct2 ^ ct3 - (2+2im)^(3+3im)
    @. w[45 ]= ct3 ^ ct2 - (3+3im)^(2+2im)
    @. w[46 ]= ct2 ^ 3 - (2+2im)^3
    @. w[47 ]= ct2 ^ (1/2) - sqrt(2+2im)
    @. w[48 ]= ct2 ^ (1/2) - sqrt(ct2)
    @. w[49 ]= 2 ^ ct3 - 2^(3+3im)
    @. w[50 ]= inv(ct3) - 1/ct3
    @. w[51 ]= inv(ct3) - 1/(3+3im)
    @. w[52 ]= t1 + ct2 - (1 + (2+2im))
    @. w[53 ]= ct2 + t1 - (1 + (2+2im))
    @. w[54 ]= t1 + (2+2im) - (1 + (2+2im))
    @. w[55 ]= (2+2im) + t1 - (1 + (2+2im))
    @. w[56 ]= t3 - ct2 - (3 - (2+2im))
    @. w[57 ]= ct2 - t3 - ((2+2im) - 3)
    @. w[58 ]= t3 - (2+2im) - (3 - (2+2im))
    @. w[59 ]= (2+2im) - t3 - ((2+2im) - 3)
    @. w[60 ]= t2 * ct3 - 2 * (3+3im)
    @. w[61 ]= ct3 * t2 - 2 * (3+3im)
    @. w[62 ]= t2 * (3+3im) - 2 * (3+3im)
    @. w[63 ]= (3+3im) * t2 - 2 * (3+3im)
    @. w[64 ]= t2 / ct3 - 2/(3+3im)
    @. w[65 ]= ct3 / t2 - (3+3im)/2
    @. w[66 ]= t2 / (3+3im) - 2/(3+3im)
    @. w[67 ]= (3+3im) / t2 - (3+3im)/2
    @. w[68 ]= t2 ^ ct3 - 2^(3+3im)
    @. w[69 ]= ct3 ^ t2 - (3+3im)^2
    @. w[70 ]= t2 ^ (3+3im) - 2^(3+3im)
    @. w[71 ]= (3+3im)^t2 - (3+3im)^2
    @. w[72 ]= sin(t)^2+cos(t)^2 - 1
    @. w[73 ]= 1/sin(t) - csc(t)
    @. w[74 ]= 1/cos(t) - sec(t)
    @. w[75 ]= 1/tan(t) - cot(t)
    @. w[76 ]= sin(t)/cos(t) - tan(t)
    @. w[77 ]= cos(2*t) - cos(t)^2 + sin(t)^2
    @. w[78 ]= sec(t)^2 - 1 - tan(t)^2
    @. w[79 ]= sin(t/2) - sqrt((1-cos(t))/2)
    @. w[80 ]= cos(t/2) - sqrt((1+cos(t))/2)
    @. w[81 ]= sqrt(real(t)^2) - abs(real(t))
    @. w[82 ]= csc(t)^2 - cot(t)^2 - 1
    @. w[83 ]= exp(log(t)) - t
    @. w[84 ]= log(exp(t)) - t
    @. w[85 ]= log(exp(t)) - exp(log(t))
    @. w[86 ]= log(t^2) - 2*log(t)
    @. w[87 ]= 5*log(real(t)) - log(real(t)^5)
    @. w[88 ]= t*log(5) - log(5^t)
    @. w[89 ]= sinc(t) - sin(pi*t)/(pi*t)
    @. w[90 ]= sinhc(t/pi) - sinh(t)/t
    @. w[91 ]= exp(im*t) - cos(t) - im*sin(t)
    @. w[92 ]= real(exp(im*real(t))) - cos(real(t))
    @. w[93 ]= imag(exp(im*real(t))) - sin(real(t))
    @. w[94 ]= sinh(t) - (exp(t) - exp(-t))/2
    @. w[95 ]= cosh(t) - (exp(t) + exp(-t))/2
    @. w[96 ]= tanh(t) - sinh(t)/cosh(t)
    @. w[97 ]= csch(t) - 1/sinh(t)
    @. w[98 ]= sech(t) - 1/cosh(t)
    @. w[99 ]= coth(t) - cosh(t)/sinh(t)
    @. w[100] = coth(t) - 1/tanh(t)
    @. w[101] = cosh(t)^2 - sinh(t)^2 - 1
    @. w[102] = 1 - tanh(t)^2 - sech(t)^2
    @. w[103] = coth(t)^2 - 1 - csch(t)^2
    @. w[104] = asin(sin(t)) - t
    @. w[105] = acos(cos(t)) - t
    @. w[106] = atan(tan(t)) - t
    @. w[107] = acsc(1/t) - asin(t)
    @. w[108] = asec(1/t) - acos(t)
    @. w[109] = acot(1/t) - atan(t)
    @. w[110] = asinh(sinh(t)) - t
    @. w[111] = acosh(cosh(t)) - t
    @. w[112] = atanh(tanh(t)) - t
    @. w[113] = acsch(t) - asinh(1/t)
    @. w[114] = asech(t) - acosh(1/t)
    @. w[115] = acoth(1/t) - atanh(t)
    @. w[116] = asinc(t/pi) - asin(t)/t
    @. w[117] = asinhc(t/pi) - asinh(t)/t
    @. w[118] = erfc(t) - 1 + erf(t)
    @. w[119] = erf(-t) + erf(t)
    @. w[120] = angle(real(t))
    @. w[121] = complex(t) - t
    @. w[122] = complex(real(t),real(t)) - (real(t)+im*real(t))
    @. w[123] = sin(t)^2+cos(t)^2 - 1
    @. w[124] = 1/sin(t) - csc(t)
    @. w[125] = 1/cos(t) - sec(t)
    @. w[126] = 1/tan(t) - cot(t)
    @. w[127] = sin(t)/cos(t) - tan(t)
    @. w[128] = cos(2*t) - cos(t)^2 + sin(t)^2
    @. w[129] = sec(t)^2 - 1 - tan(t)^2
    @. w[130] = sin(t/2) - sqrt((1-cos(t))/2)
    @. w[131] = cos(t/2) - sqrt((1+cos(t))/2)
    @. w[132] = sqrt(t^2) - t
    @. w[133] = csc(t)^2 - cot(t)^2 - 1
    @. w[134] = exp(log(t)) - t
    @. w[135] = log(exp(t)) - t
    @. w[136] = log(exp(t)) - exp(log(t))
    @. w[137] = log(t^2) - 2*log(t)
    @. w[138] = 5*log(t) - log(t^5) - 2*pi*im
    @. w[139] = t*log(5) - log(5^t)
    @. w[140] = sinc(t/pi) - sin(t)/t
    @. w[141] = sinhc(t/pi) - sinh(t)/t
    @. w[142] = exp(im*t) - cos(t) - im*sin(t)
    @. w[143] = sinh(t) - (exp(t) - exp(-t))/2
    @. w[144] = cosh(t) - (exp(t) + exp(-t))/2
    @. w[145] = tanh(t) - sinh(t)/cosh(t)
    @. w[146] = csch(t) - 1/sinh(t)
    @. w[147] = sech(t) - 1/cosh(t)
    @. w[148] = coth(t) - cosh(t)/sinh(t)
    @. w[149] = coth(t) - 1/tanh(t)
    @. w[150] = cosh(t)^2 - sinh(t)^2 - 1
    @. w[151] = 1 - tanh(t)^2 - sech(t)^2
    @. w[152] = coth(t)^2 - 1 - csch(t)^2
    @. w[153] = asin(sin(t)) - t
    @. w[154] = acos(cos(t)) - t
    @. w[155] = atan(tan(t)) - t
    @. w[156] = acsc(t) - asin(1/t)
    @. w[157] = asec(t) - acos(1/t)
    @. w[158] = acot(t) - atan(1/t)
    @. w[159] = asinh(sinh(t)) - t
    @. w[160] = acosh(cosh(t)) - t
    @. w[161] = atanh(tanh(t)) - t
    @. w[162] = acsch(t) - asinh(1/t)
    @. w[163] = asech(t) - acosh(1/t)
    @. w[164] = acoth(t) - atanh(1/t)
    @. w[165] = asinc(t/pi) - asin(t)/t
    @. w[166] = asinhc(t/pi) - asinh(t)/t
    @. w[167] = erfc(t) - 1 + erf(t)
    @. w[168] = erf(-t) + erf(t)
    @. w[169] = angle(t) - atan(imag(t),real(t))
    @. w[170] = complex(t) - t
  end

  #t = ComplexTPS64{d}()
  t[1] = ComplexTPS64(0.5+0.5im)
  #v = 0.5+0.5im
  @FastGTPSA begin
    y171 = @. sqrt(t) - sqrt(v)
    y172 = @. exp(t) - exp(v)
    y173 = @. log(t) - log(v)
    y174 = @. sin(t) - sin(v)
    y175 = @. cos(t) - cos(v)
    y176 = @. tan(t) - tan(v)
    y177 = @. csc(t) - csc(v)
    y178 = @. sec(t) - sec(v)
    y179 = @. cot(t) - cot(v)
    y180 = @. sinc(t) - sinc(v)
    y181 = @. sinh(t) - sinh(v)
    y182 = @. cosh(t) - cosh(v)
    y183 = @. tanh(t) - tanh(v)
    y184 = @. csch(t) - csch(v)
    y185 = @. sech(t) - sech(v)
    y186 = @. coth(t) - coth(v)
    y187 = @. asin(t) - asin(v)
    y188 = @. acos(t) - acos(v)
    y189 = @. atan(t) - atan(v)
    y190 = @. acsc(t) - acsc(v)
    y191 = @. asec(t) - asec(v)
    y192 = @. acot(t) - acot(v)
    y193 = @. asinh(t) - asinh(v)
    y194 = @. acosh(t) - acosh(v)
    y195 = @. atanh(t) - atanh(v)
    y196 = @. acsch(t) - acsch(v)
    y197 = @. asech(t) - asech(v)
    y198 = @. acoth(t) - acoth(v)
    y199 = @. asinc(t/pi) - asin(v)/v
    y200 = @. asinhc(t/pi) - asinh(v)/v
    y201 = @. zero(t) - zero(v)
    y202 = @. real(t) - real(v)
    y203 = @. imag(t) - imag(v)
    y204 = @. conj(t) - conj(v)
    y205 = @. sinhc(t/pi) - sinh(v)/v
    y206 = @. erf(t) - erf(v)
    y207 = @. erfc(t) - erfc(v)
    y208 = @. -im*erf(t*im) - erfi(v)
    y219 = @. angle(t2+im*t3) - angle(2+3im)
    y220 = @. angle(t2-im*t3) - angle(2-3im)
    y221 = @. angle(-t2-im*t3) - angle(-2-3im)
    y222 = @. angle(-t2+im*t3) - angle(-2+3im)
    y223 = @. angle(ct2) - angle(2+2im)
    y224 = @. angle(-ct2) - angle(-2-2im)
    y225 = @. complex(ct3) - complex(3+3im)
    y226 = @. polar(ct2) - (abs(2+2im)+im*angle(2+2im))
    y227 = @. polar(-ct1) - (abs(-1-im)+im*angle(-1-im))
    y228 = @. rect(ct2) - (2*cos(2) + im*2*sin(2))
    y229 = @. rect(-ct1) - (-1*cos(-1) + im*-1*sin(-1))
    y248 = @. abs(-t) - abs(-v) 
  end
  @FastGTPSA begin
    y209 = @. hypot(ct2,ct3) - hypot(2+2im,3+3im)
    y210 = @. hypot(2+2im,ct3) - hypot(2+2im,3+3im)
    y211 = @. hypot(ct2,3+3im) - hypot(2+2im,3+3im)
    y212 = @. hypot(ct1,ct2,ct3) - hypot(1+1im,2+2im,3+3im)
    y213 = @. hypot(1+1im, ct2, ct3) - hypot(1+1im,2+2im,3+3im)
    y214 = @. hypot(ct1, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)
    y215 = @. hypot(ct1, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)
    y216 = @. hypot(1+1im, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)
    y217 = @. hypot(1+1im, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)
    y218 = @. hypot(ct1, 2+2im, 3+3im) - hypot(1+1im,2+2im,3+3im)
    y230 = @. hypot(ct1, ct2, t3) - hypot(1+1im,2+2im,3)
    y231 = @. hypot(ct1, t2, ct3) - hypot(1+1im,2,3+3im)
    y232 = @. hypot(t1, ct2, ct3) - hypot(1,2+2im,3+3im)
    y233 = @. hypot(ct1, t2, t3) - hypot(1+1im,2,3)
    y234 = @. hypot(t1, ct2, t3) - hypot(1,2+2im,3)
    y235 = @. hypot(t1, t2, ct3) - hypot(1,2,3+3im)
    y236 = @. hypot(ct1,t2,3+3im) - hypot(1+1im,2,3+3im)
    y237 = @. hypot(t1, ct2, 3+3im) - hypot(1,2+2im,3+3im)
    y238 = @. hypot(ct1,2+2im,t3) - hypot(1+1im,2+2im,3)
    y239 = @. hypot(t1,2+2im,ct3) - hypot(1,2+2im,3+3im)
    y240 = @. hypot(1+1im,ct2,t3) - hypot(1+1im,2+2im,3)
    y241 = @. hypot(1+1im, t2, ct3) - hypot(1+1im,2,3+3im)
    y242 = @. hypot(t1,t2,3+3im) - hypot(1,2,3+3im)
    y243 = @. hypot(t1,2+2im,t3) - hypot(1,2+2im,3)
    y244 = @. hypot(1+1im,t2,t3) - hypot(1+1im,2,3)
    y245 = @. hypot(t1,2,3+3im) - hypot(1,2,3+3im)
    y246 = @. hypot(1,t2,3+3im) - hypot(1,2,3+3im)
    y247 = @. hypot(1+1im,2,t3) - hypot(1+1im,2,3)
  end

  @FastGTPSA! begin
    @. w[171] = sqrt(t) - sqrt(v)
    @. w[172] = exp(t) - exp(v)
    @. w[173] = log(t) - log(v)
    @. w[174] = sin(t) - sin(v)
    @. w[175] = cos(t) - cos(v)
    @. w[176] = tan(t) - tan(v)
    @. w[177] = csc(t) - csc(v)
    @. w[178] = sec(t) - sec(v)
    @. w[179] = cot(t) - cot(v)
    @. w[180] = sinc(t) - sinc(v)
    @. w[181] = sinh(t) - sinh(v)
    @. w[182] = cosh(t) - cosh(v)
    @. w[183] = tanh(t) - tanh(v)
    @. w[184] = csch(t) - csch(v)
    @. w[185] = sech(t) - sech(v)
    @. w[186] = coth(t) - coth(v)
    @. w[187] = asin(t) - asin(v)
    @. w[188] = acos(t) - acos(v)
    @. w[189] = atan(t) - atan(v)
    @. w[190] = acsc(t) - acsc(v)
    @. w[191] = asec(t) - asec(v)
    @. w[192] = acot(t) - acot(v)
    @. w[193] = asinh(t) - asinh(v)
    @. w[194] = acosh(t) - acosh(v)
    @. w[195] = atanh(t) - atanh(v)
    @. w[196] = acsch(t) - acsch(v)
    @. w[197] = asech(t) - asech(v)
    @. w[198] = acoth(t) - acoth(v)
    @. w[199] = asinc(t/pi) - asin(v)/v
    @. w[200] = asinhc(t/pi) - asinh(v)/v
    @. w[201] = zero(t) - zero(v)
    @. w[202] = real(t) - real(v)
    @. w[203] = imag(t) - imag(v)
    @. w[204] = conj(t) - conj(v)
    @. w[205] = sinhc(t/pi) - sinh(v)/v
    @. w[206] = erf(t) - erf(v)
    @. w[207] = erfc(t) - erfc(v)
    @. w[208] = -im*erf(t*im) - erfi(v)
    @. w[219] = angle(t2+im*t3) - angle(2+3im)
    @. w[220] = angle(t2-im*t3) - angle(2-3im)
    @. w[221] = angle(-t2-im*t3) - angle(-2-3im)
    @. w[222] = angle(-t2+im*t3) - angle(-2+3im)
    @. w[223] = angle(ct2) - angle(2+2im)
    @. w[224] = angle(-ct2) - angle(-2-2im)
    @. w[225] = complex(ct3) - complex(3+3im)
    @. w[226] = polar(ct2) - (abs(2+2im)+im*angle(2+2im))
    @. w[227] = polar(-ct1) - (abs(-1-im)+im*angle(-1-im))
    @. w[228] = rect(ct2) - (2*cos(2) + im*2*sin(2))
    @. w[229] = rect(-ct1) - (-1*cos(-1) + im*-1*sin(-1))
    @. w[248] = abs(-t) - abs(-v) 
  end

  @FastGTPSA! begin
    @. w[209] = hypot(ct2,ct3) - hypot(2+2im,3+3im)
    @. w[210] = hypot(2+2im,ct3) - hypot(2+2im,3+3im)
    @. w[211] = hypot(ct2,3+3im) - hypot(2+2im,3+3im)
    @. w[212] = hypot(ct1,ct2,ct3) - hypot(1+1im,2+2im,3+3im)
    @. w[213] = hypot(1+1im, ct2, ct3) - hypot(1+1im,2+2im,3+3im)
    @. w[214] = hypot(ct1, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)
    @. w[215] = hypot(ct1, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)
    @. w[216] = hypot(1+1im, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)
    @. w[217] = hypot(1+1im, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)
    @. w[218] = hypot(ct1, 2+2im, 3+3im) - hypot(1+1im,2+2im,3+3im)
    @. w[230] = hypot(ct1, ct2, t3) - hypot(1+1im,2+2im,3)
    @. w[231] = hypot(ct1, t2, ct3) - hypot(1+1im,2,3+3im)
    @. w[232] = hypot(t1, ct2, ct3) - hypot(1,2+2im,3+3im)
    @. w[233] = hypot(ct1, t2, t3) - hypot(1+1im,2,3)
    @. w[234] = hypot(t1, ct2, t3) - hypot(1,2+2im,3)
    @. w[235] = hypot(t1, t2, ct3) - hypot(1,2,3+3im)
    @. w[236] = hypot(ct1,t2,3+3im) - hypot(1+1im,2,3+3im)
    @. w[237] = hypot(t1, ct2, 3+3im) - hypot(1,2+2im,3+3im)
    @. w[238] = hypot(ct1,2+2im,t3) - hypot(1+1im,2+2im,3)
    @. w[239] = hypot(t1,2+2im,ct3) - hypot(1,2+2im,3+3im)
    @. w[240] = hypot(1+1im,ct2,t3) - hypot(1+1im,2+2im,3)
    @. w[241] = hypot(1+1im, t2, ct3) - hypot(1+1im,2,3+3im)
    @. w[242] = hypot(t1,t2,3+3im) - hypot(1,2,3+3im)
    @. w[243] = hypot(t1,2+2im,t3) - hypot(1,2+2im,3)
    @. w[244] = hypot(1+1im,t2,t3) - hypot(1+1im,2,3)
    @. w[245] = hypot(t1,2,3+3im) - hypot(1,2,3+3im)
    @. w[246] = hypot(1,t2,3+3im) - hypot(1,2,3+3im)
    @. w[247] = hypot(1+1im,2,t3) - hypot(1+1im,2,3)
  end

  @test norm(normTPS.(y1 )) < tol
  @test norm(normTPS.(y2 )) < tol
  @test norm(normTPS.(y3 )) < tol
  @test norm(normTPS.(y4 )) < tol
  @test norm(normTPS.(y5 )) < tol
  @test norm(normTPS.(y6 )) < tol
  @test norm(normTPS.(y7 )) < tol
  @test norm(normTPS.(y8 )) < tol
  @test norm(normTPS.(y9 )) < tol
  @test norm(normTPS.(y10)) < tol
  @test norm(normTPS.(y11)) < tol
  @test norm(normTPS.(y12)) < tol
  @test norm(normTPS.(y13)) < tol
  @test norm(normTPS.(y14)) < tol
  @test norm(normTPS.(y15)) < tol
  @test norm(normTPS.(y16)) < tol
  @test norm(normTPS.(y17)) < tol
  @test norm(normTPS.(y18)) < tol
  @test norm(normTPS.(y19)) < tol
  @test norm(normTPS.(y20)) < tol
  @test norm(normTPS.(y21)) < tol
  @test norm(normTPS.(y22)) < tol
  @test norm(normTPS.(y23)) < tol
  @test norm(normTPS.(y24)) < tol
  @test norm(normTPS.(y25)) < tol
  @test norm(normTPS.(y26)) < tol
  @test norm(normTPS.(y27)) < tol
  @test norm(normTPS.(y28)) < tol
  @test norm(normTPS.(y29)) < tol
  @test norm(normTPS.(y30)) < tol
  @test norm(normTPS.(y31)) < tol
  @test norm(normTPS.(y32)) < tol
  @test norm(normTPS.(y33)) < tol
  @test norm(normTPS.(y34)) < tol
  @test norm(normTPS.(y35)) < tol
  @test norm(normTPS.(y36)) < tol
  @test norm(normTPS.(y37)) < tol
  @test norm(normTPS.(y38)) < tol
  @test norm(normTPS.(y39)) < tol
  @test norm(normTPS.(y40)) < tol
  @test norm(normTPS.(y41)) < tol
  @test norm(normTPS.(y42)) < tol
  @test norm(normTPS.(y43)) < tol
  @test norm(normTPS.(y44)) < tol
  @test norm(normTPS.(y45)) < tol
  @test norm(normTPS.(y46)) < tol
  @test norm(normTPS.(y47)) < tol
  @test norm(normTPS.(y48)) < tol
  @test norm(normTPS.(y49)) < tol
  @test norm(normTPS.(y50)) < tol
  @test norm(normTPS.(y51)) < tol
  @test norm(normTPS.(y52)) < tol
  @test norm(normTPS.(y53)) < tol
  @test norm(normTPS.(y54)) < tol
  @test norm(normTPS.(y55)) < tol
  @test norm(normTPS.(y56)) < tol
  @test norm(normTPS.(y57)) < tol
  @test norm(normTPS.(y58)) < tol
  @test norm(normTPS.(y59)) < tol
  @test norm(normTPS.(y60)) < tol
  @test norm(normTPS.(y61)) < tol
  @test norm(normTPS.(y62)) < tol
  @test norm(normTPS.(y63)) < tol
  @test norm(normTPS.(y64)) < tol
  @test norm(normTPS.(y65)) < tol
  @test norm(normTPS.(y66)) < tol
  @test norm(normTPS.(y67)) < tol
  @test norm(normTPS.(y68)) < tol
  @test norm(normTPS.(y69)) < tol
  @test norm(normTPS.(y70)) < tol
  @test norm(normTPS.(y71)) < tol
  @test norm(normTPS.(y72 )) < tol
  @test norm(normTPS.(y73 )) < tol
  @test norm(normTPS.(y74 )) < tol
  @test norm(normTPS.(y75 )) < tol
  @test norm(normTPS.(y76 )) < tol
  @test norm(normTPS.(y77 )) < tol
  @test norm(normTPS.(y78 )) < tol
  @test norm(normTPS.(y79 )) < tol
  @test norm(normTPS.(y80 )) < tol
  @test norm(normTPS.(y81 )) < tol
  @test norm(normTPS.(y82 )) < tol
  @test norm(normTPS.(y83 )) < tol
  @test norm(normTPS.(y84 )) < tol
  @test norm(normTPS.(y85 )) < tol
  @test norm(normTPS.(y86 )) < tol
  @test norm(normTPS.(y87 )) < tol
  @test norm(normTPS.(y88 )) < tol
  @test norm(normTPS.(y89 )) < tol
  @test norm(normTPS.(y90 )) < tol
  @test norm(normTPS.(y91 )) < tol
  @test norm(normTPS.(y92 )) < tol
  @test norm(normTPS.(y93 )) < tol
  @test norm(normTPS.(y94 )) < tol
  @test norm(normTPS.(y95 )) < tol
  @test norm(normTPS.(y96 )) < tol
  @test norm(normTPS.(y97 )) < tol
  @test norm(normTPS.(y98 )) < tol
  @test norm(normTPS.(y99 )) < tol
  @test norm(normTPS.(y100)) < tol
  @test norm(normTPS.(y101)) < tol
  @test norm(normTPS.(y102)) < tol
  @test norm(normTPS.(y103)) < tol
  @test norm(normTPS.(y104)) < tol
  @test norm(normTPS.(y105)) < tol
  @test norm(normTPS.(y106)) < tol
  @test norm(normTPS.(y107)) < tol
  @test norm(normTPS.(y108)) < tol
  @test norm(normTPS.(y109)) < tol
  @test norm(normTPS.(y110)) < tol
  @test norm(normTPS.(y111)) < tol
  @test norm(normTPS.(y112)) < tol
  @test norm(normTPS.(y113)) < tol
  @test norm(normTPS.(y114)) < tol
  @test norm(normTPS.(y115)) < tol
  @test norm(normTPS.(y116)) < tol
  @test norm(normTPS.(y117)) < tol
  @test norm(normTPS.(y118)) < tol
  @test norm(normTPS.(y119)) < tol
  @test norm(normTPS.(y120)) < tol
  @test norm(normTPS.(y121)) < tol
  @test norm(normTPS.(y122)) < tol
  @test norm(normTPS.(y123)) < tol
  @test norm(normTPS.(y124)) < tol
  @test norm(normTPS.(y125)) < tol
  @test norm(normTPS.(y126)) < tol
  @test norm(normTPS.(y127)) < tol
  @test norm(normTPS.(y128)) < tol
  @test norm(normTPS.(y129)) < tol
  @test norm(normTPS.(y130)) < tol
  @test norm(normTPS.(y131)) < tol
  @test norm(normTPS.(y132)) < tol
  @test norm(normTPS.(y133)) < tol
  @test norm(normTPS.(y134)) < tol
  @test norm(normTPS.(y135)) < tol
  @test norm(normTPS.(y136)) < tol
  @test norm(normTPS.(y137)) < tol
  @test norm(normTPS.(y138)) < tol
  @test norm(normTPS.(y139)) < tol
  @test norm(normTPS.(y140)) < tol
  @test norm(normTPS.(y141)) < tol
  @test norm(normTPS.(y142)) < tol
  @test norm(normTPS.(y143)) < tol
  @test norm(normTPS.(y144)) < tol
  @test norm(normTPS.(y145)) < tol
  @test norm(normTPS.(y146)) < tol
  @test norm(normTPS.(y147)) < tol
  @test norm(normTPS.(y148)) < tol
  @test norm(normTPS.(y149)) < tol
  @test norm(normTPS.(y150)) < tol
  @test norm(normTPS.(y151)) < tol
  @test norm(normTPS.(y152)) < tol
  @test norm(normTPS.(y153)) < tol
  @test norm(normTPS.(y154)) < tol
  @test norm(normTPS.(y155)) < tol
  @test norm(normTPS.(y156)) < tol
  @test norm(normTPS.(y157)) < tol
  @test norm(normTPS.(y158)) < tol
  @test norm(normTPS.(y159)) < tol
  @test norm(normTPS.(y160)) < tol
  @test norm(normTPS.(y161)) < tol
  @test norm(normTPS.(y162)) < tol
  @test norm(normTPS.(y163)) < tol
  @test norm(normTPS.(y164)) < tol
  @test norm(normTPS.(y165)) < tol
  @test norm(normTPS.(y166)) < tol
  @test norm(normTPS.(y167)) < tol
  @test norm(normTPS.(y168)) < tol
  @test norm(normTPS.(y169)) < tol
  @test norm(normTPS.(y170)) < tol
  @test norm(normTPS.(y171)) < tol
  @test norm(normTPS.(y172)) < tol
  @test norm(normTPS.(y173)) < tol
  @test norm(normTPS.(y174)) < tol
  @test norm(normTPS.(y175)) < tol
  @test norm(normTPS.(y176)) < tol
  @test norm(normTPS.(y177)) < tol
  @test norm(normTPS.(y178)) < tol
  @test norm(normTPS.(y179)) < tol
  @test norm(normTPS.(y180)) < tol
  @test norm(normTPS.(y181)) < tol
  @test norm(normTPS.(y182)) < tol
  @test norm(normTPS.(y183)) < tol
  @test norm(normTPS.(y184)) < tol
  @test norm(normTPS.(y185)) < tol
  @test norm(normTPS.(y186)) < tol
  @test norm(normTPS.(y187)) < tol
  @test norm(normTPS.(y188)) < tol
  @test norm(normTPS.(y189)) < tol
  @test norm(normTPS.(y190)) < tol
  @test norm(normTPS.(y191)) < tol
  @test norm(normTPS.(y192)) < tol
  @test norm(normTPS.(y193)) < tol
  @test norm(normTPS.(y194)) < tol
  @test norm(normTPS.(y195)) < tol
  @test norm(normTPS.(y196)) < tol
  @test norm(normTPS.(y197)) < tol
  @test norm(normTPS.(y198)) < tol
  @test norm(normTPS.(y199)) < tol
  @test norm(normTPS.(y200)) < tol
  @test norm(normTPS.(y201)) < tol
  @test norm(normTPS.(y202)) < tol
  @test norm(normTPS.(y203)) < tol
  @test norm(normTPS.(y204)) < tol
  @test norm(normTPS.(y205)) < tol
  @test norm(normTPS.(y206)) < tol
  @test norm(normTPS.(y207)) < tol
  @test norm(normTPS.(y208)) < tol
  @test norm(normTPS.(y209)) < tol
  @test norm(normTPS.(y210)) < tol
  @test norm(normTPS.(y211)) < tol
  @test norm(normTPS.(y212)) < tol
  @test norm(normTPS.(y213)) < tol
  @test norm(normTPS.(y214)) < tol
  @test norm(normTPS.(y215)) < tol
  @test norm(normTPS.(y216)) < tol
  @test norm(normTPS.(y217)) < tol
  @test norm(normTPS.(y218)) < tol
  @test norm(normTPS.(y219)) < tol
  @test norm(normTPS.(y220)) < tol
  @test norm(normTPS.(y221)) < tol
  @test norm(normTPS.(y222)) < tol
  @test norm(normTPS.(y223)) < tol
  @test norm(normTPS.(y224)) < tol
  @test norm(normTPS.(y225)) < tol
  @test norm(normTPS.(y226)) < tol
  @test norm(normTPS.(y227)) < tol
  @test norm(normTPS.(y228)) < tol
  @test norm(normTPS.(y229)) < tol
  @test norm(normTPS.(y230)) < tol
  @test norm(normTPS.(y231)) < tol
  @test norm(normTPS.(y232)) < tol
  @test norm(normTPS.(y233)) < tol
  @test norm(normTPS.(y234)) < tol
  @test norm(normTPS.(y235)) < tol
  @test norm(normTPS.(y236)) < tol
  @test norm(normTPS.(y237)) < tol
  @test norm(normTPS.(y238)) < tol
  @test norm(normTPS.(y239)) < tol
  @test norm(normTPS.(y240)) < tol
  @test norm(normTPS.(y241)) < tol
  @test norm(normTPS.(y242)) < tol
  @test norm(normTPS.(y243)) < tol
  @test norm(normTPS.(y244)) < tol
  @test norm(normTPS.(y245)) < tol
  @test norm(normTPS.(y246)) < tol
  @test norm(normTPS.(y247)) < tol
  @test norm(normTPS.(y248)) < tol


  @test norm(normTPS.(w[1  ])) < tol
  @test norm(normTPS.(w[2  ])) < tol
  @test norm(normTPS.(w[3  ])) < tol
  @test norm(normTPS.(w[4  ])) < tol
  @test norm(normTPS.(w[5  ])) < tol
  @test norm(normTPS.(w[6  ])) < tol
  @test norm(normTPS.(w[7  ])) < tol
  @test norm(normTPS.(w[8  ])) < tol
  @test norm(normTPS.(w[9  ])) < tol
  @test norm(normTPS.(w[10 ])) < tol
  @test norm(normTPS.(w[11 ])) < tol
  @test norm(normTPS.(w[12 ])) < tol
  @test norm(normTPS.(w[13 ])) < tol
  @test norm(normTPS.(w[14 ])) < tol
  @test norm(normTPS.(w[15 ])) < tol
  @test norm(normTPS.(w[16 ])) < tol
  @test norm(normTPS.(w[17 ])) < tol
  @test norm(normTPS.(w[18 ])) < tol
  @test norm(normTPS.(w[19 ])) < tol
  @test norm(normTPS.(w[20 ])) < tol
  @test norm(normTPS.(w[21 ])) < tol
  @test norm(normTPS.(w[22 ])) < tol
  @test norm(normTPS.(w[23 ])) < tol
  @test norm(normTPS.(w[24 ])) < tol
  @test norm(normTPS.(w[25 ])) < tol
  @test norm(normTPS.(w[26 ])) < tol
  @test norm(normTPS.(w[27 ])) < tol
  @test norm(normTPS.(w[28 ])) < tol
  @test norm(normTPS.(w[29 ])) < tol
  @test norm(normTPS.(w[30 ])) < tol
  @test norm(normTPS.(w[31 ])) < tol
  @test norm(normTPS.(w[32 ])) < tol
  @test norm(normTPS.(w[33 ])) < tol
  @test norm(normTPS.(w[34 ])) < tol
  @test norm(normTPS.(w[35 ])) < tol
  @test norm(normTPS.(w[36 ])) < tol
  @test norm(normTPS.(w[37 ])) < tol
  @test norm(normTPS.(w[38 ])) < tol
  @test norm(normTPS.(w[39 ])) < tol
  @test norm(normTPS.(w[40 ])) < tol
  @test norm(normTPS.(w[41 ])) < tol
  @test norm(normTPS.(w[42 ])) < tol
  @test norm(normTPS.(w[43 ])) < tol
  @test norm(normTPS.(w[44 ])) < tol
  @test norm(normTPS.(w[45 ])) < tol
  @test norm(normTPS.(w[46 ])) < tol
  @test norm(normTPS.(w[47 ])) < tol
  @test norm(normTPS.(w[48 ])) < tol
  @test norm(normTPS.(w[49 ])) < tol
  @test norm(normTPS.(w[50 ])) < tol
  @test norm(normTPS.(w[51 ])) < tol
  @test norm(normTPS.(w[52 ])) < tol
  @test norm(normTPS.(w[53 ])) < tol
  @test norm(normTPS.(w[54 ])) < tol
  @test norm(normTPS.(w[55 ])) < tol
  @test norm(normTPS.(w[56 ])) < tol
  @test norm(normTPS.(w[57 ])) < tol
  @test norm(normTPS.(w[58 ])) < tol
  @test norm(normTPS.(w[59 ])) < tol
  @test norm(normTPS.(w[60 ])) < tol
  @test norm(normTPS.(w[61 ])) < tol
  @test norm(normTPS.(w[62 ])) < tol
  @test norm(normTPS.(w[63 ])) < tol
  @test norm(normTPS.(w[64 ])) < tol
  @test norm(normTPS.(w[65 ])) < tol
  @test norm(normTPS.(w[66 ])) < tol
  @test norm(normTPS.(w[67 ])) < tol
  @test norm(normTPS.(w[68 ])) < tol
  @test norm(normTPS.(w[69 ])) < tol
  @test norm(normTPS.(w[70 ])) < tol
  @test norm(normTPS.(w[71 ])) < tol
  @test norm(normTPS.(w[72 ])) < tol
  @test norm(normTPS.(w[73 ])) < tol
  @test norm(normTPS.(w[74 ])) < tol
  @test norm(normTPS.(w[75 ])) < tol
  @test norm(normTPS.(w[76 ])) < tol
  @test norm(normTPS.(w[77 ])) < tol
  @test norm(normTPS.(w[78 ])) < tol
  @test norm(normTPS.(w[79 ])) < tol
  @test norm(normTPS.(w[80 ])) < tol
  @test norm(normTPS.(w[81 ])) < tol
  @test norm(normTPS.(w[82 ])) < tol
  @test norm(normTPS.(w[83 ])) < tol
  @test norm(normTPS.(w[84 ])) < tol
  @test norm(normTPS.(w[85 ])) < tol
  @test norm(normTPS.(w[86 ])) < tol
  @test norm(normTPS.(w[87 ])) < tol
  @test norm(normTPS.(w[88 ])) < tol
  @test norm(normTPS.(w[89 ])) < tol
  @test norm(normTPS.(w[90 ])) < tol
  @test norm(normTPS.(w[91 ])) < tol
  @test norm(normTPS.(w[92 ])) < tol
  @test norm(normTPS.(w[93 ])) < tol
  @test norm(normTPS.(w[94 ])) < tol
  @test norm(normTPS.(w[95 ])) < tol
  @test norm(normTPS.(w[96 ])) < tol
  @test norm(normTPS.(w[97 ])) < tol
  @test norm(normTPS.(w[98 ])) < tol
  @test norm(normTPS.(w[99 ])) < tol
  @test norm(normTPS.(w[100])) < tol
  @test norm(normTPS.(w[101])) < tol
  @test norm(normTPS.(w[102])) < tol
  @test norm(normTPS.(w[103])) < tol
  @test norm(normTPS.(w[104])) < tol
  @test norm(normTPS.(w[105])) < tol
  @test norm(normTPS.(w[106])) < tol
  @test norm(normTPS.(w[107])) < tol
  @test norm(normTPS.(w[108])) < tol
  @test norm(normTPS.(w[109])) < tol
  @test norm(normTPS.(w[110])) < tol
  @test norm(normTPS.(w[111])) < tol
  @test norm(normTPS.(w[112])) < tol
  @test norm(normTPS.(w[113])) < tol
  @test norm(normTPS.(w[114])) < tol
  @test norm(normTPS.(w[115])) < tol
  @test norm(normTPS.(w[116])) < tol
  @test norm(normTPS.(w[117])) < tol
  @test norm(normTPS.(w[118])) < tol
  @test norm(normTPS.(w[119])) < tol
  @test norm(normTPS.(w[120])) < tol
  @test norm(normTPS.(w[121])) < tol
  @test norm(normTPS.(w[122])) < tol
  @test norm(normTPS.(w[123])) < tol
  @test norm(normTPS.(w[124])) < tol
  @test norm(normTPS.(w[125])) < tol
  @test norm(normTPS.(w[126])) < tol
  @test norm(normTPS.(w[127])) < tol
  @test norm(normTPS.(w[128])) < tol
  @test norm(normTPS.(w[129])) < tol
  @test norm(normTPS.(w[130])) < tol
  @test norm(normTPS.(w[131])) < tol
  @test norm(normTPS.(w[132])) < tol
  @test norm(normTPS.(w[133])) < tol
  @test norm(normTPS.(w[134])) < tol
  @test norm(normTPS.(w[135])) < tol
  @test norm(normTPS.(w[136])) < tol
  @test norm(normTPS.(w[137])) < tol
  @test norm(normTPS.(w[138])) < tol
  @test norm(normTPS.(w[139])) < tol
  @test norm(normTPS.(w[140])) < tol
  @test norm(normTPS.(w[141])) < tol
  @test norm(normTPS.(w[142])) < tol
  @test norm(normTPS.(w[143])) < tol
  @test norm(normTPS.(w[144])) < tol
  @test norm(normTPS.(w[145])) < tol
  @test norm(normTPS.(w[146])) < tol
  @test norm(normTPS.(w[147])) < tol
  @test norm(normTPS.(w[148])) < tol
  @test norm(normTPS.(w[149])) < tol
  @test norm(normTPS.(w[150])) < tol
  @test norm(normTPS.(w[151])) < tol
  @test norm(normTPS.(w[152])) < tol
  @test norm(normTPS.(w[153])) < tol
  @test norm(normTPS.(w[154])) < tol
  @test norm(normTPS.(w[155])) < tol
  @test norm(normTPS.(w[156])) < tol
  @test norm(normTPS.(w[157])) < tol
  @test norm(normTPS.(w[158])) < tol
  @test norm(normTPS.(w[159])) < tol
  @test norm(normTPS.(w[160])) < tol
  @test norm(normTPS.(w[161])) < tol
  @test norm(normTPS.(w[162])) < tol
  @test norm(normTPS.(w[163])) < tol
  @test norm(normTPS.(w[164])) < tol
  @test norm(normTPS.(w[165])) < tol
  @test norm(normTPS.(w[166])) < tol
  @test norm(normTPS.(w[167])) < tol
  @test norm(normTPS.(w[168])) < tol
  @test norm(normTPS.(w[169])) < tol
  @test norm(normTPS.(w[170])) < tol
  @test norm(normTPS.(w[171])) < tol
  @test norm(normTPS.(w[172])) < tol
  @test norm(normTPS.(w[173])) < tol
  @test norm(normTPS.(w[174])) < tol
  @test norm(normTPS.(w[175])) < tol
  @test norm(normTPS.(w[176])) < tol
  @test norm(normTPS.(w[177])) < tol
  @test norm(normTPS.(w[178])) < tol
  @test norm(normTPS.(w[179])) < tol
  @test norm(normTPS.(w[180])) < tol
  @test norm(normTPS.(w[181])) < tol
  @test norm(normTPS.(w[182])) < tol
  @test norm(normTPS.(w[183])) < tol
  @test norm(normTPS.(w[184])) < tol
  @test norm(normTPS.(w[185])) < tol
  @test norm(normTPS.(w[186])) < tol
  @test norm(normTPS.(w[187])) < tol
  @test norm(normTPS.(w[188])) < tol
  @test norm(normTPS.(w[189])) < tol
  @test norm(normTPS.(w[190])) < tol
  @test norm(normTPS.(w[191])) < tol
  @test norm(normTPS.(w[192])) < tol
  @test norm(normTPS.(w[193])) < tol
  @test norm(normTPS.(w[194])) < tol
  @test norm(normTPS.(w[195])) < tol
  @test norm(normTPS.(w[196])) < tol
  @test norm(normTPS.(w[197])) < tol
  @test norm(normTPS.(w[198])) < tol
  @test norm(normTPS.(w[199])) < tol
  @test norm(normTPS.(w[200])) < tol
  @test norm(normTPS.(w[201])) < tol
  @test norm(normTPS.(w[202])) < tol
  @test norm(normTPS.(w[203])) < tol
  @test norm(normTPS.(w[204])) < tol
  @test norm(normTPS.(w[205])) < tol
  @test norm(normTPS.(w[206])) < tol
  @test norm(normTPS.(w[207])) < tol
  @test norm(normTPS.(w[208])) < tol
  @test norm(normTPS.(w[209])) < tol
  @test norm(normTPS.(w[210])) < tol
  @test norm(normTPS.(w[211])) < tol
  @test norm(normTPS.(w[212])) < tol
  @test norm(normTPS.(w[213])) < tol
  @test norm(normTPS.(w[214])) < tol
  @test norm(normTPS.(w[215])) < tol
  @test norm(normTPS.(w[216])) < tol
  @test norm(normTPS.(w[217])) < tol
  @test norm(normTPS.(w[218])) < tol
  @test norm(normTPS.(w[219])) < tol
  @test norm(normTPS.(w[220])) < tol
  @test norm(normTPS.(w[221])) < tol
  @test norm(normTPS.(w[222])) < tol
  @test norm(normTPS.(w[223])) < tol
  @test norm(normTPS.(w[224])) < tol
  @test norm(normTPS.(w[225])) < tol
  @test norm(normTPS.(w[226])) < tol
  @test norm(normTPS.(w[227])) < tol
  @test norm(normTPS.(w[228])) < tol
  @test norm(normTPS.(w[229])) < tol
  @test norm(normTPS.(w[230])) < tol
  @test norm(normTPS.(w[231])) < tol
  @test norm(normTPS.(w[232])) < tol
  @test norm(normTPS.(w[233])) < tol
  @test norm(normTPS.(w[234])) < tol
  @test norm(normTPS.(w[235])) < tol
  @test norm(normTPS.(w[236])) < tol
  @test norm(normTPS.(w[237])) < tol
  @test norm(normTPS.(w[238])) < tol
  @test norm(normTPS.(w[239])) < tol
  @test norm(normTPS.(w[240])) < tol
  @test norm(normTPS.(w[241])) < tol
  @test norm(normTPS.(w[242])) < tol
  @test norm(normTPS.(w[243])) < tol
  @test norm(normTPS.(w[244])) < tol
  @test norm(normTPS.(w[245])) < tol
  @test norm(normTPS.(w[246])) < tol
  @test norm(normTPS.(w[247])) < tol
  @test norm(normTPS.(w[248])) < tol



  @test GTPSA.checktemps()
end

if VERSION >= v"1.10"

@testset "Static: FastGTPSA - Broadcast allocations" begin
  d = Descriptor(1,5)
  w = [[ComplexTPS64{d}()] for i in 1:248]

  t = [TPS{d}()]
  ct = [ComplexTPS64{d}()]
  # Set scalar part so both TPSs are 1
  t[1][0] = 1
  ct[1][0] = 1
  # Now do operators
  t1 = TPS{d}.(t)
  t1[1][0] = 1
  t2 = TPS{d}.(t)
  t2[1][0] = 2
  t3 = TPS{d}.(t)
  t3[1][0] = 3

  ct1 = ComplexTPS64{d}.(ct)
  ct1[1][0] = 1 + 1im
  ct2 = ComplexTPS64{d}.(ct)
  ct2[1][0] = 2 + 2im
  ct3 = ComplexTPS64{d}.(ct)
  ct3[1][0] = 3 + 3im

  t = ComplexTPS64{d}.(t)
  t[1][0] = 0.5+0.5im; t[1][[1]] = 2+2im; t[1][[2]] = 3+3im; t[1][[3]] = 4+4im; t[1][[4]] = 5+5im; t[1][[5]] = 6+6im
  v = [0.5+0.5im]
  tol = 1e-10

  # Everything is JIT-ed already so only need 1 sample 
  # to correctly calculate allocations using interpolation
  

  a1 = @benchmark @FastGTPSA begin
    y1 = @. $t1 + $t2 - $t3
    y2 = @. $t2 + $t1 - $t3
    y3 = @. $t1 + 2 - $t3
    y4 = @. 2 + $t1 - $t3
    y5 = @. $t3 - $t2 - $t1
    y6 = @. $t2 - $t3 - -$t1
    y7 = @. $t3 - 2 - $t1
    y8 = @. 2 - $t3 - -$t1
    y9 = @. $t2 * $t3 - 6
    y10 =@.  $t3 * $t2 - 6
    y11 =@.  $t2 * 5 - 10
    y12 =@.  5 * $t2 - 10 * $t1
    y13 =@.  $t1 / $t2 - 1/2
    y14 =@.  $t2 / $t1 - 2
    y15 =@.  1 / $t2 - 1/2
    y16 =@.  $t2 / 3 - 2/3
    y17 =@.  $t2 / $t2 - $t1
    y18 =@.  $t2 / $t2 - 1
    y19 =@.  $t2 ^ $t3 - 8
    y20 =@.  $t3 ^ $t2 - 9
    y21 =@.  $t2 ^ 3 - 8
    y22 =@.  $t2 ^ (1/2) - sqrt(2)
    y23 =@.  $t2 ^ (1/2) - sqrt($t2)
    y24 =@.  2 ^ $t3 - 8
    y25 =@.  inv($t3) - 1/$t3
    y26 =@.  inv($t3) - 1/3
    y27 =@.  $ct1 + $ct2 - $ct3
    y28 =@.  $ct2 + $ct1 - $ct3
    y29 =@.  $ct1 + (2+2im) - $ct3
    y30 =@.  (2+2im) + $ct1 - $ct3
    y31 =@.  $ct3 - $ct2 - $ct1
    y32 =@.  $ct2 - $ct3 - -$ct1
    y33 =@.  $ct3 - (2+2im) - $ct1
    y34 =@.  (2+2im) - $ct3 - -$ct1
    y35 =@.  $ct2 * $ct3 - (2+2im)*(3+3im)
    y36 =@.  $ct3 * $ct2 - (2+2im)*(3+3im)
    y37 =@.  $ct2 * 5 - (10+10im)
    y38 =@.  5 * $ct2 - (10 * $ct1)
    y39 =@.  $ct1 / $ct2 - (1+im)/(2+2im)
    y40 =@.  $ct2 / $ct1 - 2
    y41 =@.  1 / $ct2 - 1/(2+2im)
    y42 =@.  $ct2 / 3 - (2+2im)/3
    y43 =@.  $ct2 / $ct2 - 1
    y44 =@.  $ct2 ^ $ct3 - (2+2im)^(3+3im)
    y45 =@.  $ct3 ^ $ct2 - (3+3im)^(2+2im)
    y46 =@.  $ct2 ^ 3 - (2+2im)^3
    y47 =@.  $ct2 ^ (1/2) - sqrt(2+2im)
    y48 =@.  $ct2 ^ (1/2) - sqrt($ct2)
    y49 =@.  2 ^ $ct3 - 2^(3+3im)
    y50 =@.  inv($ct3) - 1/$ct3
    y51 =@.  inv($ct3) - 1/(3+3im)
    y52 =@.  $t1 + $ct2 - (1 + (2+2im))
    y53 =@.  $ct2 + $t1 - (1 + (2+2im))
    y54 =@.  $t1 + (2+2im) - (1 + (2+2im))
    y55 =@.  (2+2im) + $t1 - (1 + (2+2im))
    y56 =@.  $t3 - $ct2 - (3 - (2+2im))
    y57 =@.  $ct2 - $t3 - ((2+2im) - 3)
    y58 =@.  $t3 - (2+2im) - (3 - (2+2im))
    y59 =@.  (2+2im) - $t3 - ((2+2im) - 3)
    y60 =@.  $t2 * $ct3 - 2 * (3+3im)
    y61 =@.  $ct3 * $t2 - 2 * (3+3im)
    y62 =@.  $t2 * (3+3im) - 2 * (3+3im)
    y63 =@.  (3+3im) * $t2 - 2 * (3+3im)
    y64 =@.  $t2 / $ct3 - 2/(3+3im)
    y65 =@.  $ct3 / $t2 - (3+3im)/2
    y66 =@.  $t2 / (3+3im) - 2/(3+3im)
    y67 =@.  (3+3im) / $t2 - (3+3im)/2
    y68 =@.  $t2 ^ $ct3 - 2^(3+3im)
    y69 =@.  $ct3 ^ $t2 - (3+3im)^2
    y70 =@.  $t2 ^ (3+3im) - 2^(3+3im)
    y71 =@.  (3+3im)^$t2 - (3+3im)^2
    y72 =@.  sin($t)^2+cos($t)^2 - 1
    y73 =@.  1/sin($t) - csc($t)
    y74 =@.  1/cos($t) - sec($t)
    y75 =@.  1/tan($t) - cot($t)
    y76 =@.  sin($t)/cos($t) - tan($t)
    y77 =@.  cos(2*$t) - cos($t)^2 + sin($t)^2
    y78 =@.  sec($t)^2 - 1 - tan($t)^2
    y79 =@.  sin($t/2) - sqrt((1-cos($t))/2)
    y80 =@.  cos($t/2) - sqrt((1+cos($t))/2)
    y81 =@.  sqrt(real($t)^2) - abs(real($t))
    y82 =@.  csc($t)^2 - cot($t)^2 - 1
    y83 =@.  exp(log($t)) - $t
    y84 =@.  log(exp($t)) - $t
    y85 =@.  log(exp($t)) - exp(log($t))
    y86 =@.  log($t^2) - 2*log($t)
    y87 =@.  5*log(real($t)) - log(real($t)^5)
    y88 =@.  $t*log(5) - log(5^$t)
    y89 =@.  sinc($t) - sin(pi*$t)/(pi*$t)
    y90 =@.  sinhc($t/pi) - sinh($t)/$t
    y91 =@.  exp(im*$t) - cos($t) - im*sin($t)
    y92 =@.  real(exp(im*real($t))) - cos(real($t))
    y93 =@.  imag(exp(im*real($t))) - sin(real($t))
    y94 =@.  sinh($t) - (exp($t) - exp(-$t))/2
    y95 =@.  cosh($t) - (exp($t) + exp(-$t))/2
    y96 =@.  tanh($t) - sinh($t)/cosh($t)
    y97 =@.  csch($t) - 1/sinh($t)
    y98 =@.  sech($t) - 1/cosh($t)
    y99 =@.  coth($t) - cosh($t)/sinh($t)
    y100 =@. coth($t) - 1/tanh($t)
    y101 =@. cosh($t)^2 - sinh($t)^2 - 1
    y102 =@. 1 - tanh($t)^2 - sech($t)^2
    y103 =@. coth($t)^2 - 1 - csch($t)^2
    y104 =@. asin(sin($t)) - $t
    y105 =@. acos(cos($t)) - $t
    y106 =@. atan(tan($t)) - $t
    y107 =@. acsc(1/$t) - asin($t)
    y108 =@. asec(1/$t) - acos($t)
    y109 =@. acot(1/$t) - atan($t)
    y110 =@. asinh(sinh($t)) - $t
    y111 =@. acosh(cosh($t)) - $t
    y112 =@. atanh(tanh($t)) - $t
    y113 =@. acsch($t) - asinh(1/$t)
    y114 =@. asech($t) - acosh(1/$t)
    y115 =@. acoth(1/$t) - atanh($t)
    y116 =@. asinc($t/pi) - asin($t)/$t
    y117 =@. asinhc($t/pi) - asinh($t)/$t
    y118 =@. erfc($t) - 1 + erf($t)
    y119 =@. erf(-$t) + erf($t)
    y120 =@. angle(real($t))
    y121 =@. complex($t) - $t
    y122 =@. complex(real($t),real($t)) - (real($t)+im*real($t))
    y123 =@. sin($t)^2+cos($t)^2 - 1
    y124 =@. 1/sin($t) - csc($t)
    y125 =@. 1/cos($t) - sec($t)
    y126 =@. 1/tan($t) - cot($t)
    y127 =@. sin($t)/cos($t) - tan($t)
    y128 =@. cos(2*$t) - cos($t)^2 + sin($t)^2
    y129 =@. sec($t)^2 - 1 - tan($t)^2
    y130 =@. sin($t/2) - sqrt((1-cos($t))/2)
    y131 =@. cos($t/2) - sqrt((1+cos($t))/2)
    y132 =@. sqrt($t^2) - $t
    y133 =@. csc($t)^2 - cot($t)^2 - 1
    y134 =@. exp(log($t)) - $t
    y135 =@. log(exp($t)) - $t
    y136 =@. log(exp($t)) - exp(log($t))
    y137 =@. log($t^2) - 2*log($t)
    y138 =@. 5*log($t) - log($t^5) - 2*pi*im
    y139 =@. $t*log(5) - log(5^$t)
    y140 =@. sinc($t/pi) - sin($t)/$t
    y141 =@. sinhc($t/pi) - sinh($t)/$t
    y142 =@. exp(im*$t) - cos($t) - im*sin($t)
    y143 =@. sinh($t) - (exp($t) - exp(-$t))/2
    y144 =@. cosh($t) - (exp($t) + exp(-$t))/2
    y145 =@. tanh($t) - sinh($t)/cosh($t)
    y146 =@. csch($t) - 1/sinh($t)
    y147 =@. sech($t) - 1/cosh($t)
    y148 =@. coth($t) - cosh($t)/sinh($t)
    y149 =@. coth($t) - 1/tanh($t)
    y150 =@. cosh($t)^2 - sinh($t)^2 - 1
    y151 =@. 1 - tanh($t)^2 - sech($t)^2
    y152 =@. coth($t)^2 - 1 - csch($t)^2
    y153 =@. asin(sin($t)) - $t
    y154 =@. acos(cos($t)) - $t
    y155 =@. atan(tan($t)) - $t
    y156 =@. acsc($t) - asin(1/$t)
    y157 =@. asec($t) - acos(1/$t)
    y158 =@. acot($t) - atan(1/$t)
    y159 =@. asinh(sinh($t)) - $t
    y160 =@. acosh(cosh($t)) - $t
    y161 =@. atanh(tanh($t)) - $t
    y162 =@. acsch($t) - asinh(1/$t)
    y163 =@. asech($t) - acosh(1/$t)
    y164 =@. acoth($t) - atanh(1/$t)
    y165 =@. asinc($t/pi) - asin($t)/$t
    y166 =@. asinhc($t/pi) - asinh($t)/$t
    y167 =@. erfc($t) - 1 + erf($t)
    y168 =@. erf(-$t) + erf($t)
    y169 =@. angle($t) - atan(imag($t),real($t))
    y170 =@. complex($t) - $t
  end

  @test a1.allocs == 170*4
  a2 = @benchmark @FastGTPSA! begin
    @. $w[1  ]= $t1 + $t2 - $t3
    @. $w[2  ]= $t2 + $t1 - $t3
    @. $w[3  ]= $t1 + 2 - $t3
    @. $w[4  ]= 2 + $t1 - $t3
    @. $w[5  ]= $t3 - $t2 - $t1
    @. $w[6  ]= $t2 - $t3 - -$t1
    @. $w[7  ]= $t3 - 2 - $t1
    @. $w[8  ]= 2 - $t3 - -$t1
    @. $w[9  ]=$t2 * $t3 - 6
    @. $w[10 ]= $t3 * $t2 - 6
    @. $w[11 ]= $t2 * 5 - 10
    @. $w[12 ]= 5 * $t2 - 10 * $t1
    @. $w[13 ]= $t1 / $t2 - 1/2
    @. $w[14 ]= $t2 / $t1 - 2
    @. $w[15 ]= 1 / $t2 - 1/2
    @. $w[16 ]= $t2 / 3 - 2/3
    @. $w[17 ]= $t2 / $t2 - $t1
    @. $w[18 ]= $t2 / $t2 - 1
    @. $w[19 ]= $t2 ^ $t3 - 8
    @. $w[20 ]= $t3 ^ $t2 - 9
    @. $w[21 ]= $t2 ^ 3 - 8
    @. $w[22 ]= $t2 ^ (1/2) - sqrt(2)
    @. $w[23 ]= $t2 ^ (1/2) - sqrt($t2)
    @. $w[24 ]= 2 ^ $t3 - 8
    @. $w[25 ]= inv($t3) - 1/$t3
    @. $w[26 ]= inv($t3) - 1/3
    @. $w[27 ]= $ct1 + $ct2 - $ct3
    @. $w[28 ]= $ct2 + $ct1 - $ct3
    @. $w[29 ]= $ct1 + (2+2im) - $ct3
    @. $w[30 ]= (2+2im) + $ct1 - $ct3
    @. $w[31 ]= $ct3 - $ct2 - $ct1
    @. $w[32 ]= $ct2 - $ct3 - -$ct1
    @. $w[33 ]= $ct3 - (2+2im) - $ct1
    @. $w[34 ]= (2+2im) - $ct3 - -$ct1
    @. $w[35 ]= $ct2 * $ct3 - (2+2im)*(3+3im)
    @. $w[36 ]= $ct3 * $ct2 - (2+2im)*(3+3im)
    @. $w[37 ]= $ct2 * 5 - (10+10im)
    @. $w[38 ]= 5 * $ct2 - (10 * $ct1)
    @. $w[39 ]= $ct1 / $ct2 - (1+im)/(2+2im)
    @. $w[40 ]= $ct2 / $ct1 - 2
    @. $w[41 ]= 1 / $ct2 - 1/(2+2im)
    @. $w[42 ]= $ct2 / 3 - (2+2im)/3
    @. $w[43 ]= $ct2 / $ct2 - 1
    @. $w[44 ]= $ct2 ^ $ct3 - (2+2im)^(3+3im)
    @. $w[45 ]= $ct3 ^ $ct2 - (3+3im)^(2+2im)
    @. $w[46 ]= $ct2 ^ 3 - (2+2im)^3
    @. $w[47 ]= $ct2 ^ (1/2) - sqrt(2+2im)
    @. $w[48 ]= $ct2 ^ (1/2) - sqrt($ct2)
    @. $w[49 ]= 2 ^ $ct3 - 2^(3+3im)
    @. $w[50 ]= inv($ct3) - 1/$ct3
    @. $w[51 ]= inv($ct3) - 1/(3+3im)
    @. $w[52 ]= $t1 + $ct2 - (1 + (2+2im))
    @. $w[53 ]= $ct2 + $t1 - (1 + (2+2im))
    @. $w[54 ]= $t1 + (2+2im) - (1 + (2+2im))
    @. $w[55 ]= (2+2im) + $t1 - (1 + (2+2im))
    @. $w[56 ]= $t3 - $ct2 - (3 - (2+2im))
    @. $w[57 ]= $ct2 - $t3 - ((2+2im) - 3)
    @. $w[58 ]= $t3 - (2+2im) - (3 - (2+2im))
    @. $w[59 ]= (2+2im) - $t3 - ((2+2im) - 3)
    @. $w[60 ]= $t2 * $ct3 - 2 * (3+3im)
    @. $w[61 ]= $ct3 * $t2 - 2 * (3+3im)
    @. $w[62 ]= $t2 * (3+3im) - 2 * (3+3im)
    @. $w[63 ]= (3+3im) * $t2 - 2 * (3+3im)
    @. $w[64 ]= $t2 / $ct3 - 2/(3+3im)
    @. $w[65 ]= $ct3 / $t2 - (3+3im)/2
    @. $w[66 ]= $t2 / (3+3im) - 2/(3+3im)
    @. $w[67 ]= (3+3im) / $t2 - (3+3im)/2
    @. $w[68 ]= $t2 ^ $ct3 - 2^(3+3im)
    @. $w[69 ]= $ct3 ^ $t2 - (3+3im)^2
    @. $w[70 ]= $t2 ^ (3+3im) - 2^(3+3im)
    @. $w[71 ]= (3+3im)^$t2 - (3+3im)^2
    @. $w[72 ]= sin($t)^2+cos($t)^2 - 1
    @. $w[73 ]= 1/sin($t) - csc($t)
    @. $w[74 ]= 1/cos($t) - sec($t)
    @. $w[75 ]= 1/tan($t) - cot($t)
    @. $w[76 ]= sin($t)/cos($t) - tan($t)
    @. $w[77 ]= cos(2*$t) - cos($t)^2 + sin($t)^2
    @. $w[78 ]= sec($t)^2 - 1 - tan($t)^2
    @. $w[79 ]= sin($t/2) - sqrt((1-cos($t))/2)
    @. $w[80 ]= cos($t/2) - sqrt((1+cos($t))/2)
    @. $w[81 ]= sqrt(real($t)^2) - abs(real($t))
    @. $w[82 ]= csc($t)^2 - cot($t)^2 - 1
    @. $w[83 ]= exp(log($t)) - $t
    @. $w[84 ]= log(exp($t)) - $t
    @. $w[85 ]= log(exp($t)) - exp(log($t))
    @. $w[86 ]= log($t^2) - 2*log($t)
    @. $w[87 ]= 5*log(real($t)) - log(real($t)^5)
    @. $w[88 ]= $t*log(5) - log(5^$t)
    @. $w[89 ]= sinc($t) - sin(pi*$t)/(pi*$t)
    @. $w[90 ]= sinhc($t/pi) - sinh($t)/$t
    @. $w[91 ]= exp(im*$t) - cos($t) - im*sin($t)
    @. $w[92 ]= real(exp(im*real($t))) - cos(real($t))
    @. $w[93 ]= imag(exp(im*real($t))) - sin(real($t))
    @. $w[94 ]= sinh($t) - (exp($t) - exp(-$t))/2
    @. $w[95 ]= cosh($t) - (exp($t) + exp(-$t))/2
    @. $w[96 ]= tanh($t) - sinh($t)/cosh($t)
    @. $w[97 ]= csch($t) - 1/sinh($t)
    @. $w[98 ]= sech($t) - 1/cosh($t)
    @. $w[99 ]= coth($t) - cosh($t)/sinh($t)
    @. $w[100] = coth($t) - 1/tanh($t)
    @. $w[101] = cosh($t)^2 - sinh($t)^2 - 1
    @. $w[102] = 1 - tanh($t)^2 - sech($t)^2
    @. $w[103] = coth($t)^2 - 1 - csch($t)^2
    @. $w[104] = asin(sin($t)) - $t
    @. $w[105] = acos(cos($t)) - $t
    @. $w[106] = atan(tan($t)) - $t
    @. $w[107] = acsc(1/$t) - asin($t)
    @. $w[108] = asec(1/$t) - acos($t)
    @. $w[109] = acot(1/$t) - atan($t)
    @. $w[110] = asinh(sinh($t)) - $t
    @. $w[111] = acosh(cosh($t)) - $t
    @. $w[112] = atanh(tanh($t)) - $t
    @. $w[113] = acsch($t) - asinh(1/$t)
    @. $w[114] = asech($t) - acosh(1/$t)
    @. $w[115] = acoth(1/$t) - atanh($t)
    @. $w[116] = asinc($t/pi) - asin($t)/$t
    @. $w[117] = asinhc($t/pi) - asinh($t)/$t
    @. $w[118] = erfc($t) - 1 + erf($t)
    @. $w[119] = erf(-$t) + erf($t)
    @. $w[120] = angle(real($t))
    @. $w[121] = complex($t) - $t
    @. $w[122] = complex(real($t),real($t)) - (real($t)+im*real($t))
    @. $w[123] = sin($t)^2+cos($t)^2 - 1
    @. $w[124] = 1/sin($t) - csc($t)
    @. $w[125] = 1/cos($t) - sec($t)
    @. $w[126] = 1/tan($t) - cot($t)
    @. $w[127] = sin($t)/cos($t) - tan($t)
    @. $w[128] = cos(2*$t) - cos($t)^2 + sin($t)^2
    @. $w[129] = sec($t)^2 - 1 - tan($t)^2
    @. $w[130] = sin($t/2) - sqrt((1-cos($t))/2)
    @. $w[131] = cos($t/2) - sqrt((1+cos($t))/2)
    @. $w[132] = sqrt($t^2) - $t
    @. $w[133] = csc($t)^2 - cot($t)^2 - 1
    @. $w[134] = exp(log($t)) - $t
    @. $w[135] = log(exp($t)) - $t
    @. $w[136] = log(exp($t)) - exp(log($t))
    @. $w[137] = log($t^2) - 2*log($t)
    @. $w[138] = 5*log($t) - log($t^5) - 2*pi*im
    @. $w[139] = $t*log(5) - log(5^$t)
    @. $w[140] = sinc($t/pi) - sin($t)/$t
    @. $w[141] = sinhc($t/pi) - sinh($t)/$t
    @. $w[142] = exp(im*$t) - cos($t) - im*sin($t)
    @. $w[143] = sinh($t) - (exp($t) - exp(-$t))/2
    @. $w[144] = cosh($t) - (exp($t) + exp(-$t))/2
    @. $w[145] = tanh($t) - sinh($t)/cosh($t)
    @. $w[146] = csch($t) - 1/sinh($t)
    @. $w[147] = sech($t) - 1/cosh($t)
    @. $w[148] = coth($t) - cosh($t)/sinh($t)
    @. $w[149] = coth($t) - 1/tanh($t)
    @. $w[150] = cosh($t)^2 - sinh($t)^2 - 1
    @. $w[151] = 1 - tanh($t)^2 - sech($t)^2
    @. $w[152] = coth($t)^2 - 1 - csch($t)^2
    @. $w[153] = asin(sin($t)) - $t
    @. $w[154] = acos(cos($t)) - $t
    @. $w[155] = atan(tan($t)) - $t
    @. $w[156] = acsc($t) - asin(1/$t)
    @. $w[157] = asec($t) - acos(1/$t)
    @. $w[158] = acot($t) - atan(1/$t)
    @. $w[159] = asinh(sinh($t)) - $t
    @. $w[160] = acosh(cosh($t)) - $t
    @. $w[161] = atanh(tanh($t)) - $t
    @. $w[162] = acsch($t) - asinh(1/$t)
    @. $w[163] = asech($t) - acosh(1/$t)
    @. $w[164] = acoth($t) - atanh(1/$t)
    @. $w[165] = asinc($t/pi) - asin($t)/$t
    @. $w[166] = asinhc($t/pi) - asinh($t)/$t
    @. $w[167] = erfc($t) - 1 + erf($t)
    @. $w[168] = erf(-$t) + erf($t)
    @. $w[169] = angle($t) - atan(imag($t),real($t))
    @. $w[170] = complex($t) - $t
  end
  @test a2.allocs == 0

  t[1] = ComplexTPS64(0.5+0.5im)
  a1 = @benchmark @FastGTPSA begin
    y171 = @. sqrt($t) - sqrt($v)
    y172 = @. exp($t) - exp($v)
    y173 = @. log($t) - log($v)
    y174 = @. sin($t) - sin($v)
    y175 = @. cos($t) - cos($v)
    y176 = @. tan($t) - tan($v)
    y177 = @. csc($t) - csc($v)
    y178 = @. sec($t) - sec($v)
    y179 = @. cot($t) - cot($v)
    y180 = @. sinc($t) - sinc($v)
    y181 = @. sinh($t) - sinh($v)
    y182 = @. cosh($t) - cosh($v)
    y183 = @. tanh($t) - tanh($v)
    y184 = @. csch($t) - csch($v)
    y185 = @. sech($t) - sech($v)
    y186 = @. coth($t) - coth($v)
    y187 = @. asin($t) - asin($v)
    y188 = @. acos($t) - acos($v)
    y189 = @. atan($t) - atan($v)
    y190 = @. acsc($t) - acsc($v)
    y191 = @. asec($t) - asec($v)
    y192 = @. acot($t) - acot($v)
    y193 = @. asinh($t) - asinh($v)
    y194 = @. acosh($t) - acosh($v)
    y195 = @. atanh($t) - atanh($v)
    y196 = @. acsch($t) - acsch($v)
    y197 = @. asech($t) - asech($v)
    y198 = @. acoth($t) - acoth($v)
    y199 = @. asinc($t/pi) - asin($v)/$v
    y200 = @. asinhc($t/pi) - asinh($v)/$v
    y201 = @. zero($t) - zero($v)
    y202 = @. real($t) - real($v)
    y203 = @. imag($t) - imag($v)
    y204 = @. conj($t) - conj($v)
    y205 = @. sinhc($t/pi) - sinh($v)/$v
    y206 = @. erf($t) - erf($v)
    y207 = @. erfc($t) - erfc($v)
    y208 = @. -im*erf($t*im) - erfi($v)
    y219 = @. angle($t2+im*$t3) - angle(2+3im)
    y220 = @. angle($t2-im*$t3) - angle(2-3im)
    y221 = @. angle(-$t2-im*$t3) - angle(-2-3im)
    y222 = @. angle(-$t2+im*$t3) - angle(-2+3im)
    y223 = @. angle($ct2) - angle(2+2im)
    y224 = @. angle(-$ct2) - angle(-2-2im)
    y225 = @. complex($ct3) - complex(3+3im)
    y226 = @. polar($ct2) - (abs(2+2im)+im*angle(2+2im))
    y227 = @. polar(-$ct1) - (abs(-1-im)+im*angle(-1-im))
    y228 = @. rect($ct2) - (2*cos(2) + im*2*sin(2))
    y229 = @. rect(-$ct1) - (-1*cos(-1) + im*-1*sin(-1))
    y248 = @. abs(-$t) - abs(-$v) 
  end
  @test a1.allocs == 50*4

  a2 = @benchmark @FastGTPSA! begin
    @. $w[171] = sqrt($t) - sqrt($v)
    @. $w[172] = exp($t) - exp($v)
    @. $w[173] = log($t) - log($v)
    @. $w[174] = sin($t) - sin($v)
    @. $w[175] = cos($t) - cos($v)
    @. $w[176] = tan($t) - tan($v)
    @. $w[177] = csc($t) - csc($v)
    @. $w[178] = sec($t) - sec($v)
    @. $w[179] = cot($t) - cot($v)
    @. $w[180] = sinc($t) - sinc($v)
    @. $w[181] = sinh($t) - sinh($v)
    @. $w[182] = cosh($t) - cosh($v)
    @. $w[183] = tanh($t) - tanh($v)
    @. $w[184] = csch($t) - csch($v)
    @. $w[185] = sech($t) - sech($v)
    @. $w[186] = coth($t) - coth($v)
    @. $w[187] = asin($t) - asin($v)
    @. $w[188] = acos($t) - acos($v)
    @. $w[189] = atan($t) - atan($v)
    @. $w[190] = acsc($t) - acsc($v)
    @. $w[191] = asec($t) - asec($v)
    @. $w[192] = acot($t) - acot($v)
    @. $w[193] = asinh($t) - asinh($v)
    @. $w[194] = acosh($t) - acosh($v)
    @. $w[195] = atanh($t) - atanh($v)
    @. $w[196] = acsch($t) - acsch($v)
    @. $w[197] = asech($t) - asech($v)
    @. $w[198] = acoth($t) - acoth($v)
    @. $w[199] = asinc($t/pi) - asin($v)/$v
    @. $w[200] = asinhc($t/pi) - asinh($v)/$v
    @. $w[201] = zero($t) - zero($v)
    @. $w[202] = real($t) - real($v)
    @. $w[203] = imag($t) - imag($v)
    @. $w[204] = conj($t) - conj($v)
    @. $w[205] = sinhc($t/pi) - sinh($v)/$v
    @. $w[206] = erf($t) - erf($v)
    @. $w[207] = erfc($t) - erfc($v)
    @. $w[208] = -im*erf($t*im) - erfi($v)
    @. $w[219] = angle($t2+im*$t3) - angle(2+3im)
    @. $w[220] = angle($t2-im*$t3) - angle(2-3im)
    @. $w[221] = angle(-$t2-im*$t3) - angle(-2-3im)
    @. $w[222] = angle(-$t2+im*$t3) - angle(-2+3im)
    @. $w[223] = angle($ct2) - angle(2+2im)
    @. $w[224] = angle(-$ct2) - angle(-2-2im)
    @. $w[225] = complex($ct3) - complex(3+3im)
    @. $w[226] = polar($ct2) - (abs(2+2im)+im*angle(2+2im))
    @. $w[227] = polar(-$ct1) - (abs(-1-im)+im*angle(-1-im))
    @. $w[228] = rect($ct2) - (2*cos(2) + im*2*sin(2))
    @. $w[229] = rect(-$ct1) - (-1*cos(-1) + im*-1*sin(-1))
    @. $w[248] = abs(-$t) - abs(-$v) 
  end
  @test a2.allocs == 0
  @test GTPSA.checktemps()
end

end

@testset "Static: Taylor map benchmark against ForwardDiff" begin
  include("../benchmark/track.jl")
  d = Descriptor(6,2,52,2)
  z = @vars(d)
  k = @params(d)
  map = track_ring(z, 0.36+k[1], 1.2+k[2], k[3:end])
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


