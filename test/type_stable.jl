function type_stable_test()
  d = Descriptor(1, 5)
  t = TPS(use=d)
  ct = ComplexTPS(t)

  # Basics
  isequal(t , 0)
  isequal(0 , t)
  isequal(ct , 0)
  isequal(0 , ct)
  isequal(ct , t)
  isequal(t , ct)
  !(t === ct)
  isequal(t , zero(t))
  isequal(ct , zero(ct))
  t == 0
  0 == t
  ct == 0
  0 == ct
  t == ct
  ct == t
  t == zero(t)
  ct == zero(t)

  # Set scalar part so both TPSs are 1
  t[0] = 1
  ct[0] = 1

  isequal(t, 1)
  isequal(1, t)
  isequal(ct, 1)
  isequal(1, ct)
  t == 1
  1 == t
  ct == 1
  1 == ct

  # Check +, - unary functions and real, imag
  isequal(t, +t)
  t == +t
  !(t === +t)
  isequal(-1, -t)
  -1 == -t
  !(t === -t)

  isequal(ct, +ct)
  ct == +ct
  !(ct === +ct)
  isequal(-1, -ct)
  -t == -ct
  !(ct === -ct)

  isequal(t, real(t))
  isequal(real(t), ct)
  isequal(ct, real(ct))
  isequal(real(ct),1)
  isequal(imag(t), imag(ct))
  isequal(imag(ct), 0)
  !isequal(t,im) && !isequal(im, ct)
  t == real(t)
  real(t) == ct
  ct == real(ct)
  real(ct) == 1
  imag(t) == imag(ct)
  imag(ct) == 0
  t != im && ct != im

  ct == t
  t == ct
  ct+im != t
  t != ct+im
  ct+im == t+im
  t+im == ct+im
  
  # Set ct = im
  ct[0] = im
  isequal(ct, im)
  isequal(im, ct)
  isequal(real(ct), 0)
  isequal(imag(ct), t)
  isequal(t, 1)


  t1 = t
  t1[0] = 1
  t2 = zero(t1)
  t2[0] = 2
  t3 = zero(t1)
  t3[0] = 3

  t1 == 1
  1 == t1
  t1 != 2
  2 != t1
  t1 != t2
  t2 == 2
  2 == t2
  t3 == t2+t1
  t2 < t3
  t2 < 3
  t2 <= t3
  t2 <= 3
  t2+t1 <= t3
  3 <= t3

  !(t2 > t3)
  !(t2 > 3)
  !(t2 >= t3)
  !(t2 >= 3)

  t3 > t2
  t3 > 2
  t3 >= t2
  t3 >= 2
  t3 >= t1+t2
  t3 >= 3

  !(t3 < t2)
  !(t3 < 2)
  !(t3 <= t2)
  !(t3 <= 2)

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

  norm(tn) == sum([i for i in 1:6])
  norm(tcn) == sum([abs(i+i*im) for i in 1:6])

  # TPS:
  norm(t1 + t2 - t3) < tol
  norm(t2 + t1 - t3) < tol
  norm(t1 + 2 - t3) < tol
  norm(2 + t1 - t3) < tol
  norm(t3 - t2 - t1) < tol
  norm(t2 - t3 - -t1) < tol
  norm(t3 - 2 - t1) < tol
  norm(2 - t3 - -t1) < tol
  norm(t2 * t3 - 6) < tol
  norm(t3 * t2 - 6) < tol
  norm(t2 * 5 - 10) < tol
  norm(5 * t2 - 10 * t1) < tol
  norm(t1 / t2 - 1/2) < tol
  norm(t2 / t1 - 2) < tol
  norm(1 / t2 - 1/2) < tol
  norm(t2 / 3 - 2/3) < tol
  norm(t2 / t2 - t1) < tol
  norm(t2 / t2 - 1) < tol
  norm(t2 ^ t3 - 8) < tol
  norm(t3 ^ t2 - 9) < tol
  norm(t2 ^ 3 - 8) < tol
  norm(t2 ^ (1/2) - sqrt(2)) < tol
  norm(t2 ^ (1/2) - sqrt(t2)) < tol
  norm(2 ^ t3 - 8) < tol
  norm(inv(t3) - 1/t3) < tol
  norm(inv(t3) - 1/3) < tol

  # ComplexTPS:
  norm(ct1 + ct2 - ct3) < tol
  norm(ct2 + ct1 - ct3) < tol
  norm(ct1 + (2+2im) - ct3) < tol
  norm((2+2im) + ct1 - ct3) < tol
  norm(ct3 - ct2 - ct1) < tol
  norm(ct2 - ct3 - -ct1) < tol
  norm(ct3 - (2+2im) - ct1) < tol
  norm((2+2im) - ct3 - -ct1) < tol
  norm(ct2 * ct3 - (2+2im)*(3+3im)) < tol
  norm(ct3 * ct2 - (2+2im)*(3+3im)) < tol
  norm(ct2 * 5 - (10+10im)) < tol
  norm(5 * ct2 - (10 * ct1)) < tol
  norm(ct1 / ct2 - (1+im)/(2+2im)) < tol
  norm(ct2 / ct1 - 2) < tol
  norm(1 / ct2 - 1/(2+2im)) < tol
  norm(ct2 / 3 - (2+2im)/3) < tol
  norm(ct2 / ct2 - 1) < tol
  norm(ct2 ^ ct3 - (2+2im)^(3+3im)) < tol
  norm(ct3 ^ ct2 - (3+3im)^(2+2im)) < tol
  norm(ct2 ^ 3 - (2+2im)^3) < tol
  norm(ct2 ^ (1/2) - sqrt(2+2im)) < tol
  norm(ct2 ^ (1/2) - sqrt(ct2)) < tol
  norm(2 ^ ct3 - 2^(3+3im)) < tol
  norm(inv(ct3) - 1/ct3) < tol
  norm(inv(ct3) - 1/(3+3im)) < tol

  # Promotion of TPS to ComplexTPS
  norm(t1 + ct2 - (1 + (2+2im))) < tol
  norm(ct2 + t1 - (1 + (2+2im))) < tol
  norm(t1 + (2+2im) - (1 + (2+2im))) < tol
  norm((2+2im) + t1 - (1 + (2+2im))) < tol
  norm(t3 - ct2 - (3 - (2+2im))) < tol
  norm(ct2 - t3 - ((2+2im) - 3)) < tol
  norm(t3 - (2+2im) - (3 - (2+2im))) < tol
  norm((2+2im) - t3 - ((2+2im) - 3)) < tol
  norm(t2 * ct3 - 2 * (3+3im)) < tol
  norm(ct3 * t2 - 2 * (3+3im)) < tol
  norm(t2 * (3+3im) - 2 * (3+3im)) < tol
  norm((3+3im) * t2 - 2 * (3+3im)) < tol
  norm(t2 / ct3 - 2/(3+3im)) < tol
  norm(ct3 / t2 - (3+3im)/2) < tol
  norm(t2 / (3+3im) - 2/(3+3im)) < tol
  norm((3+3im) / t2 - (3+3im)/2) < tol
  norm(t2 ^ ct3 - 2^(3+3im)) < tol
  norm(ct3 ^ t2 - (3+3im)^2) < tol
  norm(t2 ^ (3+3im) - 2^(3+3im)) < tol
  norm((3+3im)^t2 - (3+3im)^2) < tol

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


  norm(abs(-t) - abs(-v) ) < tol
  norm(sqrt(t) - sqrt(v)) < tol
  norm(exp(t) - exp(v)) < tol
  norm(log(t) - log(v)) < tol
  norm(sin(t) - sin(v)) < tol
  norm(cos(t) - cos(v)) < tol
  norm(tan(t) - tan(v)) < tol
  norm(csc(t) - csc(v)) < tol
  norm(sec(t) - sec(v)) < tol
  norm(cot(t) - cot(v)) < tol
  norm(sinc(t) - sinc(v)) < tol
  norm(sinh(t) - sinh(v)) < tol
  norm(cosh(t) - cosh(v)) < tol
  norm(tanh(t) - tanh(v)) < tol
  norm(csch(t) - csch(v)) < tol
  norm(sech(t) - sech(v)) < tol
  norm(coth(t) - coth(v)) < tol
  norm(asin(t) - asin(v)) < tol
  norm(acos(t) - acos(v)) < tol
  norm(atan(t) - atan(v)) < tol
  norm(acsc(1/t) - acsc(1/v)) < tol
  norm(asec(1/t) - asec(1/v)) < tol
  norm(acot(1/t) - acot(1/v)) < tol
  norm(asinh(t) - asinh(v)) < tol
  norm(acosh(1/t) - acosh(1/v)) < tol
  norm(atanh(t) - atanh(v)) < tol
  norm(acsch(1/t) - acsch(1/v)) < tol
  norm(asech(t) - asech(v)) < tol
  norm(acoth(1/t) - acoth(1/v)) < tol
  norm(asinc(t/pi) - asin(v)/(v)) < tol
  norm(asinhc(t/pi) - asinh(v)/(v)) < tol
  norm(zero(t) - zero(v)) < tol
  norm(real(t) - real(v)) < tol
  norm(imag(t) - imag(v)) < tol
  norm(conj(t) - conj(v)) < tol
  norm(sinhc(t/pi) - sinh(v)/v) < tol
  norm(erf(t) - erf(v)) < tol
  norm(erfc(t) - erfc(v)) < tol
  norm(-im*erf(t*im) - erfi(v)) < tol
  
  norm(atan(t3,t2) - atan(3,2)) < tol
  norm(atan(t3,2) - atan(3,2)) < tol
  norm(atan(3,t2) - atan(3,2)) < tol
  norm(atan(t3,-t2) - atan(3,-2)) < tol
  norm(atan(t3,-2) - atan(3,-2)) < tol
  norm(atan(3,-t2) - atan(3,-2)) < tol
  norm(atan(-t3,-t2) - atan(-3,-2)) < tol
  norm(atan(-t3,-2) - atan(-3,-2)) < tol
  norm(atan(-3,-t2) - atan(-3,-2)) < tol
  norm(atan(-t3,t2) - atan(-3,2)) < tol
  norm(atan(-t3,2) - atan(-3,2)) < tol
  norm(atan(-3,t2) - atan(-3,2)) < tol
  
  norm(hypot(t2,t3) - hypot(2,3)) < tol
  norm(hypot(2,t3) - hypot(2,3)) < tol
  norm(hypot(t2,3) - hypot(2,3)) < tol
  norm(hypot(t1,t2,t3) - hypot(1,2,3)) < tol
  norm(hypot(1, t2, t3) - hypot(1,2,3)) < tol
  norm(hypot(t1, 2, t3) - hypot(1,2,3)) < tol
  norm(hypot(t1, t2, 3) - hypot(1,2,3)) < tol
  norm(hypot(1, 2, t3) - hypot(1,2,3)) < tol
  norm(hypot(1, t2, 3) - hypot(1,2,3)) < tol
  norm(hypot(t1, 2, 3) - hypot(1,2,3)) < tol
  norm(angle(t2) - angle(2)) < tol
  norm(angle(-t2) - angle(-2)) < tol
  norm(complex(t3) - complex(3)) < tol
  norm(complex(t2,t3) - complex(2,3)) < tol
  norm(polar(t2) - (abs(2)+im*atan(0,2))) < tol
  norm(polar(-t1) - (abs(-1)+im*atan(0,-1))) < tol
  norm(rect(t2) - (2*cos(0) + im*2*sin(0))) < tol
  norm(rect(-t1) - (-1*cos(0) + im*-1*sin(0))) < tol
  

  v = 0.5+0.5im
  t = ComplexTPS(t)
  t[0] = v
  ct1 = ComplexTPS(t)
  ct1[0] = 1 + 1im
  ct2 = zero(ct1)
  ct2[0] = 2 + 2im
  ct3 = zero(ct1)
  ct3[0] = 3 + 3im
  norm(abs(-t) - abs(-v) ) < tol
  norm(sqrt(t) - sqrt(v)) < tol
  norm(exp(t) - exp(v)) < tol
  norm(log(t) - log(v)) < tol
  norm(sin(t) - sin(v)) < tol
  norm(cos(t) - cos(v)) < tol
  norm(tan(t) - tan(v)) < tol
  norm(csc(t) - csc(v)) < tol
  norm(sec(t) - sec(v)) < tol
  norm(cot(t) - cot(v)) < tol
  norm(sinc(t) - sinc(v)) < tol
  norm(sinh(t) - sinh(v)) < tol
  norm(cosh(t) - cosh(v)) < tol
  norm(tanh(t) - tanh(v)) < tol
  norm(csch(t) - csch(v)) < tol
  norm(sech(t) - sech(v)) < tol
  norm(coth(t) - coth(v)) < tol

  norm(asin(t) - asin(v)) < tol
  norm(acos(t) - acos(v)) < tol
  norm(atan(t) - atan(v)) < tol
  norm(acsc(t) - acsc(v)) < tol
  norm(asec(t) - asec(v)) < tol
  norm(acot(t) - acot(v)) < tol
  norm(asinh(t) - asinh(v)) < tol
  norm(acosh(t) - acosh(v)) < tol
  norm(atanh(t) - atanh(v)) < tol
  norm(acsch(t) - acsch(v)) < tol
  norm(asech(t) - asech(v)) < tol
  norm(acoth(t) - acoth(v)) < tol
  norm(asinc(t/pi) - asin(v)/v) < tol
  norm(asinhc(t/pi) - asinh(v)/v) < tol

  norm(zero(t) - zero(v)) < tol
  norm(real(t) - real(v)) < tol
  norm(imag(t) - imag(v)) < tol
  norm(conj(t) - conj(v)) < tol
  norm(sinhc(t/pi) - sinh(v)/v) < tol
  norm(erf(t) - erf(v)) < tol
  norm(erfc(t) - erfc(v)) < tol
  norm(-im*erf(t*im) - erfi(v)) < tol
  norm(hypot(ct2,ct3) - hypot(2+2im,3+3im)) < tol
  norm(hypot(2+2im,ct3) - hypot(2+2im,3+3im)) < tol
  norm(hypot(ct2,3+3im) - hypot(2+2im,3+3im)) < tol
  norm(hypot(ct1,ct2,ct3) - hypot(1+1im,2+2im,3+3im)) < tol
  norm(hypot(1+1im, ct2, ct3) - hypot(1+1im,2+2im,3+3im)) < tol
  norm(hypot(ct1, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)) < tol
  norm(hypot(ct1, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)) < tol
  norm(hypot(1+1im, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)) < tol
  norm(hypot(1+1im, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)) < tol
  norm(hypot(ct1, 2+2im, 3+3im) - hypot(1+1im,2+2im,3+3im)) < tol
  
  norm(angle(t2+im*t3) - angle(2+3im)) < tol
  norm(angle(t2-im*t3) - angle(2-3im)) < tol
  norm(angle(-t2-im*t3) - angle(-2-3im)) < tol
  norm(angle(-t2+im*t3) - angle(-2+3im)) < tol
  norm(angle(ct2) - angle(2+2im)) < tol
  norm(angle(-ct2) - angle(-2-2im)) < tol
  norm(complex(ct3) - complex(3+3im)) < tol
  norm(polar(ct2) - (abs(2+2im)+im*angle(2+2im))) < tol
  norm(polar(-ct1) - (abs(-1-im)+im*angle(-1-im))) < tol
  norm(rect(ct2) - (2*cos(2) + im*2*sin(2))) < tol
  norm(rect(-ct1) - (-1*cos(-1) + im*-1*sin(-1))) < tol
  
  # Hypot, mixing TPS with ComplexTPS
  norm(hypot(ct1, ct2, t3) - hypot(1+1im,2+2im,3)) < tol
  norm(hypot(ct1, t2, ct3) - hypot(1+1im,2,3+3im)) < tol
  norm(hypot(t1, ct2, ct3) - hypot(1,2+2im,3+3im)) < tol
  norm(hypot(ct1, t2, t3) - hypot(1+1im,2,3)) < tol
  norm(hypot(t1, ct2, t3) - hypot(1,2+2im,3)) < tol
  norm(hypot(t1, t2, ct3) - hypot(1,2,3+3im)) < tol
  norm(hypot(ct1,t2,3+3im) - hypot(1+1im,2,3+3im)) < tol
  norm(hypot(t1, ct2, 3+3im) - hypot(1,2+2im,3+3im)) < tol
  norm(hypot(ct1,2+2im,t3) - hypot(1+1im,2+2im,3)) < tol
  norm(hypot(t1,2+2im,ct3) - hypot(1,2+2im,3+3im)) < tol
  norm(hypot(1+1im,ct2,t3) - hypot(1+1im,2+2im,3)) < tol
  norm(hypot(1+1im, t2, ct3) - hypot(1+1im,2,3+3im)) < tol
  norm(hypot(t1,t2,3+3im) - hypot(1,2,3+3im)) < tol
  norm(hypot(t1,2+2im,t3) - hypot(1,2+2im,3)) < tol
  norm(hypot(1+1im,t2,t3) - hypot(1+1im,2,3)) < tol
  norm(hypot(t1,2,3+3im) - hypot(1,2,3+3im)) < tol
  norm(hypot(1,t2,3+3im) - hypot(1,2,3+3im)) < tol
  norm(hypot(1+1im,2,t3) - hypot(1+1im,2,3)) < tol

  d = Descriptor(1, 5)
  t = TPS(use=d)
  t[0] = 0.5; t[[1]] = 2; t[[2]] = 3; t[[3]] = 4; t[[4]] = 5; t[[5]] = 6

  tol = 1e-10

  norm(sin(t)^2+cos(t)^2 - 1) < tol
  norm(1/sin(t) - csc(t)) < tol
  norm(1/cos(t) - sec(t)) < tol
  norm(1/tan(t) - cot(t)) < tol
  norm(sin(t)/cos(t) - tan(t)) < tol
  norm(cos(2*t) - cos(t)^2 + sin(t)^2) < tol
  norm(sec(t)^2 - 1 - tan(t)^2) < tol
  norm(sin(t/2) - sqrt((1-cos(t))/2)) < tol
  norm(cos(t/2) - sqrt((1+cos(t))/2)) < tol
  norm(sqrt(t^2) - abs(t)) < tol
  norm(csc(t)^2 - cot(t)^2 - 1) < tol
  norm(exp(log(t)) - t) < tol
  norm(log(exp(t)) - t) < tol
  norm(log(exp(t)) - exp(log(t))) < tol
  norm(log(t^2) - 2*log(t)) < tol
  norm(5*log(t) - log(t^5)) < tol
  norm(t*log(5) - log(5^t)) < tol
  norm(sinc(t) - sin(pi*t)/(pi*t)) < tol
  norm(sinhc(t/pi) - sinh(t)/t) < tol
  norm(exp(im*t) - cos(t) - im*sin(t)) < tol
  norm(real(exp(im*t)) - cos(t)) < tol
  norm(imag(exp(im*t)) - sin(t)) < tol
  norm(sinh(t) - (exp(t) - exp(-t))/2) < tol
  norm(cosh(t) - (exp(t) + exp(-t))/2) < tol
  norm(tanh(t) - sinh(t)/cosh(t)) < tol
  norm(csch(t) - 1/sinh(t)) < tol
  norm(sech(t) - 1/cosh(t)) < tol
  norm(coth(t) - cosh(t)/sinh(t)) < tol
  norm(coth(t) - 1/tanh(t)) < tol
  norm(cosh(t)^2 - sinh(t)^2 - 1) < tol
  norm(1 - tanh(t)^2 - sech(t)^2) < tol
  norm(coth(t)^2 - 1 - csch(t)^2) < tol
  norm(asin(sin(t)) - t) < tol
  norm(acos(cos(t)) - t) < tol
  norm(atan(tan(t)) - t) < tol
  norm(acsc(1/t) - asin(t)) < tol
  norm(asec(1/t) - acos(t)) < tol
  norm(acot(1/t) - atan(t)) < tol
  norm(asinh(sinh(t)) - t) < tol
  norm(acosh(cosh(t)) - t) < tol
  norm(atanh(tanh(t)) - t) < tol
  norm(acsch(t) - asinh(1/t)) < tol
  norm(asech(t) - acosh(1/t)) < tol
  norm(acoth(1/t) - atanh(t)) < tol
  norm(asinc(t/pi) - asin(t)/t) < tol
  norm(asinhc(t/pi) - asinh(t)/t) < tol
  norm(erfc(t) - 1 + erf(t)) < tol
  norm(erf(-t) + erf(t)) < tol
  norm(angle(t)) < tol
  norm(complex(t) - t) < tol
  norm(complex(t,t) - (t+im*t)) < tol

  t = ComplexTPS(t)
  t[0] = 0.5+0.5im; t[[1]] = 2+2im; t[[2]] = 3+3im; t[[3]] = 4+4im; t[[4]] = 5+5im; t[[5]] = 6+6im
  norm(sin(t)^2+cos(t)^2 - 1) < tol
  norm(1/sin(t) - csc(t)) < tol
  norm(1/cos(t) - sec(t)) < tol
  norm(1/tan(t) - cot(t)) < tol
  norm(sin(t)/cos(t) - tan(t)) < tol
  norm(cos(2*t) - cos(t)^2 + sin(t)^2) < tol
  norm(sec(t)^2 - 1 - tan(t)^2) < tol
  norm(sin(t/2) - sqrt((1-cos(t))/2)) < tol
  norm(cos(t/2) - sqrt((1+cos(t))/2)) < tol
  norm(sqrt(t^2) - t) < tol
  norm(csc(t)^2 - cot(t)^2 - 1) < tol
  norm(exp(log(t)) - t) < tol
  norm(log(exp(t)) - t) < tol
  norm(log(exp(t)) - exp(log(t))) < tol
  norm(log(t^2) - 2*log(t)) < tol
  norm(5*log(t) - log(t^5) - 2*pi*im) < tol
  norm(t*log(5) - log(5^t)) < tol
  norm(sinc(t/pi) - sin(t)/t) < tol
  norm(sinhc(t/pi) - sinh(t)/t) < tol
  norm(exp(im*t) - cos(t) - im*sin(t)) < tol
  norm(sinh(t) - (exp(t) - exp(-t))/2) < tol
  norm(cosh(t) - (exp(t) + exp(-t))/2) < tol
  norm(tanh(t) - sinh(t)/cosh(t)) < tol
  norm(csch(t) - 1/sinh(t)) < tol
  norm(sech(t) - 1/cosh(t)) < tol
  norm(coth(t) - cosh(t)/sinh(t)) < tol
  norm(coth(t) - 1/tanh(t)) < tol
  norm(cosh(t)^2 - sinh(t)^2 - 1) < tol
  norm(1 - tanh(t)^2 - sech(t)^2) < tol
  norm(coth(t)^2 - 1 - csch(t)^2) < tol
  
  norm(asin(sin(t)) - t) < tol
  norm(acos(cos(t)) - t) < tol
  norm(atan(tan(t)) - t) < tol
  norm(acsc(t) - asin(1/t)) < tol
  norm(asec(t) - acos(1/t)) < tol
  norm(acot(t) - atan(1/t)) < tol
  norm(asinh(sinh(t)) - t) < tol
  norm(acosh(cosh(t)) - t) < tol
  norm(atanh(tanh(t)) - t) < tol
  norm(acsch(t) - asinh(1/t)) < tol
  norm(asech(t) - acosh(1/t)) < tol
  norm(acoth(t) - atanh(1/t)) < tol
  norm(asinc(t/pi) - asin(t)/t) < tol
  norm(asinhc(t/pi) - asinh(t)/t) < tol
  
  norm(erfc(t) - 1 + erf(t)) < tol
  norm(erf(-t) + erf(t)) < tol
  norm(angle(t) - atan(imag(t),real(t))) < tol
  norm(complex(t) - t) < tol

  d = Descriptor(3,10,2,10)
  v = vars(d)
  p = params(d)
  tol = 1e-18

  f = sin(v[1])
  abs(f[[0]] - 0) < tol
  abs(f[[1]] - 1) < tol
  abs(f[[2]] - 0) < tol
  abs(f[[3]] - -1/factorial(3)) < tol
  abs(f[[4]] - 0) < tol
  abs(f[[5]] - 1/factorial(5)) < tol
  abs(f[[6]] - 0) < tol
  abs(f[[7]] - -1/factorial(7)) < tol
  abs(f[[8]] - 0) < tol
  abs(f[[9]] - 1/factorial(9)) < tol
  abs(f[[10]] - 0) < tol

  abs(f[[1=>1]] - f[[1]]) < tol
  abs(f[[1=>2]] - f[[2]]) < tol
  abs(f[[1=>3]] - f[[3]]) < tol
  abs(f[[1=>4]] - f[[4]]) < tol
  abs(f[[1=>5]] - f[[5]]) < tol
  abs(f[[1=>6]] - f[[6]]) < tol
  abs(f[[1=>7]] - f[[7]]) < tol
  abs(f[[1=>8]] - f[[8]]) < tol
  abs(f[[1=>9]] - f[[9]]) < tol
  abs(f[[1=>10]] - f[[10]]) < tol

  fc = complex(f)
  abs(fc[[0]] - 0) < tol
  abs(fc[[1]] - 1) < tol
  abs(fc[[2]] - 0) < tol
  abs(fc[[3]] - -1/factorial(3)) < tol
  abs(fc[[4]] - 0) < tol
  abs(fc[[5]] - 1/factorial(5)) < tol
  abs(fc[[6]] - 0) < tol
  abs(fc[[7]] - -1/factorial(7)) < tol
  abs(fc[[8]] - 0) < tol
  abs(fc[[9]] - 1/factorial(9)) < tol
  abs(fc[[10]] - 0) < tol

  abs(fc[[1=>1]] - f[[1]]) < tol
  abs(fc[[1=>2]] - f[[2]]) < tol
  abs(fc[[1=>3]] - f[[3]]) < tol
  abs(fc[[1=>4]] - f[[4]]) < tol
  abs(fc[[1=>5]] - f[[5]]) < tol
  abs(fc[[1=>6]] - f[[6]]) < tol
  abs(fc[[1=>7]] - f[[7]]) < tol
  abs(fc[[1=>8]] - f[[8]]) < tol
  abs(fc[[1=>9]] - f[[9]]) < tol
  abs(fc[[1=>10]] - f[[10]]) < tol

  f2 = sin(v[1]) + cos(v[2])
  abs(f2[[0]] - 1) < tol
  abs(f2[[1]] - 1) < tol
  abs(f2[[2]] - 0) < tol
  abs(f2[[3]] - -1/factorial(3)) < tol
  abs(f2[[4]] - 0) < tol
  abs(f2[[5]] - 1/factorial(5)) < tol
  abs(f2[[6]] - 0) < tol
  abs(f2[[7]] - -1/factorial(7)) < tol
  abs(f2[[8]] - 0) < tol
  abs(f2[[9]] - 1/factorial(9)) < tol
  abs(f2[[10]] - 0) < tol

  abs(f2[[0,0]] - 1) < tol
  abs(f2[[0,1]] - 0) < tol
  abs(f2[[0,2]] - -1/factorial(2)) < tol
  abs(f2[[0,3]] - 0) < tol
  abs(f2[[0,4]] - 1/factorial(4)) < tol
  abs(f2[[0,5]] - 0) < tol
  abs(f2[[0,6]] - -1/factorial(6)) < tol
  abs(f2[[0,7]] - 0) < tol
  abs(f2[[0,8]] - 1/factorial(8)) < tol
  abs(f2[[0,9]] - 0) < tol
  abs(f2[[0,10]] - -1/factorial(10)) < tol

  abs(f2[[1=>1]] - f2[[1]]) < tol
  abs(f2[[1=>2]] - f2[[2]]) < tol
  abs(f2[[1=>3]] - f2[[3]]) < tol
  abs(f2[[1=>4]] - f2[[4]]) < tol
  abs(f2[[1=>5]] - f2[[5]]) < tol
  abs(f2[[1=>6]] - f2[[6]]) < tol
  abs(f2[[1=>7]] - f2[[7]]) < tol
  abs(f2[[1=>8]] - f2[[8]]) < tol
  abs(f2[[1=>9]] - f2[[9]]) < tol
  abs(f2[[1=>10]] - f2[[10]]) < tol

  abs(f2[[2=>1]] - f2[[0,1]]) < tol
  abs(f2[[2=>2]] - f2[[0,2]]) < tol
  abs(f2[[2=>3]] - f2[[0,3]]) < tol
  abs(f2[[2=>4]] - f2[[0,4]]) < tol
  abs(f2[[2=>5]] - f2[[0,5]]) < tol
  abs(f2[[2=>6]] - f2[[0,6]]) < tol
  abs(f2[[2=>7]] - f2[[0,7]]) < tol
  abs(f2[[2=>8]] - f2[[0,8]]) < tol
  abs(f2[[2=>9]] - f2[[0,9]]) < tol
  abs(f2[[2=>10]] - f2[[0,10]]) < tol

  f2c = complex(sin(v[1]) + cos(v[2]))
  abs(f2c[[0]] - 1) < tol
  abs(f2c[[1]] - 1) < tol
  abs(f2c[[2]] - 0) < tol
  abs(f2c[[3]] - -1/factorial(3)) < tol
  abs(f2c[[4]] - 0) < tol
  abs(f2c[[5]] - 1/factorial(5)) < tol
  abs(f2c[[6]] - 0) < tol
  abs(f2c[[7]] - -1/factorial(7)) < tol
  abs(f2c[[8]] - 0) < tol
  abs(f2c[[9]] - 1/factorial(9)) < tol
  abs(f2c[[10]] - 0) < tol

  abs(f2c[[0,0]] - 1) < tol
  abs(f2c[[0,1]] - 0) < tol
  abs(f2c[[0,2]] - -1/factorial(2)) < tol
  abs(f2c[[0,3]] - 0) < tol
  abs(f2c[[0,4]] - 1/factorial(4)) < tol
  abs(f2c[[0,5]] - 0) < tol
  abs(f2c[[0,6]] - -1/factorial(6)) < tol
  abs(f2c[[0,7]] - 0) < tol
  abs(f2c[[0,8]] - 1/factorial(8)) < tol
  abs(f2c[[0,9]] - 0) < tol
  abs(f2c[[0,10]] - -1/factorial(10)) < tol

  abs(f2c[[1=>1]] - f2c[[1]]) < tol
  abs(f2c[[1=>2]] - f2c[[2]]) < tol
  abs(f2c[[1=>3]] - f2c[[3]]) < tol
  abs(f2c[[1=>4]] - f2c[[4]]) < tol
  abs(f2c[[1=>5]] - f2c[[5]]) < tol
  abs(f2c[[1=>6]] - f2c[[6]]) < tol
  abs(f2c[[1=>7]] - f2c[[7]]) < tol
  abs(f2c[[1=>8]] - f2c[[8]]) < tol
  abs(f2c[[1=>9]] - f2c[[9]]) < tol
  abs(f2c[[1=>10]] - f2c[[10]]) < tol

  abs(f2c[[2=>1]] - f2c[[0,1]]) < tol
  abs(f2c[[2=>2]] - f2c[[0,2]]) < tol
  abs(f2c[[2=>3]] - f2c[[0,3]]) < tol
  abs(f2c[[2=>4]] - f2c[[0,4]]) < tol
  abs(f2c[[2=>5]] - f2c[[0,5]]) < tol
  abs(f2c[[2=>6]] - f2c[[0,6]]) < tol
  abs(f2c[[2=>7]] - f2c[[0,7]]) < tol
  abs(f2c[[2=>8]] - f2c[[0,8]]) < tol
  abs(f2c[[2=>9]] - f2c[[0,9]]) < tol
  abs(f2c[[2=>10]] - f2c[[0,10]]) < tol

  f3 = sin(v[1]) + cos(v[2]) + exp(p[1])
  abs(f3[[0]] - 2) < tol
  abs(f3[[1]] - 1) < tol
  abs(f3[[2]] - 0) < tol
  abs(f3[[3]] - -1/factorial(3)) < tol
  abs(f3[[4]] - 0) < tol
  abs(f3[[5]] - 1/factorial(5)) < tol
  abs(f3[[6]] - 0) < tol
  abs(f3[[7]] - -1/factorial(7)) < tol
  abs(f3[[8]] - 0) < tol
  abs(f3[[9]] - 1/factorial(9)) < tol
  abs(f3[[10]] - 0) < tol

  abs(f3[[0,0]] - 2) < tol
  abs(f3[[0,1]] - 0) < tol
  abs(f3[[0,2]] - -1/factorial(2)) < tol
  abs(f3[[0,3]] - 0) < tol
  abs(f3[[0,4]] - 1/factorial(4)) < tol
  abs(f3[[0,5]] - 0) < tol
  abs(f3[[0,6]] - -1/factorial(6)) < tol
  abs(f3[[0,7]] - 0) < tol
  abs(f3[[0,8]] - 1/factorial(8)) < tol
  abs(f3[[0,9]] - 0) < tol
  abs(f3[[0,10]] - -1/factorial(10)) < tol

  abs(f3[[0,0,0,0]] - 2) < tol
  abs(f3[[0,0,0,1]] - 1/factorial(1)) < tol
  abs(f3[[0,0,0,2]] - 1/factorial(2)) < tol
  abs(f3[[0,0,0,3]] - 1/factorial(3)) < tol
  abs(f3[[0,0,0,4]] - 1/factorial(4)) < tol
  abs(f3[[0,0,0,5]] - 1/factorial(5)) < tol
  abs(f3[[0,0,0,6]] - 1/factorial(6)) < tol
  abs(f3[[0,0,0,7]] - 1/factorial(7)) < tol
  abs(f3[[0,0,0,8]] - 1/factorial(8)) < tol
  abs(f3[[0,0,0,9]] - 1/factorial(9)) < tol
  abs(f3[[0,0,0,10]] - 1/factorial(10)) < tol

  abs(f3[[1=>1]] - f3[[1]]) < tol
  abs(f3[[1=>2]] - f3[[2]]) < tol
  abs(f3[[1=>3]] - f3[[3]]) < tol
  abs(f3[[1=>4]] - f3[[4]]) < tol
  abs(f3[[1=>5]] - f3[[5]]) < tol
  abs(f3[[1=>6]] - f3[[6]]) < tol
  abs(f3[[1=>7]] - f3[[7]]) < tol
  abs(f3[[1=>8]] - f3[[8]]) < tol
  abs(f3[[1=>9]] - f3[[9]]) < tol
  abs(f3[[1=>10]] - f3[[10]]) < tol

  abs(f3[[2=>1]]- f3[[0,1]]) < tol
  abs(f3[[2=>2]]- f3[[0,2]]) < tol
  abs(f3[[2=>3]]- f3[[0,3]]) < tol
  abs(f3[[2=>4]]- f3[[0,4]]) < tol
  abs(f3[[2=>5]]- f3[[0,5]]) < tol
  abs(f3[[2=>6]]- f3[[0,6]]) < tol
  abs(f3[[2=>7]]- f3[[0,7]]) < tol
  abs(f3[[2=>8]]- f3[[0,8]]) < tol
  abs(f3[[2=>9]]- f3[[0,9]]) < tol
  abs(f3[[2=>10]]- f3[[0,10]]) < tol

  abs(f3[params=[1=>1]] - f3[[0,0,0,1]]) < tol
  abs(f3[params=[1=>2]] - f3[[0,0,0,2]]) < tol
  abs(f3[params=[1=>3]] - f3[[0,0,0,3]]) < tol
  abs(f3[params=[1=>4]] - f3[[0,0,0,4]]) < tol
  abs(f3[params=[1=>5]] - f3[[0,0,0,5]]) < tol
  abs(f3[params=[1=>6]] - f3[[0,0,0,6]]) < tol
  abs(f3[params=[1=>7]] - f3[[0,0,0,7]]) < tol
  abs(f3[params=[1=>8]] - f3[[0,0,0,8]]) < tol
  abs(f3[params=[1=>9]] - f3[[0,0,0,9]]) < tol
  abs(f3[params=[1=>10]] - f3[[0,0,0,10]]) < tol

  d = Descriptor(1, 5)
  t = TPS(use=d)
  ct = ComplexTPS(t)
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
  @FastGTPSA(norm(t1 + t2 - t3)) < tol
  @FastGTPSA(norm(t2 + t1 - t3)) < tol
  @FastGTPSA(norm(t1 + 2 - t3)) < tol
  @FastGTPSA(norm(2 + t1 - t3)) < tol
  @FastGTPSA(norm(t3 - t2 - t1)) < tol
  @FastGTPSA(norm(t2 - t3 - -t1)) < tol
  @FastGTPSA(norm(t3 - 2 - t1)) < tol
  @FastGTPSA(norm(2 - t3 - -t1)) < tol
  @FastGTPSA(norm(t2 * t3 - 6)) < tol
  @FastGTPSA(norm(t3 * t2 - 6)) < tol
  @FastGTPSA(norm(t2 * 5 - 10)) < tol
  @FastGTPSA(norm(5 * t2 - 10 * t1)) < tol
  @FastGTPSA(norm(t1 / t2 - 1/2)) < tol
  @FastGTPSA(norm(t2 / t1 - 2)) < tol
  @FastGTPSA(norm(1 / t2 - 1/2)) < tol
  @FastGTPSA(norm(t2 / 3 - 2/3)) < tol
  @FastGTPSA(norm(t2 / t2 - t1)) < tol
  @FastGTPSA(norm(t2 / t2 - 1)) < tol
  @FastGTPSA(norm(t2 ^ t3 - 8)) < tol
  @FastGTPSA(norm(t3 ^ t2 - 9)) < tol
  @FastGTPSA(norm(t2 ^ 3 - 8)) < tol
  @FastGTPSA(norm(t2 ^ (1/2) - sqrt(2))) < tol
  @FastGTPSA(norm(t2 ^ (1/2) - sqrt(t2))) < tol
  @FastGTPSA(norm(2 ^ t3 - 8)) < tol
  @FastGTPSA(norm(inv(t3) - 1/t3)) < tol
  @FastGTPSA(norm(inv(t3) - 1/3)) < tol

  # ComplexTPS:
  @FastGTPSA(norm(ct1 + ct2 - ct3)) < tol
  @FastGTPSA(norm(ct2 + ct1 - ct3)) < tol
  @FastGTPSA(norm(ct1 + (2+2im) - ct3)) < tol
  @FastGTPSA(norm((2+2im) + ct1 - ct3)) < tol
  @FastGTPSA(norm(ct3 - ct2 - ct1)) < tol
  @FastGTPSA(norm(ct2 - ct3 - -ct1)) < tol
  @FastGTPSA(norm(ct3 - (2+2im) - ct1)) < tol
  @FastGTPSA(norm((2+2im) - ct3 - -ct1)) < tol
  @FastGTPSA(norm(ct2 * ct3 - (2+2im)*(3+3im))) < tol
  @FastGTPSA(norm(ct3 * ct2 - (2+2im)*(3+3im))) < tol
  @FastGTPSA(norm(ct2 * 5 - (10+10im))) < tol
  @FastGTPSA(norm(5 * ct2 - (10 * ct1))) < tol
  @FastGTPSA(norm(ct1 / ct2 - (1+im)/(2+2im))) < tol
  @FastGTPSA(norm(ct2 / ct1 - 2)) < tol
  @FastGTPSA(norm(1 / ct2 - 1/(2+2im))) < tol
  @FastGTPSA(norm(ct2 / 3 - (2+2im)/3)) < tol
  @FastGTPSA(norm(ct2 / ct2 - 1)) < tol
  @FastGTPSA(norm(ct2 ^ ct3 - (2+2im)^(3+3im))) < tol
  @FastGTPSA(norm(ct3 ^ ct2 - (3+3im)^(2+2im))) < tol
  @FastGTPSA(norm(ct2 ^ 3 - (2+2im)^3)) < tol
  @FastGTPSA(norm(ct2 ^ (1/2) - sqrt(2+2im))) < tol
  @FastGTPSA(norm(ct2 ^ (1/2) - sqrt(ct2))) < tol
  @FastGTPSA(norm(2 ^ ct3 - 2^(3+3im))) < tol
  @FastGTPSA(norm(inv(ct3) - 1/ct3)) < tol
  @FastGTPSA(norm(inv(ct3) - 1/(3+3im))) < tol

  # Promotion of TPS to ComplexTPS
  @FastGTPSA(norm(t1 + ct2 - (1 + (2+2im)))) < tol
  @FastGTPSA(norm(ct2 + t1 - (1 + (2+2im)))) < tol
  @FastGTPSA(norm(t1 + (2+2im) - (1 + (2+2im)))) < tol
  @FastGTPSA(norm((2+2im) + t1 - (1 + (2+2im)))) < tol
  @FastGTPSA(norm(t3 - ct2 - (3 - (2+2im)))) < tol
  @FastGTPSA(norm(ct2 - t3 - ((2+2im) - 3))) < tol
  @FastGTPSA(norm(t3 - (2+2im) - (3 - (2+2im)))) < tol
  @FastGTPSA(norm((2+2im) - t3 - ((2+2im) - 3))) < tol
  @FastGTPSA(norm(t2 * ct3 - 2 * (3+3im))) < tol
  @FastGTPSA(norm(ct3 * t2 - 2 * (3+3im))) < tol
  @FastGTPSA(norm(t2 * (3+3im) - 2 * (3+3im))) < tol
  @FastGTPSA(norm((3+3im) * t2 - 2 * (3+3im))) < tol
  @FastGTPSA(norm(t2 / ct3 - 2/(3+3im))) < tol
  @FastGTPSA(norm(ct3 / t2 - (3+3im)/2)) < tol
  @FastGTPSA(norm(t2 / (3+3im) - 2/(3+3im))) < tol
  @FastGTPSA(norm((3+3im) / t2 - (3+3im)/2)) < tol
  @FastGTPSA(norm(t2 ^ ct3 - 2^(3+3im))) < tol
  @FastGTPSA(norm(ct3 ^ t2 - (3+3im)^2)) < tol
  @FastGTPSA(norm(t2 ^ (3+3im) - 2^(3+3im))) < tol
  @FastGTPSA(norm((3+3im)^t2 - (3+3im)^2)) < tol

  # Make sure stack is 0:
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t1.tpsa).d))
  tmpidx = unsafe_load(desc.ti)
  ctmpidx = unsafe_load(desc.cti)
  ctmpidx == 0
  tmpidx == 0

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


  @FastGTPSA(norm(abs(-t) - abs(-v) )) < tol
  @FastGTPSA(norm(sqrt(t) - sqrt(v))) < tol
  @FastGTPSA(norm(exp(t) - exp(v))) < tol
  @FastGTPSA(norm(log(t) - log(v))) < tol
  @FastGTPSA(norm(sin(t) - sin(v))) < tol
  @FastGTPSA(norm(cos(t) - cos(v))) < tol
  @FastGTPSA(norm(tan(t) - tan(v))) < tol
  @FastGTPSA(norm(csc(t) - csc(v))) < tol
  @FastGTPSA(norm(sec(t) - sec(v))) < tol
  @FastGTPSA(norm(cot(t) - cot(v))) < tol
  @FastGTPSA(norm(sinc(t) - sinc(v))) < tol
  @FastGTPSA(norm(sinh(t) - sinh(v))) < tol
  @FastGTPSA(norm(cosh(t) - cosh(v))) < tol
  @FastGTPSA(norm(tanh(t) - tanh(v))) < tol
  @FastGTPSA(norm(csch(t) - csch(v))) < tol
  @FastGTPSA(norm(sech(t) - sech(v))) < tol
  @FastGTPSA(norm(coth(t) - coth(v))) < tol
  @FastGTPSA(norm(asin(t) - asin(v))) < tol
  @FastGTPSA(norm(acos(t) - acos(v))) < tol
  @FastGTPSA(norm(atan(t) - atan(v))) < tol
  @FastGTPSA(norm(acsc(1/t) - acsc(1/v))) < tol
  @FastGTPSA(norm(asec(1/t) - asec(1/v))) < tol
  @FastGTPSA(norm(acot(1/t) - acot(1/v))) < tol
  @FastGTPSA(norm(asinh(t) - asinh(v))) < tol
  @FastGTPSA(norm(acosh(1/t) - acosh(1/v))) < tol
  @FastGTPSA(norm(atanh(t) - atanh(v))) < tol
  @FastGTPSA(norm(acsch(1/t) - acsch(1/v))) < tol
  @FastGTPSA(norm(asech(t) - asech(v))) < tol
  @FastGTPSA(norm(acoth(1/t) - acoth(1/v))) < tol
  @FastGTPSA(norm(asinc(t/pi) - asin(v)/(v))) < tol
  @FastGTPSA(norm(asinhc(t/pi) - asinh(v)/(v))) < tol
  @FastGTPSA(norm(zero(t) - zero(v))) < tol
  @FastGTPSA(norm(real(t) - real(v))) < tol
  @FastGTPSA(norm(imag(t) - imag(v))) < tol
  @FastGTPSA(norm(conj(t) - conj(v))) < tol
  @FastGTPSA(norm(sinhc(t/pi) - sinh(v)/v)) < tol
  @FastGTPSA(norm(erf(t) - erf(v))) < tol
  @FastGTPSA(norm(erfc(t) - erfc(v))) < tol
  @FastGTPSA(norm(-im*erf(t*im) - erfi(v))) < tol
  @FastGTPSA(norm(atan(t3,t2) - atan(3,2))) < tol
  @FastGTPSA(norm(atan(t3,2) - atan(3,2))) < tol
  @FastGTPSA(norm(atan(3,t2) - atan(3,2))) < tol
  @FastGTPSA(norm(atan(t3,-t2) - atan(3,-2))) < tol
  @FastGTPSA(norm(atan(t3,-2) - atan(3,-2))) < tol
  @FastGTPSA(norm(atan(3,-t2) - atan(3,-2))) < tol
  @FastGTPSA(norm(atan(-t3,-t2) - atan(-3,-2))) < tol
  @FastGTPSA(norm(atan(-t3,-2) - atan(-3,-2))) < tol
  @FastGTPSA(norm(atan(-3,-t2) - atan(-3,-2))) < tol
  @FastGTPSA(norm(atan(-t3,t2) - atan(-3,2))) < tol
  @FastGTPSA(norm(atan(-t3,2) - atan(-3,2))) < tol
  @FastGTPSA(norm(atan(-3,t2) - atan(-3,2))) < tol
  
  @FastGTPSA(norm(hypot(t2,t3) - hypot(2,3))) < tol
  @FastGTPSA(norm(hypot(2,t3) - hypot(2,3))) < tol
  @FastGTPSA(norm(hypot(t2,3) - hypot(2,3))) < tol
  @FastGTPSA(norm(hypot(t1,t2,t3) - hypot(1,2,3))) < tol
  @FastGTPSA(norm(hypot(1, t2, t3) - hypot(1,2,3))) < tol
  @FastGTPSA(norm(hypot(t1, 2, t3) - hypot(1,2,3))) < tol
  @FastGTPSA(norm(hypot(t1, t2, 3) - hypot(1,2,3))) < tol
  @FastGTPSA(norm(hypot(1, 2, t3) - hypot(1,2,3))) < tol
  @FastGTPSA(norm(hypot(1, t2, 3) - hypot(1,2,3))) < tol
  @FastGTPSA(norm(hypot(t1, 2, 3) - hypot(1,2,3))) < tol
  
  @FastGTPSA(norm(angle(t2) - angle(2))) < tol
  @FastGTPSA(norm(angle(-t2) - angle(-2))) < tol
  @FastGTPSA(norm(complex(t3) - complex(3))) < tol
  @FastGTPSA(norm(complex(t2,t3) - complex(2,3))) < tol
  @FastGTPSA(norm(polar(t2) - (abs(2)+im*atan(0,2)))) < tol
  @FastGTPSA(norm(polar(-t1) - (abs(-1)+im*atan(0,-1)))) < tol
  @FastGTPSA(norm(rect(t2) - (2*cos(0) + im*2*sin(0)))) < tol
  @FastGTPSA(norm(rect(-t1) - (-1*cos(0) + im*-1*sin(0)))) < tol
  

  v = 0.5+0.5im
  t = ComplexTPS(t)
  t[0] = v
  ct1 = ComplexTPS(t)
  ct1[0] = 1 + 1im
  ct2 = zero(ct1)
  ct2[0] = 2 + 2im
  ct3 = zero(ct1)
  ct3[0] = 3 + 3im
  @FastGTPSA(norm(abs(-t) - abs(-v) )) < tol
  @FastGTPSA(norm(sqrt(t) - sqrt(v))) < tol
  @FastGTPSA(norm(exp(t) - exp(v))) < tol
  @FastGTPSA(norm(log(t) - log(v))) < tol
  @FastGTPSA(norm(sin(t) - sin(v))) < tol
  @FastGTPSA(norm(cos(t) - cos(v))) < tol
  @FastGTPSA(norm(tan(t) - tan(v))) < tol
  @FastGTPSA(norm(csc(t) - csc(v))) < tol
  @FastGTPSA(norm(sec(t) - sec(v))) < tol
  @FastGTPSA(norm(cot(t) - cot(v))) < tol
  @FastGTPSA(norm(sinc(t) - sinc(v))) < tol
  @FastGTPSA(norm(sinh(t) - sinh(v))) < tol
  @FastGTPSA(norm(cosh(t) - cosh(v))) < tol
  @FastGTPSA(norm(tanh(t) - tanh(v))) < tol
  @FastGTPSA(norm(csch(t) - csch(v))) < tol
  @FastGTPSA(norm(sech(t) - sech(v))) < tol
  @FastGTPSA(norm(coth(t) - coth(v))) < tol

  @FastGTPSA(norm(asin(t) - asin(v))) < tol
  @FastGTPSA(norm(acos(t) - acos(v))) < tol
  @FastGTPSA(norm(atan(t) - atan(v))) < tol
  @FastGTPSA(norm(acsc(t) - acsc(v))) < tol
  @FastGTPSA(norm(asec(t) - asec(v))) < tol
  @FastGTPSA(norm(acot(t) - acot(v))) < tol
  @FastGTPSA(norm(asinh(t) - asinh(v))) < tol
  @FastGTPSA(norm(acosh(t) - acosh(v))) < tol
  @FastGTPSA(norm(atanh(t) - atanh(v))) < tol
  @FastGTPSA(norm(acsch(t) - acsch(v))) < tol
  @FastGTPSA(norm(asech(t) - asech(v))) < tol
  @FastGTPSA(norm(acoth(t) - acoth(v))) < tol
  @FastGTPSA(norm(asinc(t/pi) - asin(v)/v)) < tol
  @FastGTPSA(norm(asinhc(t/pi) - asinh(v)/v)) < tol
  
  @FastGTPSA(norm(zero(t) - zero(v))) < tol
  @FastGTPSA(norm(real(t) - real(v))) < tol
  @FastGTPSA(norm(imag(t) - imag(v))) < tol
  @FastGTPSA(norm(conj(t) - conj(v))) < tol
  @FastGTPSA(norm(sinhc(t/pi) - sinh(v)/v)) < tol
  @FastGTPSA(norm(erf(t) - erf(v))) < tol
  @FastGTPSA(norm(erfc(t) - erfc(v))) < tol
  @FastGTPSA(norm(-im*erf(t*im) - erfi(v))) < tol
  @FastGTPSA(norm(hypot(ct2,ct3) - hypot(2+2im,3+3im))) < tol
  @FastGTPSA(norm(hypot(2+2im,ct3) - hypot(2+2im,3+3im))) < tol
  @FastGTPSA(norm(hypot(ct2,3+3im) - hypot(2+2im,3+3im))) < tol
  @FastGTPSA(norm(hypot(ct1,ct2,ct3) - hypot(1+1im,2+2im,3+3im))) < tol
  @FastGTPSA(norm(hypot(1+1im, ct2, ct3) - hypot(1+1im,2+2im,3+3im))) < tol
  @FastGTPSA(norm(hypot(ct1, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im))) < tol
  @FastGTPSA(norm(hypot(ct1, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im))) < tol
  @FastGTPSA(norm(hypot(1+1im, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im))) < tol
  @FastGTPSA(norm(hypot(1+1im, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im))) < tol
  @FastGTPSA(norm(hypot(ct1, 2+2im, 3+3im) - hypot(1+1im,2+2im,3+3im))) < tol
  
  @FastGTPSA(norm(angle(t2+im*t3) - angle(2+3im))) < tol
  @FastGTPSA(norm(angle(t2-im*t3) - angle(2-3im))) < tol
  @FastGTPSA(norm(angle(-t2-im*t3) - angle(-2-3im))) < tol
  @FastGTPSA(norm(angle(-t2+im*t3) - angle(-2+3im))) < tol
  @FastGTPSA(norm(angle(ct2) - angle(2+2im))) < tol
  @FastGTPSA(norm(angle(-ct2) - angle(-2-2im))) < tol
  @FastGTPSA(norm(complex(ct3) - complex(3+3im))) < tol
  @FastGTPSA(norm(polar(ct2) - (abs(2+2im)+im*angle(2+2im)))) < tol
  @FastGTPSA(norm(polar(-ct1) - (abs(-1-im)+im*angle(-1-im)))) < tol
  @FastGTPSA(norm(rect(ct2) - (2*cos(2) + im*2*sin(2)))) < tol
  @FastGTPSA(norm(rect(-ct1) - (-1*cos(-1) + im*-1*sin(-1)))) < tol
  
  # Hypot, mixing TPS with ComplexTPS
  @FastGTPSA(norm(hypot(ct1, ct2, t3) - hypot(1+1im,2+2im,3))) < tol
  @FastGTPSA(norm(hypot(ct1, t2, ct3) - hypot(1+1im,2,3+3im))) < tol
  @FastGTPSA(norm(hypot(t1, ct2, ct3) - hypot(1,2+2im,3+3im))) < tol
  @FastGTPSA(norm(hypot(ct1, t2, t3) - hypot(1+1im,2,3))) < tol
  @FastGTPSA(norm(hypot(t1, ct2, t3) - hypot(1,2+2im,3))) < tol
  @FastGTPSA(norm(hypot(t1, t2, ct3) - hypot(1,2,3+3im))) < tol
  @FastGTPSA(norm(hypot(ct1,t2,3+3im) - hypot(1+1im,2,3+3im))) < tol
  @FastGTPSA(norm(hypot(t1, ct2, 3+3im) - hypot(1,2+2im,3+3im))) < tol
  @FastGTPSA(norm(hypot(ct1,2+2im,t3) - hypot(1+1im,2+2im,3))) < tol
  @FastGTPSA(norm(hypot(t1,2+2im,ct3) - hypot(1,2+2im,3+3im))) < tol
  @FastGTPSA(norm(hypot(1+1im,ct2,t3) - hypot(1+1im,2+2im,3))) < tol
  @FastGTPSA(norm(hypot(1+1im, t2, ct3) - hypot(1+1im,2,3+3im))) < tol
  @FastGTPSA(norm(hypot(t1,t2,3+3im) - hypot(1,2,3+3im))) < tol
  @FastGTPSA(norm(hypot(t1,2+2im,t3) - hypot(1,2+2im,3))) < tol
  @FastGTPSA(norm(hypot(1+1im,t2,t3) - hypot(1+1im,2,3))) < tol
  @FastGTPSA(norm(hypot(t1,2,3+3im) - hypot(1,2,3+3im))) < tol
  @FastGTPSA(norm(hypot(1,t2,3+3im) - hypot(1,2,3+3im))) < tol
  @FastGTPSA(norm(hypot(1+1im,2,t3) - hypot(1+1im,2,3))) < tol

  # Make sure stack is 0:
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t1.tpsa).d))
  tmpidx = unsafe_load(desc.ti)
  ctmpidx = unsafe_load(desc.cti)
  ctmpidx == 0
  tmpidx == 0

  d = Descriptor(1, 5)
  t = TPS(use=d)
  t[0] = 0.5; t[[1]] = 2; t[[2]] = 3; t[[3]] = 4; t[[4]] = 5; t[[5]] = 6

  tol = 1e-10

  @FastGTPSA(norm(sin(t)^2+cos(t)^2 - 1)) < tol
  @FastGTPSA(norm(1/sin(t) - csc(t))) < tol
  @FastGTPSA(norm(1/cos(t) - sec(t))) < tol
  @FastGTPSA(norm(1/tan(t) - cot(t))) < tol
  @FastGTPSA(norm(sin(t)/cos(t) - tan(t))) < tol
  @FastGTPSA(norm(cos(2*t) - cos(t)^2 + sin(t)^2)) < tol
  @FastGTPSA(norm(sec(t)^2 - 1 - tan(t)^2)) < tol
  @FastGTPSA(norm(sin(t/2) - sqrt((1-cos(t))/2))) < tol
  @FastGTPSA(norm(cos(t/2) - sqrt((1+cos(t))/2))) < tol
  @FastGTPSA(norm(sqrt(t^2) - abs(t))) < tol
  @FastGTPSA(norm(csc(t)^2 - cot(t)^2 - 1)) < tol
  @FastGTPSA(norm(exp(log(t)) - t)) < tol
  @FastGTPSA(norm(log(exp(t)) - t)) < tol
  @FastGTPSA(norm(log(exp(t)) - exp(log(t)))) < tol
  @FastGTPSA(norm(log(t^2) - 2*log(t))) < tol
  @FastGTPSA(norm(5*log(t) - log(t^5))) < tol
  @FastGTPSA(norm(t*log(5) - log(5^t))) < tol
  @FastGTPSA(norm(sinc(t) - sin(pi*t)/(pi*t))) < tol
  @FastGTPSA(norm(sinhc(t/pi) - sinh(t)/t)) < tol
  @FastGTPSA(norm(exp(im*t) - cos(t) - im*sin(t))) < tol
  @FastGTPSA(norm(real(exp(im*t)) - cos(t))) < tol
  @FastGTPSA(norm(imag(exp(im*t)) - sin(t))) < tol
  @FastGTPSA(norm(sinh(t) - (exp(t) - exp(-t))/2)) < tol
  @FastGTPSA(norm(cosh(t) - (exp(t) + exp(-t))/2)) < tol
  @FastGTPSA(norm(tanh(t) - sinh(t)/cosh(t))) < tol
  @FastGTPSA(norm(csch(t) - 1/sinh(t))) < tol
  @FastGTPSA(norm(sech(t) - 1/cosh(t))) < tol
  @FastGTPSA(norm(coth(t) - cosh(t)/sinh(t))) < tol
  @FastGTPSA(norm(coth(t) - 1/tanh(t))) < tol
  @FastGTPSA(norm(cosh(t)^2 - sinh(t)^2 - 1)) < tol
  @FastGTPSA(norm(1 - tanh(t)^2 - sech(t)^2)) < tol
  @FastGTPSA(norm(coth(t)^2 - 1 - csch(t)^2)) < tol
  @FastGTPSA(norm(asin(sin(t)) - t)) < tol
  @FastGTPSA(norm(acos(cos(t)) - t)) < tol
  @FastGTPSA(norm(atan(tan(t)) - t)) < tol
  @FastGTPSA(norm(acsc(1/t) - asin(t))) < tol
  @FastGTPSA(norm(asec(1/t) - acos(t))) < tol
  @FastGTPSA(norm(acot(1/t) - atan(t))) < tol
  @FastGTPSA(norm(asinh(sinh(t)) - t)) < tol
  @FastGTPSA(norm(acosh(cosh(t)) - t)) < tol
  @FastGTPSA(norm(atanh(tanh(t)) - t)) < tol
  @FastGTPSA(norm(acsch(t) - asinh(1/t))) < tol
  @FastGTPSA(norm(asech(t) - acosh(1/t))) < tol
  @FastGTPSA(norm(acoth(1/t) - atanh(t))) < tol
  @FastGTPSA(norm(asinc(t/pi) - asin(t)/t)) < tol
  @FastGTPSA(norm(asinhc(t/pi) - asinh(t)/t)) < tol
  @FastGTPSA(norm(erfc(t) - 1 + erf(t))) < tol
  @FastGTPSA(norm(erf(-t) + erf(t))) < tol
  @FastGTPSA(norm(angle(t))) < tol
  @FastGTPSA(norm(complex(t) - t)) < tol
  @FastGTPSA(norm(complex(t,t) - (t+im*t))) < tol

  t = ComplexTPS(t)
  t[0] = 0.5+0.5im; t[[1]] = 2+2im; t[[2]] = 3+3im; t[[3]] = 4+4im; t[[4]] = 5+5im; t[[5]] = 6+6im
  @FastGTPSA(norm(sin(t)^2+cos(t)^2 - 1)) < tol
  @FastGTPSA(norm(1/sin(t) - csc(t))) < tol
  @FastGTPSA(norm(1/cos(t) - sec(t))) < tol
  @FastGTPSA(norm(1/tan(t) - cot(t))) < tol
  @FastGTPSA(norm(sin(t)/cos(t) - tan(t))) < tol
  @FastGTPSA(norm(cos(2*t) - cos(t)^2 + sin(t)^2)) < tol
  @FastGTPSA(norm(sec(t)^2 - 1 - tan(t)^2)) < tol
  @FastGTPSA(norm(sin(t/2) - sqrt((1-cos(t))/2))) < tol
  @FastGTPSA(norm(cos(t/2) - sqrt((1+cos(t))/2))) < tol
  @FastGTPSA(norm(sqrt(t^2) - t)) < tol
  @FastGTPSA(norm(csc(t)^2 - cot(t)^2 - 1)) < tol
  @FastGTPSA(norm(exp(log(t)) - t)) < tol
  @FastGTPSA(norm(log(exp(t)) - t)) < tol
  @FastGTPSA(norm(log(exp(t)) - exp(log(t)))) < tol
  @FastGTPSA(norm(log(t^2) - 2*log(t))) < tol
  @FastGTPSA(norm(5*log(t) - log(t^5) - 2*pi*im)) < tol
  @FastGTPSA(norm(t*log(5) - log(5^t))) < tol
  @FastGTPSA(norm(sinc(t/pi) - sin(t)/t)) < tol
  @FastGTPSA(norm(sinhc(t/pi) - sinh(t)/t)) < tol
  @FastGTPSA(norm(exp(im*t) - cos(t) - im*sin(t))) < tol
  @FastGTPSA(norm(sinh(t) - (exp(t) - exp(-t))/2)) < tol
  @FastGTPSA(norm(cosh(t) - (exp(t) + exp(-t))/2)) < tol
  @FastGTPSA(norm(tanh(t) - sinh(t)/cosh(t))) < tol
  @FastGTPSA(norm(csch(t) - 1/sinh(t))) < tol
  @FastGTPSA(norm(sech(t) - 1/cosh(t))) < tol
  @FastGTPSA(norm(coth(t) - cosh(t)/sinh(t))) < tol
  @FastGTPSA(norm(coth(t) - 1/tanh(t))) < tol
  @FastGTPSA(norm(cosh(t)^2 - sinh(t)^2 - 1)) < tol
  @FastGTPSA(norm(1 - tanh(t)^2 - sech(t)^2)) < tol
  @FastGTPSA(norm(coth(t)^2 - 1 - csch(t)^2)) < tol
  
  @FastGTPSA(norm(asin(sin(t)) - t)) < tol
  @FastGTPSA(norm(acos(cos(t)) - t)) < tol
  @FastGTPSA(norm(atan(tan(t)) - t)) < tol
  @FastGTPSA(norm(acsc(t) - asin(1/t))) < tol
  @FastGTPSA(norm(asec(t) - acos(1/t))) < tol
  @FastGTPSA(norm(acot(t) - atan(1/t))) < tol
  @FastGTPSA(norm(asinh(sinh(t)) - t)) < tol
  @FastGTPSA(norm(acosh(cosh(t)) - t)) < tol
  @FastGTPSA(norm(atanh(tanh(t)) - t)) < tol
  @FastGTPSA(norm(acsch(t) - asinh(1/t))) < tol
  @FastGTPSA(norm(asech(t) - acosh(1/t))) < tol
  @FastGTPSA(norm(acoth(t) - atanh(1/t))) < tol
  @FastGTPSA(norm(asinc(t/pi) - asin(t)/t)) < tol
  @FastGTPSA(norm(asinhc(t/pi) - asinh(t)/t)) < tol
  
  @FastGTPSA(norm(erfc(t) - 1 + erf(t))) < tol
  @FastGTPSA(norm(erf(-t) + erf(t))) < tol
  @FastGTPSA(norm(angle(t) - atan(imag(t),real(t)))) < tol
  @FastGTPSA(norm(complex(t) - t)) < tol

  # Make sure stack is 0:
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d))
  tmpidx = unsafe_load(desc.ti)
  ctmpidx = unsafe_load(desc.cti)
  ctmpidx == 0
  tmpidx == 0
  return
end
