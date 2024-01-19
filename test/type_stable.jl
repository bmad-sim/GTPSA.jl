function type_stable_test()
  d = Descriptor(1, 5)
  t = TPS(d)
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
  tn[0] = 1; tn[1] = 2; tn[2] = 3; tn[3] = 4; tn[4] = 5; tn[5] = 6
  tcn = zero(ct1)
  tcn[0] = 1+1im; tcn[1] = 2+2im; tcn[2] = 3+3im; tcn[3] = 4+4im; tcn[4] = 5+5im; tcn[5] = 6+6im
  norm(tn) == sum([i for i in 1:6])
  norm(tcn) == sum([abs(i+i*im) for i in 1:6])

  # TPS:
  norm(t1 + t2 - t3)
  norm(t2 + t1 - t3)
  norm(t1 + 2 - t3)
  norm(2 + t1 - t3)
  norm(t3 - t2 - t1)
  norm(t2 - t3 - -t1)
  norm(t3 - 2 - t1)
  norm(2 - t3 - -t1)
  norm(t2 * t3 - 6)
  norm(t3 * t2 - 6)
  norm(t2 * 5 - 10)
  norm(5 * t2 - 10 * t1)
  norm(t1 / t2 - 1/2)
  norm(t2 / t1 - 2)
  norm(1 / t2 - 1/2)
  norm(t2 / 3 - 2/3)
  norm(t2 / t2 - t1)
  norm(t2 / t2 - 1)
  norm(t2 ^ t3 - 8)
  norm(t3 ^ t2 - 9)
  norm(t2 ^ 3 - 8)
  norm(t2 ^ (1/2) - sqrt(2))
  norm(t2 ^ (1/2) - sqrt(t2))
  norm(2 ^ t3 - 8)
  norm(inv(t3) - 1/t3)
  norm(inv(t3) - 1/3)

  # ComplexTPS:
  norm(ct1 + ct2 - ct3)
  norm(ct2 + ct1 - ct3)
  norm(ct1 + (2+2im) - ct3)
  norm((2+2im) + ct1 - ct3)
  norm(ct3 - ct2 - ct1)
  norm(ct2 - ct3 - -ct1)
  norm(ct3 - (2+2im) - ct1)
  norm((2+2im) - ct3 - -ct1)
  norm(ct2 * ct3 - (2+2im)*(3+3im))
  norm(ct3 * ct2 - (2+2im)*(3+3im))
  norm(ct2 * 5 - (10+10im))
  norm(5 * ct2 - (10 * ct1))
  norm(ct1 / ct2 - (1+im)/(2+2im))
  norm(ct2 / ct1 - 2)
  norm(1 / ct2 - 1/(2+2im))
  norm(ct2 / 3 - (2+2im)/3)
  norm(ct2 / ct2 - 1)
  norm(ct2 ^ ct3 - (2+2im)^(3+3im))
  norm(ct3 ^ ct2 - (3+3im)^(2+2im))
  norm(ct2 ^ 3 - (2+2im)^3)
  norm(ct2 ^ (1/2) - sqrt(2+2im))
  norm(ct2 ^ (1/2) - sqrt(ct2))
  norm(2 ^ ct3 - 2^(3+3im))
  norm(inv(ct3) - 1/ct3)
  norm(inv(ct3) - 1/(3+3im))

  # Promotion of TPS to ComplexTPS
  norm(t1 + ct2 - (1 + (2+2im)))
  norm(ct2 + t1 - (1 + (2+2im)))
  norm(t1 + (2+2im) - (1 + (2+2im)))
  norm((2+2im) + t1 - (1 + (2+2im)))
  norm(t3 - ct2 - (3 - (2+2im)))
  norm(ct2 - t3 - ((2+2im) - 3))
  norm(t3 - (2+2im) - (3 - (2+2im)))
  norm((2+2im) - t3 - ((2+2im) - 3))
  norm(t2 * ct3 - 2 * (3+3im))
  norm(ct3 * t2 - 2 * (3+3im))
  norm(t2 * (3+3im) - 2 * (3+3im))
  norm((3+3im) * t2 - 2 * (3+3im))
  norm(t2 / ct3 - 2/(3+3im))
  norm(ct3 / t2 - (3+3im)/2)
  norm(t2 / (3+3im) - 2/(3+3im))
  norm((3+3im) / t2 - (3+3im)/2)
  norm(t2 ^ ct3 - 2^(3+3im))
  norm(ct3 ^ t2 - (3+3im)^2)
  norm(t2 ^ (3+3im) - 2^(3+3im))
  norm((3+3im)^t2 - (3+3im)^2)

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


  norm(abs(-t) - abs(-v) )
  norm(sqrt(t) - sqrt(v))
  norm(exp(t) - exp(v))
  norm(log(t) - log(v))
  norm(sin(t) - sin(v))
  norm(cos(t) - cos(v))
  norm(tan(t) - tan(v))
  norm(csc(t) - csc(v))
  norm(sec(t) - sec(v))
  norm(cot(t) - cot(v))
  norm(sinc(t) - sinc(v))
  norm(sinh(t) - sinh(v))
  norm(cosh(t) - cosh(v))
  norm(tanh(t) - tanh(v))
  norm(csch(t) - csch(v))
  norm(sech(t) - sech(v))
  norm(coth(t) - coth(v))
  norm(asin(t) - asin(v))
  norm(acos(t) - acos(v))
  norm(atan(t) - atan(v))
  norm(acsc(1/t) - acsc(1/v))
  norm(asec(1/t) - asec(1/v))
  norm(acot(1/t) - acot(1/v))
  norm(asinh(t) - asinh(v))
  norm(acosh(1/t) - acosh(1/v))
  norm(atanh(t) - atanh(v))
  norm(acsch(1/t) - acsch(1/v))
  norm(asech(t) - asech(v))
  norm(acoth(1/t) - acoth(1/v))
  norm(asinc(t/pi) - asin(v)/(v))
  norm(asinhc(t/pi) - asinh(v)/(v))
  norm(zero(t) - zero(v))
  norm(real(t) - real(v))
  norm(imag(t) - imag(v))
  norm(conj(t) - conj(v))
  norm(sinhc(t/pi) - sinh(v)/v)
  #=
  norm(GTPSA.erf(t) - SF.erf(v))
  norm(GTPSA.erfc(t) - SF.erfc(v))
  norm(-im*GTPSA.erf(t*im) - SF.erfi(v))
  =#

  #= Uncomment when fixed
  norm(atan(t3,t2) - atan(3,2))
  norm(atan(t3,2) - atan(3,2))
  norm(atan(3,t2) - atan(3,2))
  norm(atan(t3,-t2) - atan(3,-2))
  norm(atan(t3,-2) - atan(3,-2))
  norm(atan(3,-t2) - atan(3,-2))
  norm(atan(-t3,-t2) - atan(-3,-2))
  norm(atan(-t3,-2) - atan(-3,-2))
  norm(atan(-3,-t2) - atan(-3,-2))
  norm(atan(-t3,t2) - atan(-3,2))
  norm(atan(-t3,2) - atan(-3,2))
  norm(atan(-3,t2) - atan(-3,2))
  =#
  norm(hypot(t2,t3) - hypot(2,3))
  norm(hypot(2,t3) - hypot(2,3))
  norm(hypot(t2,3) - hypot(2,3))
  norm(hypot(t1,t2,t3) - hypot(1,2,3))
  norm(hypot(1, t2, t3) - hypot(1,2,3))
  norm(hypot(t1, 2, t3) - hypot(1,2,3))
  norm(hypot(t1, t2, 3) - hypot(1,2,3))
  norm(hypot(1, 2, t3) - hypot(1,2,3))
  norm(hypot(1, t2, 3) - hypot(1,2,3))
  norm(hypot(t1, 2, 3) - hypot(1,2,3))
  #= Uncomment when fixed
  norm(angle(t2) - angle(2))
  norm(angle(-t2) - angle(-2))
  norm(complex(t3) - complex(3))
  norm(complex(t2,t3) - complex(2,3))
  norm(polar(t2) - (abs(2)+im*atan(0,2)))
  norm(polar(-t1) - (abs(-1)+im*atan(0,-1)))
  norm(rect(t2) - (2*cos(0) + 2*sin(0)))
  norm(rect(-t1) - (-1*cos(0) + -1*sin(0)))
  =#

  v = 0.5+0.5im
  t = ComplexTPS(t)
  t[0] = v
  ct1 = ComplexTPS(t)
  ct1[0] = 1 + 1im
  ct2 = zero(ct1)
  ct2[0] = 2 + 2im
  ct3 = zero(ct1)
  ct3[0] = 3 + 3im
  norm(abs(-t) - abs(-v) )
  norm(sqrt(t) - sqrt(v))
  norm(exp(t) - exp(v))
  norm(log(t) - log(v))
  norm(sin(t) - sin(v))
  norm(cos(t) - cos(v))
  norm(tan(t) - tan(v))
  norm(csc(t) - csc(v))
  norm(sec(t) - sec(v))
  norm(cot(t) - cot(v))
  norm(sinc(t) - sinc(v))
  norm(sinh(t) - sinh(v))
  norm(cosh(t) - cosh(v))
  norm(tanh(t) - tanh(v))
  norm(csch(t) - csch(v))
  norm(sech(t) - sech(v))
  norm(coth(t) - coth(v))

  #= Uncomment these when inverse trig C code is fixed
  norm(asin(t) - asin(v))
  norm(acos(t) - acos(v))
  norm(atan(t) - atan(v))
  norm(acsc(t) - acsc(v))
  norm(asec(t) - asec(v))
  norm(acot(t) - acot(v))
  norm(asinh(t) - asinh(v))
  norm(acosh(t) - acosh(v))
  norm(atanh(t) - atanh(v))
  norm(acsch(t) - acsch(v))
  norm(asech(t) - asech(v))
  norm(acoth(t) - acoth(v))
  norm(asinc(t) - asin(v)/v)
  norm(asinhc(t) - asinh(v)/v)
  =#
  norm(zero(t) - zero(v))
  norm(real(t) - real(v))
  norm(imag(t) - imag(v))
  norm(conj(t) - conj(v))
  norm(sinhc(t/pi) - sinh(v)/v)
  #=
  norm(GTPSA.erf(t) - SF.erf(v))
  norm(GTPSA.erfc(t) - SF.erfc(v))
  norm(-im*GTPSA.erf(t*im) - SF.erfi(v))
  =#
  norm(hypot(ct2,ct3) - hypot(2+2im,3+3im))
  norm(hypot(2+2im,ct3) - hypot(2+2im,3+3im))
  norm(hypot(ct2,3+3im) - hypot(2+2im,3+3im))
  norm(hypot(ct1,ct2,ct3) - hypot(1+1im,2+2im,3+3im))
  norm(hypot(1+1im, ct2, ct3) - hypot(1+1im,2+2im,3+3im))
  norm(hypot(ct1, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im))
  norm(hypot(ct1, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im))
  norm(hypot(1+1im, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im))
  norm(hypot(1+1im, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im))
  norm(hypot(ct1, 2+2im, 3+3im) - hypot(1+1im,2+2im,3+3im))

  #= Uncomment when fixed
  norm(angle(t2+im*t3) - angle(2+3im))
  norm(angle(t2-im*t3) - angle(2-3im))
  norm(angle(-t2-im*t3) - angle(-2-3im))
  norm(angle(-t2+im*t3) - angle(-2+3im))
  norm(angle(ct2) - angle(2+2im))
  norm(angle(-ct2) - angle(-2-2im))
  norm(complex(ct3) - complex(3+3im))
  norm(polar(ct2) - (abs(2+2im)+im*angle(2+2im)))
  norm(polar(-ct1) - (abs(-1-im)+im*angle(-1-im)))
  norm(rect(ct2) - (2*cos(2) + 2*sin(2)))
  norm(rect(-ct1) - (-1*cos(-1) + -1*sin(-1)))
  =#

  # Hypot, mixing TPS with ComplexTPS
  norm(hypot(ct1, ct2, t3) - hypot(1+1im,2+2im,3))
  norm(hypot(ct1, t2, ct3) - hypot(1+1im,2,3+3im))
  norm(hypot(t1, ct2, ct3) - hypot(1,2+2im,3+3im))
  norm(hypot(ct1, t2, t3) - hypot(1+1im,2,3))
  norm(hypot(t1, ct2, t3) - hypot(1,2+2im,3))
  norm(hypot(t1, t2, ct3) - hypot(1,2,3+3im))
  norm(hypot(ct1,t2,3+3im) - hypot(1+1im,2,3+3im))
  norm(hypot(t1, ct2, 3+3im) - hypot(1,2+2im,3+3im))
  norm(hypot(ct1,2+2im,t3) - hypot(1+1im,2+2im,3))
  norm(hypot(t1,2+2im,ct3) - hypot(1,2+2im,3+3im))
  norm(hypot(1+1im,ct2,t3) - hypot(1+1im,2+2im,3))
  norm(hypot(1+1im, t2, ct3) - hypot(1+1im,2,3+3im))
  norm(hypot(t1,t2,3+3im) - hypot(1,2,3+3im))
  norm(hypot(t1,2+2im,t3) - hypot(1,2+2im,3))
  norm(hypot(1+1im,t2,t3) - hypot(1+1im,2,3))
  norm(hypot(t1,2,3+3im) - hypot(1,2,3+3im))
  norm(hypot(1,t2,3+3im) - hypot(1,2,3+3im))
  norm(hypot(1+1im,2,t3) - hypot(1+1im,2,3))

  d = Descriptor(1, 5)
  t = TPS(d)
  t[0] = 0.5; t[1] = 2; t[2] = 3; t[3] = 4; t[4] = 5; t[5] = 6

  tol = 1e-10

  norm(sin(t)^2+cos(t)^2 - 1)
  norm(1/sin(t) - csc(t))
  norm(1/cos(t) - sec(t))
  norm(1/tan(t) - cot(t))
  norm(sin(t)/cos(t) - tan(t))
  norm(cos(2*t) - cos(t)^2 + sin(t)^2)
  norm(sec(t)^2 - 1 - tan(t)^2)
  norm(sin(t/2) - sqrt((1-cos(t))/2))
  norm(cos(t/2) - sqrt((1+cos(t))/2))
  norm(sqrt(t^2) - abs(t))
  norm(csc(t)^2 - cot(t)^2 - 1)
  norm(exp(log(t)) - t)
  norm(log(exp(t)) - t)
  norm(log(exp(t)) - exp(log(t)))
  norm(log(t^2) - 2*log(t))
  norm(5*log(t) - log(t^5))
  norm(t*log(5) - log(5^t))
  norm(sinc(t) - sin(pi*t)/(pi*t))
  norm(sinhc(t/pi) - sinh(t)/t)
  norm(exp(im*t) - cos(t) - im*sin(t))
  norm(real(exp(im*t)) - cos(t))
  norm(imag(exp(im*t)) - sin(t))
  norm(sinh(t) - (exp(t) - exp(-t))/2)
  norm(cosh(t) - (exp(t) + exp(-t))/2)
  norm(tanh(t) - sinh(t)/cosh(t))
  norm(csch(t) - 1/sinh(t))
  norm(sech(t) - 1/cosh(t))
  norm(coth(t) - cosh(t)/sinh(t))
  norm(coth(t) - 1/tanh(t))
  norm(cosh(t)^2 - sinh(t)^2 - 1)
  norm(1 - tanh(t)^2 - sech(t)^2)
  norm(coth(t)^2 - 1 - csch(t)^2)
  norm(asin(sin(t)) - t)
  norm(acos(cos(t)) - t)
  norm(atan(tan(t)) - t)
  norm(acsc(1/t) - asin(t))
  norm(asec(1/t) - acos(t))
  norm(acot(1/t) - atan(t))
  norm(asinh(sinh(t)) - t)
  norm(acosh(cosh(t)) - t)
  norm(atanh(tanh(t)) - t)
  norm(acsch(t) - asinh(1/t))
  norm(asech(t) - acosh(1/t))
  norm(acoth(1/t) - atanh(t))
  norm(asinc(t/pi) - asin(t)/t)
  norm(asinhc(t/pi) - asinh(t)/t)
  #=
  norm(GTPSA.erfc(t) - 1 + GTPSA.erf(t))
  norm(GTPSA.erf(-t) + GTPSA.erf(t))
  =#
  norm(angle(t))
  norm(complex(t) - t)
  norm(complex(t,t) - (t+im*t))

  t = ComplexTPS(t)
  t[0] = 0.5+0.5im; t[1] = 2+2im; t[2] = 3+3im; t[3] = 4+4im; t[4] = 5+5im; t[5] = 6+6im
  norm(sin(t)^2+cos(t)^2 - 1)
  norm(1/sin(t) - csc(t))
  norm(1/cos(t) - sec(t))
  norm(1/tan(t) - cot(t))
  norm(sin(t)/cos(t) - tan(t))
  norm(cos(2*t) - cos(t)^2 + sin(t)^2)
  norm(sec(t)^2 - 1 - tan(t)^2)
  norm(sin(t/2) - sqrt((1-cos(t))/2))
  norm(cos(t/2) - sqrt((1+cos(t))/2))
  norm(sqrt(t^2) - t)
  norm(csc(t)^2 - cot(t)^2 - 1)
  norm(exp(log(t)) - t)
  norm(log(exp(t)) - t)
  norm(log(exp(t)) - exp(log(t)))
  norm(log(t^2) - 2*log(t))
  norm(5*log(t) - log(t^5) - 2*pi*im)
  norm(t*log(5) - log(5^t))
  norm(sinc(t/pi) - sin(t)/t)
  norm(sinhc(t/pi) - sinh(t)/t)
  norm(exp(im*t) - cos(t) - im*sin(t))
  norm(sinh(t) - (exp(t) - exp(-t))/2)
  norm(cosh(t) - (exp(t) + exp(-t))/2)
  norm(tanh(t) - sinh(t)/cosh(t))
  norm(csch(t) - 1/sinh(t))
  norm(sech(t) - 1/cosh(t))
  norm(coth(t) - cosh(t)/sinh(t))
  norm(coth(t) - 1/tanh(t))
  norm(cosh(t)^2 - sinh(t)^2 - 1)
  norm(1 - tanh(t)^2 - sech(t)^2)
  norm(coth(t)^2 - 1 - csch(t)^2)

  #= Uncomment these when C code corrects for unbounded imaginary domain
  norm(asin(sin(t)) - t)
  norm(acos(cos(t)) - t)
  norm(atan(tan(t)) - t)
  norm(acsc(t) - asin(1/t))
  norm(asec(t) - acos(1/t))
  norm(acot(t) - acot(1/t))
  norm(asinh(sinh(t)) - t)
  norm(acosh(cosh(t)) - t)
  norm(atanh(tanh(t)) - t)
  norm(acsch(t) - asinh(1/t))
  norm(asech(t) - acosh(1/t))
  norm(acoth(t) - acoth(1/t))
  norm(asinc(t) - asin(t)/t)
  norm(asinhc(t) - asinh(t)/t)
  =#
  #=
  norm(GTPSA.erfc(t) - 1 + GTPSA.erf(t))
  norm(GTPSA.erf(-t) + GTPSA.erf(t))
  =#
  norm(angle(t) - atan(imag(t),real(t)))
  norm(complex(t) - t)

  d = Descriptor(1, 5)
  t = TPS(d)
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
  @FastGTPSA(norm(t1 + t2 - t3))
  @FastGTPSA(norm(t2 + t1 - t3))
  @FastGTPSA(norm(t1 + 2 - t3))
  @FastGTPSA(norm(2 + t1 - t3))
  @FastGTPSA(norm(t3 - t2 - t1))
  @FastGTPSA(norm(t2 - t3 - -t1))
  @FastGTPSA(norm(t3 - 2 - t1))
  @FastGTPSA(norm(2 - t3 - -t1))
  @FastGTPSA(norm(t2 * t3 - 6))
  @FastGTPSA(norm(t3 * t2 - 6))
  @FastGTPSA(norm(t2 * 5 - 10))
  @FastGTPSA(norm(5 * t2 - 10 * t1))
  @FastGTPSA(norm(t1 / t2 - 1/2))
  @FastGTPSA(norm(t2 / t1 - 2))
  @FastGTPSA(norm(1 / t2 - 1/2))
  @FastGTPSA(norm(t2 / 3 - 2/3))
  @FastGTPSA(norm(t2 / t2 - t1))
  @FastGTPSA(norm(t2 / t2 - 1))
  @FastGTPSA(norm(t2 ^ t3 - 8))
  @FastGTPSA(norm(t3 ^ t2 - 9))
  @FastGTPSA(norm(t2 ^ 3 - 8))
  @FastGTPSA(norm(t2 ^ (1/2) - sqrt(2)))
  @FastGTPSA(norm(t2 ^ (1/2) - sqrt(t2)))
  @FastGTPSA(norm(2 ^ t3 - 8))
  @FastGTPSA(norm(inv(t3) - 1/t3))
  @FastGTPSA(norm(inv(t3) - 1/3))

  # ComplexTPS:
  @FastGTPSA(norm(ct1 + ct2 - ct3))
  @FastGTPSA(norm(ct2 + ct1 - ct3))
  @FastGTPSA(norm(ct1 + (2+2im) - ct3))
  @FastGTPSA(norm((2+2im) + ct1 - ct3))
  @FastGTPSA(norm(ct3 - ct2 - ct1))
  @FastGTPSA(norm(ct2 - ct3 - -ct1))
  @FastGTPSA(norm(ct3 - (2+2im) - ct1))
  @FastGTPSA(norm((2+2im) - ct3 - -ct1))
  @FastGTPSA(norm(ct2 * ct3 - (2+2im)*(3+3im)))
  @FastGTPSA(norm(ct3 * ct2 - (2+2im)*(3+3im)))
  @FastGTPSA(norm(ct2 * 5 - (10+10im)))
  @FastGTPSA(norm(5 * ct2 - (10 * ct1)))
  @FastGTPSA(norm(ct1 / ct2 - (1+im)/(2+2im)))
  @FastGTPSA(norm(ct2 / ct1 - 2))
  @FastGTPSA(norm(1 / ct2 - 1/(2+2im)))
  @FastGTPSA(norm(ct2 / 3 - (2+2im)/3))
  @FastGTPSA(norm(ct2 / ct2 - 1))
  @FastGTPSA(norm(ct2 ^ ct3 - (2+2im)^(3+3im)))
  @FastGTPSA(norm(ct3 ^ ct2 - (3+3im)^(2+2im)))
  @FastGTPSA(norm(ct2 ^ 3 - (2+2im)^3))
  @FastGTPSA(norm(ct2 ^ (1/2) - sqrt(2+2im)))
  @FastGTPSA(norm(ct2 ^ (1/2) - sqrt(ct2)))
  @FastGTPSA(norm(2 ^ ct3 - 2^(3+3im)))
  @FastGTPSA(norm(inv(ct3) - 1/ct3))
  @FastGTPSA(norm(inv(ct3) - 1/(3+3im)))

  # Promotion of TPS to ComplexTPS
  @FastGTPSA(norm(t1 + ct2 - (1 + (2+2im))))
  @FastGTPSA(norm(ct2 + t1 - (1 + (2+2im))))
  @FastGTPSA(norm(t1 + (2+2im) - (1 + (2+2im))))
  @FastGTPSA(norm((2+2im) + t1 - (1 + (2+2im))))
  @FastGTPSA(norm(t3 - ct2 - (3 - (2+2im))))
  @FastGTPSA(norm(ct2 - t3 - ((2+2im) - 3)))
  @FastGTPSA(norm(t3 - (2+2im) - (3 - (2+2im))))
  @FastGTPSA(norm((2+2im) - t3 - ((2+2im) - 3)))
  @FastGTPSA(norm(t2 * ct3 - 2 * (3+3im)))
  @FastGTPSA(norm(ct3 * t2 - 2 * (3+3im)))
  @FastGTPSA(norm(t2 * (3+3im) - 2 * (3+3im)))
  @FastGTPSA(norm((3+3im) * t2 - 2 * (3+3im)))
  @FastGTPSA(norm(t2 / ct3 - 2/(3+3im)))
  @FastGTPSA(norm(ct3 / t2 - (3+3im)/2))
  @FastGTPSA(norm(t2 / (3+3im) - 2/(3+3im)))
  @FastGTPSA(norm((3+3im) / t2 - (3+3im)/2))
  @FastGTPSA(norm(t2 ^ ct3 - 2^(3+3im)))
  @FastGTPSA(norm(ct3 ^ t2 - (3+3im)^2))
  @FastGTPSA(norm(t2 ^ (3+3im) - 2^(3+3im)))
  @FastGTPSA(norm((3+3im)^t2 - (3+3im)^2))

  # Make sure stack is 0:
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t1.tpsa).d))
  tmpidx = unsafe_load(desc.ti)
  ctmpidx = unsafe_load(desc.cti)
  ctmpidx == 0
  tmpidx == 0

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


  @FastGTPSA(norm(abs(-t) - abs(-v) ))
  @FastGTPSA(norm(sqrt(t) - sqrt(v)))
  @FastGTPSA(norm(exp(t) - exp(v)))
  @FastGTPSA(norm(log(t) - log(v)))
  @FastGTPSA(norm(sin(t) - sin(v)))
  @FastGTPSA(norm(cos(t) - cos(v)))
  @FastGTPSA(norm(tan(t) - tan(v)))
  @FastGTPSA(norm(csc(t) - csc(v)))
  @FastGTPSA(norm(sec(t) - sec(v)))
  @FastGTPSA(norm(cot(t) - cot(v)))
  @FastGTPSA(norm(sinc(t) - sinc(v)))
  @FastGTPSA(norm(sinh(t) - sinh(v)))
  @FastGTPSA(norm(cosh(t) - cosh(v)))
  @FastGTPSA(norm(tanh(t) - tanh(v)))
  @FastGTPSA(norm(csch(t) - csch(v)))
  @FastGTPSA(norm(sech(t) - sech(v)))
  @FastGTPSA(norm(coth(t) - coth(v)))
  @FastGTPSA(norm(asin(t) - asin(v)))
  @FastGTPSA(norm(acos(t) - acos(v)))
  @FastGTPSA(norm(atan(t) - atan(v)))
  @FastGTPSA(norm(acsc(1/t) - acsc(1/v)))
  @FastGTPSA(norm(asec(1/t) - asec(1/v)))
  @FastGTPSA(norm(acot(1/t) - acot(1/v)))
  @FastGTPSA(norm(asinh(t) - asinh(v)))
  @FastGTPSA(norm(acosh(1/t) - acosh(1/v)))
  @FastGTPSA(norm(atanh(t) - atanh(v)))
  @FastGTPSA(norm(acsch(1/t) - acsch(1/v)))
  @FastGTPSA(norm(asech(t) - asech(v)))
  @FastGTPSA(norm(acoth(1/t) - acoth(1/v)))
  @FastGTPSA(norm(asinc(t/pi) - asin(v)/(v)))
  @FastGTPSA(norm(asinhc(t/pi) - asinh(v)/(v)))
  @FastGTPSA(norm(zero(t) - zero(v)))
  @FastGTPSA(norm(real(t) - real(v)))
  @FastGTPSA(norm(imag(t) - imag(v)))
  @FastGTPSA(norm(conj(t) - conj(v)))
  @FastGTPSA(norm(sinhc(t/pi) - sinh(v)/v))
  #=
  @FastGTPSA(norm(GTPSA.erf(t) - SF.erf(v)))
  @FastGTPSA(norm(GTPSA.erfc(t) - SF.erfc(v)))
  @FastGTPSA(norm(-im*GTPSA.erf(t*im) - SF.erfi(v)))
  =#
  #= Uncomment when fixed
  @FastGTPSA(norm(atan(t3,t2) - atan(3,2)))
  @FastGTPSA(norm(atan(t3,2) - atan(3,2)))
  @FastGTPSA(norm(atan(3,t2) - atan(3,2)))
  @FastGTPSA(norm(atan(t3,-t2) - atan(3,-2)))
  @FastGTPSA(norm(atan(t3,-2) - atan(3,-2)))
  @FastGTPSA(norm(atan(3,-t2) - atan(3,-2)))
  @FastGTPSA(norm(atan(-t3,-t2) - atan(-3,-2)))
  @FastGTPSA(norm(atan(-t3,-2) - atan(-3,-2)))
  @FastGTPSA(norm(atan(-3,-t2) - atan(-3,-2)))
  @FastGTPSA(norm(atan(-t3,t2) - atan(-3,2)))
  @FastGTPSA(norm(atan(-t3,2) - atan(-3,2)))
  @FastGTPSA(norm(atan(-3,t2) - atan(-3,2)))
  =#
  @FastGTPSA(norm(hypot(t2,t3) - hypot(2,3)))
  @FastGTPSA(norm(hypot(2,t3) - hypot(2,3)))
  @FastGTPSA(norm(hypot(t2,3) - hypot(2,3)))
  @FastGTPSA(norm(hypot(t1,t2,t3) - hypot(1,2,3)))
  @FastGTPSA(norm(hypot(1, t2, t3) - hypot(1,2,3)))
  @FastGTPSA(norm(hypot(t1, 2, t3) - hypot(1,2,3)))
  @FastGTPSA(norm(hypot(t1, t2, 3) - hypot(1,2,3)))
  @FastGTPSA(norm(hypot(1, 2, t3) - hypot(1,2,3)))
  @FastGTPSA(norm(hypot(1, t2, 3) - hypot(1,2,3)))
  @FastGTPSA(norm(hypot(t1, 2, 3) - hypot(1,2,3)))

  #= Uncomment when fixed
  @FastGTPSA(norm(angle(t2) - angle(2)))
  @FastGTPSA(norm(angle(-t2) - angle(-2)))
  @FastGTPSA(norm(complex(t3) - complex(3)))
  @FastGTPSA(norm(complex(t2,t3) - complex(2,3)))
  @FastGTPSA(norm(polar(t2) - (abs(2)+im*atan(0,2))))
  @FastGTPSA(norm(polar(-t1) - (abs(-1)+im*atan(0,-1))))
  @FastGTPSA(norm(rect(t2) - (2*cos(0) + 2*sin(0))))
  @FastGTPSA(norm(rect(-t1) - (-1*cos(0) + -1*sin(0))))
  =#

  v = 0.5+0.5im
  t = ComplexTPS(t)
  t[0] = v
  ct1 = ComplexTPS(t)
  ct1[0] = 1 + 1im
  ct2 = zero(ct1)
  ct2[0] = 2 + 2im
  ct3 = zero(ct1)
  ct3[0] = 3 + 3im
  @FastGTPSA(norm(abs(-t) - abs(-v) ))
  @FastGTPSA(norm(sqrt(t) - sqrt(v)))
  @FastGTPSA(norm(exp(t) - exp(v)))
  @FastGTPSA(norm(log(t) - log(v)))
  @FastGTPSA(norm(sin(t) - sin(v)))
  @FastGTPSA(norm(cos(t) - cos(v)))
  @FastGTPSA(norm(tan(t) - tan(v)))
  @FastGTPSA(norm(csc(t) - csc(v)))
  @FastGTPSA(norm(sec(t) - sec(v)))
  @FastGTPSA(norm(cot(t) - cot(v)))
  @FastGTPSA(norm(sinc(t) - sinc(v)))
  @FastGTPSA(norm(sinh(t) - sinh(v)))
  @FastGTPSA(norm(cosh(t) - cosh(v)))
  @FastGTPSA(norm(tanh(t) - tanh(v)))
  @FastGTPSA(norm(csch(t) - csch(v)))
  @FastGTPSA(norm(sech(t) - sech(v)))
  @FastGTPSA(norm(coth(t) - coth(v)))

  #= Uncomment these when inverse trig C code is fixed
  @FastGTPSA(norm(asin(t) - asin(v)))
  @FastGTPSA(norm(acos(t) - acos(v)))
  @FastGTPSA(norm(atan(t) - atan(v)))
  @FastGTPSA(norm(acsc(t) - acsc(v)))
  @FastGTPSA(norm(asec(t) - asec(v)))
  @FastGTPSA(norm(acot(t) - acot(v)))
  @FastGTPSA(norm(asinh(t) - asinh(v)))
  @FastGTPSA(norm(acosh(t) - acosh(v)))
  @FastGTPSA(norm(atanh(t) - atanh(v)))
  @FastGTPSA(norm(acsch(t) - acsch(v)))
  @FastGTPSA(norm(asech(t) - asech(v)))
  @FastGTPSA(norm(acoth(t) - acoth(v)))
  @FastGTPSA(norm(asinc(t) - asin(v)/v))
  @FastGTPSA(norm(asinhc(t) - asinh(v)/v))
  =#
  @FastGTPSA(norm(zero(t) - zero(v)))
  @FastGTPSA(norm(real(t) - real(v)))
  @FastGTPSA(norm(imag(t) - imag(v)))
  @FastGTPSA(norm(conj(t) - conj(v)))
  @FastGTPSA(norm(sinhc(t/pi) - sinh(v)/v))
  #=
  @FastGTPSA(norm(GTPSA.erf(t) - SF.erf(v)))
  @FastGTPSA(norm(GTPSA.erfc(t) - SF.erfc(v)))
  @FastGTPSA(norm(-im*GTPSA.erf(t*im) - SF.erfi(v)))
  =#
  @FastGTPSA(norm(hypot(ct2,ct3) - hypot(2+2im,3+3im)))
  @FastGTPSA(norm(hypot(2+2im,ct3) - hypot(2+2im,3+3im)))
  @FastGTPSA(norm(hypot(ct2,3+3im) - hypot(2+2im,3+3im)))
  @FastGTPSA(norm(hypot(ct1,ct2,ct3) - hypot(1+1im,2+2im,3+3im)))
  @FastGTPSA(norm(hypot(1+1im, ct2, ct3) - hypot(1+1im,2+2im,3+3im)))
  @FastGTPSA(norm(hypot(ct1, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)))
  @FastGTPSA(norm(hypot(ct1, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)))
  @FastGTPSA(norm(hypot(1+1im, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)))
  @FastGTPSA(norm(hypot(1+1im, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)))
  @FastGTPSA(norm(hypot(ct1, 2+2im, 3+3im) - hypot(1+1im,2+2im,3+3im)))

  #= Uncomment when fixed
  @FastGTPSA(norm(angle(t2+im*t3) - angle(2+3im)))
  @FastGTPSA(norm(angle(t2-im*t3) - angle(2-3im)))
  @FastGTPSA(norm(angle(-t2-im*t3) - angle(-2-3im)))
  @FastGTPSA(norm(angle(-t2+im*t3) - angle(-2+3im)))
  @FastGTPSA(norm(angle(ct2) - angle(2+2im)))
  @FastGTPSA(norm(angle(-ct2) - angle(-2-2im)))
  @FastGTPSA(norm(complex(ct3) - complex(3+3im)))
  @FastGTPSA(norm(polar(ct2) - (abs(2+2im)+im*angle(2+2im))))
  @FastGTPSA(norm(polar(-ct1) - (abs(-1-im)+im*angle(-1-im))))
  @FastGTPSA(norm(rect(ct2) - (2*cos(2) + 2*sin(2))))
  @FastGTPSA(norm(rect(-ct1) - (-1*cos(-1) + -1*sin(-1))))
  =#
  # Hypot, mixing TPS with ComplexTPS
  @FastGTPSA(norm(hypot(ct1, ct2, t3) - hypot(1+1im,2+2im,3)))
  @FastGTPSA(norm(hypot(ct1, t2, ct3) - hypot(1+1im,2,3+3im)))
  @FastGTPSA(norm(hypot(t1, ct2, ct3) - hypot(1,2+2im,3+3im)))
  @FastGTPSA(norm(hypot(ct1, t2, t3) - hypot(1+1im,2,3)))
  @FastGTPSA(norm(hypot(t1, ct2, t3) - hypot(1,2+2im,3)))
  @FastGTPSA(norm(hypot(t1, t2, ct3) - hypot(1,2,3+3im)))
  @FastGTPSA(norm(hypot(ct1,t2,3+3im) - hypot(1+1im,2,3+3im)))
  @FastGTPSA(norm(hypot(t1, ct2, 3+3im) - hypot(1,2+2im,3+3im)))
  @FastGTPSA(norm(hypot(ct1,2+2im,t3) - hypot(1+1im,2+2im,3)))
  @FastGTPSA(norm(hypot(t1,2+2im,ct3) - hypot(1,2+2im,3+3im)))
  @FastGTPSA(norm(hypot(1+1im,ct2,t3) - hypot(1+1im,2+2im,3)))
  @FastGTPSA(norm(hypot(1+1im, t2, ct3) - hypot(1+1im,2,3+3im)))
  @FastGTPSA(norm(hypot(t1,t2,3+3im) - hypot(1,2,3+3im)))
  @FastGTPSA(norm(hypot(t1,2+2im,t3) - hypot(1,2+2im,3)))
  @FastGTPSA(norm(hypot(1+1im,t2,t3) - hypot(1+1im,2,3)))
  @FastGTPSA(norm(hypot(t1,2,3+3im) - hypot(1,2,3+3im)))
  @FastGTPSA(norm(hypot(1,t2,3+3im) - hypot(1,2,3+3im)))
  @FastGTPSA(norm(hypot(1+1im,2,t3) - hypot(1+1im,2,3)))

  # Make sure stack is 0:
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t1.tpsa).d))
  tmpidx = unsafe_load(desc.ti)
  ctmpidx = unsafe_load(desc.cti)
  ctmpidx == 0
  tmpidx == 0
  d = Descriptor(1, 5)
  t = TPS(d)
  t[0] = 0.5; t[1] = 2; t[2] = 3; t[3] = 4; t[4] = 5; t[5] = 6

  tol = 1e-10

  @FastGTPSA(norm(sin(t)^2+cos(t)^2 - 1))
  @FastGTPSA(norm(1/sin(t) - csc(t)))
  @FastGTPSA(norm(1/cos(t) - sec(t)))
  @FastGTPSA(norm(1/tan(t) - cot(t)))
  @FastGTPSA(norm(sin(t)/cos(t) - tan(t)))
  @FastGTPSA(norm(cos(2*t) - cos(t)^2 + sin(t)^2))
  @FastGTPSA(norm(sec(t)^2 - 1 - tan(t)^2))
  @FastGTPSA(norm(sin(t/2) - sqrt((1-cos(t))/2)))
  @FastGTPSA(norm(cos(t/2) - sqrt((1+cos(t))/2)))
  @FastGTPSA(norm(sqrt(t^2) - abs(t)))
  @FastGTPSA(norm(csc(t)^2 - cot(t)^2 - 1))
  @FastGTPSA(norm(exp(log(t)) - t))
  @FastGTPSA(norm(log(exp(t)) - t))
  @FastGTPSA(norm(log(exp(t)) - exp(log(t))))
  @FastGTPSA(norm(log(t^2) - 2*log(t)))
  @FastGTPSA(norm(5*log(t) - log(t^5)))
  @FastGTPSA(norm(t*log(5) - log(5^t)))
  @FastGTPSA(norm(sinc(t) - sin(pi*t)/(pi*t)))
  @FastGTPSA(norm(sinhc(t/pi) - sinh(t)/t))
  @FastGTPSA(norm(exp(im*t) - cos(t) - im*sin(t)))
  @FastGTPSA(norm(real(exp(im*t)) - cos(t)))
  @FastGTPSA(norm(imag(exp(im*t)) - sin(t)))
  @FastGTPSA(norm(sinh(t) - (exp(t) - exp(-t))/2))
  @FastGTPSA(norm(cosh(t) - (exp(t) + exp(-t))/2))
  @FastGTPSA(norm(tanh(t) - sinh(t)/cosh(t)))
  @FastGTPSA(norm(csch(t) - 1/sinh(t)))
  @FastGTPSA(norm(sech(t) - 1/cosh(t)))
  @FastGTPSA(norm(coth(t) - cosh(t)/sinh(t)))
  @FastGTPSA(norm(coth(t) - 1/tanh(t)))
  @FastGTPSA(norm(cosh(t)^2 - sinh(t)^2 - 1))
  @FastGTPSA(norm(1 - tanh(t)^2 - sech(t)^2))
  @FastGTPSA(norm(coth(t)^2 - 1 - csch(t)^2))
  @FastGTPSA(norm(asin(sin(t)) - t))
  @FastGTPSA(norm(acos(cos(t)) - t))
  @FastGTPSA(norm(atan(tan(t)) - t))
  @FastGTPSA(norm(acsc(1/t) - asin(t)))
  @FastGTPSA(norm(asec(1/t) - acos(t)))
  @FastGTPSA(norm(acot(1/t) - atan(t)))
  @FastGTPSA(norm(asinh(sinh(t)) - t))
  @FastGTPSA(norm(acosh(cosh(t)) - t))
  @FastGTPSA(norm(atanh(tanh(t)) - t))
  @FastGTPSA(norm(acsch(t) - asinh(1/t)))
  @FastGTPSA(norm(asech(t) - acosh(1/t)))
  @FastGTPSA(norm(acoth(1/t) - atanh(t)))
  @FastGTPSA(norm(asinc(t/pi) - asin(t)/t))
  @FastGTPSA(norm(asinhc(t/pi) - asinh(t)/t))
  #=
  @FastGTPSA(norm(GTPSA.erfc(t) - 1 + GTPSA.erf(t)))
  @FastGTPSA(norm(GTPSA.erf(-t) + GTPSA.erf(t)))
  =#
  @FastGTPSA(norm(angle(t)))
  @FastGTPSA(norm(complex(t) - t))
  @FastGTPSA(norm(complex(t,t) - (t+im*t)))

  t = ComplexTPS(t)
  t[0] = 0.5+0.5im; t[1] = 2+2im; t[2] = 3+3im; t[3] = 4+4im; t[4] = 5+5im; t[5] = 6+6im
  @FastGTPSA(norm(sin(t)^2+cos(t)^2 - 1))
  @FastGTPSA(norm(1/sin(t) - csc(t)))
  @FastGTPSA(norm(1/cos(t) - sec(t)))
  @FastGTPSA(norm(1/tan(t) - cot(t)))
  @FastGTPSA(norm(sin(t)/cos(t) - tan(t)))
  @FastGTPSA(norm(cos(2*t) - cos(t)^2 + sin(t)^2))
  @FastGTPSA(norm(sec(t)^2 - 1 - tan(t)^2))
  @FastGTPSA(norm(sin(t/2) - sqrt((1-cos(t))/2)))
  @FastGTPSA(norm(cos(t/2) - sqrt((1+cos(t))/2)))
  @FastGTPSA(norm(sqrt(t^2) - t))
  @FastGTPSA(norm(csc(t)^2 - cot(t)^2 - 1))
  @FastGTPSA(norm(exp(log(t)) - t))
  @FastGTPSA(norm(log(exp(t)) - t))
  @FastGTPSA(norm(log(exp(t)) - exp(log(t))))
  @FastGTPSA(norm(log(t^2) - 2*log(t)))
  @FastGTPSA(norm(5*log(t) - log(t^5) - 2*pi*im))
  @FastGTPSA(norm(t*log(5) - log(5^t)))
  @FastGTPSA(norm(sinc(t/pi) - sin(t)/t))
  @FastGTPSA(norm(sinhc(t/pi) - sinh(t)/t))
  @FastGTPSA(norm(exp(im*t) - cos(t) - im*sin(t)))
  @FastGTPSA(norm(sinh(t) - (exp(t) - exp(-t))/2))
  @FastGTPSA(norm(cosh(t) - (exp(t) + exp(-t))/2))
  @FastGTPSA(norm(tanh(t) - sinh(t)/cosh(t)))
  @FastGTPSA(norm(csch(t) - 1/sinh(t)))
  @FastGTPSA(norm(sech(t) - 1/cosh(t)))
  @FastGTPSA(norm(coth(t) - cosh(t)/sinh(t)))
  @FastGTPSA(norm(coth(t) - 1/tanh(t)))
  @FastGTPSA(norm(cosh(t)^2 - sinh(t)^2 - 1))
  @FastGTPSA(norm(1 - tanh(t)^2 - sech(t)^2))
  @FastGTPSA(norm(coth(t)^2 - 1 - csch(t)^2))

  # Uncomment these when C code corrects for unbounded imaginary domain
  #=
  @FastGTPSA(norm(asin(sin(t)) - t))
  @FastGTPSA(norm(acos(cos(t)) - t))
  @FastGTPSA(norm(atan(tan(t)) - t))
  @FastGTPSA(norm(acsc(t) - asin(1/t)))
  @FastGTPSA(norm(asec(t) - acos(1/t)))
  @FastGTPSA(norm(acot(t) - acot(1/t)))
  @FastGTPSA(norm(asinh(sinh(t)) - t))
  @FastGTPSA(norm(acosh(cosh(t)) - t))
  @FastGTPSA(norm(atanh(tanh(t)) - t))
  @FastGTPSA(norm(acsch(t) - asinh(1/t)))
  @FastGTPSA(norm(asech(t) - acosh(1/t)))
  @FastGTPSA(norm(acoth(t) - acoth(1/t)))
  @FastGTPSA(norm(asinc(t) - asin(t)/t))
  @FastGTPSA(norm(asinhc(t) - asinh(t)/t))
  =#
  #=
  @FastGTPSA(norm(GTPSA.erfc(t) - 1 + GTPSA.erf(t)))
  @FastGTPSA(norm(GTPSA.erf(-t) + GTPSA.erf(t)))
  =#
  @FastGTPSA(norm(angle(t) - atan(imag(t),real(t))))
  @FastGTPSA(norm(complex(t) - t))

  # Make sure stack is 0:
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d))
  tmpidx = unsafe_load(desc.ti)
  ctmpidx = unsafe_load(desc.cti)
  ctmpidx == 0
  tmpidx == 0
  return
end
