function type_stable_test()
  d = Descriptor(1, 5)
  t = TPS{Float64}(use=d)
  ct = TPS{ComplexF64}(t)

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

  normTPS(tn) == sum([i for i in 1:6])
  normTPS(tcn) == sum([abs(i+i*im) for i in 1:6])

  # TPS{Float64}:
  normTPS(t1 + t2 - t3) < tol
  normTPS(t2 + t1 - t3) < tol
  normTPS(t1 + 2 - t3) < tol
  normTPS(2 + t1 - t3) < tol
  normTPS(t3 - t2 - t1) < tol
  normTPS(t2 - t3 - -t1) < tol
  normTPS(t3 - 2 - t1) < tol
  normTPS(2 - t3 - -t1) < tol
  normTPS(t2 * t3 - 6) < tol
  normTPS(t3 * t2 - 6) < tol
  normTPS(t2 * 5 - 10) < tol
  normTPS(5 * t2 - 10 * t1) < tol
  normTPS(t1 / t2 - 1/2) < tol
  normTPS(t2 / t1 - 2) < tol
  normTPS(1 / t2 - 1/2) < tol
  normTPS(t2 / 3 - 2/3) < tol
  normTPS(t2 / t2 - t1) < tol
  normTPS(t2 / t2 - 1) < tol
  normTPS(t2 ^ t3 - 8) < tol
  normTPS(t3 ^ t2 - 9) < tol
  normTPS(t2 ^ 3 - 8) < tol
  normTPS(t2 ^ (1/2) - sqrt(2)) < tol
  normTPS(t2 ^ (1/2) - sqrt(t2)) < tol
  normTPS(2 ^ t3 - 8) < tol
  normTPS(inv(t3) - 1/t3) < tol
  normTPS(inv(t3) - 1/3) < tol

  # TPS{ComplexF64}:
  normTPS(ct1 + ct2 - ct3) < tol
  normTPS(ct2 + ct1 - ct3) < tol
  normTPS(ct1 + (2+2im) - ct3) < tol
  normTPS((2+2im) + ct1 - ct3) < tol
  normTPS(ct3 - ct2 - ct1) < tol
  normTPS(ct2 - ct3 - -ct1) < tol
  normTPS(ct3 - (2+2im) - ct1) < tol
  normTPS((2+2im) - ct3 - -ct1) < tol
  normTPS(ct2 * ct3 - (2+2im)*(3+3im)) < tol
  normTPS(ct3 * ct2 - (2+2im)*(3+3im)) < tol
  normTPS(ct2 * 5 - (10+10im)) < tol
  normTPS(5 * ct2 - (10 * ct1)) < tol
  normTPS(ct1 / ct2 - (1+im)/(2+2im)) < tol
  normTPS(ct2 / ct1 - 2) < tol
  normTPS(1 / ct2 - 1/(2+2im)) < tol
  normTPS(ct2 / 3 - (2+2im)/3) < tol
  normTPS(ct2 / ct2 - 1) < tol
  normTPS(ct2 ^ ct3 - (2+2im)^(3+3im)) < tol
  normTPS(ct3 ^ ct2 - (3+3im)^(2+2im)) < tol
  normTPS(ct2 ^ 3 - (2+2im)^3) < tol
  normTPS(ct2 ^ (1/2) - sqrt(2+2im)) < tol
  normTPS(ct2 ^ (1/2) - sqrt(ct2)) < tol
  normTPS(2 ^ ct3 - 2^(3+3im)) < tol
  normTPS(inv(ct3) - 1/ct3) < tol
  normTPS(inv(ct3) - 1/(3+3im)) < tol

  # Promotion of TPS{Float64} to TPS{ComplexF64}
  normTPS(t1 + ct2 - (1 + (2+2im))) < tol
  normTPS(ct2 + t1 - (1 + (2+2im))) < tol
  normTPS(t1 + (2+2im) - (1 + (2+2im))) < tol
  normTPS((2+2im) + t1 - (1 + (2+2im))) < tol
  normTPS(t3 - ct2 - (3 - (2+2im))) < tol
  normTPS(ct2 - t3 - ((2+2im) - 3)) < tol
  normTPS(t3 - (2+2im) - (3 - (2+2im))) < tol
  normTPS((2+2im) - t3 - ((2+2im) - 3)) < tol
  normTPS(t2 * ct3 - 2 * (3+3im)) < tol
  normTPS(ct3 * t2 - 2 * (3+3im)) < tol
  normTPS(t2 * (3+3im) - 2 * (3+3im)) < tol
  normTPS((3+3im) * t2 - 2 * (3+3im)) < tol
  normTPS(t2 / ct3 - 2/(3+3im)) < tol
  normTPS(ct3 / t2 - (3+3im)/2) < tol
  normTPS(t2 / (3+3im) - 2/(3+3im)) < tol
  normTPS((3+3im) / t2 - (3+3im)/2) < tol
  normTPS(t2 ^ ct3 - 2^(3+3im)) < tol
  normTPS(ct3 ^ t2 - (3+3im)^2) < tol
  normTPS(t2 ^ (3+3im) - 2^(3+3im)) < tol
  normTPS((3+3im)^t2 - (3+3im)^2) < tol

  d = Descriptor(1, 5)
  t = TPS{Float64}(use=d)
  v = 0.5
  t[0] = v
  tol = 1e-14
  t1 = TPS{Float64}(t)
  t1[0] = 1
  t2 = zero(t1)
  t2[0] = 2
  t3 = zero(t1)
  t3[0] = 3


  normTPS(abs(-t) - abs(-v) ) < tol
  normTPS(sqrt(t) - sqrt(v)) < tol
  normTPS(exp(t) - exp(v)) < tol
  normTPS(log(t) - log(v)) < tol
  normTPS(sin(t) - sin(v)) < tol
  normTPS(cos(t) - cos(v)) < tol
  normTPS(tan(t) - tan(v)) < tol
  normTPS(csc(t) - csc(v)) < tol
  normTPS(sec(t) - sec(v)) < tol
  normTPS(cot(t) - cot(v)) < tol
  normTPS(sinc(t) - sinc(v)) < tol
  normTPS(sinh(t) - sinh(v)) < tol
  normTPS(cosh(t) - cosh(v)) < tol
  normTPS(tanh(t) - tanh(v)) < tol
  normTPS(csch(t) - csch(v)) < tol
  normTPS(sech(t) - sech(v)) < tol
  normTPS(coth(t) - coth(v)) < tol
  normTPS(asin(t) - asin(v)) < tol
  normTPS(acos(t) - acos(v)) < tol
  normTPS(atan(t) - atan(v)) < tol
  normTPS(acsc(1/t) - acsc(1/v)) < tol
  normTPS(asec(1/t) - asec(1/v)) < tol
  normTPS(acot(1/t) - acot(1/v)) < tol
  normTPS(asinh(t) - asinh(v)) < tol
  normTPS(acosh(1/t) - acosh(1/v)) < tol
  normTPS(atanh(t) - atanh(v)) < tol
  normTPS(acsch(1/t) - acsch(1/v)) < tol
  normTPS(asech(t) - asech(v)) < tol
  normTPS(acoth(1/t) - acoth(1/v)) < tol
  normTPS(asinc(t/pi) - asin(v)/(v)) < tol
  normTPS(asinhc(t/pi) - asinh(v)/(v)) < tol
  normTPS(zero(t) - zero(v)) < tol
  normTPS(real(t) - real(v)) < tol
  normTPS(imag(t) - imag(v)) < tol
  normTPS(conj(t) - conj(v)) < tol
  normTPS(sinhc(t/pi) - sinh(v)/v) < tol
  normTPS(erf(t) - erf(v)) < tol
  normTPS(erfc(t) - erfc(v)) < tol
  normTPS(-im*erf(t*im) - erfi(v)) < tol
  
  normTPS(atan(t3,t2) - atan(3,2)) < tol
  normTPS(atan(t3,2) - atan(3,2)) < tol
  normTPS(atan(3,t2) - atan(3,2)) < tol
  normTPS(atan(t3,-t2) - atan(3,-2)) < tol
  normTPS(atan(t3,-2) - atan(3,-2)) < tol
  normTPS(atan(3,-t2) - atan(3,-2)) < tol
  normTPS(atan(-t3,-t2) - atan(-3,-2)) < tol
  normTPS(atan(-t3,-2) - atan(-3,-2)) < tol
  normTPS(atan(-3,-t2) - atan(-3,-2)) < tol
  normTPS(atan(-t3,t2) - atan(-3,2)) < tol
  normTPS(atan(-t3,2) - atan(-3,2)) < tol
  normTPS(atan(-3,t2) - atan(-3,2)) < tol
  #=
  normTPS(hypot(t2,t3) - hypot(2,3)) < tol
  normTPS(hypot(2,t3) - hypot(2,3)) < tol
  normTPS(hypot(t2,3) - hypot(2,3)) < tol
  normTPS(hypot(t1,t2,t3) - hypot(1,2,3)) < tol
  normTPS(hypot(1, t2, t3) - hypot(1,2,3)) < tol
  normTPS(hypot(t1, 2, t3) - hypot(1,2,3)) < tol
  normTPS(hypot(t1, t2, 3) - hypot(1,2,3)) < tol
  normTPS(hypot(1, 2, t3) - hypot(1,2,3)) < tol
  normTPS(hypot(1, t2, 3) - hypot(1,2,3)) < tol
  normTPS(hypot(t1, 2, 3) - hypot(1,2,3)) < tol
  =#
  normTPS(angle(t2) - angle(2)) < tol
  normTPS(angle(-t2) - angle(-2)) < tol
  normTPS(complex(t3) - complex(3)) < tol
  normTPS(complex(t2,t3) - complex(2,3)) < tol
  normTPS(polar(t2) - (abs(2)+im*atan(0,2))) < tol
  normTPS(polar(-t1) - (abs(-1)+im*atan(0,-1))) < tol
  normTPS(rect(t2) - (2*cos(0) + im*2*sin(0))) < tol
  normTPS(rect(-t1) - (-1*cos(0) + im*-1*sin(0))) < tol
  

  v = 0.5+0.5im
  t = TPS{ComplexF64}(t)
  t[0] = v
  ct1 = TPS{ComplexF64}(t)
  ct1[0] = 1 + 1im
  ct2 = zero(ct1)
  ct2[0] = 2 + 2im
  ct3 = zero(ct1)
  ct3[0] = 3 + 3im
  normTPS(abs(-t) - abs(-v) ) < tol
  normTPS(sqrt(t) - sqrt(v)) < tol
  normTPS(exp(t) - exp(v)) < tol
  normTPS(log(t) - log(v)) < tol
  normTPS(sin(t) - sin(v)) < tol
  normTPS(cos(t) - cos(v)) < tol
  normTPS(tan(t) - tan(v)) < tol
  normTPS(csc(t) - csc(v)) < tol
  normTPS(sec(t) - sec(v)) < tol
  normTPS(cot(t) - cot(v)) < tol
  normTPS(sinc(t) - sinc(v)) < tol
  normTPS(sinh(t) - sinh(v)) < tol
  normTPS(cosh(t) - cosh(v)) < tol
  normTPS(tanh(t) - tanh(v)) < tol
  normTPS(csch(t) - csch(v)) < tol
  normTPS(sech(t) - sech(v)) < tol
  normTPS(coth(t) - coth(v)) < tol

  normTPS(asin(t) - asin(v)) < tol
  normTPS(acos(t) - acos(v)) < tol
  normTPS(atan(t) - atan(v)) < tol
  normTPS(acsc(t) - acsc(v)) < tol
  normTPS(asec(t) - asec(v)) < tol
  normTPS(acot(t) - acot(v)) < tol
  normTPS(asinh(t) - asinh(v)) < tol
  normTPS(acosh(t) - acosh(v)) < tol
  normTPS(atanh(t) - atanh(v)) < tol
  normTPS(acsch(t) - acsch(v)) < tol
  normTPS(asech(t) - asech(v)) < tol
  normTPS(acoth(t) - acoth(v)) < tol
  normTPS(asinc(t/pi) - asin(v)/v) < tol
  normTPS(asinhc(t/pi) - asinh(v)/v) < tol

  normTPS(zero(t) - zero(v)) < tol
  normTPS(real(t) - real(v)) < tol
  normTPS(imag(t) - imag(v)) < tol
  normTPS(conj(t) - conj(v)) < tol
  normTPS(sinhc(t/pi) - sinh(v)/v) < tol
  normTPS(erf(t) - erf(v)) < tol
  normTPS(erfc(t) - erfc(v)) < tol
  normTPS(-im*erf(t*im) - erfi(v)) < tol
  #=
  normTPS(hypot(ct2,ct3) - hypot(2+2im,3+3im)) < tol
  normTPS(hypot(2+2im,ct3) - hypot(2+2im,3+3im)) < tol
  normTPS(hypot(ct2,3+3im) - hypot(2+2im,3+3im)) < tol
  normTPS(hypot(ct1,ct2,ct3) - hypot(1+1im,2+2im,3+3im)) < tol
  normTPS(hypot(1+1im, ct2, ct3) - hypot(1+1im,2+2im,3+3im)) < tol
  normTPS(hypot(ct1, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)) < tol
  normTPS(hypot(ct1, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)) < tol
  normTPS(hypot(1+1im, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im)) < tol
  normTPS(hypot(1+1im, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im)) < tol
  normTPS(hypot(ct1, 2+2im, 3+3im) - hypot(1+1im,2+2im,3+3im)) < tol
  =#
  normTPS(angle(t2+im*t3) - angle(2+3im)) < tol
  normTPS(angle(t2-im*t3) - angle(2-3im)) < tol
  normTPS(angle(-t2-im*t3) - angle(-2-3im)) < tol
  normTPS(angle(-t2+im*t3) - angle(-2+3im)) < tol
  normTPS(angle(ct2) - angle(2+2im)) < tol
  normTPS(angle(-ct2) - angle(-2-2im)) < tol
  normTPS(complex(ct3) - complex(3+3im)) < tol
  normTPS(polar(ct2) - (abs(2+2im)+im*angle(2+2im))) < tol
  normTPS(polar(-ct1) - (abs(-1-im)+im*angle(-1-im))) < tol
  normTPS(rect(ct2) - (2*cos(2) + im*2*sin(2))) < tol
  normTPS(rect(-ct1) - (-1*cos(-1) + im*-1*sin(-1))) < tol
  
  # Hypot, mixing TPS{Float64} with TPS{ComplexF64}
  #=
  normTPS(hypot(ct1, ct2, t3) - hypot(1+1im,2+2im,3)) < tol
  normTPS(hypot(ct1, t2, ct3) - hypot(1+1im,2,3+3im)) < tol
  normTPS(hypot(t1, ct2, ct3) - hypot(1,2+2im,3+3im)) < tol
  normTPS(hypot(ct1, t2, t3) - hypot(1+1im,2,3)) < tol
  normTPS(hypot(t1, ct2, t3) - hypot(1,2+2im,3)) < tol
  normTPS(hypot(t1, t2, ct3) - hypot(1,2,3+3im)) < tol
  normTPS(hypot(ct1,t2,3+3im) - hypot(1+1im,2,3+3im)) < tol
  normTPS(hypot(t1, ct2, 3+3im) - hypot(1,2+2im,3+3im)) < tol
  normTPS(hypot(ct1,2+2im,t3) - hypot(1+1im,2+2im,3)) < tol
  normTPS(hypot(t1,2+2im,ct3) - hypot(1,2+2im,3+3im)) < tol
  normTPS(hypot(1+1im,ct2,t3) - hypot(1+1im,2+2im,3)) < tol
  normTPS(hypot(1+1im, t2, ct3) - hypot(1+1im,2,3+3im)) < tol
  normTPS(hypot(t1,t2,3+3im) - hypot(1,2,3+3im)) < tol
  normTPS(hypot(t1,2+2im,t3) - hypot(1,2+2im,3)) < tol
  normTPS(hypot(1+1im,t2,t3) - hypot(1+1im,2,3)) < tol
  normTPS(hypot(t1,2,3+3im) - hypot(1,2,3+3im)) < tol
  normTPS(hypot(1,t2,3+3im) - hypot(1,2,3+3im)) < tol
  normTPS(hypot(1+1im,2,t3) - hypot(1+1im,2,3)) < tol
=#
  d = Descriptor(1, 5)
  t = TPS{Float64}(use=d)
  t[0] = 0.5; t[[1]] = 2; t[[2]] = 3; t[[3]] = 4; t[[4]] = 5; t[[5]] = 6

  tol = 1e-10

  normTPS(sin(t)^2+cos(t)^2 - 1) < tol
  normTPS(1/sin(t) - csc(t)) < tol
  normTPS(1/cos(t) - sec(t)) < tol
  normTPS(1/tan(t) - cot(t)) < tol
  normTPS(sin(t)/cos(t) - tan(t)) < tol
  normTPS(cos(2*t) - cos(t)^2 + sin(t)^2) < tol
  normTPS(sec(t)^2 - 1 - tan(t)^2) < tol
  normTPS(sin(t/2) - sqrt((1-cos(t))/2)) < tol
  normTPS(cos(t/2) - sqrt((1+cos(t))/2)) < tol
  normTPS(sqrt(t^2) - abs(t)) < tol
  normTPS(csc(t)^2 - cot(t)^2 - 1) < tol
  normTPS(exp(log(t)) - t) < tol
  normTPS(log(exp(t)) - t) < tol
  normTPS(log(exp(t)) - exp(log(t))) < tol
  normTPS(log(t^2) - 2*log(t)) < tol
  normTPS(5*log(t) - log(t^5)) < tol
  normTPS(t*log(5) - log(5^t)) < tol
  normTPS(sinc(t) - sin(pi*t)/(pi*t)) < tol
  normTPS(sinhc(t/pi) - sinh(t)/t) < tol
  normTPS(exp(im*t) - cos(t) - im*sin(t)) < tol
  normTPS(real(exp(im*t)) - cos(t)) < tol
  normTPS(imag(exp(im*t)) - sin(t)) < tol
  normTPS(sinh(t) - (exp(t) - exp(-t))/2) < tol
  normTPS(cosh(t) - (exp(t) + exp(-t))/2) < tol
  normTPS(tanh(t) - sinh(t)/cosh(t)) < tol
  normTPS(csch(t) - 1/sinh(t)) < tol
  normTPS(sech(t) - 1/cosh(t)) < tol
  normTPS(coth(t) - cosh(t)/sinh(t)) < tol
  normTPS(coth(t) - 1/tanh(t)) < tol
  normTPS(cosh(t)^2 - sinh(t)^2 - 1) < tol
  normTPS(1 - tanh(t)^2 - sech(t)^2) < tol
  normTPS(coth(t)^2 - 1 - csch(t)^2) < tol
  normTPS(asin(sin(t)) - t) < tol
  normTPS(acos(cos(t)) - t) < tol
  normTPS(atan(tan(t)) - t) < tol
  normTPS(acsc(1/t) - asin(t)) < tol
  normTPS(asec(1/t) - acos(t)) < tol
  normTPS(acot(1/t) - atan(t)) < tol
  normTPS(asinh(sinh(t)) - t) < tol
  normTPS(acosh(cosh(t)) - t) < tol
  normTPS(atanh(tanh(t)) - t) < tol
  normTPS(acsch(t) - asinh(1/t)) < tol
  normTPS(asech(t) - acosh(1/t)) < tol
  normTPS(acoth(1/t) - atanh(t)) < tol
  normTPS(asinc(t/pi) - asin(t)/t) < tol
  normTPS(asinhc(t/pi) - asinh(t)/t) < tol
  normTPS(erfc(t) - 1 + erf(t)) < tol
  normTPS(erf(-t) + erf(t)) < tol
  normTPS(angle(t)) < tol
  normTPS(complex(t) - t) < tol
  normTPS(complex(t,t) - (t+im*t)) < tol

  t = TPS{ComplexF64}(t)
  t[0] = 0.5+0.5im; t[[1]] = 2+2im; t[[2]] = 3+3im; t[[3]] = 4+4im; t[[4]] = 5+5im; t[[5]] = 6+6im
  normTPS(sin(t)^2+cos(t)^2 - 1) < tol
  normTPS(1/sin(t) - csc(t)) < tol
  normTPS(1/cos(t) - sec(t)) < tol
  normTPS(1/tan(t) - cot(t)) < tol
  normTPS(sin(t)/cos(t) - tan(t)) < tol
  normTPS(cos(2*t) - cos(t)^2 + sin(t)^2) < tol
  normTPS(sec(t)^2 - 1 - tan(t)^2) < tol
  normTPS(sin(t/2) - sqrt((1-cos(t))/2)) < tol
  normTPS(cos(t/2) - sqrt((1+cos(t))/2)) < tol
  normTPS(sqrt(t^2) - t) < tol
  normTPS(csc(t)^2 - cot(t)^2 - 1) < tol
  normTPS(exp(log(t)) - t) < tol
  normTPS(log(exp(t)) - t) < tol
  normTPS(log(exp(t)) - exp(log(t))) < tol
  normTPS(log(t^2) - 2*log(t)) < tol
  normTPS(5*log(t) - log(t^5) - 2*pi*im) < tol
  normTPS(t*log(5) - log(5^t)) < tol
  normTPS(sinc(t/pi) - sin(t)/t) < tol
  normTPS(sinhc(t/pi) - sinh(t)/t) < tol
  normTPS(exp(im*t) - cos(t) - im*sin(t)) < tol
  normTPS(sinh(t) - (exp(t) - exp(-t))/2) < tol
  normTPS(cosh(t) - (exp(t) + exp(-t))/2) < tol
  normTPS(tanh(t) - sinh(t)/cosh(t)) < tol
  normTPS(csch(t) - 1/sinh(t)) < tol
  normTPS(sech(t) - 1/cosh(t)) < tol
  normTPS(coth(t) - cosh(t)/sinh(t)) < tol
  normTPS(coth(t) - 1/tanh(t)) < tol
  normTPS(cosh(t)^2 - sinh(t)^2 - 1) < tol
  normTPS(1 - tanh(t)^2 - sech(t)^2) < tol
  normTPS(coth(t)^2 - 1 - csch(t)^2) < tol
  
  normTPS(asin(sin(t)) - t) < tol
  normTPS(acos(cos(t)) - t) < tol
  normTPS(atan(tan(t)) - t) < tol
  normTPS(acsc(t) - asin(1/t)) < tol
  normTPS(asec(t) - acos(1/t)) < tol
  normTPS(acot(t) - atan(1/t)) < tol
  normTPS(asinh(sinh(t)) - t) < tol
  normTPS(acosh(cosh(t)) - t) < tol
  normTPS(atanh(tanh(t)) - t) < tol
  normTPS(acsch(t) - asinh(1/t)) < tol
  normTPS(asech(t) - acosh(1/t)) < tol
  normTPS(acoth(t) - atanh(1/t)) < tol
  normTPS(asinc(t/pi) - asin(t)/t) < tol
  normTPS(asinhc(t/pi) - asinh(t)/t) < tol
  
  normTPS(erfc(t) - 1 + erf(t)) < tol
  normTPS(erf(-t) + erf(t)) < tol
  normTPS(angle(t) - atan(imag(t),real(t))) < tol
  normTPS(complex(t) - t) < tol

  d = Descriptor(3,10,2,10)
  v = @vars(d)
  p = @params(d)
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
  t = TPS{Float64}(use=d)
  ct = TPS{ComplexF64}(t)
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

  # TPS{Float64}:
  @FastGTPSA(normTPS(t1 + t2 - t3)) < tol
  @FastGTPSA(normTPS(t2 + t1 - t3)) < tol
  @FastGTPSA(normTPS(t1 + 2 - t3)) < tol
  @FastGTPSA(normTPS(2 + t1 - t3)) < tol
  @FastGTPSA(normTPS(t3 - t2 - t1)) < tol
  @FastGTPSA(normTPS(t2 - t3 - -t1)) < tol
  @FastGTPSA(normTPS(t3 - 2 - t1)) < tol
  @FastGTPSA(normTPS(2 - t3 - -t1)) < tol
  @FastGTPSA(normTPS(t2 * t3 - 6)) < tol
  @FastGTPSA(normTPS(t3 * t2 - 6)) < tol
  @FastGTPSA(normTPS(t2 * 5 - 10)) < tol
  @FastGTPSA(normTPS(5 * t2 - 10 * t1)) < tol
  @FastGTPSA(normTPS(t1 / t2 - 1/2)) < tol
  @FastGTPSA(normTPS(t2 / t1 - 2)) < tol
  @FastGTPSA(normTPS(1 / t2 - 1/2)) < tol
  @FastGTPSA(normTPS(t2 / 3 - 2/3)) < tol
  @FastGTPSA(normTPS(t2 / t2 - t1)) < tol
  @FastGTPSA(normTPS(t2 / t2 - 1)) < tol
  @FastGTPSA(normTPS(t2 ^ t3 - 8)) < tol
  @FastGTPSA(normTPS(t3 ^ t2 - 9)) < tol
  @FastGTPSA(normTPS(t2 ^ 3 - 8)) < tol
  @FastGTPSA(normTPS(t2 ^ (1/2) - sqrt(2))) < tol
  @FastGTPSA(normTPS(t2 ^ (1/2) - sqrt(t2))) < tol
  @FastGTPSA(normTPS(2 ^ t3 - 8)) < tol
  @FastGTPSA(normTPS(inv(t3) - 1/t3)) < tol
  @FastGTPSA(normTPS(inv(t3) - 1/3)) < tol

  # TPS{ComplexF64}:
  @FastGTPSA(normTPS(ct1 + ct2 - ct3)) < tol
  @FastGTPSA(normTPS(ct2 + ct1 - ct3)) < tol
  @FastGTPSA(normTPS(ct1 + (2+2im) - ct3)) < tol
  @FastGTPSA(normTPS((2+2im) + ct1 - ct3)) < tol
  @FastGTPSA(normTPS(ct3 - ct2 - ct1)) < tol
  @FastGTPSA(normTPS(ct2 - ct3 - -ct1)) < tol
  @FastGTPSA(normTPS(ct3 - (2+2im) - ct1)) < tol
  @FastGTPSA(normTPS((2+2im) - ct3 - -ct1)) < tol
  @FastGTPSA(normTPS(ct2 * ct3 - (2+2im)*(3+3im))) < tol
  @FastGTPSA(normTPS(ct3 * ct2 - (2+2im)*(3+3im))) < tol
  @FastGTPSA(normTPS(ct2 * 5 - (10+10im))) < tol
  @FastGTPSA(normTPS(5 * ct2 - (10 * ct1))) < tol
  @FastGTPSA(normTPS(ct1 / ct2 - (1+im)/(2+2im))) < tol
  @FastGTPSA(normTPS(ct2 / ct1 - 2)) < tol
  @FastGTPSA(normTPS(1 / ct2 - 1/(2+2im))) < tol
  @FastGTPSA(normTPS(ct2 / 3 - (2+2im)/3)) < tol
  @FastGTPSA(normTPS(ct2 / ct2 - 1)) < tol
  @FastGTPSA(normTPS(ct2 ^ ct3 - (2+2im)^(3+3im))) < tol
  @FastGTPSA(normTPS(ct3 ^ ct2 - (3+3im)^(2+2im))) < tol
  @FastGTPSA(normTPS(ct2 ^ 3 - (2+2im)^3)) < tol
  @FastGTPSA(normTPS(ct2 ^ (1/2) - sqrt(2+2im))) < tol
  @FastGTPSA(normTPS(ct2 ^ (1/2) - sqrt(ct2))) < tol
  @FastGTPSA(normTPS(2 ^ ct3 - 2^(3+3im))) < tol
  @FastGTPSA(normTPS(inv(ct3) - 1/ct3)) < tol
  @FastGTPSA(normTPS(inv(ct3) - 1/(3+3im))) < tol

  # Promotion of TPS{Float64} to TPS{ComplexF64}
  @FastGTPSA(normTPS(t1 + ct2 - (1 + (2+2im)))) < tol
  @FastGTPSA(normTPS(ct2 + t1 - (1 + (2+2im)))) < tol
  @FastGTPSA(normTPS(t1 + (2+2im) - (1 + (2+2im)))) < tol
  @FastGTPSA(normTPS((2+2im) + t1 - (1 + (2+2im)))) < tol
  @FastGTPSA(normTPS(t3 - ct2 - (3 - (2+2im)))) < tol
  @FastGTPSA(normTPS(ct2 - t3 - ((2+2im) - 3))) < tol
  @FastGTPSA(normTPS(t3 - (2+2im) - (3 - (2+2im)))) < tol
  @FastGTPSA(normTPS((2+2im) - t3 - ((2+2im) - 3))) < tol
  @FastGTPSA(normTPS(t2 * ct3 - 2 * (3+3im))) < tol
  @FastGTPSA(normTPS(ct3 * t2 - 2 * (3+3im))) < tol
  @FastGTPSA(normTPS(t2 * (3+3im) - 2 * (3+3im))) < tol
  @FastGTPSA(normTPS((3+3im) * t2 - 2 * (3+3im))) < tol
  @FastGTPSA(normTPS(t2 / ct3 - 2/(3+3im))) < tol
  @FastGTPSA(normTPS(ct3 / t2 - (3+3im)/2)) < tol
  @FastGTPSA(normTPS(t2 / (3+3im) - 2/(3+3im))) < tol
  @FastGTPSA(normTPS((3+3im) / t2 - (3+3im)/2)) < tol
  @FastGTPSA(normTPS(t2 ^ ct3 - 2^(3+3im))) < tol
  @FastGTPSA(normTPS(ct3 ^ t2 - (3+3im)^2)) < tol
  @FastGTPSA(normTPS(t2 ^ (3+3im) - 2^(3+3im))) < tol
  @FastGTPSA(normTPS((3+3im)^t2 - (3+3im)^2)) < tol

  # Make sure stack is 0:
  desc = unsafe_load(GTPSA.getdesc(t1).desc)
  tmpidx = unsafe_load(desc.ti)
  ctmpidx = unsafe_load(desc.cti)
  ctmpidx == 0
  tmpidx == 0

  d = Descriptor(1, 5)
  t = TPS{Float64}(use=d)
  v = 0.5
  t[0] = v
  tol = 1e-14
  t1 = TPS{Float64}(t)
  t1[0] = 1
  t2 = zero(t1)
  t2[0] = 2
  t3 = zero(t1)
  t3[0] = 3


  @FastGTPSA(normTPS(abs(-t) - abs(-v) )) < tol
  @FastGTPSA(normTPS(sqrt(t) - sqrt(v))) < tol
  @FastGTPSA(normTPS(exp(t) - exp(v))) < tol
  @FastGTPSA(normTPS(log(t) - log(v))) < tol
  @FastGTPSA(normTPS(sin(t) - sin(v))) < tol
  @FastGTPSA(normTPS(cos(t) - cos(v))) < tol
  @FastGTPSA(normTPS(tan(t) - tan(v))) < tol
  @FastGTPSA(normTPS(csc(t) - csc(v))) < tol
  @FastGTPSA(normTPS(sec(t) - sec(v))) < tol
  @FastGTPSA(normTPS(cot(t) - cot(v))) < tol
  @FastGTPSA(normTPS(sinc(t) - sinc(v))) < tol
  @FastGTPSA(normTPS(sinh(t) - sinh(v))) < tol
  @FastGTPSA(normTPS(cosh(t) - cosh(v))) < tol
  @FastGTPSA(normTPS(tanh(t) - tanh(v))) < tol
  @FastGTPSA(normTPS(csch(t) - csch(v))) < tol
  @FastGTPSA(normTPS(sech(t) - sech(v))) < tol
  @FastGTPSA(normTPS(coth(t) - coth(v))) < tol
  @FastGTPSA(normTPS(asin(t) - asin(v))) < tol
  @FastGTPSA(normTPS(acos(t) - acos(v))) < tol
  @FastGTPSA(normTPS(atan(t) - atan(v))) < tol
  @FastGTPSA(normTPS(acsc(1/t) - acsc(1/v))) < tol
  @FastGTPSA(normTPS(asec(1/t) - asec(1/v))) < tol
  @FastGTPSA(normTPS(acot(1/t) - acot(1/v))) < tol
  @FastGTPSA(normTPS(asinh(t) - asinh(v))) < tol
  @FastGTPSA(normTPS(acosh(1/t) - acosh(1/v))) < tol
  @FastGTPSA(normTPS(atanh(t) - atanh(v))) < tol
  @FastGTPSA(normTPS(acsch(1/t) - acsch(1/v))) < tol
  @FastGTPSA(normTPS(asech(t) - asech(v))) < tol
  @FastGTPSA(normTPS(acoth(1/t) - acoth(1/v))) < tol
  @FastGTPSA(normTPS(asinc(t/pi) - asin(v)/(v))) < tol
  @FastGTPSA(normTPS(asinhc(t/pi) - asinh(v)/(v))) < tol
  @FastGTPSA(normTPS(zero(t) - zero(v))) < tol
  @FastGTPSA(normTPS(real(t) - real(v))) < tol
  @FastGTPSA(normTPS(imag(t) - imag(v))) < tol
  @FastGTPSA(normTPS(conj(t) - conj(v))) < tol
  @FastGTPSA(normTPS(sinhc(t/pi) - sinh(v)/v)) < tol
  @FastGTPSA(normTPS(erf(t) - erf(v))) < tol
  @FastGTPSA(normTPS(erfc(t) - erfc(v))) < tol
  @FastGTPSA(normTPS(-im*erf(t*im) - erfi(v))) < tol
  @FastGTPSA(normTPS(atan(t3,t2) - atan(3,2))) < tol
  @FastGTPSA(normTPS(atan(t3,2) - atan(3,2))) < tol
  @FastGTPSA(normTPS(atan(3,t2) - atan(3,2))) < tol
  @FastGTPSA(normTPS(atan(t3,-t2) - atan(3,-2))) < tol
  @FastGTPSA(normTPS(atan(t3,-2) - atan(3,-2))) < tol
  @FastGTPSA(normTPS(atan(3,-t2) - atan(3,-2))) < tol
  @FastGTPSA(normTPS(atan(-t3,-t2) - atan(-3,-2))) < tol
  @FastGTPSA(normTPS(atan(-t3,-2) - atan(-3,-2))) < tol
  @FastGTPSA(normTPS(atan(-3,-t2) - atan(-3,-2))) < tol
  @FastGTPSA(normTPS(atan(-t3,t2) - atan(-3,2))) < tol
  @FastGTPSA(normTPS(atan(-t3,2) - atan(-3,2))) < tol
  @FastGTPSA(normTPS(atan(-3,t2) - atan(-3,2))) < tol
  #=
  @FastGTPSA(normTPS(hypot(t2,t3) - hypot(2,3))) < tol
  @FastGTPSA(normTPS(hypot(2,t3) - hypot(2,3))) < tol
  @FastGTPSA(normTPS(hypot(t2,3) - hypot(2,3))) < tol
  @FastGTPSA(normTPS(hypot(t1,t2,t3) - hypot(1,2,3))) < tol
  @FastGTPSA(normTPS(hypot(1, t2, t3) - hypot(1,2,3))) < tol
  @FastGTPSA(normTPS(hypot(t1, 2, t3) - hypot(1,2,3))) < tol
  @FastGTPSA(normTPS(hypot(t1, t2, 3) - hypot(1,2,3))) < tol
  @FastGTPSA(normTPS(hypot(1, 2, t3) - hypot(1,2,3))) < tol
  @FastGTPSA(normTPS(hypot(1, t2, 3) - hypot(1,2,3))) < tol
  @FastGTPSA(normTPS(hypot(t1, 2, 3) - hypot(1,2,3))) < tol
  =#
  @FastGTPSA(normTPS(angle(t2) - angle(2))) < tol
  @FastGTPSA(normTPS(angle(-t2) - angle(-2))) < tol
  @FastGTPSA(normTPS(complex(t3) - complex(3))) < tol
  @FastGTPSA(normTPS(complex(t2,t3) - complex(2,3))) < tol
  @FastGTPSA(normTPS(polar(t2) - (abs(2)+im*atan(0,2)))) < tol
  @FastGTPSA(normTPS(polar(-t1) - (abs(-1)+im*atan(0,-1)))) < tol
  @FastGTPSA(normTPS(rect(t2) - (2*cos(0) + im*2*sin(0)))) < tol
  @FastGTPSA(normTPS(rect(-t1) - (-1*cos(0) + im*-1*sin(0)))) < tol
  

  v = 0.5+0.5im
  t = TPS{ComplexF64}(t)
  t[0] = v
  ct1 = TPS{ComplexF64}(t)
  ct1[0] = 1 + 1im
  ct2 = zero(ct1)
  ct2[0] = 2 + 2im
  ct3 = zero(ct1)
  ct3[0] = 3 + 3im
  @FastGTPSA(normTPS(abs(-t) - abs(-v) )) < tol
  @FastGTPSA(normTPS(sqrt(t) - sqrt(v))) < tol
  @FastGTPSA(normTPS(exp(t) - exp(v))) < tol
  @FastGTPSA(normTPS(log(t) - log(v))) < tol
  @FastGTPSA(normTPS(sin(t) - sin(v))) < tol
  @FastGTPSA(normTPS(cos(t) - cos(v))) < tol
  @FastGTPSA(normTPS(tan(t) - tan(v))) < tol
  @FastGTPSA(normTPS(csc(t) - csc(v))) < tol
  @FastGTPSA(normTPS(sec(t) - sec(v))) < tol
  @FastGTPSA(normTPS(cot(t) - cot(v))) < tol
  @FastGTPSA(normTPS(sinc(t) - sinc(v))) < tol
  @FastGTPSA(normTPS(sinh(t) - sinh(v))) < tol
  @FastGTPSA(normTPS(cosh(t) - cosh(v))) < tol
  @FastGTPSA(normTPS(tanh(t) - tanh(v))) < tol
  @FastGTPSA(normTPS(csch(t) - csch(v))) < tol
  @FastGTPSA(normTPS(sech(t) - sech(v))) < tol
  @FastGTPSA(normTPS(coth(t) - coth(v))) < tol

  @FastGTPSA(normTPS(asin(t) - asin(v))) < tol
  @FastGTPSA(normTPS(acos(t) - acos(v))) < tol
  @FastGTPSA(normTPS(atan(t) - atan(v))) < tol
  @FastGTPSA(normTPS(acsc(t) - acsc(v))) < tol
  @FastGTPSA(normTPS(asec(t) - asec(v))) < tol
  @FastGTPSA(normTPS(acot(t) - acot(v))) < tol
  @FastGTPSA(normTPS(asinh(t) - asinh(v))) < tol
  @FastGTPSA(normTPS(acosh(t) - acosh(v))) < tol
  @FastGTPSA(normTPS(atanh(t) - atanh(v))) < tol
  @FastGTPSA(normTPS(acsch(t) - acsch(v))) < tol
  @FastGTPSA(normTPS(asech(t) - asech(v))) < tol
  @FastGTPSA(normTPS(acoth(t) - acoth(v))) < tol
  @FastGTPSA(normTPS(asinc(t/pi) - asin(v)/v)) < tol
  @FastGTPSA(normTPS(asinhc(t/pi) - asinh(v)/v)) < tol
  
  @FastGTPSA(normTPS(zero(t) - zero(v))) < tol
  @FastGTPSA(normTPS(real(t) - real(v))) < tol
  @FastGTPSA(normTPS(imag(t) - imag(v))) < tol
  @FastGTPSA(normTPS(conj(t) - conj(v))) < tol
  @FastGTPSA(normTPS(sinhc(t/pi) - sinh(v)/v)) < tol
  @FastGTPSA(normTPS(erf(t) - erf(v))) < tol
  @FastGTPSA(normTPS(erfc(t) - erfc(v))) < tol
  @FastGTPSA(normTPS(-im*erf(t*im) - erfi(v))) < tol
  #=
  @FastGTPSA(normTPS(hypot(ct2,ct3) - hypot(2+2im,3+3im))) < tol
  @FastGTPSA(normTPS(hypot(2+2im,ct3) - hypot(2+2im,3+3im))) < tol
  @FastGTPSA(normTPS(hypot(ct2,3+3im) - hypot(2+2im,3+3im))) < tol
  @FastGTPSA(normTPS(hypot(ct1,ct2,ct3) - hypot(1+1im,2+2im,3+3im))) < tol
  @FastGTPSA(normTPS(hypot(1+1im, ct2, ct3) - hypot(1+1im,2+2im,3+3im))) < tol
  @FastGTPSA(normTPS(hypot(ct1, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im))) < tol
  @FastGTPSA(normTPS(hypot(ct1, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im))) < tol
  @FastGTPSA(normTPS(hypot(1+1im, 2+2im, ct3) - hypot(1+1im,2+2im,3+3im))) < tol
  @FastGTPSA(normTPS(hypot(1+1im, ct2, 3+3im) - hypot(1+1im,2+2im,3+3im))) < tol
  @FastGTPSA(normTPS(hypot(ct1, 2+2im, 3+3im) - hypot(1+1im,2+2im,3+3im))) < tol
  =#
  @FastGTPSA(normTPS(angle(t2+im*t3) - angle(2+3im))) < tol
  @FastGTPSA(normTPS(angle(t2-im*t3) - angle(2-3im))) < tol
  @FastGTPSA(normTPS(angle(-t2-im*t3) - angle(-2-3im))) < tol
  @FastGTPSA(normTPS(angle(-t2+im*t3) - angle(-2+3im))) < tol
  @FastGTPSA(normTPS(angle(ct2) - angle(2+2im))) < tol
  @FastGTPSA(normTPS(angle(-ct2) - angle(-2-2im))) < tol
  @FastGTPSA(normTPS(complex(ct3) - complex(3+3im))) < tol
  @FastGTPSA(normTPS(polar(ct2) - (abs(2+2im)+im*angle(2+2im)))) < tol
  @FastGTPSA(normTPS(polar(-ct1) - (abs(-1-im)+im*angle(-1-im)))) < tol
  @FastGTPSA(normTPS(rect(ct2) - (2*cos(2) + im*2*sin(2)))) < tol
  @FastGTPSA(normTPS(rect(-ct1) - (-1*cos(-1) + im*-1*sin(-1)))) < tol
  #=
  # Hypot, mixing TPS{Float64} with TPS{ComplexF64}
  @FastGTPSA(normTPS(hypot(ct1, ct2, t3) - hypot(1+1im,2+2im,3))) < tol
  @FastGTPSA(normTPS(hypot(ct1, t2, ct3) - hypot(1+1im,2,3+3im))) < tol
  @FastGTPSA(normTPS(hypot(t1, ct2, ct3) - hypot(1,2+2im,3+3im))) < tol
  @FastGTPSA(normTPS(hypot(ct1, t2, t3) - hypot(1+1im,2,3))) < tol
  @FastGTPSA(normTPS(hypot(t1, ct2, t3) - hypot(1,2+2im,3))) < tol
  @FastGTPSA(normTPS(hypot(t1, t2, ct3) - hypot(1,2,3+3im))) < tol
  @FastGTPSA(normTPS(hypot(ct1,t2,3+3im) - hypot(1+1im,2,3+3im))) < tol
  @FastGTPSA(normTPS(hypot(t1, ct2, 3+3im) - hypot(1,2+2im,3+3im))) < tol
  @FastGTPSA(normTPS(hypot(ct1,2+2im,t3) - hypot(1+1im,2+2im,3))) < tol
  @FastGTPSA(normTPS(hypot(t1,2+2im,ct3) - hypot(1,2+2im,3+3im))) < tol
  @FastGTPSA(normTPS(hypot(1+1im,ct2,t3) - hypot(1+1im,2+2im,3))) < tol
  @FastGTPSA(normTPS(hypot(1+1im, t2, ct3) - hypot(1+1im,2,3+3im))) < tol
  @FastGTPSA(normTPS(hypot(t1,t2,3+3im) - hypot(1,2,3+3im))) < tol
  @FastGTPSA(normTPS(hypot(t1,2+2im,t3) - hypot(1,2+2im,3))) < tol
  @FastGTPSA(normTPS(hypot(1+1im,t2,t3) - hypot(1+1im,2,3))) < tol
  @FastGTPSA(normTPS(hypot(t1,2,3+3im) - hypot(1,2,3+3im))) < tol
  @FastGTPSA(normTPS(hypot(1,t2,3+3im) - hypot(1,2,3+3im))) < tol
  @FastGTPSA(normTPS(hypot(1+1im,2,t3) - hypot(1+1im,2,3))) < tol
=#
  # Make sure stack is 0:
  desc = unsafe_load(GTPSA.getdesc(t1).desc)
  tmpidx = unsafe_load(desc.ti)
  ctmpidx = unsafe_load(desc.cti)
  ctmpidx == 0
  tmpidx == 0

  d = Descriptor(1, 5)
  t = TPS{Float64}(use=d)
  t[0] = 0.5; t[[1]] = 2; t[[2]] = 3; t[[3]] = 4; t[[4]] = 5; t[[5]] = 6

  tol = 1e-10

  @FastGTPSA(normTPS(sin(t)^2+cos(t)^2 - 1)) < tol
  @FastGTPSA(normTPS(1/sin(t) - csc(t))) < tol
  @FastGTPSA(normTPS(1/cos(t) - sec(t))) < tol
  @FastGTPSA(normTPS(1/tan(t) - cot(t))) < tol
  @FastGTPSA(normTPS(sin(t)/cos(t) - tan(t))) < tol
  @FastGTPSA(normTPS(cos(2*t) - cos(t)^2 + sin(t)^2)) < tol
  @FastGTPSA(normTPS(sec(t)^2 - 1 - tan(t)^2)) < tol
  @FastGTPSA(normTPS(sin(t/2) - sqrt((1-cos(t))/2))) < tol
  @FastGTPSA(normTPS(cos(t/2) - sqrt((1+cos(t))/2))) < tol
  @FastGTPSA(normTPS(sqrt(t^2) - abs(t))) < tol
  @FastGTPSA(normTPS(csc(t)^2 - cot(t)^2 - 1)) < tol
  @FastGTPSA(normTPS(exp(log(t)) - t)) < tol
  @FastGTPSA(normTPS(log(exp(t)) - t)) < tol
  @FastGTPSA(normTPS(log(exp(t)) - exp(log(t)))) < tol
  @FastGTPSA(normTPS(log(t^2) - 2*log(t))) < tol
  @FastGTPSA(normTPS(5*log(t) - log(t^5))) < tol
  @FastGTPSA(normTPS(t*log(5) - log(5^t))) < tol
  @FastGTPSA(normTPS(sinc(t) - sin(pi*t)/(pi*t))) < tol
  @FastGTPSA(normTPS(sinhc(t/pi) - sinh(t)/t)) < tol
  @FastGTPSA(normTPS(exp(im*t) - cos(t) - im*sin(t))) < tol
  @FastGTPSA(normTPS(real(exp(im*t)) - cos(t))) < tol
  @FastGTPSA(normTPS(imag(exp(im*t)) - sin(t))) < tol
  @FastGTPSA(normTPS(sinh(t) - (exp(t) - exp(-t))/2)) < tol
  @FastGTPSA(normTPS(cosh(t) - (exp(t) + exp(-t))/2)) < tol
  @FastGTPSA(normTPS(tanh(t) - sinh(t)/cosh(t))) < tol
  @FastGTPSA(normTPS(csch(t) - 1/sinh(t))) < tol
  @FastGTPSA(normTPS(sech(t) - 1/cosh(t))) < tol
  @FastGTPSA(normTPS(coth(t) - cosh(t)/sinh(t))) < tol
  @FastGTPSA(normTPS(coth(t) - 1/tanh(t))) < tol
  @FastGTPSA(normTPS(cosh(t)^2 - sinh(t)^2 - 1)) < tol
  @FastGTPSA(normTPS(1 - tanh(t)^2 - sech(t)^2)) < tol
  @FastGTPSA(normTPS(coth(t)^2 - 1 - csch(t)^2)) < tol
  @FastGTPSA(normTPS(asin(sin(t)) - t)) < tol
  @FastGTPSA(normTPS(acos(cos(t)) - t)) < tol
  @FastGTPSA(normTPS(atan(tan(t)) - t)) < tol
  @FastGTPSA(normTPS(acsc(1/t) - asin(t))) < tol
  @FastGTPSA(normTPS(asec(1/t) - acos(t))) < tol
  @FastGTPSA(normTPS(acot(1/t) - atan(t))) < tol
  @FastGTPSA(normTPS(asinh(sinh(t)) - t)) < tol
  @FastGTPSA(normTPS(acosh(cosh(t)) - t)) < tol
  @FastGTPSA(normTPS(atanh(tanh(t)) - t)) < tol
  @FastGTPSA(normTPS(acsch(t) - asinh(1/t))) < tol
  @FastGTPSA(normTPS(asech(t) - acosh(1/t))) < tol
  @FastGTPSA(normTPS(acoth(1/t) - atanh(t))) < tol
  @FastGTPSA(normTPS(asinc(t/pi) - asin(t)/t)) < tol
  @FastGTPSA(normTPS(asinhc(t/pi) - asinh(t)/t)) < tol
  @FastGTPSA(normTPS(erfc(t) - 1 + erf(t))) < tol
  @FastGTPSA(normTPS(erf(-t) + erf(t))) < tol
  @FastGTPSA(normTPS(angle(t))) < tol
  @FastGTPSA(normTPS(complex(t) - t)) < tol
  @FastGTPSA(normTPS(complex(t,t) - (t+im*t))) < tol

  t = TPS{ComplexF64}(t)
  t[0] = 0.5+0.5im; t[[1]] = 2+2im; t[[2]] = 3+3im; t[[3]] = 4+4im; t[[4]] = 5+5im; t[[5]] = 6+6im
  @FastGTPSA(normTPS(sin(t)^2+cos(t)^2 - 1)) < tol
  @FastGTPSA(normTPS(1/sin(t) - csc(t))) < tol
  @FastGTPSA(normTPS(1/cos(t) - sec(t))) < tol
  @FastGTPSA(normTPS(1/tan(t) - cot(t))) < tol
  @FastGTPSA(normTPS(sin(t)/cos(t) - tan(t))) < tol
  @FastGTPSA(normTPS(cos(2*t) - cos(t)^2 + sin(t)^2)) < tol
  @FastGTPSA(normTPS(sec(t)^2 - 1 - tan(t)^2)) < tol
  @FastGTPSA(normTPS(sin(t/2) - sqrt((1-cos(t))/2))) < tol
  @FastGTPSA(normTPS(cos(t/2) - sqrt((1+cos(t))/2))) < tol
  @FastGTPSA(normTPS(sqrt(t^2) - t)) < tol
  @FastGTPSA(normTPS(csc(t)^2 - cot(t)^2 - 1)) < tol
  @FastGTPSA(normTPS(exp(log(t)) - t)) < tol
  @FastGTPSA(normTPS(log(exp(t)) - t)) < tol
  @FastGTPSA(normTPS(log(exp(t)) - exp(log(t)))) < tol
  @FastGTPSA(normTPS(log(t^2) - 2*log(t))) < tol
  @FastGTPSA(normTPS(5*log(t) - log(t^5) - 2*pi*im)) < tol
  @FastGTPSA(normTPS(t*log(5) - log(5^t))) < tol
  @FastGTPSA(normTPS(sinc(t/pi) - sin(t)/t)) < tol
  @FastGTPSA(normTPS(sinhc(t/pi) - sinh(t)/t)) < tol
  @FastGTPSA(normTPS(exp(im*t) - cos(t) - im*sin(t))) < tol
  @FastGTPSA(normTPS(sinh(t) - (exp(t) - exp(-t))/2)) < tol
  @FastGTPSA(normTPS(cosh(t) - (exp(t) + exp(-t))/2)) < tol
  @FastGTPSA(normTPS(tanh(t) - sinh(t)/cosh(t))) < tol
  @FastGTPSA(normTPS(csch(t) - 1/sinh(t))) < tol
  @FastGTPSA(normTPS(sech(t) - 1/cosh(t))) < tol
  @FastGTPSA(normTPS(coth(t) - cosh(t)/sinh(t))) < tol
  @FastGTPSA(normTPS(coth(t) - 1/tanh(t))) < tol
  @FastGTPSA(normTPS(cosh(t)^2 - sinh(t)^2 - 1)) < tol
  @FastGTPSA(normTPS(1 - tanh(t)^2 - sech(t)^2)) < tol
  @FastGTPSA(normTPS(coth(t)^2 - 1 - csch(t)^2)) < tol
  
  @FastGTPSA(normTPS(asin(sin(t)) - t)) < tol
  @FastGTPSA(normTPS(acos(cos(t)) - t)) < tol
  @FastGTPSA(normTPS(atan(tan(t)) - t)) < tol
  @FastGTPSA(normTPS(acsc(t) - asin(1/t))) < tol
  @FastGTPSA(normTPS(asec(t) - acos(1/t))) < tol
  @FastGTPSA(normTPS(acot(t) - atan(1/t))) < tol
  @FastGTPSA(normTPS(asinh(sinh(t)) - t)) < tol
  @FastGTPSA(normTPS(acosh(cosh(t)) - t)) < tol
  @FastGTPSA(normTPS(atanh(tanh(t)) - t)) < tol
  @FastGTPSA(normTPS(acsch(t) - asinh(1/t))) < tol
  @FastGTPSA(normTPS(asech(t) - acosh(1/t))) < tol
  @FastGTPSA(normTPS(acoth(t) - atanh(1/t))) < tol
  @FastGTPSA(normTPS(asinc(t/pi) - asin(t)/t)) < tol
  @FastGTPSA(normTPS(asinhc(t/pi) - asinh(t)/t)) < tol
  
  @FastGTPSA(normTPS(erfc(t) - 1 + erf(t))) < tol
  @FastGTPSA(normTPS(erf(-t) + erf(t))) < tol
  @FastGTPSA(normTPS(angle(t) - atan(imag(t),real(t)))) < tol
  @FastGTPSA(normTPS(complex(t) - t)) < tol

  # Make sure stack is 0:
  desc = unsafe_load(GTPSA.getdesc(t).desc)
  tmpidx = unsafe_load(desc.ti)
  ctmpidx = unsafe_load(desc.cti)
  ctmpidx == 0
  tmpidx == 0
  return
end
