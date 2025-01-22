using GTPSA
using ForwardDiff
using BenchmarkTools: @btime, @benchmark

# As of 07/25/2024, Julia v1.10.3 on Mac M2 Ultra: Comparison with GTPSA for 58 inputs and 6 outputs
# Numbers calculated using BenchmarkTools.@btime
#
# 3rd Order ---------------------------------------------------------
# Using the @FastGTPSA macro:
# GTPSA:                    199.609 ms (2929 allocations: 360.36 MiB)
# ForwardDiff:            3.427 s      (85142 allocations: 3.93 GiB)
#
# Without the @FastGTPSA macro (including ForwardDiff as control):
# GTPSA:                    202.778 ms (19129 allocations: 2.52 GiB)
# ForwardDiff:            3.944 s      (85142 allocations: 3.93 GiB)
#
# 2nd Order ---------------------------------------------------------
# Using the @FastGTPSA macro:
# GTPSA:                    6.465 ms (2927 allocations: 17.81 MiB)
# ForwardDiff:             25.577 ms (9166 allocations: 63.47 MiB)
#
# Without the @FastGTPSA macro (including ForwardDiff as control):
# GTPSA:                   13.131 ms (19129 allocations: 127.70 MiB)
# ForwardDiff:             22.839 ms (9166 allocations: 63.47 MiB)
#
# 1st Order ---------------------------------------------------------
# Using the @FastGTPSA macro:
# GTPSA:                  282.375 μs (2927 allocations: 715.48 KiB)
# ForwardDiff:            223.375 μs (1520 allocations: 1.10 MiB)
#
# Without the @FastGTPSA macro (including ForwardDiff as control):
# GTPSA:                  473.917 μs (19129 allocations: 4.84 MiB)
# ForwardDiff:            158.208 μs (1520 allocations: 1.10 MiB)
#
# Note that @FastGTPSA is transparent to all types except TPS types, so it can be
# inserted into functions while still maintaining generic code, as shown here.

function track_qf(z0, k1, hkick)
  z = Vector{promote_type(eltype(z0),typeof(k1),typeof(hkick))}(undef, length(z0))
  lbend=0.1
  
  @FastGTPSA begin
  L  = 0.5/(1.0+z0[6])
  h  = -L*(z0[2]^2+k1*z0[1]^2+ z0[4]^2-k1*z0[3]^2)/(1.0+z0[6])/2.0
  z[1] = cos(sqrt(k1)*L)*z0[1]+1/sqrt(k1)*sin(sqrt(k1)*L)*z0[2]
  z[2] = -sqrt(k1)*sin(sqrt(k1)*L)*z0[1]+cos(sqrt(k1)*L)*z0[2]+hkick+lbend*z0[6]
  z[3] = cosh(sqrt(k1)*L)*z0[3]+1/sqrt(k1)*sinh(sqrt(k1)*L)*z0[4]
  z[4] = sqrt(k1)*sinh(sqrt(k1)*L)*z0[3]+cosh(sqrt(k1)*L)*z0[4]
  z[5] = z0[5]+h-lbend*z[1]
  z[6] = z0[6]
  end

  return z
end

function track_qd(z0, k1, vkick)
  z = Vector{promote_type(eltype(z0),typeof(k1),typeof(vkick))}(undef, length(z0))

  @FastGTPSA begin
  L  = 0.5/(1.0+z0[6])
  h  = -L*(z0[2]^2-k1*z0[1]^2+z0[4]^2+k1*z0[3]^2)/(1.0+z0[6])/2.0
  z[1] = cosh(sqrt(k1)*L)*z0[1]+1/sqrt(k1)*sinh(sqrt(k1)*L)*z0[2]
  z[2] = sqrt(k1)*sinh(sqrt(k1)*L)*z0[1]+cosh(sqrt(k1)*L)*z0[2]
  z[3] = cos(sqrt(k1)*L)*z0[3]+1/sqrt(k1)*sin(sqrt(k1)*L)*z0[4]
  z[4] = -sqrt(k1)*sin(sqrt(k1)*L)*z0[3]+cos(sqrt(k1)*L)*z0[4]+vkick 
  z[5] = z0[5]+h
  z[6] = z0[6]
  end
  
  return z
end

function track_drift(z0)
  z = Vector{eltype(z0)}(undef, length(z0))

  L = 0.75
  @FastGTPSA begin
  z[1] = z0[1]+z0[2]*L/(1.0+z0[6])
  z[2] = z0[2]
  z[3] = z0[3]+z0[4]*L/(1.0+z0[6])
  z[4] = z0[4]
  z[5] = z0[5]-L*((z0[2]^2)+(z0[4]^2))/(1.0+z0[6])^2/2.0
  z[6] = z0[6] 
  end
  return z
end

function track_cav(z0)
  z = Vector{eltype(z0)}(undef, length(z0))
  z[1] = z0[1]
  z[2] = z0[2]
  z[3] = z0[3]
  z[4] = z0[4]
  z[5] = z0[5]
  z[6] = @FastGTPSA z0[6]+0.0001*z0[5]
  return z
end

function track_sextupole(z0, k2l)
  z = Vector{promote_type(eltype(z0),typeof(k2l))}(undef, length(z0))

  @FastGTPSA begin
  z[1] = z0[1]
  z[2] = z0[2]-k2l*(z0[1]^2-z0[3]^2)
  z[3] = z0[3]
  z[4] = z0[4]+k2l*2.0*z0[1]*z0[3]
  z[5] = z0[5]
  z[6] = z0[6]
  end
  return z
end

function track_fodo(z0, k1, k2l, kick)
  z1 = track_qf(z0, k1, kick)
  z2 = track_sextupole(z1, k2l)
  z3 = track_drift(z2)
  z4 = track_qd(z3, k1, 0)
  z5 = track_sextupole(z4, -k2l)
  z6 = track_drift(z5)
  return z6
end

function track_ring(z0, k1=0.36, k2l=1.2, kick=zeros(50))
  for i=1:50
    z0 = track_fodo(z0, k1, k2l, kick[i])
  end
  z0 = track_cav(z0)
  return z0
end

function benchmark_GTPSA1()
  d = Descriptor(6,1,52,1)
  z = @vars(d,dynamic=true)
  k = @params(d,dynamic=true)
  map = track_ring(z, 0.36+k[1], 1.2+k[2], k[3:end])
  return map
end

function benchmark_GTPSA2()
  d = Descriptor(6,2,52,2)
  z = @vars(d,dynamic=true)
  k = @params(d,dynamic=true)
  map = track_ring(z, 0.36+k[1], 1.2+k[2], k[3:end])
  return map
end

function benchmark_GTPSA3()
  d = Descriptor(6,3,52,3)
  z = @vars(d,dynamic=true)
  k = @params(d,dynamic=true)
  map = track_ring(z, 0.36+k[1], 1.2+k[2], k[3:end])
  return map
end

function benchmark_ForwardDiff1()
  m(z) = track_ring([z[1], z[2], z[3], z[4], z[5], z[6]], 0.36+z[7], 1.2+z[8], z[9:end])
  j = Array{Float64}(undef,6,58)
 
 
  ForwardDiff.jacobian!(j, m, zeros(58))
  
 
  return j
end

function benchmark_ForwardDiff2()
  m(z) = track_ring([z[1], z[2], z[3], z[4], z[5], z[6]], 0.36+z[7], 1.2+z[8], z[9:end])
  j = Array{Float64}(undef,6,58)
  h = Array{Float64}(undef,348,58)
  
  ForwardDiff.jacobian!(j, m, zeros(58))
  ForwardDiff.jacobian!(h, z->ForwardDiff.jacobian(z->m(z), z), zeros(58))
  
  return j, h
end

function benchmark_ForwardDiff3()
  m(z) = track_ring([z[1], z[2], z[3], z[4], z[5], z[6]], 0.36+z[7], 1.2+z[8], z[9:end])
  j = Array{Float64}(undef,6,58)
  h = Array{Float64}(undef,348,58)
  c = Array{Float64}(undef,20184,58)
  ForwardDiff.jacobian!(j, m, zeros(58))
  ForwardDiff.jacobian!(h, z->ForwardDiff.jacobian(z->m(z), z), zeros(58))
  ForwardDiff.jacobian!(c, z->ForwardDiff.jacobian(z->ForwardDiff.jacobian(z->m(z), z), z), zeros(58))
  return j, h, c
end

