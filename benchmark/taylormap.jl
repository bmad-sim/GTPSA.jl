using GTPSA
using ForwardDiff
using BenchmarkTools: @btime, @benchmark

# Comparison with GTPSA for 4 variables to 2nd order and 50 parameters to 2nd order
# As of 1/4/2023 (Julia v1.10)
# Using the @usetemps macro:
# GTPSA: 6.107 ms (1229 allocations: 140.53 KiB)
# ForwardDiff: 80.202 ms (31875 allocations: 167.75 MiB)
#
# Without the @usetemps macro:
# GTPSA: 21.141 ms (6629 allocations: 224.91 KiB)
# ForwardDiff: 78.312 ms (31875 allocations: 167.75 MiB)

function track_qf(z0, k1, hkick)
  L = 0.5
  z1 = @usetemps cos(sqrt(k1)*L)*z0[1]           + 1. /sqrt(k1)*sin(sqrt(k1)*L)*z0[2]
  z2 = @usetemps -sqrt(k1)*sin(sqrt(k1)*L)*z0[1] + cos(sqrt(k1)*L)*z0[2] + hkick
  z3 = @usetemps cosh(sqrt(k1)*L)*z0[3]          + 1. /sqrt(k1)*sinh(sqrt(k1)*L)*z0[4]
  z4 = @usetemps sqrt(k1)*sinh(sqrt(k1)*L)*z0[3] + cosh(sqrt(k1)*L)*z0[4]
  return [z1,z2,z3,z4]
end

function track_qd(z0, k1, vkick)
  L = 0.5
  z1 = @usetemps cosh(sqrt(k1)*L)*z0[1]          + 1. /sqrt(k1)*sinh(sqrt(k1)*L)*z0[2]
  z2 = @usetemps sqrt(k1)*sinh(sqrt(k1)*L)*z0[1] + cosh(sqrt(k1)*L)*z0[2]
  z3 = @usetemps cos(sqrt(k1)*L)*z0[3]           + 1. /sqrt(k1)*sin(sqrt(k1)*L)*z0[4]
  z4 = @usetemps -sqrt(k1)*sin(sqrt(k1)*L)*z0[3] + cos(sqrt(k1)*L)*z0[4] + vkick
  return [z1,z2,z3,z4] 
end

function track_drift(z0)
  L = 0.75
  z1 = @usetemps z0[1]+z0[2]*L
  z3 = @usetemps z0[3]+z0[4]*L
  return [z1,z0[2],z3 , z0[4]]
end

function track_sextupole(z0, k2l)
  z2 = @usetemps z0[2]-k2l/2.0*(z0[1]^2 - z0[3]^2)
  z4 = @usetemps z0[4]+k2l/2.0*z0[1]*z0[3]
  return  [z0[1], z2, z0[3], z4]
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

function track_ring(z0, k1, k2l, kick)
  for i=1:50
    z0 = track_fodo(z0, k1, k2l, kick[i])
  end
  return z0
end

function benchmark_GTPSA()
  m(z) =track_ring([z[1], z[2], z[3], z[4]], 0.36+z[5], z[6], z[7:end])
  d = Descriptor(4,2,52,2)#,52,2)
  z = vars(d)
  k = params(d)
  map = m([z...,k...])
  j = jacobian(map, include_params=true)
  h1 = hessian(map[1], include_params=true)
  h2 = hessian(map[2], include_params=true)
  h3 = hessian(map[3], include_params=true)
  h4 = hessian(map[4], include_params=true)
  return j, h1, h2, h3, h4
end


function benchmark_ForwardDiff()
  m(z) = track_ring([z[1], z[2], z[3], z[4]], 0.36+z[5], z[6], z[7:end])
  j = Array{Float64}(undef,4,56)
  h1 = Array{Float64}(undef,56,56)
  h2 = Array{Float64}(undef,56,56)
  h3 = Array{Float64}(undef,56,56)
  h4 = Array{Float64}(undef,56,56)
  ForwardDiff.jacobian!(j, m, zeros(56))
  ForwardDiff.hessian!(h1, z->m(z)[1], zeros(56))
  ForwardDiff.hessian!(h2, z->m(z)[2], zeros(56))
  ForwardDiff.hessian!(h3, z->m(z)[3], zeros(56))
  ForwardDiff.hessian!(h4, z->m(z)[4], zeros(56))
  return j, h1, h2, h3, h4
end

#m_GTPSA = @btime benchmark_GTPSA()
#m_ForwardDiff = @btime benchmark_ForwardDiff()