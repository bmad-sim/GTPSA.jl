using GTPSA
using ForwardDiff
using TaylorSeries
using BenchmarkTools: @btime, @benchmark

# Comparison with GTPSA for 4 variables to 2nd order and 2 parameters to 2nd order
# As of 1/1/2023 (Julia v1.10)
# GTPSA: 1.137 ms (12919 allocations: 239.98 KiB)
# ForwardDiff: 1.235 ms (3051 allocations: 3.95 MiB)
# TaylorSeries: 12.193 ms (268516 allocations: 26.95 MiB)

function track_qf(z0, k1)
  L = 0.5
  z1 =  cos(sqrt(k1)*L)*z0[1]           + 1. /sqrt(k1)*sin(sqrt(k1)*L)*z0[2]
  z2 =  -sqrt(k1)*sin(sqrt(k1)*L)*z0[1] + cos(sqrt(k1)*L)*z0[2] 
  z3 =  cosh(sqrt(k1)*L)*z0[3]          + 1. /sqrt(k1)*sinh(sqrt(k1)*L)*z0[4]
  z4 =  sqrt(k1)*sinh(sqrt(k1)*L)*z0[3] + cosh(sqrt(k1)*L)*z0[4]
  return [z1,z2,z3,z4]
end

function track_qd(z0, k1)
  L = 0.5
  z1 =  cosh(sqrt(k1)*L)*z0[1]          + 1. /sqrt(k1)*sinh(sqrt(k1)*L)*z0[2]
  z2 =  sqrt(k1)*sinh(sqrt(k1)*L)*z0[1] + cosh(sqrt(k1)*L)*z0[2]
  z3 =  cos(sqrt(k1)*L)*z0[3]           + 1. /sqrt(k1)*sin(sqrt(k1)*L)*z0[4]
  z4 =  -sqrt(k1)*sin(sqrt(k1)*L)*z0[3] + cos(sqrt(k1)*L)*z0[4]
  return [z1,z2,z3,z4] 
end

function track_drift(z0)
  L = 0.75
  z1 =  z0[1]+z0[2]*L
  z3 =  z0[3]+z0[4]*L
  return [z1,z0[2],z3 , z0[4]]
end

function track_sextupole(z0, k2l)
  z2 =  z0[2]-k2l/2.0*(z0[1]^2 - z0[3]^2)
  z4 =  z0[4]+k2l/2.0*z0[1]*z0[3]
  return  [z0[1], z2, z0[3], z4]
end

function track_fodo(z0, k1, k2l)
  z1 = track_qf(z0, k1)
  z2 = track_sextupole(z1, k2l)
  z3 = track_drift(z2)
  z4 = track_qd(z3, k1)
  z5 = track_sextupole(z4, -k2l)
  z6 = track_drift(z5)
  return z6
end

function track_ring(z0, k1, k2l)
  for i=1:100
    z0 = track_fodo(z0, k1, k2l)
  end
  return z0
end

function benchmark_GTPSA()
  m(z) =track_ring([z[1], z[2], z[3], z[4]], 0.36+z[5], z[6])
  d = Descriptor(6,2)
  z = vars(d)
  map = m(z)
  #j = jacobian(map)
  #h1 = hessian(map[1])
  #h2 = hessian(map[2])
  #h3 = hessian(map[3])
  #h4 = hessian(map[4])
  return map #j, h1, h2, h3, h4
end


function benchmark_ForwardDiff()
  m(z) = track_ring([z[1], z[2], z[3], z[4]], 0.36+z[5], z[6])
  j = ForwardDiff.jacobian(m, [0,0,0,0,0,0])
  h1 = ForwardDiff.hessian(z->m(z)[1], [0,0,0,0,0,0])
  h2 = ForwardDiff.hessian(z->m(z)[2], [0,0,0,0,0,0])
  h3 = ForwardDiff.hessian(z->m(z)[3], [0,0,0,0,0,0])
  h4 = ForwardDiff.hessian(z->m(z)[4], [0,0,0,0,0,0])
  return j, h1, h2, h3, h4
end


function benchmark_TaylorSeries()
  # TPSA with 4 variables of order 2 and 2 parameters of order 2
  x0, px0, y0, py0, dk2l, dk1 = set_variables("x0 px0 y0 py0 dk2l dk1", order=2)
  k2l_0  = 0.
  k1_0 = 0.36

  k1 = k1_0 + dk1
  k2l = k2l_0 + dk2l
  map = track_ring([x0, px0, y0, py0], k1, k2l)
  return map
end

#m_GTPSA = @btime benchmark_GTPSA()
#m_ForwardDiff = @btime benchmark_ForwardDiff()
#m_TaylorSeries = @btime benchmark_TaylorSeries()