include("../src/GTPSA.jl")
using .GTPSA
using BenchmarkTools

# Simple example of GTPSA with a FODO cell

function track_qf(z0::Vector{TPSA}, k1::TPSA)::Vector{TPSA}
  L = 0.5
  M_qf  = [cos(sqrt(k1)*L)            1. /sqrt(k1)*sin(sqrt(k1)*L)    0.                           0.;                             
          -sqrt(k1)*sin(sqrt(k1)*L)  cos(sqrt(k1)*L)               0.                          0.                          ;
          0.                          0.                             cosh(sqrt(k1)*L)            1. /sqrt(k1)*sinh(sqrt(k1)*L);
          0.                          0.                             sqrt(k1)*sinh(sqrt(k1)*L)   cosh(sqrt(k1)*L)             ]

  return M_qf*z0
end

function track_qd(z0::Vector{TPSA}, k1::TPSA)::Vector{TPSA}
  L = 0.5
  M_qd = [cosh(sqrt(k1)*L)            1. /sqrt(k1)*sinh(sqrt(k1)*L)    0.                           0.;                             
          sqrt(k1)*sinh(sqrt(k1)*L)  cosh(sqrt(k1)*L)               0.                          0.                          ;
          0.                          0.                             cos(sqrt(k1)*L)            1. /sqrt(k1)*sin(sqrt(k1)*L);
          0.                          0.                             -sqrt(k1)*sin(sqrt(k1)*L)   cos(sqrt(k1)*L)             ]  
  return M_qd*z0
end

function track_drift(z0::Vector{TPSA})::Vector{TPSA}
  L = 0.75
  M_d =  [1. L 0. 0.;
          0. 1. 0. 0.;
          0. 0. 1. L;
          0. 0. 0. 1.]
  return M_d*z0
end

function track_sextupole(z0::Vector{TPSA}, k2l::TPSA)::Vector{TPSA}
  return z0+[0., -k2l/2.0*(z0[1]^2 - z0[3]^2), +k2l/2.0*z0[1]*z0[3], 0.]
end

function track_fodo(z0::Vector{TPSA}, k1::TPSA, k2l::TPSA)::Vector{TPSA}
  z1 = track_qf(z0, k1)
  z2 = track_sextupole(z1, k2l)
  z3 = track_drift(z2)
  z4 = track_qd(z3, k1)
  z5 = track_sextupole(z4, -k2l)
  z6 = track_drift(z5)
  return z6
end

function track_ring(z0::Vector{TPSA}, k1::TPSA, k2l::TPSA)::Vector{TPSA}
  for i=1:100
    z0 = track_fodo(z0, k1, k2l)
  end
  return z0
end

function benchmark_GTPSA()::Vector{TPSA}
  # Fifth order TPSA with 4 variables of order 5 and 2 knobs to 1st order
  d = Descriptor(4, 5, 2, 1)
  x0 = TPSA(d)
  px0 = TPSA(d)
  y0 = TPSA(d)
  py0 = TPSA(d)

  k2l_0  = 0.
  k1_0 = 0.36

  dk1 = TPSA(d)
  dk2l = TPSA(d)

  # Set TPSAs
  mad_tpsa_setv!(x0.tpsa, Int32(0), Int32(1+6), Base.unsafe_convert(Ptr{Float64},   [0.,1.,0.,0.,0.,0.,0.]))
  mad_tpsa_setv!(px0.tpsa, Int32(0), Int32(1+6), Base.unsafe_convert(Ptr{Float64},  [0.,0.,1.,0.,0.,0.,0.]))
  mad_tpsa_setv!(y0.tpsa, Int32(0), Int32(1+6), Base.unsafe_convert(Ptr{Float64},   [0.,0.,0.,1.,0.,0.,0.]))
  mad_tpsa_setv!(py0.tpsa, Int32(0), Int32(1+6), Base.unsafe_convert(Ptr{Float64},  [0.,0.,0.,0.,1.,0.,0.]))
  mad_tpsa_setv!(dk1.tpsa, Int32(0), Int32(1+6), Base.unsafe_convert(Ptr{Float64},  [0.,0.,0.,0.,0.,1.,0.]))
  mad_tpsa_setv!(dk2l.tpsa, Int32(0), Int32(1+6), Base.unsafe_convert(Ptr{Float64}, [0.,0.,0.,0.,0.,0.,0.]))

  k1 = k1_0 + dk1
  k2l = k2l_0 + dk2l
  map = track_ring([x0, px0, y0, py0], k1, k2l)
  return map
end

m = @btime benchmark_GTPSA()
