using GTPSA

function track_qf(z0, k1)
  L = 0.5
  M_qf  = [cos(sqrt(k1)*L)            1. /sqrt(k1)*sin(sqrt(k1)*L)    0.                           0.;                             
          -sqrt(k1)*sin(sqrt(k1)*L)  cos(sqrt(k1)*L)               0.                          0.                          ;
          0.                          0.                             cosh(sqrt(k1)*L)            1. /sqrt(k1)*sinh(sqrt(k1)*L);
          0.                          0.                             sqrt(k1)*sinh(sqrt(k1)*L)   cosh(sqrt(k1)*L)             ]

  return M_qf*z0
end

function track_qd(z0, k1)
  L = 0.5
  M_qd = [cosh(sqrt(k1)*L)            1. /sqrt(k1)*sinh(sqrt(k1)*L)    0.                           0.;                             
          sqrt(k1)*sinh(sqrt(k1)*L)  cosh(sqrt(k1)*L)               0.                          0.                          ;
          0.                          0.                             cos(sqrt(k1)*L)            1. /sqrt(k1)*sin(sqrt(k1)*L);
          0.                          0.                             -sqrt(k1)*sin(sqrt(k1)*L)   cos(sqrt(k1)*L)             ]  
  return M_qd*z0
end

function track_drift(z0)
  L = 0.75
  M_d =  [1. L 0. 0.;
          0. 1. 0. 0.;
          0. 0. 1. L;
          0. 0. 0. 1.]
  return M_d*z0
end

function track_sextupole(z0, k2l)
  return z0+[0., -k2l/2.0*(z0[1]^2 - z0[3]^2), +k2l/2.0*z0[1]*z0[3], 0.]
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
  # TPSA with 4 variables of order 2 and 2 parameters of order 2
  d = Descriptor(4, 2, 2, 2)
  x0 = TPSA(d)
  px0 = TPSA(d)
  y0 = TPSA(d)
  py0 = TPSA(d)

  k2l_0  = 0.
  k1_0 = 0.36

  dk1 = TPSA(d)
  dk2l = TPSA(d)

  # Set TPSAs
  x0[1,0,0,0,0,0] = 1
  px0[0,1,0,0,0,0] = 1
  y0[0,0,1,0,0,0] = 1
  py0[0,0,0,1,0,0] = 1
  dk1[0,0,0,0,1,0] = 1
  dk2l[0,0,0,0,0,1] = 1

  k1 = k1_0 + dk1
  k2l = k2l_0 + dk2l
  map = track_ring([x0, px0, y0, py0], k1, k2l)
  return map
end
