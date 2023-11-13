include("../src/GTPSA.jl")
using .GTPSA
using ForwardDiff

using BenchmarkTools


# Comparison of ForwardDiff with GTPSA for 4 variables to 2nd order and 2 knobs to 2nd order
# As of 11/08/2023, ForwardDiff gives ~19 ms and GTPSA ~7.5 ms

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
  # TPSA with 4 variables of order 3 and 2 knobs of order 3
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
  mad_tpsa_setv!(x0.tpsa, Int32(0), Int32(1+6), Base.unsafe_convert(Ptr{Float64},   [0.,1.,0.,0.,0.,0.,0.]))
  mad_tpsa_setv!(px0.tpsa, Int32(0), Int32(1+6), Base.unsafe_convert(Ptr{Float64},  [0.,0.,1.,0.,0.,0.,0.]))
  mad_tpsa_setv!(y0.tpsa, Int32(0), Int32(1+6), Base.unsafe_convert(Ptr{Float64},   [0.,0.,0.,1.,0.,0.,0.]))
  mad_tpsa_setv!(py0.tpsa, Int32(0), Int32(1+6), Base.unsafe_convert(Ptr{Float64},  [0.,0.,0.,0.,1.,0.,0.]))
  mad_tpsa_setv!(dk1.tpsa, Int32(0), Int32(1+6), Base.unsafe_convert(Ptr{Float64},  [0.,0.,0.,0.,0.,1.,0.]))
  mad_tpsa_setv!(dk2l.tpsa, Int32(0), Int32(1+6), Base.unsafe_convert(Ptr{Float64}, [0.,0.,0.,0.,0.,0.,1.]))

  k1 = k1_0 + dk1
  k2l = k2l_0 + dk2l
  map = track_ring([x0, px0, y0, py0], k1, k2l)
  return map
end


function benchmark_ForwardDiff()
  m1(x0,px0,y0,py0,k1,k2l) = track_ring([x0, px0, y0, py0], k1, k2l)[1]
  m2(x0,px0,y0,py0,k1,k2l) = track_ring([x0, px0, y0, py0], k1, k2l)[2]
  m3(x0,px0,y0,py0,k1,k2l) = track_ring([x0, px0, y0, py0], k1, k2l)[3]
  m4(x0,px0,y0,py0,k1,k2l) = track_ring([x0, px0, y0, py0], k1, k2l)[4]
  k1_0 = 0.36
  coefs = zeros(27, 4)

  # For each thing, calculate
  map = [m1, m2, m3, m4]
  
  for i=1:4
      # First do each 
      m = map[i]
      mx(x0, px0, y0, py0, k1, k2l)   = ForwardDiff.derivative(x0->m(x0,px0,y0,py0,k1,k2l), x0)
      mpx(x0, px0, y0, py0, k1, k2l)  = ForwardDiff.derivative(px0->m(x0,px0,y0,py0,k1,k2l),px0)
      my(x0, px0, y0, py0, k1, k2l)   = ForwardDiff.derivative(y0->m(x0,px0,y0,py0,k1,k2l), y0)
      mpy(x0, px0, y0, py0, k1, k2l)  = ForwardDiff.derivative(py0->m(x0,px0,y0,py0,k1,k2l), py0)
      mk1(x0, px0, y0, py0, k1, k2l)  = ForwardDiff.derivative(k1->m(x0,px0,y0,py0,k1,k2l), k1)
      mk2l(x0, px0, y0, py0, k1, k2l) = ForwardDiff.derivative(k2l->m(x0,px0,y0,py0,k1,k2l), k2l)

      # Again
      mxx(x0, px0, y0, py0, k1, k2l)   = ForwardDiff.derivative(x0->mx(x0,px0,y0,py0,k1,k2l), x0)
      mpxpx(x0, px0, y0, py0, k1, k2l)  = ForwardDiff.derivative(px0->mpx(x0,px0,y0,py0,k1,k2l),px0)
      myy(x0, px0, y0, py0, k1, k2l)   = ForwardDiff.derivative(y0->my(x0,px0,y0,py0,k1,k2l), y0)
      mpypy(x0, px0, y0, py0, k1, k2l)  = ForwardDiff.derivative(py0->mpy(x0,px0,y0,py0,k1,k2l), py0)
      mk1k1(x0, px0, y0, py0, k1, k2l)  = ForwardDiff.derivative(k1->mk1(x0,px0,y0,py0,k1,k2l), k1)
      mk2lk2l(x0, px0, y0, py0, k1, k2l) = ForwardDiff.derivative(k2l->mk2l(x0,px0,y0,py0,k1,k2l), k2l)

      # Now all cross terms
      mxpx(x0, px0, y0, py0, k1, k2l)   = ForwardDiff.derivative(px0->mx(x0,px0,y0,py0,k1,k2l), px0)
      mxy(x0, px0, y0, py0, k1, k2l)   = ForwardDiff.derivative(y0->mx(x0,px0,y0,py0,k1,k2l), y0)
      mxpy(x0, px0, y0, py0, k1, k2l)   = ForwardDiff.derivative(py0->mx(x0,px0,y0,py0,k1,k2l), py0)
      mxk1(x0, px0, y0, py0, k1, k2l)   = ForwardDiff.derivative(k1->mx(x0,px0,y0,py0,k1,k2l), k1)
      mxk2l(x0, px0, y0, py0, k1, k2l)   = ForwardDiff.derivative(k2l->mx(x0,px0,y0,py0,k1,k2l), k2l)

      mpxy(x0, px0, y0, py0, k1, k2l)  = ForwardDiff.derivative(y0->mpx(x0,px0,y0,py0,k1,k2l),y0)
      mpxpy(x0, px0, y0, py0, k1, k2l)  = ForwardDiff.derivative(py0->mpx(x0,px0,y0,py0,k1,k2l),py0)
      mpxk1(x0, px0, y0, py0, k1, k2l)  = ForwardDiff.derivative(k1->mpx(x0,px0,y0,py0,k1,k2l),k1)
      mpxk2l(x0, px0, y0, py0, k1, k2l)  = ForwardDiff.derivative(k2l->mpx(x0,px0,y0,py0,k1,k2l),k2l)

      mypy(x0, px0, y0, py0, k1, k2l)   = ForwardDiff.derivative(py0->my(x0,px0,y0,py0,k1,k2l), py0)
      myk1(x0, px0, y0, py0, k1, k2l)   = ForwardDiff.derivative(k1->my(x0,px0,y0,py0,k1,k2l), k1)
      myk2l(x0, px0, y0, py0, k1, k2l)   = ForwardDiff.derivative(k2l->my(x0,px0,y0,py0,k1,k2l), k2l)

      mpyk1(x0, px0, y0, py0, k1, k2l)  = ForwardDiff.derivative(k1->mpy(x0,px0,y0,py0,k1,k2l), k1)
      mpyk2l(x0, px0, y0, py0, k1, k2l)  = ForwardDiff.derivative(k2l->mpy(x0,px0,y0,py0,k1,k2l), k2l)

      mk1k2l(x0, px0, y0, py0, k1, k2l)  = ForwardDiff.derivative(k2l->mk1(x0,px0,y0,py0,k1,k2l), k2l)
      
      coefs[1 ,i] = mx(0,0,0,0,k1_0,0)
      coefs[2 ,i] = mpx(0,0,0,0,k1_0,0)
      coefs[3 ,i] = my(0,0,0,0,k1_0,0)
      coefs[4 ,i] = mpy(0,0,0,0,k1_0,0)
      coefs[5 ,i] = mk1(0,0,0,0,k1_0,0)
      coefs[6 ,i] = mk2l(0,0,0,0,k1_0,0)
      coefs[7 ,i] = mxx(0,0,0,0,k1_0,0)   
      coefs[8 ,i] = mpxpx(0,0,0,0,k1_0,0) 
      coefs[9 ,i] = myy(0,0,0,0,k1_0,0)   
      coefs[10,i] = mpypy(0,0,0,0,k1_0,0) 
      coefs[11,i] = mk1k1(0,0,0,0,k1_0,0) 
      coefs[12,i] = mk2lk2l(0,0,0,0,k1_0,0)
      coefs[13,i] = mxpx(0,0,0,0,k1_0,0)  
      coefs[14,i] = mxy(0,0,0,0,k1_0,0)   
      coefs[15,i] = mxpy(0,0,0,0,k1_0,0)  
      coefs[16,i] = mxk1(0,0,0,0,k1_0,0)  
      coefs[17,i] = mxk2l(0,0,0,0,k1_0,0) 
      coefs[18,i] = mpxy(0,0,0,0,k1_0,0)  
      coefs[19,i] = mpxpy(0,0,0,0,k1_0,0) 
      coefs[20,i] = mpxk1(0,0,0,0,k1_0,0) 
      coefs[21,i] = mpxk2l(0,0,0,0,k1_0,0)
      coefs[22,i] = mypy(0,0,0,0,k1_0,0)  
      coefs[23,i] = myk1(0,0,0,0,k1_0,0)  
      coefs[24,i] = myk2l(0,0,0,0,k1_0,0) 
      coefs[25,i] = mpyk1(0,0,0,0,k1_0,0) 
      coefs[26,i] = mpyk2l(0,0,0,0,k1_0,0)
      coefs[27,i] = mk1k2l(0,0,0,0,k1_0,0)

  end

  return coefs
end

#m = @btime benchmark_ForwardDiff()
m = @btime benchmark_GTPSA()
