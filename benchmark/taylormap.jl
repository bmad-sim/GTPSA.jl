using GTPSA
using ForwardDiff
using TaylorSeries
using BenchmarkTools: @btime, @benchmark

# Comparison with GTPSA for 4 variables to 2nd order and 2 parameters to 2nd order
# As of 12/27/2023 (Julia v1.10)
# GTPSA:          1.147 ms (12919 allocations: 239.98 KiB)
# ForwardDiff:    2.966 ms (65018 allocations: 11.00 MiB)
# TaylorSeries:  12.333 ms (268516 allocations: 26.95 MiB)

function track_qf(z0, k1)
  L = 0.5
  return [cos(sqrt(k1)*L)*z0[1]           + 1. /sqrt(k1)*sin(sqrt(k1)*L)*z0[2] ,
          -sqrt(k1)*sin(sqrt(k1)*L)*z0[1] + cos(sqrt(k1)*L)*z0[2]              ,
          cosh(sqrt(k1)*L)*z0[3]          + 1. /sqrt(k1)*sinh(sqrt(k1)*L)*z0[4],
          sqrt(k1)*sinh(sqrt(k1)*L)*z0[3] + cosh(sqrt(k1)*L)*z0[4]]
end

function track_qd(z0, k1)
  L = 0.5
  return [cosh(sqrt(k1)*L)*z0[1]          + 1. /sqrt(k1)*sinh(sqrt(k1)*L)*z0[2],
          sqrt(k1)*sinh(sqrt(k1)*L)*z0[1] + cosh(sqrt(k1)*L)*z0[2],
          cos(sqrt(k1)*L)*z0[3]           + 1. /sqrt(k1)*sin(sqrt(k1)*L)*z0[4],
          -sqrt(k1)*sin(sqrt(k1)*L)*z0[3] + cos(sqrt(k1)*L)*z0[4]]  
end

function track_drift(z0)
  L = 0.75
  return  [z0[1]+z0[2]*L, z0[2], z0[3]+z0[4]*L, z0[4]]
end

function track_sextupole(z0, k2l)
  return  [z0[1], z0[2]-k2l/2.0*(z0[1]^2 - z0[3]^2), z0[3]+k2l/2.0*z0[1]*z0[3], z0[4]]
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
  x = vars(d)
  k = params(d)

  k2l_0  = 0.
  k1_0 = 0.36

  k[1] = k1_0 + k[1]
  k[2] = k2l_0 + k[2]
  map = track_ring([x[1], x[2], x[3], x[4]], k[1], k[2])
  return map
end


function benchmark_ForwardDiff()
  m1(x0,px0,y0,py0,k1,k2l) = track_ring([x0, px0, y0, py0], k1, k2l)[1]
  m2(x0,px0,y0,py0,k1,k2l) = track_ring([x0, px0, y0, py0], k1, k2l)[2]
  m3(x0,px0,y0,py0,k1,k2l) = track_ring([x0, px0, y0, py0], k1, k2l)[3]
  m4(x0,px0,y0,py0,k1,k2l) = track_ring([x0, px0, y0, py0], k1, k2l)[4]
  k1_0 = 0.36
  coefs = zeros(27, 4)

  map = [m1,m2,m3,m4]

  for i =1:4
    m = map[i]
    mx(x0, px0, y0, py0, k1, k2l)   = ForwardDiff.derivative(x0->m(x0,px0,y0,py0,k1,k2l), x0)
    mpx(x0, px0, y0, py0, k1, k2l)  = ForwardDiff.derivative(px0->m(x0,px0,y0,py0,k1,k2l),px0)
    my(x0, px0, y0, py0, k1, k2l)   = ForwardDiff.derivative(y0->m(x0,px0,y0,py0,k1,k2l), y0)
    mpy(x0, px0, y0, py0, k1, k2l)  = ForwardDiff.derivative(py0->m(x0,px0,y0,py0,k1,k2l), py0)
    mk1(x0, px0, y0, py0, k1, k2l)  = ForwardDiff.derivative(k1->m(x0,px0,y0,py0,k1,k2l), k1)
    mk2l(x0, px0, y0, py0, k1, k2l) = ForwardDiff.derivative(k2l->m(x0,px0,y0,py0,k1,k2l), k2l)

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