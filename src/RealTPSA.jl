module RealTPSA
export RTPSA

mutable struct RTPSA{T}
  d::T                      # Ptr to tpsa descriptor
  uid::Cint                 # Special user field for external use (and padding)
  mo::Cuchar                # max ord (allocated)
  lo::Cuchar                # lowest used ord
  hi::Cuchar                # highest used ord
  nz::Culonglong            # zero/nonzero homogenous polynomials. Int64 if 64 bit else 32 bit
  nam::Ref{Cuchar}       # tpsa name
  coef::Ptr{Cdouble}     # warning: must be identical to ctpsa up to coef excluded
end
end
