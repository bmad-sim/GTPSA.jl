
function _vars(::Val{D}) where {D}
  nv = numvars(D)
  x = Vector{TPS{Float64,D}}(undef, nv)
  for i=1:nv
    t = TPS{Float64,D}()
    @inbounds t[i] = 1.0
    @inbounds x[i] = t
  end
  return x
end

function _params(::Val{D}) where {D}
  np = numparams(D)
  nv = numvars(D)
  k = Vector{TPS{Float64,D}}(undef, np)
  for i=1:np
    t = TPS{Float64,D}()
    @inbounds t[nv+i] = 1.0
    @inbounds k[i] = t
  end
  return k
end