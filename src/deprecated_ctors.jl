# ===== These will be un-deprecated in a future release for python support ===== #

"""
    vars(use::Union{Descriptor,TPS}=GTPSA.desc_current)

Returns a vector of `TPS`s corresponding to the variables for the 
`Descriptor` specified by `use`. Default value is `GTPSA.desc_current`.

### Input
- `use` -- (Optional) Specify which `Descriptor` to use, default is `GTPSA.desc_current`

### Output
- `x`   -- `Vector` containing unit `TPS{Float64}`s corresponding to each variable
"""
function vars(use::Union{Descriptor,TPS}=GTPSA.desc_current)
  #Base.depwarn("`vars` is deprecated, use the @vars macro instead.", :vars)
  getdesc(use).desc != C_NULL || error("Descriptor not defined!")
  nv = numvars(use)
  x = Vector{TPS{Float64,Dynamic}}(undef, nv)
  for i=1:nv
    t = TPS{Float64}(use=use)
    @inbounds t[i] = 1.0
    @inbounds x[i] = t
  end
  return x
end



"""
    params(use::Union{Descriptor,TPS}=GTPSA.desc_current)

Returns a vector of `TPS`s corresponding to the parameters for the 
`Descriptor` specified by `use`. Default value is `GTPSA.desc_current`.

### Input
- `use` -- (Optional) Specify which `Descriptor` to use, default is `GTPSA.desc_current`

### Output
- `x`   -- `Vector` containing unit `TPS{Float64}`s corresponding to each parameters
"""
function params(use::Union{Descriptor,TPS}=GTPSA.desc_current)
  #Base.depwarn("`params` is deprecated, use the @params macro instead.", :params)
  getdesc(use).desc != C_NULL || error("Descriptor not defined!")
  np = numparams(use)
  nv = numvars(use)
  k = Vector{TPS{Float64,Dynamic}}(undef, np)
  for i=1:np
    t = TPS{Float64}(use=use)
    @inbounds t[nv+i] = 1.0
    @inbounds k[i] = t
  end
  return k
end



"""
    complexvars(use::Union{Descriptor,TPS}=GTPSA.desc_current)

Returns a vector of `ComplexTPS64`s corresponding to the variables for the 
`Descriptor` specified by `use`. Default value is `GTPSA.desc_current`.

### Input
- `use` -- (Optional) Specify which `Descriptor` to use, default is `GTPSA.desc_current`

### Output
- `x`   -- `Vector` containing unit `ComplexTPS64`s corresponding to each variable
"""
function complexvars(use::Union{Descriptor,TPS}=GTPSA.desc_current)
  #Base.depwarn("`complexvars` is deprecated, use the @vars macro instead.", :complexvars)
  getdesc(use).desc != C_NULL || error("Descriptor not defined!")
  nv = numvars(use)
  x = Vector{TPS{ComplexF64}}(undef, nv)
  for i=1:nv
    t = TPS{ComplexF64}(use=use)
    @inbounds t[i] = 1.0
    @inbounds x[i] = t
  end
  return x
end

"""
    complexparams(use::Union{Descriptor,TPS}=GTPSA.desc_current)

Returns a vector of `ComplexTPS64`s corresponding to the parameters for the 
`Descriptor` specified by `use`. Default value is `GTPSA.desc_current`.

### Input
- `use` -- (Optional) Specify which `Descriptor` to use, default is `GTPSA.desc_current`

### Output
- `x`   -- `Vector` containing unit `ComplexTPS64`s corresponding to each parameters
"""
function complexparams(use::Union{Descriptor,TPS}=GTPSA.desc_current)
  #Base.depwarn("`complexparams` is deprecated, use the @params macro instead.", :complexparams)
  getdesc(use).desc != C_NULL || error("Descriptor not defined!")
  np = numparams(use)
  nv = numvars(use)
  k = Vector{TPS{ComplexF64}}(undef, np)
  for i=1:np
    t = TPS{ComplexF64}(use=use)
    @inbounds t[nv+i] = 1.0
    @inbounds k[i] = t
  end
  return k
end

