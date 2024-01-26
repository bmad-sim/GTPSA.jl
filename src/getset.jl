# --- Setters ---
function setindex!(t::TPS, v::Real, ords::Integer...)
  mad_tpsa_setm!(t.tpsa, convert(Cint, length(ords)), convert(Vector{Cuchar}, [ords...]), 0.0, convert(Cdouble, v))
end

function setindex!(t::TPS, v::Real, vars::Pair{<:Integer, <:Integer}...; params::Vector{<:Pair{<:Integer,<:Integer}}=Pair{Int,Int}[])
  sm, n = pairs_to_sm(t, vars, params=params)
  mad_tpsa_setsm!(t.tpsa, n, sm, 0.0, convert(Cdouble, v))
end

function setindex!(ct::ComplexTPS, v::Number, ords::Integer...)
  mad_ctpsa_setm!(ct.tpsa, convert(Cint, length(ords)), convert(Vector{Cuchar}, [ords...]), convert(ComplexF64, 0), convert(ComplexF64, v))
end

function setindex!(ct::ComplexTPS, v::Number, vars::Pair{<:Integer, <:Integer}...; params::Vector{<:Pair{<:Integer,<:Integer}}=Pair{Int,Int}[])
  sm, n = pairs_to_sm(ct, vars, params=params)
  mad_ctpsa_setsm!(ct.tpsa, n, sm, convert(ComplexF64, 0), convert(ComplexF64, v))
end


# --- Getters ---
function getindex(t::TPS, ords::Integer...)::Float64
  return mad_tpsa_getm(t.tpsa, convert(Cint, length(ords)), convert(Vector{Cuchar}, [ords...]))
end

function getindex(t::TPS, vars::Pair{<:Integer, <:Integer}...; params::Vector{<:Pair{<:Integer,<:Integer}}=Pair{Int,Int}[])::Float64
  # use sparse monomial getter
  sm, n = pairs_to_sm(t, vars, params=params)
  return mad_tpsa_getsm(t.tpsa, n, sm)
end


function getindex(ct::ComplexTPS, ords::Integer...)::ComplexF64
  return mad_ctpsa_getm(ct.tpsa, convert(Cint, length(ords)), convert(Vector{Cuchar}, [ords...]))
end

function getindex(ct::ComplexTPS, vars::Pair{<:Integer, <:Integer}...; params::Vector{<:Pair{<:Integer,<:Integer}}=Pair{Int,Int}[])::ComplexF64
  # use sparse monomial getter
  sm, n = pairs_to_sm(ct, vars, params=params)
  return mad_ctpsa_getsm(ct.tpsa, n, sm)
end

# --- gradient, jacobian, hessian getters ---
"""
    gradient!(result::Vector{Float64}, t::TPS; include_params=false)

Extracts the first-order partial derivatives (evaluated at 0) from the `TPS` and fills the `result` 
vector in-place. The partial derivatives wrt the parameters will also be extracted 
when the `include_params` flag is set to `true`. Note that this function is not 
calculating anything - just extracting the first-order monomial coefficients already 
in the `TPS`.

### Input
- `t`              -- `TPS` to extract the gradient from
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `result`         -- Preallocated `Vector` to fill with the gradient of the TPS
"""
function gradient!(result::Vector{Float64}, t::TPS; include_params=false)
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d))
  n = desc.nv
  if include_params
    n += desc.np
  end
  if length(result) != n
    error("Incorrect size for result")
  end
  mad_tpsa_getv!(t.tpsa, Cint(1), n, result)
end

"""
    gradient(t::TPS; include_params=false)::Vector{Float64}

Extracts the first-order partial derivatives (evaluated at 0) from the `TPS`. The partial 
derivatives wrt the parameters will also be extracted when the `include_params` flag is 
set to `true`. Note that this function is not calculating anything - just extracting the 
first-order monomial coefficients already in the `TPS`.

### Input
- `t`              -- `TPS` to extract the gradient from
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `grad`           -- Gradient of the TPS
"""
function gradient(t::TPS; include_params=false)::Vector{Float64}
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d))
  n = desc.nv
  if include_params
    n += desc.np
  end
  grad = Vector{Float64}(undef, n)
  mad_tpsa_getv!(t.tpsa, Cint(1), n, grad)
  return grad
end

"""
    gradient!(result::Vector{ComplexF64}, ct::ComplexTPS; include_params=false)

Extracts the first-order partial derivatives (evaluated at 0) from the `ComplexTPS` and fills the `result` 
vector in-place. The partial derivatives wrt the parameters will also be extracted 
when the `include_params` flag is set to `true`. Note that this function is not 
calculating anything - just extracting the first-order monomial coefficients already 
in the `ComplexTPS`.

### Input
- `ct`              -- `ComplexTPS` to extract the gradient from
- `include_params`  -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `result`          -- Preallocated `Vector` to fill with the gradient of the ComplexTPS
"""
function gradient!(result::Vector{ComplexF64}, ct::ComplexTPS; include_params=false)
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(ct.tpsa).d))
  n = desc.nv
  if include_params
    n += desc.np
  end
  if length(result) != n
    error("Incorrect size for result")
  end
  mad_ctpsa_getv!(ct.tpsa, Cint(1), n, result)
end

"""
    gradient(ct::ComplexTPS; include_params=false)::Vector{ComplexF64}

Extracts the first-order partial derivatives (evaluated at 0) from the `ComplexTPS`. The partial 
derivatives wrt the parameters will also be extracted when the `include_params` flag is 
set to `true`. Note that this function is not calculating anything - just extracting the 
first-order monomial coefficients already in the `ComplexTPS`.

### Input
- `ct`              -- `ComplexTPS` to extract the gradient from
- `include_params`  -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `grad`            -- Gradient of the ComplexTPS
"""
function gradient(ct::ComplexTPS; include_params=false)::Vector{ComplexF64}
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(ct.tpsa).d))
  n = desc.nv
  if include_params
    n += desc.np
  end
  grad = Vector{ComplexF64}(undef, n)
  mad_ctpsa_getv!(ct.tpsa, Cint(1), n, grad)
  return grad
end

"""
    jacobian!(result::Matrix{Float64}, m::Vector{TPS}; include_params=false)

Extracts the first-order partial derivatives (evaluated at 0) from the Vector of `TPS`s. 
and fills the `result` matrix in-place. The partial derivatives wrt the parameters will 
also be extracted when the `include_params` flag is set to `true`. Note that this function 
is not calculating anything - just extracting the first-order monomial coefficients already 
in the `TPS`s..

### Input
- `m`              -- `Vector` of `TPS`s. to extract the Jacobian from
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `result`         -- Preallocated matrix to fill with the Jacobian of `m`
"""
function jacobian!(result::Matrix{Float64}, m::Vector{TPS}; include_params=false)
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(m[1].tpsa).d))
  n = desc.nv
  if include_params
    n += desc.np
  end
  if size(result)[2] != n
    error("Incorrect size for result")
  end
  grad = Vector{Float64}(undef, n)
  # This is not fully in-place technically, bc Julia is column-major and 
  # filling each row in place without allocating temp would require row-major
  # So there are allocations for the array grad
  for i=1:length(m)
    mad_tpsa_getv!(m[i].tpsa, Cint(1), n, grad)
    result[i,:] = grad
  end
end

"""
    jacobian(m::Vector{TPS}; include_params=false)::Matrix{Float64}

Extracts the first-order partial derivatives (evaluated at 0) from the Vector of `TPS`s.. 
The partial derivatives wrt the parameters will also be extracted when the `include_params` 
flag is set to `true`. Note that this function is not calculating anything - just extracting 
the first-order monomial coefficients already in the `TPS`s..

### Input
- `m`              -- `Vector` of `TPS`s. to extract the Jacobian from
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `J`              -- Jacobian of `m`
"""
function jacobian(m::Vector{TPS}; include_params=false)::Matrix{Float64}
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(m[1].tpsa).d))
  n = desc.nv
  if include_params
    n += desc.np
  end
  J = Matrix{Float64}(undef, length(m), n)
  grad = Vector{Float64}(undef, n)
  for i=1:length(m)
    mad_tpsa_getv!(m[i].tpsa, Cint(1), n, grad)
    J[i,:] = grad
  end
  return J
end

"""
    jacobian!(result::Matrix{ComplexF64}, m::Vector{ComplexTPS}; include_params=false)

Extracts the first-order partial derivatives (evaluated at 0) from the Vector of `ComplexTPS`s. 
and fills the `result` matrix in-place. The partial derivatives wrt the parameters will 
also be extracted when the `include_params` flag is set to `true`. Note that this function 
is not calculating anything - just extracting the first-order monomial coefficients already 
in the `ComplexTPS`s..

### Input
- `m`              -- `Vector` of `ComplexTPS`s. to extract the Jacobian from
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `result`         -- Preallocated matrix to fill with the Jacobian of `m`
"""
function jacobian!(result::Matrix{ComplexF64}, m::Vector{ComplexTPS}; include_params=false)
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(m[1].tpsa).d))
  n = desc.nv
  if include_params
    n += desc.np
  end
  if size(result)[2] != n
    error("Incorrect size for result")
  end
  grad = Vector{ComplexF64}(undef, n)
  # This is not fully in-place technically, bc Julia is column-major and 
  # filling each row in place without allocating temp would require row-major
  # So there are allocations for the array grad
  for i=1:length(m)
    mad_ctpsa_getv!(m[i].tpsa, Cint(1), n, grad)
    result[i,:] = grad
  end
end

"""
    jacobian(m::Vector{ComplexTPS}; include_params=false)::Matrix{ComplexF64}

Extracts the first-order partial derivatives (evaluated at 0) from the Vector of `ComplexTPS`s.. 
The partial derivatives wrt the parameters will also be extracted when the `include_params` 
flag is set to `true`. Note that this function is not calculating anything - just extracting 
the first-order monomial coefficients already in the `ComplexTPS`s..

### Input
- `m`              -- `Vector` of `ComplexTPS`s. to extract the Jacobian from
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `J`              -- Jacobian of `m`
"""
function jacobian(m::Vector{ComplexTPS}; include_params=false)::Matrix{ComplexF64}
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(m[1].tpsa).d))
  n = desc.nv
  if include_params
    n += desc.np
  end
  J = Matrix{ComplexF64}(undef, length(m), n)
  grad = Vector{ComplexF64}(undef, n)
  for i=1:length(m)
    mad_ctpsa_getv!(m[i].tpsa, Cint(1), n, grad)
    J[i,:] = grad
  end
  return J
end

"""
    hessian!(result::Matrix{Float64},t::TPS; include_params=false)

Extracts the second-order partial derivatives (evaluated at 0) from the `TPS` 
and fills the `result` matrix in-place. The partial derivatives wrt the parameters will 
also be extracted when the `include_params` flag is set to `true`. Note that this function 
is not calculating anything - just extracting the second-order monomial coefficients already 
in the `TPS`.

### Input
- `t`              -- `TPS` to extract the Hessian from
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `result`         -- Preallocated matrix to fill with the Hessian of the TPS
"""
function hessian!(result::Matrix{Float64},t::TPS; include_params=false)
  d = Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d)
  desc = unsafe_load(d)
  n = desc.nv
  if include_params
    n += desc.np
  end
  if size(result) != (n,n)
    error("Incorrect size for result")
  end
  # Check that all vars/params are >= 2nd orders
  for i=1:n
    if unsafe_load(desc.no, i) < 0x2
      error("Hessian undefined for TPSA with at least one variable/parameter of order < 2")
    end
  end
  result[:] .= 0.
  idx = Cint(desc.nv+desc.np)
  maxidx = Cint(floor(n*(n+1)/2))+n
  v = Ref{Cdouble}()
  mono = Vector{UInt8}(undef, n)
  idx = mad_tpsa_cycle!(t.tpsa, idx, n, mono, v)
  while idx > 0 && idx <= maxidx
    i = findfirst(x->x==0x1, mono)
    if isnothing(i)
      i = findfirst(x->x==0x2, mono)
      H[i,i] = v[]*2    # Multiply by 2 because taylor coefficient on diagonal is 1/2!*d2f/dx2
    else 
      j = findlast(x->x==0x1, mono)
      H[i,j] = v[]
      H[j,i] = v[]
    end
    idx = mad_tpsa_cycle!(t.tpsa, idx, n, mono, v)
  end
end

"""
    hessian(t::TPS; include_params=false)::Matrix{Float64}

Extracts the second-order partial derivatives (evaluated at 0) from the `TPS`.
The partial derivatives wrt the parameters will also be extracted when the `include_params` 
flag is set to `true`. Note that this function is not calculating anything - just extracting 
the second-order monomial coefficients already in the `TPS`.

### Input
- `t`              -- `TPS` to extract the Hessian from
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `H`              -- Hessian of the TPS
"""
function hessian(t::TPS; include_params=false)::Matrix{Float64}
  d = Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d)
  desc = unsafe_load(d)
  n = desc.nv
  if include_params
    n += desc.np
  end
  # Check that all vars/params are >= 2nd orders
  for i=1:n
    if unsafe_load(desc.no, i) < 0x2
      error("Hessian undefined for TPSA with at least one variable/parameter of order < 2")
    end
  end
  H = zeros(Float64, n, n)
  idx = Cint(desc.nv+desc.np)
  maxidx = Cint(floor(n*(n+1)/2))+n
  v = Ref{Cdouble}()
  mono = Vector{UInt8}(undef, n)
  idx = mad_tpsa_cycle!(t.tpsa, idx, n, mono, v)
  while idx > 0 && idx <= maxidx
    i = findfirst(x->x==0x1, mono)
    if isnothing(i)
      i = findfirst(x->x==0x2, mono)
      H[i,i] = v[]*2    # Multiply by 2 because taylor coefficient on diagonal is 1/2!*d2f/dx2
    else 
      j = findlast(x->x==0x1, mono)
      H[i,j] = v[]
      H[j,i] = v[]
    end
    idx = mad_tpsa_cycle!(t.tpsa, idx, n, mono, v)
  end
  return H
end

"""
    hessian!(result::Matrix{ComplexF64},ct::ComplexTPS; include_params=false)

Extracts the second-order partial derivatives (evaluated at 0) from the `ComplexTPS` 
and fills the `result` matrix in-place. The partial derivatives wrt the parameters will 
also be extracted when the `include_params` flag is set to `true`. Note that this function 
is not calculating anything - just extracting the second-order monomial coefficients already 
in the `ComplexTPS`.

### Input
- `ct`              -- `ComplexTPS` to extract the Hessian from
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `result`         -- Preallocated matrix to fill with the Hessian of the ComplexTPS
"""
function hessian!(result::Matrix{ComplexF64},ct::ComplexTPS; include_params=false)
  d = Base.unsafe_convert(Ptr{Desc}, unsafe_load(ct.tpsa).d)
  desc = unsafe_load(d)
  n = desc.nv
  if include_params
    n += desc.np
  end
  if size(result) != (n,n)
    error("Incorrect size for result")
  end
  # Check that all vars/params are >= 2nd orders
  for i=1:n
    if unsafe_load(desc.no, i) < 0x2
      error("Hessian undefined for TPSA with at least one variable/parameter of order < 2")
    end
  end
  result[:] .= 0.
  idx = Cint(desc.nv+desc.np)
  maxidx = Cint(floor(n*(n+1)/2))+n
  v = Ref{ComplexF64}()
  mono = Vector{UInt8}(undef, n)
  idx = mad_ctpsa_cycle!(ct.tpsa, idx, n, mono, v)
  while idx > 0 && idx <= maxidx
    i = findfirst(x->x==0x1, mono)
    if isnothing(i)
      i = findfirst(x->x==0x2, mono)
      H[i,i] = v[]*2    # Multiply by 2 because taylor coefficient on diagonal is 1/2!*d2f/dx2
    else 
      j = findlast(x->x==0x1, mono)
      H[i,j] = v[]
      H[j,i] = v[]
    end
    idx = mad_ctpsa_cycle!(ct.tpsa, idx, n, mono, v)
  end
end

"""
    hessian(ct::ComplexTPS; include_params=false)::Matrix{ComplexF64}

Extracts the second-order partial derivatives (evaluated at 0) from the `ComplexTPS`.
The partial derivatives wrt the parameters will also be extracted when the `include_params` 
flag is set to `true`. Note that this function is not calculating anything - just extracting 
the second-order monomial coefficients already in the `ComplexTPS`.

### Input
- `t`              -- `ComplexTPS` to extract the Hessian from
- `include_params` -- (Optional) Extract partial derivatives wrt parameters. Default is false

### Output
- `H`              -- Hessian of the ComplexTPS
"""
function hessian(ct::ComplexTPS; include_params=false)::Matrix{ComplexF64}
  d = Base.unsafe_convert(Ptr{Desc}, unsafe_load(ct.tpsa).d)
  desc = unsafe_load(d)
  n = desc.nv
  if include_params
    n += desc.np
  end
  for i=1:n
    if unsafe_load(desc.no, i) < 0x2
      error("Hessian undefined for TPSA with at least one variable/parameter of order < 2")
    end
  end
  H = zeros(ComplexF64, n, n)
  idx = Cint(desc.nv+desc.np)
  maxidx = Cint(floor(n*(n+1)/2))+n
  v = Ref{ComplexF64}()
  mono = Vector{UInt8}(undef, n)
  idx = mad_ctpsa_cycle!(ct.tpsa, idx, n, mono, v)
  while idx > 0 && idx <= maxidx
    i = findfirst(x->x==0x1, mono)
    if isnothing(i)
      i = findfirst(x->x==0x2, mono)
      H[i,i] = v[]*2    # Multiply by 2 because taylor coefficient on diagonal is 1/2!*d2f/dx2
    else 
      j = findlast(x->x==0x1, mono)
      H[i,j] = v[]
      H[j,i] = v[]
    end
    idx = mad_ctpsa_cycle!(ct.tpsa, idx, n, mono, v)
  end
  return H
end