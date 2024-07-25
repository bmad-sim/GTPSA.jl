struct Descriptor
  desc::Ptr{Desc}
  function Descriptor(desc::Ptr{Desc})::Descriptor
    d = new(desc)
    return d
  end
end

"""
    Descriptor(nv::Integer, mo::Integer)::Descriptor

Creates a TPSA `Descriptor` with `nv` variables, and a maximum truncation order `mo`.

### Input
- `nv` -- Number of variables in the TPSA
- `mo` -- Maximum truncation order of the TPSA
"""
function Descriptor(nv::Integer, mo::Integer)::Descriptor
  d = Descriptor(mad_desc_newv(convert(Cint, nv), convert(Cuchar, mo)))
  GTPSA.desc_current = d
  return d
end

"""
    Descriptor(vos::Vector{<:Integer}, mo::Integer)::Descriptor

Creates a TPSA `Descriptor` with `length(vos)` variables with individual truncation 
orders specified in the Vector `vos`, and a maximum truncation order `mo` for the TPSA.

### Input
- `vos` -- `Vector` of the individual truncation orders of each variable
- `mo`  -- Maximum truncation order of the TPSA, <= sum(vos)
"""
function Descriptor(vos::Vector{<:Integer}, mo::Integer)::Descriptor
  all(vos .<= mo) == true || error("Atleast one variable truncation order is > the maximum truncation order!")
  mo <= sum(vos) || (@info "Maximum order too high: setting maximum order = sum($vos) = $(sum(vos))"; mo = sum(vos))
  nv = length(vos)
  np = 0
  po = 0
  no = vos
  d = Descriptor(mad_desc_newvpo(convert(Cint, nv), convert(Cuchar, mo), convert(Cint, np), convert(Cuchar, po), convert(Vector{Cuchar}, no)))
  GTPSA.desc_current = d
  return d
end

"""
    Descriptor(nv::Integer, mo::Integer, np::Integer, po::Integer)::Descriptor

Creates a TPSA `Descriptor` with `nv` variables and `np` parameters. The maximum 
truncation order is `mo` (including the parameters), and the truncation order for 
the parameters part of a monomial is `po`.

### Input
- `nv` -- Number of variables in the TPSA
- `mo` -- Maximum truncation order of the TPSA including variables and parameters
- `np` -- Number of parameters in the TPSA
- `po` -- Truncation order of the parameters part of a monomial
"""
function Descriptor(nv::Integer, mo::Integer, np::Integer, po::Integer)::Descriptor
  d = Descriptor(mad_desc_newvp(convert(Cint, nv), convert(Cuchar, mo), convert(Cint, np), convert(Cuchar, po)))
  GTPSA.desc_current = d
  return d
end


"""
    Descriptor(vos::Vector{<:Integer}, mo::Integer, pos::Vector{<:Integer}, po::Integer)::Descriptor

Creates a TPSA `Descriptor` with `length(vos)` variables with individual truncation 
orders specified in `vos`, and `length(pos)` parameters with individual truncation 
orders specified in `pos`. The maximum truncation order including both variables and 
parameters is `mo`, and the truncation order for just the parameters part of the is `po`.

### Input
- `vos` -- `Vector` of the individual truncation orders of each variable
- `mo`  -- Maximum truncation order of the TPSA including variables and parameters
- `pos` -- `Vector` of the individual truncation orders of each parameter
- `po` -- Truncation order of the parameters part of a monomial
"""
function Descriptor(vos::Vector{<:Integer}, mo::Integer, pos::Vector{<:Integer}, po::Integer)::Descriptor
  nv = length(vos)
  np = length(pos)
  no = vcat(vos,pos)
  d = Descriptor(mad_desc_newvpo(convert(Cint, nv), convert(Cuchar, mo), convert(Cint, np), convert(Cuchar, po), convert(Vector{Cuchar}, no)))
  GTPSA.desc_current = d
  return d
end
