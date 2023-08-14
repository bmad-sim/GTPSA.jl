



"""
    mad_mono_rcopy!(n::Cint, mono_a::Ptr{Cuchar}, mono_r::Ptr{Cuchar})

???

### Input
- `n`      -- length of monomials
- `mono_a` -- source monomial
- `mono_r` -- destination monomial
"""
function mad_mono_rcopy!(n::Cint, mono_a::Ptr{Cuchar}, mono_r::Ptr{Cuchar})
  @ccall MAD_TPSA.mad_mono_rcopy(n::Cint, mono_a::Ptr{Cuchar}, mono_r::Ptr{Cuchar})::Cvoid
end









"""
    mad_mono_gt(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cuchar

Checks if monomial mono_a is greater than monomial mono_b.

### Input
- `n`      -- Length of monomials
- `mono_a` -- Monomial a
- `mono_b` -- Monomial b

### Output
- `ret`    -- True if mono_a > mono_b, false otherwise
"""
function mad_mono_gt(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_mono_gt(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cuchar
  return ret
end



"""
    mad_mono_ge(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cuchar

Checks if monomial mono_a is greater than or equal to monomial mono_b.

  ### Input
  - `n`      -- Length of monomials
  - `mono_a` -- Monomial a
  - `mono_b` -- Monomial b
  
  ### Output
  - `ret`    -- True if mono_a >= mono_b, false otherwise
  """
function mad_mono_ge(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_mono_ge(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cuchar
  return ret
end

"""
    mad_mono_sort!(n::Cint, mono_a::Ptr{Cuchar}, idxs::Ptr{Cint})

???

### Input
- `n`      -- Length of monomial
- `mono_a` -- Source monomial
- `idxs`   -- Index lookup: a[idxs[i]] is sorted by order
"""
function mad_mono_sort!(n::Cint, mono_a::Ptr{Cuchar}, idxs::Ptr{Cint})
  @ccall MAD_TPSA.mad_mono_sort(n::Cint, mono_a::Ptr{Cuchar}, idxs::Ptr{Cint})::Cvoid
end