module Monomial


"""
    mad_mono_str(n::Cint, a::Ptr{Cuchar}, s::Cstring)::Cint

???

### Input
- `n` -- Monomial and string length
- `a` -- Monomial
- `s` -- Monomial as string "[0-9]*"

### Output
- `i`    -- Adjusted size n if "\0" found
"""
function mad_mono_str(n::Cint, mono_a::Ptr{Cuchar}, s::Cstring)::Cint
  i = @ccall MAD_TPSA.mad_mono_str(n::Cint, mono_a::Ptr{Cuchar}, s::Cstring)::Cint
  return i
end


"""
  mad_mono_prt(n::Cint, a::Ptr{Cuchar}, s::Cstring)::Cstring

???

### Input
- `n` -- Monomial and string length
- `a` -- Monomial
- `s` -- Monomial as string

### Output
- `s` -- Monomial as string
"""
function mad_mono_prt(n::Cint, a::Ptr{Cuchar}, s::Cstring)::Cstring
  s = @ccall MAD_TPSA.mad_mono_prt(n::Cint, a::Ptr{Cuchar}, s::Cstring)::Cstring
  return s
end 


"""
    mad_mono_fill!(n::Cint, a::Ptr{Cuchar}, v::Cuchar)

Fills the monomial a with the value v.

### Input
- `n` -- Monomial length
- `a` -- Monomial
- `v` -- Value
"""
function mad_mono_fill!(n::Cint, a::Ptr{Cuchar}, v::Cuchar)
  @ccall MAD_TPSA.mad_mono_fill(n::Cint, a::Ptr{Cuchar}, v::Cuchar)::Cvoid
end


"""
    mad_mono_copy!(n::Cint, a::Ptr{Cuchar}, r::Ptr{Cuchar})

Copies monomial a to monomial r.  

### Input
- `n` -- Length of monomials
- `a` -- Source monomial
- `r` -- Destination monomial
"""
function mad_mono_copy!(n::Cint, a::Ptr{Cuchar}, r::Ptr{Cuchar})
  @ccall MAD_TPSA.mad_mono_copy(n::Cint, a::Ptr{Cuchar}, r::Ptr{Cuchar})::Cvoid
end


"""
  mad_mono_min(n::Cint, a::Ptr{Cuchar})::Cuchar

??? min of monomial?

### Input
- `n`  -- Length of monomial
- `a`  -- Monomial

### Output
- `mo` -- Min???
"""
function mad_mono_min(n::Cint, a::Ptr{Cuchar})::Cuchar
  mo = @ccall MAD_TPSA.mad_mono_min(n::Cint, a::Ptr{Cuchar})::Cuchar
  return mo
end


"""
  mad_mono_max(n::Cint, a::Ptr{Cuchar})::Cuchar

??? max of monomial?

### Input
- `n`  -- Length of monomial
- `a`  -- Monomial

### Output
- `mo` -- Max???
"""
function mad_mono_max(n::Cint, a::Ptr{Cuchar})::Cuchar
  mo = @ccall MAD_TPSA.mad_mono_max(n::Cint, a::Ptr{Cuchar})::Cuchar
  return mo
end


"""
    mad_mono_ord(n::Cint, a::Ptr{Cuchar})::Cint

Returns the sum of the orders of the monomial a. 

### Input
- `n` -- Monomial length
- `a` -- Monomial

### Output
- `s` -- Sum of orders of monomial
"""
function mad_mono_ord(n::Cint, a::Ptr{Cuchar})::Cint
  s = @ccall MAD_TPSA.mad_mono_ord(n::Cint, a::Ptr{Cuchar})::Cint
  return s
end


"""
    mad_mono_ordp(n::Cint, a::Ptr{Cuchar}, stp::Cint)::Cdouble

Returns the product of the orders of the monomial a.

### Input
- `n`   -- Monomial length
- `a`   -- Monomial
- `stp` -- ????

### Output
- `p`   -- Product of orders of monomial
"""
function mad_mono_ordp(n::Cint, a::Ptr{Cuchar}, stp::Cint)::Cdouble
  p = @ccall MAD_TPSA.mad_mono_ordp(n::Cint, a::Ptr{Cuchar})::Cdouble
  return p
end


"""
    mad_mono_ordpf(n::Cint, a::Ptr{Cuchar}, stp::Cint)::Cdouble

Returns the product of factorials of orders of the monomial a.

### Input
- `n`   -- Monomial length
- `a`   -- Monomial
- `stp` -- ????

### Output
- `p`   -- Product of factorials of orders of monomial
"""
function mad_mono_ordpf(n::Cint, a::Ptr{Cuchar}, stp::Cint)::Cdouble
  p = @ccall MAD_TPSA.mad_mono_ordpf(n::Cint, a::Ptr{Cuchar})::Cdouble
  return p
end


"""
    mad_mono_eq(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cuchar

Checks if the monomial a is equal to the monomial b.

### Input
- `n`   -- Length of monomials
- `a`   -- Monomial a
- `b`   -- Monomial b

### Output
- `ret` -- True if the monomials are equal, false if otherwise
"""
function mad_mono_eq(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_mono_eq(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cuchar
  return ret
end


"""
    mad_mono_lt(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cuchar

Checks if monomial a is less than monomial b.

### Input
- `n`  -- Length of monomials
- `a`  -- Monomial a
- `b`  -- Monomial b

### Output
- `ret` -- True if a < b, false otherwise
"""
function mad_mono_lt(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_mono_lt(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cuchar
  return ret
end


"""
    mad_mono_le(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cuchar

Checks if monomial a is less than or equal to monomial b.

### Input
- `n`   -- Length of monomials
- `a`   -- Monomial a
- `b`   -- Monomial b

### Output
- `ret` -- True if a <= mono_b, false otherwise
"""
function mad_mono_le(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_mono_le(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cuchar
  return ret
end


"""
    mad_mono_cmp(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cint

???

### Input
- `n`   -- Length of monomials
- `a`   -- Monomial a
- `b`   -- Monomial b

### Output
- `ret` -- First a[i]-b[i] != 0 
"""
function mad_mono_cmp(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cint
  ret = @ccall MAD_TPSA.mad_mono_cmp(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cint
  return ret
end


"""
    mad_mono_rcmp(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cint

Compare from end. ???

### Input
- `n`   -- Length of monomials
- `a`   -- Monomial a
- `b`   -- Monomial b

### Output
- `ret` -- First a[i]-b[i] != 0
"""
function mad_mono_rcmp(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cint
  ret = @ccall MAD_TPSA.mad_mono_rcmp(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cint
  return ret
end


"""
    mad_mono_add!(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar}, r::Ptr{Cuchar})

Sets monomial r = a + b.

### Input
- `n` -- Length of monomials
- `a` -- Source monomial a
- `b` -- Source monomial b
- `r` -- Destination monomial, r = a + b
"""
function mad_mono_add!(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar}, r::Ptr{Cuchar})
  @ccall MAD_TPSA.mad_mono_add(n::Cint, a::Ptr{Cuchar},b::Ptr{Cuchar}, r::Ptr{Cuchar})::Cvoid
end


"""
    mad_mono_sub!(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar}, r::Ptr{Cuchar})

Sets monomial r = a - b.

### Input
- `n` -- Length of monomials
- `a` -- Source monomial a
- `b` -- Source monomial b
- `r` -- Destination monomial, r = a - b
"""
function mad_mono_sub!(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar}, r::Ptr{Cuchar})
  @ccall MAD_TPSA.mad_mono_sub(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar}, r::Ptr{Cuchar})::Cvoid
end


"""
    mad_mono_cat!(n::Cint, a::Ptr{Cuchar}, m::Cint, b::Ptr{Cuchar}, r::Ptr{Cuchar})

Sets monomial r equal to the concatenation of the monomials a and b

### Input
- `n` -- Length of monomonial a
- `a` -- Source monomial a
- `m` -- Length of monomial b
- `b` -- Source monomial b
- `r` -- Destination monomial of concatenation of a and b (length n+m)
"""
function mad_mono_cat!(n::Cint, a::Ptr{Cuchar}, m::Cint, b::Ptr{Cuchar}, r::Ptr{Cuchar})
  @ccall MAD_TPSA.mad_mono_cat(n::Cint, a::Ptr{Cuchar}, m::Cint, b::Ptr{Cuchar}, r::Ptr{Cuchar})::Cvoid
end

"""
  mad_mono_rev!(n::Cint, a::Ptr{Cuchar}, r::Ptr{Cuchar})

Sets destination monomial r equal to the reverse of source monomial a.

### Input
- `n` -- Lengths of Monomials
- `a` -- Source monomial a
- `r` -- Destination monomial of reverse monomial a
"""
function mad_mono_rev!(n::Cint, a::Ptr{Cuchar}, r::Ptr{Cuchar})
  @ccall MAD_TPSA.mad_mono_rev(n::Cint, a::Ptr{Cuchar}, r::Ptr{Cuchar})::Cvoid
end


"""
    mad_mono_print(n::Cint, a::Ptr{Cuchar}, fp::Ptr{Cvoid})

Prints the monomial to stdout.

### Input
- `n`  -- Length of monomial
- `a`  -- Source monomial to print to stdout
- `fp` -- C FILE pointer, if null will print to stdout 
"""
function mad_mono_print(n::Cint, a::Ptr{Cuchar}, fp::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_mono_print(n::Cint, a::Ptr{Cuchar}, fp::Ptr{Cvoid})::Cvoid
end

end