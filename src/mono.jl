"""
    mad_mono_str!(n::Cint, a::Vector{Cuchar}, s::Cstring)::Cint

Writes the monomial defined in the string `s`, which stores the orders in a human-readable format 
(e.g. 10 is 10, not 0xa), into the byte array `a` with the orders specified in hexadecimal.

### Input
- `n` -- Monomial and string length
- `s` -- Monomial as string "[0-9]*"

### Output
- `a` -- Monomial as a byte array converted from the input string
- `i` -- Adjusted size `n` of byte array if '\0' found
"""
function mad_mono_str!(n::Cint, a::Vector{Cuchar}, s::Cstring)::Cint
  i = @ccall MAD_TPSA.mad_mono_str(n::Cint, a::Ptr{Cuchar}, s::Cstring)::Cint
  return i
end


"""
    mad_mono_prt(n::Cint, a::Vector{Cuchar}, s::Ptr{Cuchar})::Cstring

Writes the monomial defined by the byte array `a` (with orders stored as hexadecimal) into 
a null terminated string `s`.

### Input
- `n` -- Monomial and string length
- `a` -- Monomial as byte array

### Output
- `ret` -- Monomial as string
"""
function mad_mono_prt!(n::Cint, a::Vector{Cuchar}, s::Ptr{Cuchar})::Cstring
  ret = @ccall MAD_TPSA.mad_mono_prt(n::Cint, a::Ptr{Cuchar}, s::Ptr{Cuchar})::Cstring
  return ret
end 


"""
    mad_mono_fill!(n::Cint, a::Vector{Cuchar}, v::Cuchar)

Fills the monomial `a` with the value `v`.

### Input
- `n` -- Monomial length
- `a` -- Monomial
- `v` -- Value
"""
function mad_mono_fill!(n::Cint, a::Vector{Cuchar}, v::Cuchar)
  @ccall MAD_TPSA.mad_mono_fill(n::Cint, a::Ptr{Cuchar}, v::Cuchar)::Cvoid
end


"""
    mad_mono_copy!(n::Cint, a::Vector{Cuchar}, r::Vector{Cuchar})

Copies monomial `a` to monomial `r`.  

### Input
- `n` -- Length of monomials
- `a` -- Source monomial
- `r` -- Destination monomial
"""
function mad_mono_copy!(n::Cint, a::Vector{Cuchar}, r::Vector{Cuchar})
  @ccall MAD_TPSA.mad_mono_copy(n::Cint, a::Ptr{Cuchar}, r::Ptr{Cuchar})::Cvoid
end


"""
    mad_mono_min(n::Cint, a::Vector{Cuchar})::Cuchar

Returns the minimum order of the monomial.

### Input
- `n`  -- Length of monomial
- `a`  -- Monomial

### Output
- `mo` -- Mininum order of monomial `a`
"""
function mad_mono_min(n::Cint, a::Vector{Cuchar})::Cuchar
  mo = @ccall MAD_TPSA.mad_mono_min(n::Cint, a::Ptr{Cuchar})::Cuchar
  return mo
end


"""
    mad_mono_max(n::Cint, a::Vector{Cuchar})::Cuchar

Returns the maximum order of the monomial.

  ### Input
  - `n`  -- Length of monomial
  - `a`  -- Monomial
  
  ### Output
  - `mo` -- Maximum order of monomial `a`
"""
function mad_mono_max(n::Cint, a::Vector{Cuchar})::Cuchar
  mo = @ccall MAD_TPSA.mad_mono_max(n::Cint, a::Ptr{Cuchar})::Cuchar
  return mo
end


"""
    mad_mono_ord(n::Cint, a::Vector{Cuchar})::Cint

Returns the sum of the orders of the monomial `a`.

### Input
- `n` -- Monomial length
- `a` -- Monomial

### Output
- `s` -- Sum of orders of monomial
"""
function mad_mono_ord(n::Cint, a::Vector{Cuchar})::Cint
  s = @ccall MAD_TPSA.mad_mono_ord(n::Cint, a::Ptr{Cuchar})::Cint
  return s
end


"""
    mad_mono_ordp(n::Cint, a::Vector{Cuchar}, stp::Cint)::Cdouble

Returns the product of each `stp`-th order in monomial `a`. For example, `stp = 2` collects every order
in the monomial with a step of 2 between each. As `a` is a pointer, the product can be started at any 
element in the monomial.

### Input
- `n`   -- Monomial length
- `a`   -- Monomial as byte array
- `stp` -- Step over which orders to include in the product

### Output
- `p`   -- Product of orders of monomial separated by `stp`.
"""
function mad_mono_ordp(n::Cint, a::Vector{Cuchar}, stp::Cint)::Cdouble
  p = @ccall MAD_TPSA.mad_mono_ordp(n::Cint, a::Ptr{Cuchar}, stp::Cint)::Cdouble
  return p
end


"""
    mad_mono_ordpf(n::Cint, a::Vector{Cuchar}, stp::Cint)::Cdouble

Returns the product of factorials each `stp`-th order in monomial a. For example, `stp = 2` collects 
every order in the monomial with a step of 2 between each. As `a` is a pointer, the product can be started 
at any element in the monomial.

### Input
- `n`   -- Monomial length
- `a`   -- Monomial as byte array
- `stp` -- Step over which orders to include in the product of factorials

### Output
- `p`   -- Product of factorials of orders of monomial separated by `stp`
"""
function mad_mono_ordpf(n::Cint, a::Vector{Cuchar}, stp::Cint)::Cdouble
  p = @ccall MAD_TPSA.mad_mono_ordpf(n::Cint, a::Ptr{Cuchar}, stp::Cint)::Cdouble
  return p
end


"""
    mad_mono_eq(n::Cint, a::Vector{Cuchar}, b::Vector{Cuchar})::Cuchar

Checks if the monomial `a` is equal to the monomial `b`.

### Input
- `n`   -- Length of monomials
- `a`   -- Monomial `a`
- `b`   -- Monomial `b`

### Output
- `ret` -- True if the monomials are equal, false if otherwise
"""
function mad_mono_eq(n::Cint, a::Vector{Cuchar}, b::Vector{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_mono_eq(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cuchar
  return ret
end


"""
    mad_mono_lt(n::Cint, a::Vector{Cuchar}, b::Vector{Cuchar})::Cuchar

Checks if monomial `a` is less than monomial `b`.

### Input
- `n`  -- Length of monomials
- `a`  -- Monomial `a`
- `b`  -- Monomial `b`

### Output
- `ret` -- True if `a < b`, false otherwise
"""
function mad_mono_lt(n::Cint, a::Vector{Cuchar}, b::Vector{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_mono_lt(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cuchar
  return ret
end


"""
    mad_mono_le(n::Cint, a::Vector{Cuchar}, b::Vector{Cuchar})::Cuchar

Checks if monomial `a` is less than or equal to monomial `b`.

### Input
- `n`   -- Length of monomials
- `a`   -- Monomial `a`
- `b`   -- Monomial `b`

### Output
- `ret` -- True if `a <= mono_b`, false otherwise
"""
function mad_mono_le(n::Cint, a::Vector{Cuchar}, b::Vector{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_mono_le(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cuchar
  return ret
end


"""
    mad_mono_cmp(n::Cint, a::Vector{Cuchar}, b::Vector{Cuchar})::Cint

Compares monomial `a` to monomial `b`, and returns the first difference in the lowest order variables.

### Input
- `n`   -- Length of monomials
- `a`   -- Monomial `a`
- `b`   -- Monomial `b`

### Output
- `ret` -- First `a[i]-b[i] != 0`
"""
function mad_mono_cmp(n::Cint, a::Vector{Cuchar}, b::Vector{Cuchar})::Cint
  ret = @ccall MAD_TPSA.mad_mono_cmp(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cint
  return ret
end


"""
    mad_mono_rcmp(n::Cint, a::Vector{Cuchar}, b::Vector{Cuchar})::Cint

Compares monomial `a` to monomial `b` starting from the right (when the monomials are ordered by variable, 
which is almost never the case) and returns the first difference in the lowest order variables. 

### Input
- `n`   -- Length of monomials
- `a`   -- Monomial `a`
- `b`   -- Monomial `b`

### Output
- `ret` -- First `a[i]-b[i] != 0` where `i` starts from the end.
"""
function mad_mono_rcmp(n::Cint, a::Vector{Cuchar}, b::Vector{Cuchar})::Cint
  ret = @ccall MAD_TPSA.mad_mono_rcmp(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cint
  return ret
end


"""
    mad_mono_add!(n::Cint, a::Vector{Cuchar}, b::Vector{Cuchar}, r::Vector{Cuchar})

Sets monomial `r = a + b`.

### Input
- `n` -- Length of monomials
- `a` -- Source monomial `a`
- `b` -- Source monomial `b`

### Output
- `r` -- Destination monomial, `r = a + b`
"""
function mad_mono_add!(n::Cint, a::Vector{Cuchar}, b::Vector{Cuchar}, r::Vector{Cuchar})
  @ccall MAD_TPSA.mad_mono_add(n::Cint, a::Ptr{Cuchar},b::Ptr{Cuchar}, r::Ptr{Cuchar})::Cvoid
end


"""
    mad_mono_sub!(n::Cint, a::Vector{Cuchar}, b::Vector{Cuchar}, r::Vector{Cuchar})

Sets monomial `r = a - b`.

### Input
- `n` -- Length of monomials
- `a` -- Source monomial `a`
- `b` -- Source monomial `b`

### Output
- `r` -- Destination monomial, `r = a - b`
"""
function mad_mono_sub!(n::Cint, a::Vector{Cuchar}, b::Vector{Cuchar}, r::Vector{Cuchar})
  @ccall MAD_TPSA.mad_mono_sub(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar}, r::Ptr{Cuchar})::Cvoid
end


"""
    mad_mono_cat!(n::Cint, a::Vector{Cuchar}, m::Cint, b::Vector{Cuchar}, r::Vector{Cuchar})

Sets monomial `r` equal to the concatenation of the monomials `a` and `b`

### Input
- `n` -- Length of monomonial `a`
- `a` -- Source monomial `a`
- `m` -- Length of monomial `b`
- `b` -- Source monomial `b`

### Output
- `r` -- Destination monomial of concatenation of `a` and `b` (length `n+m`)
"""
function mad_mono_cat!(n::Cint, a::Vector{Cuchar}, m::Cint, b::Vector{Cuchar}, r::Vector{Cuchar})
  @ccall MAD_TPSA.mad_mono_cat(n::Cint, a::Ptr{Cuchar}, m::Cint, b::Ptr{Cuchar}, r::Ptr{Cuchar})::Cvoid
end

"""
    mad_mono_rev!(n::Cint, a::Vector{Cuchar}, r::Vector{Cuchar})

Sets destination monomial `r` equal to the reverse of source monomial `a`.

### Input
- `n` -- Lengths of monomials
- `a` -- Source monomial `a`

### Output
- `r` -- Destination monomial of reverse monomial `a`
"""
function mad_mono_rev!(n::Cint, a::Vector{Cuchar}, r::Vector{Cuchar})
  @ccall MAD_TPSA.mad_mono_rev(n::Cint, a::Ptr{Cuchar}, r::Ptr{Cuchar})::Cvoid
end


"""
    mad_mono_print(n::Cint, a::Vector{Cuchar}, fp_::Ptr{Cvoid})

Prints the monomial to `stdout`.

### Input
- `n`  -- Length of monomial
- `a`  -- Source monomial to print to `stdout`
- `fp_` -- C `FILE` pointer, if null will print to `stdout`
"""
function mad_mono_print(n::Cint, a::Vector{Cuchar}, fp_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_mono_print(n::Cint, a::Ptr{Cuchar}, fp_::Ptr{Cvoid})::Cvoid
end