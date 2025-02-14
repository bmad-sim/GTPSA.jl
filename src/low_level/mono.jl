"""
    mad_mono_str!(n::Cint, a, s::Cstring)::Cint

Writes the monomial defined in the string `s`, which stores the orders in a human-readable format 
(e.g. 10 is 10, not 0xa), into the byte array `a` with the orders specified in hexadecimal.

### Input
- `n` -- Monomial and string length
- `s` -- Monomial as string "[0-9]*"

### Output
- `a` -- Monomial as a byte array converted from the input string
- `i` -- Adjusted size `n` of byte array if '\0' found
"""
function mad_mono_str!(n::Cint, a, s::Cstring)::Cint
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  i = @ccall MAD_TPSA.mad_mono_str(n::Cint, a::Ptr{Cuchar}, s::Cstring)::Cint
  return i
end


"""
    mad_mono_prt(n::Cint, a, s::Ptr{Cuchar})::Cstring

Writes the monomial defined by the byte array `a` (with orders stored as hexadecimal) into 
a null terminated string `s`.

### Input
- `n` -- Monomial and string length
- `a` -- Monomial as byte array

### Output
- `ret` -- Monomial as string
"""
function mad_mono_prt!(n::Cint, a, s::Ptr{Cuchar})::Cstring
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  ret = @ccall MAD_TPSA.mad_mono_prt(n::Cint, a::Ptr{Cuchar}, s::Ptr{Cuchar})::Cstring
  return ret
end 


"""
    mad_mono_fill!(n::Cint, a, v::Cuchar)

Fills the monomial `a` with the value `v`.

### Input
- `n` -- Monomial length
- `a` -- Monomial
- `v` -- Value
"""
function mad_mono_fill!(n::Cint, a, v::Cuchar)
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  @ccall MAD_TPSA.mad_mono_fill(n::Cint, a::Ptr{Cuchar}, v::Cuchar)::Cvoid
end


"""
    mad_mono_copy!(n::Cint, a, r)

Copies monomial `a` to monomial `r`.  

### Input
- `n` -- Length of monomials
- `a` -- Source monomial
- `r` -- Destination monomial
"""
function mad_mono_copy!(n::Cint, a, r)
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  eltype(r) == Cuchar || error("r must have eltype Cuchar !")
  @ccall MAD_TPSA.mad_mono_copy(n::Cint, a::Ptr{Cuchar}, r::Ptr{Cuchar})::Cvoid
end


"""
    mad_mono_min(n::Cint, a)::Cuchar

Returns the minimum order of the monomial.

### Input
- `n`  -- Length of monomial
- `a`  -- Monomial

### Output
- `mo` -- Mininum order of monomial `a`
"""
function mad_mono_min(n::Cint, a)::Cuchar
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  mo = @ccall MAD_TPSA.mad_mono_min(n::Cint, a::Ptr{Cuchar})::Cuchar
  return mo
end


"""
    mad_mono_max(n::Cint, a)::Cuchar

Returns the maximum order of the monomial.

  ### Input
  - `n`  -- Length of monomial
  - `a`  -- Monomial
  
  ### Output
  - `mo` -- Maximum order of monomial `a`
"""
function mad_mono_max(n::Cint, a)::Cuchar
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  mo = @ccall MAD_TPSA.mad_mono_max(n::Cint, a::Ptr{Cuchar})::Cuchar
  return mo
end


"""
    mad_mono_ord(n::Cint, a)::Cint

Returns the sum of the orders of the monomial `a`.

### Input
- `n` -- Monomial length
- `a` -- Monomial

### Output
- `s` -- Sum of orders of monomial
"""
function mad_mono_ord(n::Cint, a)::Cint
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  s = @ccall MAD_TPSA.mad_mono_ord(n::Cint, a::Ptr{Cuchar})::Cint
  return s
end


"""
    mad_mono_ordp(n::Cint, a, stp::Cint)::Cdouble

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
function mad_mono_ordp(n::Cint, a, stp::Cint)::Cdouble
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  p = @ccall MAD_TPSA.mad_mono_ordp(n::Cint, a::Ptr{Cuchar}, stp::Cint)::Cdouble
  return p
end


"""
    mad_mono_ordpf(n::Cint, a, stp::Cint)::Cdouble

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
function mad_mono_ordpf(n::Cint, a, stp::Cint)::Cdouble
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  p = @ccall MAD_TPSA.mad_mono_ordpf(n::Cint, a::Ptr{Cuchar}, stp::Cint)::Cdouble
  return p
end


"""
    mad_mono_eqn(n::Cint, a, b::Cuchar)::Bool

???
"""
function mad_mono_eqn(n::Cint, a, b::Cuchar)::Bool
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  ret = @ccall MAD_TPSA.mad_mono_eqn(n::Cint, a::Ptr{Cuchar}, b::Cuchar)::Bool
  return ret
end


"""
    mad_mono_eq(n::Cint, a, b)::Bool

Checks if the monomial `a` is equal to the monomial `b`.

### Input
- `n`   -- Length of monomials
- `a`   -- Monomial `a`
- `b`   -- Monomial `b`

### Output
- `ret` -- True if the monomials are equal, false if otherwise
"""
function mad_mono_eq(n::Cint, a, b)::Bool
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  eltype(b) == Cuchar || error("b must have eltype Cuchar !")
  ret = @ccall MAD_TPSA.mad_mono_eq(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Bool
  return ret
end


"""
    mad_mono_lt(n::Cint, a, b)::Bool

Checks if monomial `a` is less than monomial `b`.

### Input
- `n`  -- Length of monomials
- `a`  -- Monomial `a`
- `b`  -- Monomial `b`

### Output
- `ret` -- True if `a < b`, false otherwise
"""
function mad_mono_lt(n::Cint, a, b)::Bool
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  eltype(b) == Cuchar || error("b must have eltype Cuchar !")
  ret = @ccall MAD_TPSA.mad_mono_lt(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Bool
  return ret
end


"""
    mad_mono_le(n::Cint, a, b)::Bool

Checks if monomial `a` is less than or equal to monomial `b`.

### Input
- `n`   -- Length of monomials
- `a`   -- Monomial `a`
- `b`   -- Monomial `b`

### Output
- `ret` -- True if `a <= mono_b`, false otherwise
"""
function mad_mono_le(n::Cint, a, b)::Bool
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  eltype(b) == Cuchar || error("b must have eltype Cuchar !")
  ret = @ccall MAD_TPSA.mad_mono_le(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Bool
  return ret
end


"""
    mad_mono_ok(n::Cint, a, b)::Bool

Returns `true` if the sum of all orders in `a` (order of `a`) is less than or equal to the single maximum 
order in `b`.

### Input
- `n`   -- Length of monomials
- `a`   -- Monomial `a`
- `b`   -- Monomial `b`

### Output
- `ret` -- `true` if `sum(a) <= max(b)`
"""
function mad_mono_ok(n::Cint, a, b)::Bool
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  eltype(b) == Cuchar || error("b must have eltype Cuchar !")
  ret = @ccall MAD_TPSA.mad_mono_ok(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Bool
  return ret
end


"""
    mad_mono_ok_(n::Cint, a, b)::Bool

Returns `true` if the sum of only nonzero orders in `a` (order of `a`) is less than or equal to the 
single maximum order in `b` *considering only those nonzero variables as a*

### Input
- `n`   -- Length of monomials
- `a`   -- Monomial `a`
- `b`   -- Monomial `b`

### Output
- `ret` -- `true` if `sum(a[a .!= 0]) <= max(b[a .!= 0])`
"""
function mad_mono_ok_(n::Cint, a, b)::Bool
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  eltype(b) == Cuchar || error("b must have eltype Cuchar !")
  ret = @ccall MAD_TPSA.mad_mono_ok_(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Bool
  return ret
end



"""
    mad_mono_cmp(n::Cint, a, b)::Cint

Compares monomial `a` to monomial `b`, and returns the first difference in the lowest order variables.

### Input
- `n`   -- Length of monomials
- `a`   -- Monomial `a`
- `b`   -- Monomial `b`

### Output
- `ret` -- First `a[i]-b[i] != 0`
"""
function mad_mono_cmp(n::Cint, a, b)::Cint
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  eltype(b) == Cuchar || error("b must have eltype Cuchar !")
  ret = @ccall MAD_TPSA.mad_mono_cmp(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cint
  return ret
end


"""
    mad_mono_rcmp(n::Cint, a, b)::Cint

Compares monomial `a` to monomial `b` starting from the right (when the monomials are ordered by variable, 
which is almost never the case) and returns the first difference in the lowest order variables. 

### Input
- `n`   -- Length of monomials
- `a`   -- Monomial `a`
- `b`   -- Monomial `b`

### Output
- `ret` -- First `a[i]-b[i] != 0` where `i` starts from the end.
"""
function mad_mono_rcmp(n::Cint, a, b)::Cint
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  eltype(b) == Cuchar || error("b must have eltype Cuchar !")
  ret = @ccall MAD_TPSA.mad_mono_rcmp(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar})::Cint
  return ret
end


"""
    mad_mono_add!(n::Cint, a, b, r)

Sets monomial `r = a + b`.

### Input
- `n` -- Length of monomials
- `a` -- Source monomial `a`
- `b` -- Source monomial `b`

### Output
- `r` -- Destination monomial, `r = a + b`
"""
function mad_mono_add!(n::Cint, a, b, r)
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  eltype(b) == Cuchar || error("b must have eltype Cuchar !")
  eltype(r) == Cuchar || error("r must have eltype Cuchar !")
  @ccall MAD_TPSA.mad_mono_add(n::Cint, a::Ptr{Cuchar},b::Ptr{Cuchar}, r::Ptr{Cuchar})::Cvoid
end


"""
    mad_mono_sub!(n::Cint, a, b, r)

Sets monomial `r = a - b`.

### Input
- `n` -- Length of monomials
- `a` -- Source monomial `a`
- `b` -- Source monomial `b`

### Output
- `r` -- Destination monomial, `r = a - b`
"""
function mad_mono_sub!(n::Cint, a, b, r)
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  eltype(b) == Cuchar || error("b must have eltype Cuchar !")
  eltype(r) == Cuchar || error("r must have eltype Cuchar !")
  @ccall MAD_TPSA.mad_mono_sub(n::Cint, a::Ptr{Cuchar}, b::Ptr{Cuchar}, r::Ptr{Cuchar})::Cvoid
end


"""
    mad_mono_cat!(n::Cint, a, m::Cint, b, r)

Sets monomial `r` equal to the concatenation of the monomials `a` and `b`

### Input
- `n` -- Length of monomonial `a`
- `a` -- Source monomial `a`
- `m` -- Length of monomial `b`
- `b` -- Source monomial `b`

### Output
- `r` -- Destination monomial of concatenation of `a` and `b` (length `n+m`)
"""
function mad_mono_cat!(n::Cint, a, m::Cint, b, r)
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  eltype(b) == Cuchar || error("b must have eltype Cuchar !")
  eltype(r) == Cuchar || error("r must have eltype Cuchar !")
  @ccall MAD_TPSA.mad_mono_cat(n::Cint, a::Ptr{Cuchar}, m::Cint, b::Ptr{Cuchar}, r::Ptr{Cuchar})::Cvoid
end

"""
    mad_mono_rev!(n::Cint, a, r)

Sets destination monomial `r` equal to the reverse of source monomial `a`.

### Input
- `n` -- Lengths of monomials
- `a` -- Source monomial `a`

### Output
- `r` -- Destination monomial of reverse monomial `a`
"""
function mad_mono_rev!(n::Cint, a, r)
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  eltype(r) == Cuchar || error("r must have eltype Cuchar !")
  @ccall MAD_TPSA.mad_mono_rev(n::Cint, a::Ptr{Cuchar}, r::Ptr{Cuchar})::Cvoid
end


"""
    mad_mono_print(n::Cint, a, sep_::Cstring, fp_::Ptr{Cvoid})

Prints the monomial to `stdout`.

### Input
- `n`  -- Length of monomial
- `a`  -- Source monomial to print to `stdout`
- `sep_` -- Separator string
- `fp_` -- C `FILE` pointer, if null will print to `stdout`
"""
function mad_mono_print(n::Cint, a, sep_::Cstring, fp_::Ptr{Cvoid})
  eltype(a) == Cuchar || error("a must have eltype Cuchar !")
  @ccall MAD_TPSA.mad_mono_print(n::Cint, a::Ptr{Cuchar}, sep_::Cstring, fp_::Ptr{Cvoid})::Cvoid
end