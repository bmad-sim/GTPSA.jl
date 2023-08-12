module TPSA
include("Descriptor.jl")
include("RealTPSA.jl")
include("ComplexTPSA.jl")
using .Descriptor
using .RealTPSA
using .ComplexTPSA
using Printf
#import Base: sin
export Desc, RTPSA, CTPSA, new_desc,new_TPSA,set_TPSA!,print_TPSA,sin!,del!,asin!,set_name!,cleanup,desc_maxlen,MAD_TPSA_DEFAULT, MAD_TPSA_SAME

const MAD_TPSA = :("libmad_tpsa")
const MAD_TPSA_DEFAULT::Cuchar = 255
const MAD_TPSA_SAME::Cuchar = 254

# ------------------------------------------------------------------------------------------
# LOW LEVEL FUNCTIONS:
"""
Variable type conversions:
"ord_t": "Cuchar",
"c_int": "Cint",
"c_num_t": "Cdouble",
"c_cpx_t": "ComplexF64",
"c_ssz_t": "Cint",
"c_idx_t": "Cint",
"c_ptr desc": "Ptr{Desc{RTPSA,CTPSA}}",
"c_ptr tpsa": "Ptr{RTPSA{Desc}}",
"c_ptr ctpsa": "Ptr{CTPSA{Desc}}"
"""

"""
    mad_mono_str(n::Cint, mono_a::Ptr{Cuchar}, s::Cstring)::Cint

???

### Input
- `n`       -- Monomial and string length
- `mono_a`  -- Monomial
- `s`       -- Monomial as string "[0-9]*"

### Output
- `size`    -- Adjusted size n if "\0" found
"""
function mad_mono_str(n::Cint, mono_a::Ptr{Cuchar}, s::Cstring)::Cint
  size = @ccall MAD_TPSA.mad_mono_str(n::Cint, mono_a::Ptr{Cuchar}, s::Cstring)::Cint
  return size
end


"""
    mad_mono_fill!(n::Cint, mono_a::Ptr{Cuchar}, v::Cuchar)

Fills the monomial mono_a with the value v.

### Input
- `n`      -- Monomial length
- `mono_a` -- Monomial
- `v`      -- Value
"""
function mad_mono_fill!(n::Cint, mono_a::Ptr{Cuchar}, v::Cuchar)
  @ccall MAD_TPSA.mad_mono_fill(n::Cint, mono_a::Ptr{Cuchar}, v::Cuchar)::Cvoid
end


"""
    mad_mono_copy!(n::Cint, mono_a::Ptr{Cuchar}, mono_r::Ptr{Cuchar})

Copies monomial mono_a to monomial mono_b.  

### Input
- `n`      -- length of monomials
- `mono_a` -- source monomial
- `mono_r` -- destination monomial
"""
function mad_mono_copy!(n::Cint, mono_a::Ptr{Cuchar}, mono_r::Ptr{Cuchar})
  @ccall MAD_TPSA.mad_mono_copy(n::Cint, mono_a::Ptr{Cuchar}, mono_r::Ptr{Cuchar})::Cvoid
end


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
    mad_mono_min(n::Cint, mono_a::Ptr{Cuchar})::Cuchar

???

### Input
- `n`      -- Length of monomial
- `mono_a` -- Monomial

### Output
- `min`    -- Minimum order of monomial
"""
function mad_mono_min(n::Cint, mono_a::Ptr{Cuchar})::Cuchar
  min = @ccall MAD_TPSA.mad_mono_min(n::Cint, mono_a::Ptr{Cuchar})::Cuchar
  return min
end


"""
    mad_mono_max(n::Cint, mono_a::Ptr{Cuchar})::Cuchar

???

### Input
- `n`      -- Monomial length
- `mono_a` -- Monomial

### Output
- `max`    -- Maximum order of monomial
"""
function mad_mono_max(n::Cint, mono_a::Ptr{Cuchar})::Cuchar
  max = @ccall MAD_TPSA.mad_mono_max(n::Cint, mono_a::Ptr{Cuchar})::Cuchar
  return max
end


"""
    mad_mono_ord(n::Cint, mono_a::Ptr{Cuchar})::Cint

???

### Input
- `n`      -- Monomial length
- `mono_a` -- Monomial

### Output
- `ord`    -- Order of monomial (sum)
"""
function mad_mono_ord(n::Cint, mono_a::Ptr{Cuchar})::Cint
  ord = @ccall MAD_TPSA.mad_mono_ord(n::Cint, mono_a::Ptr{Cuchar})::Cint
  return ord
end


"""
    mad_mono_eq(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cuchar

Checks if the monomial mono_a is equal to the monomial mono_b.

### Input
- `n`      -- Length of monomials
- `mono_a` -- Monomial a
- `mono_b` -- Monomial b

### Output
- `ret`    -- True if the monomials are equal, false if otherwise
"""
function mad_mono_eq(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_mono_eq(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cuchar
  return ret
end


"""
    mad_mono_lt(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cuchar

Checks if monomial mono_a is less than monomial mono_b.

### Input
- `n`      -- Length of monomials
- `mono_a` -- Monomial a
- `mono_b` -- Monomial b

### Output
- `ret`    -- True if mono_a < mono_b, false otherwise
"""
function mad_mono_lt(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_mono_lt(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cuchar
  return ret
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

Che
"""
    mad_mono_le(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cuchar

Checks if monomial mono_a is less than or equal to monomial mono_b.

### Input
- `n`      -- Length of monomials
- `mono_a` -- Monomial a
- `mono_b` -- Monomial b

### Output
- `ret`    -- True if mono_a <= mono_b, false otherwise
"""
function mad_mono_le(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_mono_le(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cuchar
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
    mad_mono_cmp(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cint

???

### Input
- `n`      -- Length of monomials
- `mono_a` -- Monomial a
- `mono_b` -- Monomial b

### Output
- `ret`    -- First a[i]-b[i] != 0 
"""
function mad_mono_cmp(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cint
  ret = @ccall MAD_TPSA.mad_mono_cmp(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cint
  return ret
end


"""
    mad_mono_rcmp(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cint

Compare from end. ???

### Input
- `n`      -- Length of monomials
- `mono_a` -- Monomial a
- `mono_b` -- Monomial b

### Output
- `ret`    -- First a[i]-b[i] != 0
"""
function mad_mono_rcmp(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cint
  ret = @ccall MAD_TPSA.mad_mono_rcmp(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar})::Cint
  return ret
end


"""
    mad_mono_add!(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar}, mono_r::Ptr{Cuchar})

Sets monomial mono_c = mono_a + mono_b.

### Input
- `n`      -- Length of monomials
- `mono_a` -- Source monomial a
- `mono_b` -- Source monomial b
- `mono_r` -- Destination monomial, mono_c = mono_a + mono_b
"""
function mad_mono_add!(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar}, mono_r::Ptr{Cuchar})
  @ccall MAD_TPSA.mad_mono_add(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar}, mono_r::Ptr{Cuchar})::Cvoid
end


"""
    mad_mono_sub!(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar}, mono_r::Ptr{Cuchar})

Sets monomial mono_c = mono_a - mono_b.

### Input
- `n`      -- Length of monomials
- `mono_a` -- Source monomial a
- `mono_b` -- Source monomial b
- `mono_r` -- Destination monomial, mono_c = mono_a - mono_b
"""
function mad_mono_sub!(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar}, mono_r::Ptr{Cuchar})
  @ccall MAD_TPSA.mad_mono_sub(n::Cint, mono_a::Ptr{Cuchar}, mono_b::Ptr{Cuchar}, mono_r::Ptr{Cuchar})::Cvoid
end


"""
    mad_mono_cat!(na::Cint, mono_a::Ptr{Cuchar}, nb::Cint, mono_b::Ptr{Cuchar}, mono_r::Ptr{Cuchar})

Sets mono_c equal to the concatenation of the monomials mono_a and mono_b

### Input
- `na`     -- Length of mono_a
- `mono_a` -- Source monomial a
- `nb`     -- Length of mono_b
- `mono_b` -- Source monomial b
- `mono_r` -- Destination monomial of concatenation of mono_a and mono_b (length na+nb)
"""
function mad_mono_cat!(na::Cint, mono_a::Ptr{Cuchar}, nb::Cint, mono_b::Ptr{Cuchar}, mono_r::Ptr{Cuchar})
  @ccall MAD_TPSA.mad_mono_cat(na::Cint, mono_a::Ptr{Cuchar}, nb::Cint, mono_b::Ptr{Cuchar}, mono_r::Ptr{Cuchar})::Cvoid
end


"""
Original Fortran subroutine:
n,mono_a,idxs
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: n   ! monomial length
  integer(c_ord_t), intent(in) :: mono_a(*)  ! src
  integer(c_idx_t) :: idxs(*)  ! index lookup: a[idxs[i]] is sorted by order
"""
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


"""
    mad_mono_print(n::Cint, mono_a::Ptr{Cuchar})

Prints the monomial to stdout.

### Input
- `n`      -- Length of monomial
- `mono_a` -- Source monomial to print to stdout
"""
function mad_mono_print(n::Cint, mono_a::Ptr{Cuchar})
  @ccall MAD_TPSA.mad_mono_print(n::Cint, mono_a::Ptr{Cuchar})::Cvoid
end


"""
Original Fortran function:
nv,mo_
bind(C)
! mo = max(1, mo)
  import ; implicit none
  type(c_ptr) :: desc                          ! descriptor
  integer(c_int), value, intent(in) :: nv      ! #vars
  integer(c_ord_t), value, intent(in) :: mo_   ! order of tpsa, mo=max(1,mo_)
"""
"""
    mad_desc_newv(nv::Cint, mo_::Cuchar)::Ptr{Desc{RTPSA,CTPSA}}

Creates a TPSA descriptor with the specified number of variables and maximum order. 
The number of parameters is set to 0. 

### Input
- `nv`   -- Number of variables in th
- `mo_`  -- Maximum order of TPSA, mo_ = max(1, mo_)

### Output
- `desc` -- Descriptor with the specified number of variables and maximum order
"""
function mad_desc_newv(nv::Cint, mo_::Cuchar)::Ptr{Desc{RTPSA,CTPSA}}
  desc = @ccall MAD_TPSA.mad_desc_newv(nv::Cint, mo_::Cuchar)::Ptr{Desc{RTPSA,CTPSA}}
  return desc
end


"""
    mad_desc_newvp(nv::Cint, mo_::Cuchar, np_::Cint, po_::Cuchar)::Ptr{Desc{RTPSA,CTPSA}}

Creates a TPSA descriptor with the specifed number of variables, maximum order, number of 
parameters, and parameter order.

### Input
- `nv`   -- Number of variables
- `mo_`  -- Maximum order of TPSA, mo_ = max(1, mo_)
- `np_`  -- Number of parameters
- `po_`  -- Order of parameters, po = max(1, po_)

### Output
- `desc` -- Descriptor with the specified nv, mo, np, and po.
"""
function mad_desc_newvp(nv::Cint, mo_::Cuchar, np_::Cint, po_::Cuchar)::Ptr{Desc{RTPSA,CTPSA}}
  desc = @ccall MAD_TPSA.mad_desc_newvp(nv::Cint, mo_::Cuchar, np_::Cint, po_::Cuchar)::Ptr{Desc{RTPSA,CTPSA}}
  return desc
end


"""
    mad_desc_newvpo(nv::Cint, mo_::Cuchar, np_::Cint, po_::Cuchar, no_::Ptr{Cuchar})::Ptr{Desc{RTPSA,CTPSA}}

Creates a TPSA descriptor with the specifed number of variables, maximum order, number of parameters, 
parameter order, and individual variable/parameter orders specified in no. The first nv entries in no 
correspond to the variables' orders and the next np entries correspond the parameters' orders.

### Input
- `nv`   -- Number of variables
- `mo_`  -- Maximum order of TPSA (mo = max(no_))
- `np_`  -- Number of parameters
- `po_`  -- Order of parameters
- `no_`  -- Array of orders of variables and parameters

### Output
- `desc` -- Descriptor with the specified nv, mo, np, po, no.
"""
function mad_desc_newvpo(nv::Cint, mo_::Cuchar, np_::Cint, po_::Cuchar, no_::Ptr{Cuchar})::Ptr{Desc{RTPSA,CTPSA}}
  desc = @ccall MAD_TPSA.mad_desc_newvpo(nv::Cint, mo_::Cuchar, np_::Cint, po_::Cuchar, no_::Ptr{Cuchar})::Ptr{Desc{RTPSA,CTPSA}}
  return desc
end


"""
    mad_desc_del!(desc::Ptr{Desc{RTPSA,CTPSA}})

Calls the destructor for the descriptor.

### Input
- `desc` -- Descriptor to destruct
"""
function mad_desc_del!(desc::Ptr{Desc{RTPSA,CTPSA}})
  @ccall MAD_TPSA.mad_desc_del(desc::Ptr{Desc{RTPSA,CTPSA}})::Cvoid
end


"""
    mad_desc_getnv(desc::Ptr{Desc{RTPSA,CTPSA}}, mo_::Cuchar, np_::Cint, po_::Cuchar)::Cint

???

### Input
- `desc` -- Descriptor
- `mo_`  -- Maximum order
- `np_`  -- Number of parameters
- `po_`  -- Parameter order

### Output
- `nv`   -- Number of variables in TPSA
"""
function mad_desc_getnv(desc::Ptr{Desc{RTPSA,CTPSA}}, mo_::Cuchar, np_::Cint, po_::Cuchar)::Cint
  nv = @ccall MAD_TPSA.mad_desc_getnv(desc::Ptr{Desc{RTPSA,CTPSA}}, mo_::Cuchar, np_::Cint, po_::Cuchar)::Cint
  return nv
end


"""
    mad_desc_maxord(desc::Ptr{Desc{RTPSA,CTPSA}}, nn::Cint, no_::Ptr{Cuchar})::Cuchar

Sets the order of the variables and parameters of the TPSA to those specified in no_ and 
returns the maximum order of the TPSA.

### Input
- `desc` -- Descriptor
- `nn`   -- Number of variables + number of parameters, no_[1..nn]
- `no_`  -- Orders of parameters to be filled if provided

### Output
- `mo`   -- Maximum order of TPSA
"""
function mad_desc_maxord(desc::Ptr{Desc{RTPSA,CTPSA}}, nn::Cint, no_::Ptr{Cuchar})::Cuchar
  mo = @ccall MAD_TPSA.mad_desc_maxord(desc::Ptr{Desc{RTPSA,CTPSA}}, nn::Cint, no_::Ptr{Cuchar})::Cuchar
  return mo
end


"""
    mad_desc_maxlen(desc::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Cint

???

### Input
- `desc` -- Descriptor
- `mo`   -- ordlen(maxord) == maxlen

### Output
- `olen` -- monomials in 0..order
"""
function mad_desc_maxlen(desc::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Cint
  olen = @ccall MAD_TPSA.mad_desc_maxlen(desc::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Cint
  return olen
end


"""
    mad_desc_gtrunc!(desc::Ptr{Desc{RTPSA,CTPSA}}, to::Cuchar)::Cuchar

Sets the global truncation order (to) of the TPSA, and returns the old global truncation order.

### Input
- `desc`  -- Descriptor
- `to`    -- New global truncation order

### Output
- `oldto` -- Old global truncation order
"""
function mad_desc_gtrunc!(desc::Ptr{Desc{RTPSA,CTPSA}}, to::Cuchar)::Cuchar
  oldto = @ccall MAD_TPSA.mad_desc_gtrunc(desc::Ptr{Desc{RTPSA,CTPSA}}, to::Cuchar)::Cuchar
  return oldto
end


"""
    mad_desc_isvalids(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, s::Cstring)::Cuchar

???

### Input
- `desc` -- Descriptor
- `n`    -- String length of 0 (unknown)
- `s`    -- Monomial as string "[0-9]*"

### Output
- `ret`  -- True or false
"""
function mad_desc_isvalids(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, s::Cstring)::Cuchar
  ret = @ccall MAD_TPSA.mad_desc_isvalids(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, s::Cstring)::Cuchar
  return ret
end


"""
    mad_desc_isvalidm(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cuchar

???

### Input
- `desc` -- Descriptor
- `n`    -- Length of monomial
- `m`    -- Monomial

### Output
- `ret`  -- True or false
"""
function mad_desc_isvalidm(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_desc_isvalidm(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cuchar
  return ret
end


"""
Original Fortran function:
desc,n,m
bind(C)
import ; implicit none
  logical(c_bool) :: ret                   ! true or false
  type(c_ptr), value, intent(in) :: desc   !
  integer(c_ssz_t), value, intent(in) :: n ! monomial length
  integer(c_int), intent(in) :: m(*)       ! sparse monomial (idx,ord)
"""
"""
    mad_desc_isvalidsm(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cuchar

???

### Input
- `desc` -- Descriptor
- `n`    -- Length of monomial
- `m`    -- Sparse monomial (idx, ord)

### Output
- `ret`  -- True or false.
"""
function mad_desc_isvalidsm(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cuchar
  ret = @ccall MAD_TPSA.mad_desc_isvalidsm(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cuchar
  return ret
end


"""
    mad_desc_idxs(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, s::Cstring)::Cint

???

### Input
- `desc` -- Descriptor
- `n`    -- String length or 0 (unknown)
- `s`    -- Monomial as string "[0-9]*"

### Output
- `idx`  -- Monomial index or -1
"""
function mad_desc_idxs(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, s::Cstring)::Cint
  idx = @ccall MAD_TPSA.mad_desc_idxs(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, s::Cstring)::Cint
  return idx
end


"""
    mad_desc_idxm(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint

???

### Input
- `desc` -- Descriptor
- `n`    -- Monomial length
- `m`    -- Monomial

### Output
- `idx`  -- Monomial index or -1
"""
function mad_desc_idxm(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint
  idx = @ccall MAD_TPSA.mad_desc_idxm(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint
  return idx
end


"""
    mad_desc_idxsm(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint

???

### Input
- `desc` -- Descriptor
- `n`    -- Monomial length
- `m`    -- Sparse monomial (idx,ord)

### Output
- `idx`  -- Monomial index or -1
"""
function mad_desc_idxsm(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint
  idx = @ccall MAD_TPSA.mad_desc_idxsm(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint
  return idx
end


"""
    mad_desc_nxtbyvar(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint

???

### Input
- `desc` -- Descriptor
- `n`    -- Monomial length
- `m`    -- Monomial

### Output
- `idx`  -- Monomial index or -1
"""
function mad_desc_nxtbyvar(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint
  idx = @ccall MAD_TPSA.mad_desc_nxtbyvar(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint
  return idx
end


"""
    mad_desc_nxtbyord(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint

???

### Input
- `desc` -- Descriptor
- `n`    -- Monomial length
- `m`    -- Monomial

### Output
- `idx`  -- Monomial index or -1
"""
function mad_desc_nxtbyord(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint
  idx = @ccall MAD_TPSA.mad_desc_nxtbyord(desc::Ptr{Desc{RTPSA,CTPSA}}, n::Cint, m::Ptr{Cuchar})::Cint
  return idx
end


"""
    mad_desc_mono(desc::Ptr{Desc{RTPSA,CTPSA}}, i::Cint, n::Cint, m_::Ptr{Cuchar})::Cuchar

???

### Input
- `desc` -- Descriptor
- `i`    -- Slot index (must be valid)
- `n`    -- Monomial length
- `m_`   -- Monomial to fill (if provided)

### Output
- `ord`   -- Monomial order
"""
function mad_desc_mono(desc::Ptr{Desc{RTPSA,CTPSA}}, i::Cint, n::Cint, m_::Ptr{Cuchar})::Cuchar
  ord = @ccall MAD_TPSA.mad_desc_mono(desc::Ptr{Desc{RTPSA,CTPSA}}, i::Cint, n::Cint, m_::Ptr{Cuchar})::Cuchar
  return ord
end


"""
    mad_tpsa_newd(desc::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{RTPSA{Desc}}

Creates a real TPSA defined by the specified descriptor and maximum order.
If MAD_TPSA_DEFAULT is passed for mo, the mo defined in the descriptor is used.

### Input
- `desc`    -- Descriptor
- `mo`      -- Maximum order

### Output
- `newtpsa` -- New real TPSA defined by the descriptor
"""
function mad_tpsa_newd(desc::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
  newtpsa = @ccall MAD_TPSA.mad_tpsa_newd(desc::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
  return newtpsa
end


"""
    mad_tpsa_new(tpsa::Ptr{RTPSA{Desc}}, mo::Cuchar)::Ptr{RTPSA{Desc}}

Creates a real TPSA copy of the inputted TPSA, with maximum order specified by mo.
If MAD_TPSA_SAME is passed for mo, the mo currently in tpsa is used for the created TPSA.

### Input
- `tpsa`    -- Real TPSA to copy
- `mo`      -- Maximum order of new TPSA

### Output
- `newtpsa` -- New real TPSA with maximum order mo
"""
function mad_tpsa_new(tpsa::Ptr{RTPSA{Desc}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
  newtpsa = @ccall MAD_TPSA.mad_tpsa_new(tpsa::Ptr{RTPSA{Desc}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
  return newtpsa
end


"""
    mad_tpsa_del!(tpsa::Ptr{RTPSA{Desc}})

Calls the destructor for the real TPSA.

### Input
- `tpsa` -- Real TPSA to destruct
"""
function mad_tpsa_del!(tpsa::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_del(tpsa::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_desc(tpsa::Ptr{RTPSA{Desc}})::Ptr{Desc{RTPSA,CTPSA}}

Gets the descriptor for the real TPSA.

### Input
- `tpsa`  -- Real TPSA

### Output
- `desc`  -- Descriptor for the tpsa
"""
function mad_tpsa_desc(tpsa::Ptr{RTPSA{Desc}})::Ptr{Desc{RTPSA,CTPSA}}
  desc = @ccall MAD_TPSA.mad_tpsa_desc(tpsa::Ptr{RTPSA{Desc}})::Ptr{Desc{RTPSA,CTPSA}}
  return desc
end


"""
    mad_tpsa_uid!(tpsa::Ptr{RTPSA{Desc}}, uid_::Cint)::Cint

Sets the TPSA uid if uid_ != 0, and returns the current (previous if set) TPSA uid. 

### Input
- `tpsa` -- Real TPSA
- `uid_` -- uid to set in the TPSA if uid_ != 0

### Output
- `uid`  -- Current (previous if set) TPSA uid
"""
function mad_tpsa_uid!(tpsa::Ptr{RTPSA{Desc}}, uid_::Cint)::Cint
  uid = @ccall MAD_TPSA.mad_tpsa_uid(tpsa::Ptr{RTPSA{Desc}}, uid_::Cint)::Cint
  return uid
end


"""
    mad_tpsa_len(tpsa::Ptr{RTPSA{Desc}})::Cint

???

### Input
- `tpsa`  -- Real TPSA

### Output
- `len`   -- Monomials in tpsa
"""
function mad_tpsa_len(tpsa::Ptr{RTPSA{Desc}})::Cint
  len = @ccall MAD_TPSA.mad_tpsa_len(tpsa::Ptr{RTPSA{Desc}})::Cint
  return len
end


"""
    mad_tpsa_nam(tpsa::Ptr{RTPSA{Desc}})::Cstring

Get the name of the TPSA.

### Input
- `tpsa` -- Real TPSA

### Output
- `nam`  -- Name of tpsa (nul term in C)
"""
function mad_tpsa_nam(tpsa::Ptr{RTPSA{Desc}})::Cstring
  nam = @ccall MAD_TPSA.mad_tpsa_nam(tpsa::Ptr{RTPSA{Desc}})::Cstring
  return nam
end


"""
    mad_tpsa_ord(tpsa::Ptr{RTPSA{Desc}})::Cuchar

Gets the TPSA order.

### Input
- `tpsa` -- Real TPSA

### Output
- `ord`  -- Order of TPSA
"""
function mad_tpsa_ord(tpsa::Ptr{RTPSA{Desc}})::Cuchar
  ord = @ccall MAD_TPSA.mad_tpsa_ord(tpsa::Ptr{RTPSA{Desc}})::Cuchar
  return ord
end


"""
Original Fortran function:
n,tpsa
bind(C)
import ; implicit none
  integer(c_ord_t) :: ord                   ! max of all tpsas order
  integer(c_ssz_t), value, intent(in) :: n  ! #tpsa
  type(c_ptr), intent(in) :: tpsa(*)
"""
"""
    mad_tpsa_ordn(n::Cint, tpsa::Ptr{RTPSA{Desc}})::Cuchar

Gets the max of all TPSAs order. ???

### Input
- `n`    -- #tpsa
- `tpsa` -- TPSA

### Output
- `ord` -- Order
"""
function mad_tpsa_ordn(n::Cint, tpsa::Ptr{RTPSA{Desc}})::Cuchar
  ord = @ccall MAD_TPSA.mad_tpsa_ordn(n::Cint, tpsa::Ptr{RTPSA{Desc}})::Cuchar
  return ord
end


"""
    mad_tpsa_isvalid(tpsa::Ptr{RTPSA{Desc}})::Cuchar

Sanity check of the TPSA integrity.

### Input
- `tpsa` -- Real TPSA to check if valid

### Output
- `ret`  -- True if valid TPSA, false otherwise
"""
function mad_tpsa_isvalid(tpsa::Ptr{RTPSA{Desc}})::Cuchar
  ret = @ccall MAD_TPSA.mad_tpsa_isvalid(tpsa::Ptr{RTPSA{Desc}})::Cuchar
  return ret
end


"""
    mad_tpsa_copy!(tpsa::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

Makes a copy of the real TPSA tpsa to tpsa_r.

### Input
- `tpsa`   -- Source real TPSA
- `tpsa_r` -- Destination real TPSA
"""
function mad_tpsa_copy!(tpsa::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_copy(tpsa::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
    mad_tpsa_sclord!(tpsa::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, inv::Cuchar)

???

### Input
- `tpsa`   -- Source real TPSA
- `tpsa_r` -- Destination real TPSA
- `inv`    -- scl by inverse
"""
function mad_tpsa_sclord!(tpsa::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, inv::Cuchar)
  @ccall MAD_TPSA.mad_tpsa_sclord(tpsa::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, inv::Cuchar)::Cvoid
end


"""
    mad_tpsa_getord!(tpsa::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, ord::Cint)

???
is ! ?

### Input
- `tpsa`   -- Source real TPSA
- `tpsa_r` -- Destination real TPSA
- `ord`    -- Order to retrieve
"""
function mad_tpsa_getord!(tpsa::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, ord::Cuchar)
  @ccall MAD_TPSA.mad_tpsa_getord(tpsa::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, ord::Cuchar)::Cvoid
end


"""
    mad_tpsa_cutord!(tpsa::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, ord::Cint)

???

### Input
- `tpsa`   -- Source real TPSA
- `tpsa_r` -- Destination real TPSA
- `ord`    -- Cut order: 0..-ord or ord..mo
"""
function mad_tpsa_cutord!(tpsa::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, ord::Cint)
  @ccall MAD_TPSA.mad_tpsa_cutord(tpsa::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, ord::Cint)::Cvoid
end


"""
    mad_tpsa_setvar!(tpsa::Ptr{RTPSA{Desc}}, v::Cdouble, iv_::Cint, scl_::Cdouble)

???

### Input
- `tpsa` -- Real TPSA
- `v`    -- 0th order value
- `iv_`  -- Variable index
- `scl_` -- 1st order variable value
"""
function mad_tpsa_setvar!(tpsa::Ptr{RTPSA{Desc}}, v::Cdouble, iv_::Cint, scl_::Cdouble)
  @ccall MAD_TPSA.mad_tpsa_setvar(tpsa::Ptr{RTPSA{Desc}}, v::Cdouble, iv_::Cint, scl_::Cdouble)::Cvoid
end


"""
    mad_tpsa_setnam!(tpsa::Ptr{RTPSA{Desc}}, nam::Cstring)

Sets the name of the tpsa.

### Input
- `tpsa` -- Real TPSA
- `nam`  -- Name to set for tpsa
"""
function mad_tpsa_setnam!(tpsa::Ptr{RTPSA{Desc}}, nam::Cstring)
  @ccall MAD_TPSA.mad_tpsa_setnam(tpsa::Ptr{RTPSA{Desc}}, nam::Cstring)::Cvoid
end


"""
    mad_tpsa_clear!(tpsa::Ptr{RTPSA{Desc}})

Clears the TPSA (reset to 0)

### Input
- `tpsa` -- Real TPSA
"""
function mad_tpsa_clear!(tpsa::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_clear(tpsa::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran function:
tpsa
bind(C)
import ; implicit none
  logical(c_bool) :: ret                    ! true or false
  type(c_ptr), value, intent(in) :: tpsa
"""
"""
    mad_tpsa_isnul(tpsa::Ptr{RTPSA{Desc}})::Cuchar

### Input
- `tpsa`

### Output
- `ret`
"""
function mad_tpsa_isnul(tpsa::Ptr{RTPSA{Desc}})::Cuchar
  ret = @ccall MAD_TPSA.mad_tpsa_isnul(tpsa::Ptr{RTPSA{Desc}})::Cuchar
  return ret
end


"""
Original Fortran subroutine:
tpsa,tpsa_r,n,t2r_,pb
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa    ! src
  type(c_ptr), value :: tpsa_r              ! dst
  integer(c_ssz_t), value, intent(in) :: n  ! vector length
  integer(c_idx_t), intent(in) :: t2r_(*)   ! vector of index lookup
  integer(c_int), value, intent(in) :: pb   ! poisson bracket 0,1:fwd,-1:bwd
"""
"""
    mad_tpsa_convert!(tpsa::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)

### Input
- `tpsa`
- `tpsa_r`
- `n`
- `t2r_`
- `pb`
"""
function mad_tpsa_convert!(tpsa::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)
  @ccall MAD_TPSA.mad_tpsa_convert(tpsa::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)::Cvoid
end


"""
Original Fortran function:
tpsa,n,m_,i
bind(C)
import ; implicit none
  integer(c_ord_t) :: ord                  ! monomial order
  type(c_ptr), value, intent(in) :: tpsa   !
  integer(c_idx_t), value, intent(in) :: i ! slot index (must be valid)
  integer(c_ssz_t), value, intent(in) :: n ! monomial length
  integer(c_ord_t) :: m_(*)                ! monomial to fill (if provided)
"""
"""
    mad_tpsa_mono(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m_::Ptr{Cuchar}, i::Cint)::Cint

### Input
- `tpsa`
- `n`
- `m_`
- `i`

### Output
- `ord`
"""
function mad_tpsa_mono(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m_::Ptr{Cuchar}, i::Cint)::Cint
  ord = @ccall MAD_TPSA.mad_tpsa_mono(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m_::Ptr{Cuchar}, i::Cint)::Cint
  return ord
end


"""
Original Fortran function:
tpsa,n,s
bind(C)
import ; implicit none
  integer(c_idx_t) :: idx                  ! monomial index or -1
  type(c_ptr), value, intent(in) :: tpsa   !
  integer(c_ssz_t), value, intent(in) :: n ! string length or 0 (unknown)
  character(c_char), intent(in) :: s(*)    ! monomial as C string "[0-9]*"
"""
"""
    mad_tpsa_idxs(tpsa::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring)::Cint

### Input
- `tpsa`
- `n`
- `s`

### Output
- `idx`
"""
function mad_tpsa_idxs(tpsa::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring)::Cint
  idx = @ccall MAD_TPSA.mad_tpsa_idxs(tpsa::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring)::Cint
  return idx
end


"""
Original Fortran function:
tpsa,n,m
bind(C)
import ; implicit none
  integer(c_idx_t) :: idx                  ! monomial index or -1
  type(c_ptr), value, intent(in) :: tpsa   !
  integer(c_ssz_t), value, intent(in) :: n ! monomial length
  integer(c_ord_t), intent(in) :: m(*)     ! monomial
"""
"""
    mad_tpsa_idxm(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint

### Input
- `tpsa`
- `n`
- `m`

### Output
- `idx`
"""
function mad_tpsa_idxm(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint
  idx = @ccall MAD_TPSA.mad_tpsa_idxm(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint
  return idx
end


"""
Original Fortran function:
tpsa,n,m
bind(C)
import ; implicit none
  integer(c_idx_t) :: idx                  ! monomial index or -1
  type(c_ptr), value, intent(in) :: tpsa   !
  integer(c_ssz_t), value, intent(in) :: n ! monomial length
  integer(c_int), intent(in) :: m(*)       ! sparse monomial (idx,ord)
"""
"""
    mad_tpsa_idxsm(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint

### Input
- `tpsa`
- `n`
- `m`

### Output
- `idx`
"""
function mad_tpsa_idxsm(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint
  idx = @ccall MAD_TPSA.mad_tpsa_idxsm(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint
  return idx
end


"""
Original Fortran function:
tpsa,i,n,m_,v_
bind(C)
import ; implicit none                   ! scan for non-zero coefs starting at i
  integer(c_idx_t) :: idx                  ! next index to start searching or -1
  type(c_ptr), value, intent(in) :: tpsa   !
  integer(c_idx_t), value, intent(in) :: i ! index to start searching
  integer(c_ssz_t), value, intent(in) :: n ! monomial length
  integer(c_ord_t) :: m_(*)                ! monomial to fill (if provided)
  real(c_num_t), intent(out) :: v_         ! coeff to fill (if provided)
"""
"""
    mad_tpsa_cycle(tpsa::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, v_::Cdouble)::Cint

### Input
- `tpsa`
- `i`
- `n`
- `m_`
- `v_`

### Output
- `idx`
"""
function mad_tpsa_cycle(tpsa::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, v_::Cdouble)::Cint
  idx = @ccall MAD_TPSA.mad_tpsa_cycle(tpsa::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, v_::Cdouble)::Cint
  return idx
end


"""
Original Fortran function:
tpsa
bind(C)
import ; implicit none
  real(c_num_t) :: val                     ! value at order 0 (index 0)
  type(c_ptr), value, intent(in) :: tpsa   !
"""
"""
    mad_tpsa_get0(tpsa::Ptr{RTPSA{Desc}})::Cdouble

### Input
- `tpsa`

### Output
- `val`
"""
function mad_tpsa_get0(tpsa::Ptr{RTPSA{Desc}})::Cdouble
  val = @ccall MAD_TPSA.mad_tpsa_get0(tpsa::Ptr{RTPSA{Desc}})::Cdouble
  return val
end


"""
Original Fortran function:
tpsa,i
bind(C)
import ; implicit none
  real(c_num_t) :: val                     ! value at index i
  type(c_ptr), value, intent(in) :: tpsa   !
  integer(c_idx_t), value, intent(in) :: i ! slot index (must be valid)
"""
"""
    mad_tpsa_geti(tpsa::Ptr{RTPSA{Desc}}, i::Cint)::Cdouble

### Input
- `tpsa`
- `i`

### Output
- `val`
"""
function mad_tpsa_geti(tpsa::Ptr{RTPSA{Desc}}, i::Cint)::Cdouble
  val = @ccall MAD_TPSA.mad_tpsa_geti(tpsa::Ptr{RTPSA{Desc}}, i::Cint)::Cdouble
  return val
end


"""
Original Fortran function:
tpsa,n,s
bind(C)
import ; implicit none
  real(c_num_t) :: val                     ! value at string monomial
  type(c_ptr), value, intent(in) :: tpsa   !
  integer(c_ssz_t), value, intent(in) :: n ! string length or 0 (unknown)
  character(c_char), intent(in) :: s(*)    ! monomial as string "[0-9]*"
"""
"""
    mad_tpsa_gets(tpsa::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring)::Cdouble

### Input
- `tpsa`
- `n`
- `s`

### Output
- `val`
"""
function mad_tpsa_gets(tpsa::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring)::Cdouble
  val = @ccall MAD_TPSA.mad_tpsa_gets(tpsa::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring)::Cdouble
  return val
end


"""
Original Fortran function:
tpsa,n,m
bind(C)
import ; implicit none
  real(c_num_t) :: val                     ! value at monomial
  type(c_ptr), value, intent(in) :: tpsa   !
  integer(c_ssz_t), value, intent(in) :: n ! monomial length
  integer(c_ord_t), intent(in) :: m(*)     ! monomial
"""
"""
    mad_tpsa_getm(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cdouble

### Input
- `tpsa`
- `n`
- `m`

### Output
- `val`
"""
function mad_tpsa_getm(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cdouble
  val = @ccall MAD_TPSA.mad_tpsa_getm(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cdouble
  return val
end


"""
Original Fortran function:
tpsa,n,m
bind(C)
import ; implicit none
  real(c_num_t) :: val                      ! value at sparse monomial
  type(c_ptr), value, intent(in) :: tpsa
  integer(c_ssz_t), value, intent(in) :: n  ! monomial length
  integer(c_int), intent(in) :: m(*)        ! sparse monomial (idx,ord)
"""
"""
    mad_tpsa_getsm(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cdouble

### Input
- `tpsa`
- `n`
- `m`

### Output
- `val`
"""
function mad_tpsa_getsm(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cdouble
  val = @ccall MAD_TPSA.mad_tpsa_getsm(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cdouble
  return val
end


"""
Original Fortran subroutine:
tpsa,i,n,v
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa
  integer(c_idx_t), value, intent(in) :: i  ! slot index (must be valid)
  integer(c_ssz_t), value, intent(in) :: n  ! vector length
  real(c_num_t) :: v(*)                     ! vector to fill
"""
"""
    mad_tpsa_getv!(tpsa::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, v::Cuchar)

### Input
- `tpsa`
- `i`
- `n`
- `v`
"""
function mad_tpsa_getv!(tpsa::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, v::Cuchar)
  @ccall MAD_TPSA.mad_tpsa_getv(tpsa::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, v::Cuchar)::Cvoid
end


"""
Original Fortran subroutine:
tpsa,a,b
bind(C)
import ; implicit none
  type(c_ptr), value :: tpsa
  real(c_num_t), value, intent(in) :: a, b   ! t[0] = a*t[0]+b
"""
"""
    mad_tpsa_set0!(tpsa::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble)

### Input
- `tpsa`
- `a`
- `b`
"""
function mad_tpsa_set0!(tpsa::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble)
  @ccall MAD_TPSA.mad_tpsa_set0(tpsa::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble)::Cvoid
end


"""
Original Fortran subroutine:
tpsa,i,a,b
bind(C)
import ; implicit none
  type(c_ptr), value :: tpsa
  integer(c_idx_t), value, intent(in) :: i   ! slot index (must be valid)
  real(c_num_t), value, intent(in) :: a, b   ! t[i] = a*t[i]+b
"""
"""
    mad_tpsa_seti!(tpsa::Ptr{RTPSA{Desc}}, i::Cint, a::Cdouble, b::Cdouble)

### Input
- `tpsa`
- `i`
- `a`
- `b`
"""
function mad_tpsa_seti!(tpsa::Ptr{RTPSA{Desc}}, i::Cint, a::Cdouble, b::Cdouble)
  @ccall MAD_TPSA.mad_tpsa_seti(tpsa::Ptr{RTPSA{Desc}}, i::Cint, a::Cdouble, b::Cdouble)::Cvoid
end


"""
Original Fortran subroutine:
tpsa,n,s,a,b
bind(C)
import ; implicit none
  type(c_ptr), value :: tpsa
  integer(c_ssz_t), value, intent(in) :: n   ! string length or 0 (unknown)
  character(c_char), intent(in) :: s(*)      ! monomial as string "[0-9]*"
  real(c_num_t), value, intent(in) :: a, b   ! t[s] = a*t[s]+b
"""
"""
    mad_tpsa_sets!(tpsa::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring, a::Cdouble, b::Cdouble)

### Input
- `tpsa`
- `n`
- `s`
- `a`
- `b`
"""
function mad_tpsa_sets!(tpsa::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring, a::Cdouble, b::Cdouble)
  @ccall MAD_TPSA.mad_tpsa_sets(tpsa::Ptr{RTPSA{Desc}}, n::Cint, s::Cstring, a::Cdouble, b::Cdouble)::Cvoid
end


"""
Original Fortran subroutine:
tpsa,n,m,a,b
bind(C)
import ; implicit none
  type(c_ptr), value :: tpsa
  integer(c_ssz_t), value, intent(in) :: n   ! monomial length
  integer(c_ord_t), intent(in) :: m(*)       ! monomial
  real(c_num_t), value, intent(in) :: a, b   ! t[m] = a*t[m]+b
"""
"""
    mad_tpsa_setm!(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::Cdouble, b::Cdouble)

### Input
- `tpsa`
- `n`
- `m`
- `a`
- `b`
"""
function mad_tpsa_setm!(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::Cdouble, b::Cdouble)
  @ccall MAD_TPSA.mad_tpsa_setm(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::Cdouble, b::Cdouble)::Cvoid
end


"""
Original Fortran subroutine:
tpsa,n,m,a,b
bind(C)
import ; implicit none
  type(c_ptr), value :: tpsa
  integer(c_ssz_t), value, intent(in) :: n   ! monomial length
  integer(c_int), intent(in) :: m(*)         ! sparse monomial (idx,ord)
  real(c_num_t), value, intent(in) :: a, b   ! t[m] = a*t[m]+b
"""
"""
    mad_tpsa_setsm!(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::Cdouble, b::Cdouble)

### Input
- `tpsa`
- `n`
- `m`
- `a`
- `b`
"""
function mad_tpsa_setsm!(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::Cdouble, b::Cdouble)
  @ccall MAD_TPSA.mad_tpsa_setsm(tpsa::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::Cdouble, b::Cdouble)::Cvoid
end


"""
Original Fortran subroutine:
tpsa,i,n,v
bind(C)
import ; implicit none
  type(c_ptr), value :: tpsa
  integer(c_idx_t), value, intent(in) :: i   ! slot index (must be valid)
  integer(c_ssz_t), value, intent(in) :: n   ! vector length
  real(c_num_t), intent(in) :: v(*)          ! vector to copy
"""
"""
    mad_tpsa_setv!(tpsa::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, v::Cuchar)

### Input
- `tpsa`
- `i`
- `n`
- `v`
"""
function mad_tpsa_setv!(tpsa::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, v::Cuchar)
  @ccall MAD_TPSA.mad_tpsa_setv(tpsa::Ptr{RTPSA{Desc}}, i::Cint, n::Cint, v::Cuchar)::Cvoid
end


"""
Original Fortran function:
tpsa_a,tpsa_b,eps_
bind(C)
import ; implicit none
  logical(c_bool) :: ret                    ! true or false
  type(c_ptr), value, intent(in) :: tpsa_a, tpsa_b
  real(c_num_t), value, intent(in) :: eps_  ! tolerance during comparison
"""
"""
    mad_tpsa_equ(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, eps_::Cdouble)::Cuchar

### Input
- `tpsa_a`
- `tpsa_b`
- `eps_`

### Output
- `ret`
"""
function mad_tpsa_equ(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, eps_::Cdouble)::Cuchar
  ret = @ccall MAD_TPSA.mad_tpsa_equ(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, eps_::Cdouble)::Cuchar
  return ret
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_b,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a, tpsa_b  ! lhs, rhs
  type(c_ptr), value :: tpsa_r                      ! dst
"""
"""
    mad_tpsa_dif!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_b`
- `tpsa_r`
"""
function mad_tpsa_dif!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_dif(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_b,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a, tpsa_b  ! lhs, rhs
  type(c_ptr), value :: tpsa_r                      ! dst
"""
"""
    mad_tpsa_add!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_b`
- `tpsa_r`
"""
function mad_tpsa_add!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_add(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_b,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a, tpsa_b  ! lhs, rhs
  type(c_ptr), value :: tpsa_r                      ! dst
"""
"""
    mad_tpsa_sub!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_b`
- `tpsa_r`
"""
function mad_tpsa_sub!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sub(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_b,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a, tpsa_b  ! lhs, rhs
  type(c_ptr), value :: tpsa_r                      ! dst
"""
"""
    mad_tpsa_mul!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_b`
- `tpsa_r`
"""
function mad_tpsa_mul!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_mul(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_b,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a, tpsa_b  ! lhs, rhs
  type(c_ptr), value :: tpsa_r                      ! dst
"""
"""
    mad_tpsa_div!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_b`
- `tpsa_r`
"""
function mad_tpsa_div!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_div(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_b,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a, tpsa_b  ! lhs, rhs
  type(c_ptr), value :: tpsa_r                      ! dst
"""
"""
    mad_tpsa_pow!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_b`
- `tpsa_r`
"""
function mad_tpsa_pow!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_pow(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,n,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a          ! src
  integer(c_int), value, intent(in) :: n            ! power (integer)
  type(c_ptr), value :: tpsa_r                      ! dst
"""
"""
    mad_tpsa_powi!(tpsa_a::Ptr{RTPSA{Desc}}, n::Cint, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `n`
- `tpsa_r`
"""
function mad_tpsa_powi!(tpsa_a::Ptr{RTPSA{Desc}}, n::Cint, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_powi(tpsa_a::Ptr{RTPSA{Desc}}, n::Cint, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,v,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a          ! src
  real(c_num_t), value, intent(in) :: v             ! power (real)
  type(c_ptr), value :: tpsa_r                      ! dst
"""
"""
    mad_tpsa_pown!(tpsa_a::Ptr{RTPSA{Desc}}, v::Cuchar, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `v`
- `tpsa_r`
"""
function mad_tpsa_pown!(tpsa_a::Ptr{RTPSA{Desc}}, v::Cuchar, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_pown(tpsa_a::Ptr{RTPSA{Desc}}, v::Cuchar, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a   ! src
  type(c_ptr), value :: tpsa_r               ! dst=|src|
"""
"""
    mad_tpsa_abs!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_abs!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_abs(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran function:
tpsa_a,tpsa_b_
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a, tpsa_b_
  real(c_num_t) :: nrm                       ! sum_i |a[i]-b_[i]|
"""
"""
    mad_tpsa_nrm(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b_::Ptr{RTPSA{Desc}})::Cdouble

### Input
- `tpsa_a`
- `tpsa_b_`

### Output
- `nrm`
"""
function mad_tpsa_nrm(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b_::Ptr{RTPSA{Desc}})::Cdouble
  nrm = @ccall MAD_TPSA.mad_tpsa_nrm(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b_::Ptr{RTPSA{Desc}})::Cdouble
  return nrm
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r,iv
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a   ! src
  type(c_ptr), value :: tpsa_r               ! dst
  integer(c_int), value, intent(in) :: iv    ! variable number (1st order)
"""
"""
    mad_tpsa_integ!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, iv::Cint)

### Input
- `tpsa_a`
- `tpsa_r`
- `iv`
"""
function mad_tpsa_integ!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, iv::Cint)
  @ccall MAD_TPSA.mad_tpsa_integ(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, iv::Cint)::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r,iv
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a   ! src
  type(c_ptr), value :: tpsa_r               ! dst
  integer(c_int), value, intent(in) :: iv    ! variable number (1st order)
"""
"""
    mad_tpsa_deriv!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, iv::Cint)

### Input
- `tpsa_a`
- `tpsa_r`
- `iv`
"""
function mad_tpsa_deriv!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, iv::Cint)
  @ccall MAD_TPSA.mad_tpsa_deriv(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, iv::Cint)::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r,n,m
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a   ! src
  type(c_ptr), value :: tpsa_r               ! dst
  integer(c_ssz_t), value, intent(in) :: n   ! monomial length
  integer(c_ord_t), intent(in) :: m(*)       ! monomial
"""
"""
    mad_tpsa_derivm!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})

### Input
- `tpsa_a`
- `tpsa_r`
- `n`
- `m`
"""
function mad_tpsa_derivm!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})
  @ccall MAD_TPSA.mad_tpsa_derivm(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_b,tpsa_r,nv
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a, tpsa_b ! src
  type(c_ptr), value :: tpsa_r                     ! dst
  integer(c_int), value, intent(in) :: nv          ! #variables (desc%nv if 0)
"""
"""
    mad_tpsa_poisbra!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, nv::Cint)

### Input
- `tpsa_a`
- `tpsa_b`
- `tpsa_r`
- `nv`
"""
function mad_tpsa_poisbra!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, nv::Cint)
  @ccall MAD_TPSA.mad_tpsa_poisbra(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, nv::Cint)::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,n,coef,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
  integer(c_ssz_t), value, intent(in) :: n    ! vector length
  real(c_num_t), intent(in) :: coef(*)        ! vector of taylor coefs
"""
"""
    mad_tpsa_taylor!(tpsa_a::Ptr{RTPSA{Desc}}, n::Cint, coef::Cdouble, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `n`
- `coef`
- `tpsa_r`
"""
function mad_tpsa_taylor!(tpsa_a::Ptr{RTPSA{Desc}}, n::Cint, coef::Cdouble, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_taylor(tpsa_a::Ptr{RTPSA{Desc}}, n::Cint, coef::Cdouble, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,v,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! src and dst
  real(c_num_t), value, intent(in) :: v       ! r = r + v*a (r not reset!)
"""
"""
    mad_tpsa_acc!(tpsa_a::Ptr{RTPSA{Desc}}, v::Cuchar, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `v`
- `tpsa_r`
"""
function mad_tpsa_acc!(tpsa_a::Ptr{RTPSA{Desc}}, v::Cuchar, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_acc(tpsa_a::Ptr{RTPSA{Desc}}, v::Cuchar, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,v,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
  real(c_num_t), value, intent(in) :: v       ! r = v*a
"""
"""
    mad_tpsa_scl!(tpsa_a::Ptr{RTPSA{Desc}}, v::Cuchar, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `v`
- `tpsa_r`
"""
function mad_tpsa_scl!(tpsa_a::Ptr{RTPSA{Desc}}, v::Cuchar, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_scl(tpsa_a::Ptr{RTPSA{Desc}}, v::Cuchar, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,v,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
  real(c_num_t), value, intent(in) :: v       ! r = v/a
"""
"""
    mad_tpsa_inv!(tpsa_a::Ptr{RTPSA{Desc}}, v::Cuchar, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `v`
- `tpsa_r`
"""
function mad_tpsa_inv!(tpsa_a::Ptr{RTPSA{Desc}}, v::Cuchar, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_inv(tpsa_a::Ptr{RTPSA{Desc}}, v::Cuchar, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,v,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
  real(c_num_t), value, intent(in) :: v       ! r = v/sqrt(a)
"""
"""
    mad_tpsa_invsqrt!(tpsa_a::Ptr{RTPSA{Desc}}, v::Cuchar, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `v`
- `tpsa_r`
"""
function mad_tpsa_invsqrt!(tpsa_a::Ptr{RTPSA{Desc}}, v::Cuchar, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_invsqrt(tpsa_a::Ptr{RTPSA{Desc}}, v::Cuchar, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_sqrt!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_sqrt!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sqrt(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_exp!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_exp!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_exp(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_log!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_log!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_log(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_s,tpsa_c
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_s, tpsa_c        ! dst_sin, dst_cos
"""
"""
    mad_tpsa_sincos!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_s::Ptr{RTPSA{Desc}}, tpsa_c::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_s`
- `tpsa_c`
"""
function mad_tpsa_sincos!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_s::Ptr{RTPSA{Desc}}, tpsa_c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sincos(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_s::Ptr{RTPSA{Desc}}, tpsa_c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_sin!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_sin!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sin(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_cos!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_cos!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_cos(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_tan!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_tan!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_tan(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_cot!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_cot!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_cot(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_sinc!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_sinc!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sinc(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_s,tpsa_c
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_s, tpsa_c        ! dst_sin, dst_cos
"""
"""
    mad_tpsa_sincosh!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_s::Ptr{RTPSA{Desc}}, tpsa_c::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_s`
- `tpsa_c`
"""
function mad_tpsa_sincosh!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_s::Ptr{RTPSA{Desc}}, tpsa_c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sincosh(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_s::Ptr{RTPSA{Desc}}, tpsa_c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_sinh!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_sinh!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sinh(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_cosh!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_cosh!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_cosh(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_tanh!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_tanh!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_tanh(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_coth!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_coth!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_coth(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_sinhc!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_sinhc!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sinhc(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_asin!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_asin!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_asin(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_acos!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_acos!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_acos(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_atan!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_atan!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_atan(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_acot!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_acot!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_acot(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_asinh!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_asinh!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_asinh(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_acosh!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_acosh!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_acosh(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_atanh!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_atanh!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_atanh(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_acoth!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_acoth!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_acoth(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_erf!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_erf!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_erf(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a    ! src
  type(c_ptr), value :: tpsa_r                ! dst
"""
"""
    mad_tpsa_erfc!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_erfc!(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_erfc(tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,tpsa_x,b,tpsa_r
bind(C)
import ; implicit none
  real(c_num_t), value, intent(in) :: a, b    ! coefs
  type(c_ptr), value, intent(in) :: tpsa_x    ! src
  type(c_ptr), value :: tpsa_r                ! dst=a*x+b
"""
"""
    mad_tpsa_axpb!(a::Cdouble, tpsa_x::Ptr{RTPSA{Desc}}, b::Cdouble, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `a`
- `tpsa_x`
- `b`
- `tpsa_r`
"""
function mad_tpsa_axpb!(a::Cdouble, tpsa_x::Ptr{RTPSA{Desc}}, b::Cdouble, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_axpb(a::Cdouble, tpsa_x::Ptr{RTPSA{Desc}}, b::Cdouble, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,tpsa_x,b,tpsa_y,c,tpsa_r
bind(C)
import ; implicit none
  real(c_num_t), value, intent(in) :: a, b, c       ! coefs
  type(c_ptr), value, intent(in) :: tpsa_x, tpsa_y  ! src
  type(c_ptr), value :: tpsa_r                      ! dst=a*x+b*y+c
"""
"""
    mad_tpsa_axpbypc!(a::Cdouble, tpsa_x::Ptr{RTPSA{Desc}}, b::Cdouble, tpsa_y::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `a`
- `tpsa_x`
- `b`
- `tpsa_y`
- `c`
- `tpsa_r`
"""
function mad_tpsa_axpbypc!(a::Cdouble, tpsa_x::Ptr{RTPSA{Desc}}, b::Cdouble, tpsa_y::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_axpbypc(a::Cdouble, tpsa_x::Ptr{RTPSA{Desc}}, b::Cdouble, tpsa_y::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,tpsa_x,tpsa_y,b,tpsa_r
bind(C)
import ; implicit none
  real(c_num_t), value, intent(in) :: a, b          ! coefs
  type(c_ptr), value, intent(in) :: tpsa_x, tpsa_y  ! src
  type(c_ptr), value :: tpsa_r                      ! dst=a*x*y+b
"""
"""
    mad_tpsa_axypb!(a::Cdouble, tpsa_x::Ptr{RTPSA{Desc}}, tpsa_y::Ptr{RTPSA{Desc}}, b::Cdouble, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `a`
- `tpsa_x`
- `tpsa_y`
- `b`
- `tpsa_r`
"""
function mad_tpsa_axypb!(a::Cdouble, tpsa_x::Ptr{RTPSA{Desc}}, tpsa_y::Ptr{RTPSA{Desc}}, b::Cdouble, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_axypb(a::Cdouble, tpsa_x::Ptr{RTPSA{Desc}}, tpsa_y::Ptr{RTPSA{Desc}}, b::Cdouble, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,tpsa_x,tpsa_y,b,tpsa_z,c,tpsa_r
bind(C)
import ; implicit none
  real(c_num_t), value, intent(in) :: a, b, c              ! coefs
  type(c_ptr), value, intent(in) :: tpsa_x, tpsa_y, tpsa_z ! src
  type(c_ptr), value :: tpsa_r                             ! dst=a*x*y+b*z+c
"""
"""
    mad_tpsa_axypbzpc!(a::Cdouble, tpsa_x::Ptr{RTPSA{Desc}}, tpsa_y::Ptr{RTPSA{Desc}}, b::Cdouble, tpsa_z::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `a`
- `tpsa_x`
- `tpsa_y`
- `b`
- `tpsa_z`
- `c`
- `tpsa_r`
"""
function mad_tpsa_axypbzpc!(a::Cdouble, tpsa_x::Ptr{RTPSA{Desc}}, tpsa_y::Ptr{RTPSA{Desc}}, b::Cdouble, tpsa_z::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_axypbzpc(a::Cdouble, tpsa_x::Ptr{RTPSA{Desc}}, tpsa_y::Ptr{RTPSA{Desc}}, b::Cdouble, tpsa_z::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,tpsa_x,tpsa_y,b,tpsa_u,tpsa_v,c,tpsa_r
bind(C)
import ; implicit none
  real(c_num_t), value, intent(in) :: a, b, c            ! coefs
  type(c_ptr), value, intent(in) :: tpsa_x, tpsa_y, tpsa_u, tpsa_v ! src
  type(c_ptr), value :: tpsa_r                           ! dst=a*x*y+b*u*v+c
"""
"""
    mad_tpsa_axypbvwpc!(a::Cdouble, tpsa_x::Ptr{RTPSA{Desc}}, tpsa_y::Ptr{RTPSA{Desc}}, b::Cdouble, tpsa_u::Ptr{RTPSA{Desc}}, tpsa_v::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `a`
- `tpsa_x`
- `tpsa_y`
- `b`
- `tpsa_u`
- `tpsa_v`
- `c`
- `tpsa_r`
"""
function mad_tpsa_axypbvwpc!(a::Cdouble, tpsa_x::Ptr{RTPSA{Desc}}, tpsa_y::Ptr{RTPSA{Desc}}, b::Cdouble, tpsa_u::Ptr{RTPSA{Desc}}, tpsa_v::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_axypbvwpc(a::Cdouble, tpsa_x::Ptr{RTPSA{Desc}}, tpsa_y::Ptr{RTPSA{Desc}}, b::Cdouble, tpsa_u::Ptr{RTPSA{Desc}}, tpsa_v::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,tpsa_x,b,tpsa_y,c,tpsa_z,tpsa_r
bind(C)
import ; implicit none
  real(c_num_t), value, intent(in) :: a, b, c      ! coefs
  type(c_ptr), value, intent(in) :: tpsa_x, tpsa_y, tpsa_z ! src
  type(c_ptr), value :: tpsa_r                     ! dst=a*x^2+b*y^2+c*z^2
"""
"""
    mad_tpsa_ax2pby2pcz2!(a::Cdouble, tpsa_x::Ptr{RTPSA{Desc}}, b::Cdouble, tpsa_y::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, tpsa_z::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `a`
- `tpsa_x`
- `b`
- `tpsa_y`
- `c`
- `tpsa_z`
- `tpsa_r`
"""
function mad_tpsa_ax2pby2pcz2!(a::Cdouble, tpsa_x::Ptr{RTPSA{Desc}}, b::Cdouble, tpsa_y::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, tpsa_z::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_ax2pby2pcz2(a::Cdouble, tpsa_x::Ptr{RTPSA{Desc}}, b::Cdouble, tpsa_y::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, tpsa_z::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_x,a,b,c,tpsa_r
bind(C)
import ; implicit none
  real(c_num_t), value, intent(in) :: a, b, c   ! coefs
  type(c_ptr), value, intent(in) :: tpsa_x      ! src
  type(c_ptr), value :: tpsa_r                  ! dst=a*x+sqrt(b+c*x^2)
"""
"""
    mad_tpsa_axpsqrtbpcx2!(tpsa_x::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_x`
- `a`
- `b`
- `c`
- `tpsa_r`
"""
function mad_tpsa_axpsqrtbpcx2!(tpsa_x::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_axpsqrtbpcx2(tpsa_x::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_x,a,b,c,tpsa_r
bind(C)
import ; implicit none
  real(c_num_t), value, intent(in) :: a, b, c   ! coefs
  type(c_ptr), value, intent(in) :: tpsa_x      ! src
  type(c_ptr), value :: tpsa_r                  ! dst=log(a*x+sqrt(b+c*x^2))
"""
"""
    mad_tpsa_logaxpsqrtbpcx2!(tpsa_x::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_x`
- `a`
- `b`
- `c`
- `tpsa_r`
"""
function mad_tpsa_logaxpsqrtbpcx2!(tpsa_x::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_logaxpsqrtbpcx2(tpsa_x::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_x,tpsa_y,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_x, tpsa_y ! src
  type(c_ptr), value :: tpsa_r                     ! dst=log(x/y)
"""
"""
    mad_tpsa_logxdy!(tpsa_x::Ptr{RTPSA{Desc}}, tpsa_y::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `tpsa_x`
- `tpsa_y`
- `tpsa_r`
"""
function mad_tpsa_logxdy!(tpsa_x::Ptr{RTPSA{Desc}}, tpsa_y::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_logxdy(tpsa_x::Ptr{RTPSA{Desc}}, tpsa_y::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran function:
na,tpsa_a
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na     ! vectors lengths
  type(c_ptr), intent(in) :: tpsa_a(*)          ! src
  real(c_num_t) :: mnrm                         ! nrm
"""
"""
    mad_tpsa_mnrm(na::Cint, tpsa_a::Ptr{RTPSA{Desc}})::Cdouble

### Input
- `na`
- `tpsa_a`

### Output
- `mnrm`
"""
function mad_tpsa_mnrm(na::Cint, tpsa_a::Ptr{RTPSA{Desc}})::Cdouble
  mnrm = @ccall MAD_TPSA.mad_tpsa_mnrm(na::Cint, tpsa_a::Ptr{RTPSA{Desc}})::Cdouble
  return mnrm
end


"""
Original Fortran subroutine:
na,tpsa_a,tpsa_r
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na     ! vectors lengths
  type(c_ptr), intent(in) :: tpsa_a(*)          ! src
  type(c_ptr) :: tpsa_r(*)                      ! dst
"""
"""
    mad_tpsa_minv!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `na`
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_minv!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_minv(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
na,tpsa_a,tpsa_r,select
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na     ! vectors lengths
  type(c_ptr), intent(in) :: tpsa_a(*)          ! src
  type(c_ptr) :: tpsa_r(*)                      ! dst
  integer(c_idx_t), intent(in) :: select(*)     ! slots to selected
"""
"""
    mad_tpsa_pminv!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, select::Ptr{Cint})

### Input
- `na`
- `tpsa_a`
- `tpsa_r`
- `select`
"""
function mad_tpsa_pminv!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, select::Ptr{Cint})
  @ccall MAD_TPSA.mad_tpsa_pminv(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}}, select::Ptr{Cint})::Cvoid
end


"""
Original Fortran subroutine:
na,tpsa_a,nb,tpsa_b,tpsa_r
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na, nb   ! vectors lengths
  type(c_ptr), intent(in) :: tpsa_a(*), tpsa_b(*) ! src
  type(c_ptr) :: tpsa_r(*)                        ! dst[na]
"""
"""
    mad_tpsa_compose!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, nb::Cint, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `na`
- `tpsa_a`
- `nb`
- `tpsa_b`
- `tpsa_r`
"""
function mad_tpsa_compose!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, nb::Cint, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_compose(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, nb::Cint, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
na,tpsa_a,nb,vb,tpsa_r
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na, nb   ! vectors lengths
  type(c_ptr), intent(in) :: tpsa_a(*)            ! src
  real(c_num_t), intent(in) :: vb(*)              ! src
  type(c_ptr) :: tpsa_r(*)                        ! dst[na]
"""
"""
    mad_tpsa_translate!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, nb::Cint, vb::Ptr{Cdouble}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `na`
- `tpsa_a`
- `nb`
- `vb`
- `tpsa_r`
"""
function mad_tpsa_translate!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, nb::Cint, vb::Ptr{Cdouble}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_translate(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, nb::Cint, vb::Ptr{Cdouble}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
na,tpsa_a,nb,vb,vr
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na, nb   ! vectors lengths
  type(c_ptr), intent(in) :: tpsa_a(*)            ! src
  real(c_num_t), intent(in) :: vb(*)              ! src
  real(c_num_t) :: vr(*)                          ! dst[nb]
"""
"""
    mad_tpsa_eval!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, nb::Cint, vb::Ptr{Cdouble}, vr::Ptr{Cdouble})

### Input
- `na`
- `tpsa_a`
- `nb`
- `vb`
- `vr`
"""
function mad_tpsa_eval!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, nb::Cint, vb::Ptr{Cdouble}, vr::Ptr{Cdouble})
  @ccall MAD_TPSA.mad_tpsa_eval(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, nb::Cint, vb::Ptr{Cdouble}, vr::Ptr{Cdouble})::Cvoid
end


"""
Original Fortran subroutine:
na,tpsa_a,nr,tpsa_r,n,t2r_,pb
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na, nr   ! vectors lengths
  type(c_ptr), intent(in) :: tpsa_a(*)            ! src
  type(c_ptr) :: tpsa_r(*)                        ! dst
  integer(c_ssz_t), value, intent(in) :: n        ! vector length
  integer(c_idx_t), intent(in) :: t2r_(*)         ! vector of index lookup
  integer(c_int), value, intent(in) :: pb         ! poisson bracket 0,1:fwd,-1:bwd
"""
"""
    mad_tpsa_mconv!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, nr::Cint, tpsa_r::Ptr{RTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)

### Input
- `na`
- `tpsa_a`
- `nr`
- `tpsa_r`
- `n`
- `t2r_`
- `pb`
"""
function mad_tpsa_mconv!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, nr::Cint, tpsa_r::Ptr{RTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)
  @ccall MAD_TPSA.mad_tpsa_mconv(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, nr::Cint, tpsa_r::Ptr{RTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)::Cvoid
end


"""
Original Fortran subroutine:
na,tpsa_a,tpsa_r
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na     ! vectors length
  type(c_ptr), value, intent(in) :: tpsa_a      ! src
  type(c_ptr), intent(out) :: tpsa_r(*)         ! dst
"""
"""
    mad_tpsa_vec2fld!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `na`
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_vec2fld!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_vec2fld(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
na,tpsa_a,tpsa_r
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na     ! vectors length
  type(c_ptr), intent(in) :: tpsa_a(*)          ! src
  type(c_ptr), value :: tpsa_r                  ! dst
"""
"""
    mad_tpsa_fld2vec!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `na`
- `tpsa_a`
- `tpsa_r`
"""
function mad_tpsa_fld2vec!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_fld2vec(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
na,tpsa_a,tpsa_b,tpsa_r
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na     ! vectors length
  type(c_ptr), intent(in) :: tpsa_a(*)          ! src
  type(c_ptr), value, intent(in) :: tpsa_b      ! src
  type(c_ptr), value :: tpsa_r                  ! dst
"""
"""
    mad_tpsa_fgrad!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `na`
- `tpsa_a`
- `tpsa_b`
- `tpsa_r`
"""
function mad_tpsa_fgrad!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_fgrad(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
na,tpsa_a,tpsa_b,tpsa_r
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na       ! vectors length
  type(c_ptr), intent(in) :: tpsa_a(*), tpsa_b(*) ! src
  type(c_ptr), intent(out) :: tpsa_r(*)           ! dst[na]
"""
"""
    mad_tpsa_liebra!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `na`
- `tpsa_a`
- `tpsa_b`
- `tpsa_r`
"""
function mad_tpsa_liebra!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_liebra(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
na,tpsa_a,tpsa_b,tpsa_r
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na       ! vectors length
  type(c_ptr), intent(in) :: tpsa_a(*), tpsa_b(*) ! src
  type(c_ptr), intent(out) :: tpsa_r(*)           ! dst[na]
"""
"""
    mad_tpsa_exppb!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `na`
- `tpsa_a`
- `tpsa_b`
- `tpsa_r`
"""
function mad_tpsa_exppb!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_exppb(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
na,tpsa_a,tpsa_b,tpsa_r
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na       ! vectors length
  type(c_ptr), intent(in) :: tpsa_a(*)            ! src
  type(c_ptr), intent(in), optional :: tpsa_b(*)  ! src
  type(c_ptr), intent(out) :: tpsa_r(*)           ! dst[na]
"""
"""
    mad_tpsa_logpb!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `na`
- `tpsa_a`
- `tpsa_b`
- `tpsa_r`
"""
function mad_tpsa_logpb!(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_logpb(na::Cint, tpsa_a::Ptr{RTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa,name_,eps_,nohdr_,stream_
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa      ! src
  character(c_char), intent(in) :: name_(*)   ! tpsa name (nul term. C str)
  real(c_num_t), value, intent(in) :: eps_    ! display precision, e.g. 1d-12
  integer(c_int), value, intent(in) :: nohdr_ ! discard header if not zero
  type(c_ptr), value :: stream_               ! dst=c_null_ptr => stdio
"""
"""
    mad_tpsa_print!(tpsa::Ptr{RTPSA{Desc}}, name_::Cstring, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid})

### Input
- `tpsa`
- `name_`
- `eps_`
- `nohdr_`
- `stream_`
"""
function mad_tpsa_print!(tpsa::Ptr{RTPSA{Desc}}, name_::Cstring, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_tpsa_print(tpsa::Ptr{RTPSA{Desc}}, name_::Cstring, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid})::Cvoid
end


"""
Original Fortran function:
stream_
bind(C)
import ; implicit none
  type(c_ptr) :: tpsa                         ! tpsa to read
  type(c_ptr), value, intent(in) :: stream_   ! src=c_null_ptr => stdin
"""
"""
    mad_tpsa_scan(stream_::Ptr{Cvoid})::Ptr{RTPSA{Desc}}

### Input
- `stream_`

### Output
- `tpsa`
"""
function mad_tpsa_scan(stream_::Ptr{Cvoid})::Ptr{RTPSA{Desc}}
  tpsa = @ccall MAD_TPSA.mad_tpsa_scan(stream_::Ptr{Cvoid})::Ptr{RTPSA{Desc}}
  return tpsa
end


"""
Original Fortran function:
kind_,name_,stream_
bind(C)
import ; implicit none
  type(c_ptr) :: desc                         ! descriptor from header
  integer(c_int), optional, intent(out) :: kind_! tpsa kind (0 real, 1 complex)
  character(c_char), optional, intent(out) :: name_(*) ! tpsa name (nul term. C str)
  type(c_ptr), value, intent(in) :: stream_   ! src=c_null_ptr => stdin
"""
"""
    mad_tpsa_scan_hdr(kind_::Cint, name_::Cstring, stream_::Ptr{Cvoid})::Ptr{Desc{RTPSA,CTPSA}}

### Input
- `kind_`
- `name_`
- `stream_`

### Output
- `desc`
"""
function mad_tpsa_scan_hdr(kind_::Cint, name_::Cstring, stream_::Ptr{Cvoid})::Ptr{Desc{RTPSA,CTPSA}}
  desc = @ccall MAD_TPSA.mad_tpsa_scan_hdr(kind_::Cint, name_::Cstring, stream_::Ptr{Cvoid})::Ptr{Desc{RTPSA,CTPSA}}
  return desc
end


"""
Original Fortran subroutine:
tpsa,stream_
bind(C)
import ; implicit none
  type(c_ptr), value :: tpsa                 ! tpsa to read
  type(c_ptr), value, intent(in) :: stream_  ! src=c_null_ptr => stdin
"""
"""
    mad_tpsa_scan_coef!(tpsa::Ptr{RTPSA{Desc}}, stream_::Ptr{Cvoid})

### Input
- `tpsa`
- `stream_`
"""
function mad_tpsa_scan_coef!(tpsa::Ptr{RTPSA{Desc}}, stream_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_tpsa_scan_coef(tpsa::Ptr{RTPSA{Desc}}, stream_::Ptr{Cvoid})::Cvoid
end


"""
Original Fortran subroutine:
tpsa,name_,fnam_,line_,stream_
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa     ! src
  character(c_char), intent(in) :: name_(*)  ! tpsa name (nul term. C str)
  character(c_char), intent(in) :: fnam_(*)  ! filename  (nul term. C str)
  integer(c_int), value, intent(in) :: line_ ! line number or 0
  type(c_ptr), value :: stream_              ! dst=c_null_ptr => stdio
"""
"""
    mad_tpsa_debug!(tpsa::Ptr{RTPSA{Desc}}, name_::Cstring, fnam_::Cstring, line_::Cint, stream_::Ptr{Cvoid})

### Input
- `tpsa`
- `name_`
- `fnam_`
- `line_`
- `stream_`
"""
function mad_tpsa_debug!(tpsa::Ptr{RTPSA{Desc}}, name_::Cstring, fnam_::Cstring, line_::Cint, stream_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_tpsa_debug(tpsa::Ptr{RTPSA{Desc}}, name_::Cstring, fnam_::Cstring, line_::Cint, stream_::Ptr{Cvoid})::Cvoid
end


"""
Original Fortran function:
desc,mo
bind(C)
import ; implicit none
  type(c_ptr) :: newtpsa                    ! new tpsa
  type(c_ptr), value, intent(in) :: desc    ! descriptor
  integer(c_ord_t), value, intent(in) :: mo ! if mo > d_mo, mo = d_mo
"""
"""
    mad_ctpsa_newd(desc::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{RTPSA{Desc}}

### Input
- `desc`
- `mo`

### Output
- `newtpsa`
"""
function mad_ctpsa_newd(desc::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
  newtpsa = @ccall MAD_TPSA.mad_ctpsa_newd(desc::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
  return newtpsa
end


"""
Original Fortran function:
ctpsa,mo
bind(C)
import ; implicit none
  type(c_ptr) :: newtpsa                    ! new tpsa
  type(c_ptr), value, intent(in) :: ctpsa   ! (reference) tpsa
  integer(c_ord_t), value, intent(in) :: mo ! if mo > d_mo, mo = d_mo
"""
"""
    mad_ctpsa_new(ctpsa::Ptr{CTPSA{Desc}}, mo::Cuchar)::Ptr{RTPSA{Desc}}

### Input
- `ctpsa`
- `mo`

### Output
- `newtpsa`
"""
function mad_ctpsa_new(ctpsa::Ptr{CTPSA{Desc}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
  newtpsa = @ccall MAD_TPSA.mad_ctpsa_new(ctpsa::Ptr{CTPSA{Desc}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
  return newtpsa
end


"""
Original Fortran subroutine:
ctpsa
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa   ! tpsa to delete
"""
"""
    mad_ctpsa_del!(ctpsa::Ptr{CTPSA{Desc}})

### Input
- `ctpsa`
"""
function mad_ctpsa_del!(ctpsa::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_del(ctpsa::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran function:
ctpsa
bind(C)
import ; implicit none
  type(c_ptr) :: desc                       ! tpsa descriptor
  type(c_ptr), value, intent(in) :: ctpsa
"""
"""
    mad_ctpsa_desc(ctpsa::Ptr{CTPSA{Desc}})::Ptr{Desc{RTPSA,CTPSA}}

### Input
- `ctpsa`

### Output
- `desc`
"""
function mad_ctpsa_desc(ctpsa::Ptr{CTPSA{Desc}})::Ptr{Desc{RTPSA,CTPSA}}
  desc = @ccall MAD_TPSA.mad_ctpsa_desc(ctpsa::Ptr{CTPSA{Desc}})::Ptr{Desc{RTPSA,CTPSA}}
  return desc
end


"""
Original Fortran function:
ctpsa
bind(C)
import ; implicit none
  integer(c_ssz_t) :: len                   ! #monomials in tpsa
  type(c_ptr), value, intent(in) :: ctpsa
"""
"""
    mad_ctpsa_len(ctpsa::Ptr{CTPSA{Desc}})::Cint

### Input
- `ctpsa`

### Output
- `len`
"""
function mad_ctpsa_len(ctpsa::Ptr{CTPSA{Desc}})::Cint
  len = @ccall MAD_TPSA.mad_ctpsa_len(ctpsa::Ptr{CTPSA{Desc}})::Cint
  return len
end


"""
Original Fortran function:
ctpsa
bind(C)
import ; implicit none
  type(c_ptr) :: nam                        ! tpsa name (nul term. C str)
  type(c_ptr), value, intent(in) :: ctpsa
"""
"""
    mad_ctpsa_nam(ctpsa::Ptr{CTPSA{Desc}})::Cstring

### Input
- `ctpsa`

### Output
- `nam`
"""
function mad_ctpsa_nam(ctpsa::Ptr{CTPSA{Desc}})::Cstring
  nam = @ccall MAD_TPSA.mad_ctpsa_nam(ctpsa::Ptr{CTPSA{Desc}})::Cstring
  return nam
end


"""
Original Fortran function:
ctpsa
bind(C)
import ; implicit none
  integer(c_ord_t) :: ord                   ! tpsa order
  type(c_ptr), value, intent(in) :: ctpsa
"""
"""
    mad_ctpsa_ord(ctpsa::Ptr{CTPSA{Desc}})::Cint

### Input
- `ctpsa`

### Output
- `ord`
"""
function mad_ctpsa_ord(ctpsa::Ptr{CTPSA{Desc}})::Cint
  ord = @ccall MAD_TPSA.mad_ctpsa_ord(ctpsa::Ptr{CTPSA{Desc}})::Cint
  return ord
end


"""
Original Fortran function:
n,ctpsa
bind(C)
import ; implicit none
  integer(c_ord_t) :: ord                   ! max of all tpsas order
  integer(c_ssz_t), value, intent(in) :: n  ! #ctpsa
  type(c_ptr), intent(in) :: ctpsa(*)
"""
"""
    mad_ctpsa_ordn(n::Cint, ctpsa::Ptr{CTPSA{Desc}})::Cint

### Input
- `n`
- `ctpsa`

### Output
- `ord`
"""
function mad_ctpsa_ordn(n::Cint, ctpsa::Ptr{CTPSA{Desc}})::Cint
  ord = @ccall MAD_TPSA.mad_ctpsa_ordn(n::Cint, ctpsa::Ptr{CTPSA{Desc}})::Cint
  return ord
end


"""
Original Fortran function:
ctpsa
bind(C)
import ; implicit none
  logical(c_bool) :: ret                  ! true or false
  type(c_ptr), value, intent(in) :: ctpsa ! sanity check on TPSA integrity
"""
"""
    mad_ctpsa_isvalid(ctpsa::Ptr{CTPSA{Desc}})::Cuchar

### Input
- `ctpsa`

### Output
- `ret`
"""
function mad_ctpsa_isvalid(ctpsa::Ptr{CTPSA{Desc}})::Cuchar
  ret = @ccall MAD_TPSA.mad_ctpsa_isvalid(ctpsa::Ptr{CTPSA{Desc}})::Cuchar
  return ret
end


"""
Original Fortran subroutine:
ctpsa,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa    ! src
  type(c_ptr), value :: ctpsa_r              ! dst
"""
"""
    mad_ctpsa_copy!(ctpsa::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa`
- `ctpsa_r`
"""
function mad_ctpsa_copy!(ctpsa::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_copy(ctpsa::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa,ctpsa_r,inv
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa    ! src
  type(c_ptr), value :: ctpsa_r              ! dst
  logical(c_bool), value, intent(in) :: inv  ! scl by inverse
"""
"""
    mad_ctpsa_sclord!(ctpsa::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, inv::Cuchar)

### Input
- `ctpsa`
- `ctpsa_r`
- `inv`
"""
function mad_ctpsa_sclord!(ctpsa::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, inv::Cuchar)
  @ccall MAD_TPSA.mad_ctpsa_sclord(ctpsa::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, inv::Cuchar)::Cvoid
end


"""
Original Fortran subroutine:
ctpsa,ctpsa_r,ord
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa    ! src
  type(c_ptr), value :: ctpsa_r              ! dst
  integer(c_ord_t), value, intent(in) :: ord ! order to retrieve
"""
"""
    mad_ctpsa_getord!(ctpsa::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, ord::Cint)

### Input
- `ctpsa`
- `ctpsa_r`
- `ord`
"""
function mad_ctpsa_getord!(ctpsa::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, ord::Cint)
  @ccall MAD_TPSA.mad_ctpsa_getord(ctpsa::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, ord::Cint)::Cvoid
end


"""
Original Fortran subroutine:
ctpsa,ctpsa_r,ord
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa    ! src
  type(c_ptr), value :: ctpsa_r              ! dst
  integer(c_int), value, intent(in) :: ord   ! cut order: 0..-ord or ord..mo
"""
"""
    mad_ctpsa_cutord!(ctpsa::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, ord::Cint)

### Input
- `ctpsa`
- `ctpsa_r`
- `ord`
"""
function mad_ctpsa_cutord!(ctpsa::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, ord::Cint)
  @ccall MAD_TPSA.mad_ctpsa_cutord(ctpsa::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, ord::Cint)::Cvoid
end


"""
Original Fortran subroutine:
ctpsa,v,iv_,scl_
bind(C)
import ; implicit none
  type(c_ptr), value :: ctpsa
  complex(c_cpx_t), value, intent(in) :: v, scl_  ! 0th and 1st order values
  integer(c_idx_t), value, intent(in) :: iv_      ! variable index (1st order)
"""
"""
    mad_ctpsa_setvar!(ctpsa::Ptr{CTPSA{Desc}}, v::Cuchar, iv_::Cint, scl_::Cdouble)

### Input
- `ctpsa`
- `v`
- `iv_`
- `scl_`
"""
function mad_ctpsa_setvar!(ctpsa::Ptr{CTPSA{Desc}}, v::Cuchar, iv_::Cint, scl_::Cdouble)
  @ccall MAD_TPSA.mad_ctpsa_setvar(ctpsa::Ptr{CTPSA{Desc}}, v::Cuchar, iv_::Cint, scl_::Cdouble)::Cvoid
end


"""
Original Fortran subroutine:
ctpsa,nam
bind(C)
import ; implicit none
  type(c_ptr), value :: ctpsa
  character(c_char), intent(in) :: nam(*)    ! tpsa name (nul term. C str)
"""
"""
    mad_ctpsa_setnam!(ctpsa::Ptr{CTPSA{Desc}}, nam::Cstring)

### Input
- `ctpsa`
- `nam`
"""
function mad_ctpsa_setnam!(ctpsa::Ptr{CTPSA{Desc}}, nam::Cstring)
  @ccall MAD_TPSA.mad_ctpsa_setnam(ctpsa::Ptr{CTPSA{Desc}}, nam::Cstring)::Cvoid
end


"""
Original Fortran subroutine:
ctpsa
bind(C)
import ; implicit none
  type(c_ptr), value :: ctpsa
"""
"""
    mad_ctpsa_clear!(ctpsa::Ptr{CTPSA{Desc}})

### Input
- `ctpsa`
"""
function mad_ctpsa_clear!(ctpsa::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_clear(ctpsa::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran function:
ctpsa
bind(C)
import ; implicit none
  logical(c_bool) :: ret                     ! true or false
  type(c_ptr), value, intent(in) :: ctpsa
"""
"""
    mad_ctpsa_isnul(ctpsa::Ptr{CTPSA{Desc}})::Cuchar

### Input
- `ctpsa`

### Output
- `ret`
"""
function mad_ctpsa_isnul(ctpsa::Ptr{CTPSA{Desc}})::Cuchar
  ret = @ccall MAD_TPSA.mad_ctpsa_isnul(ctpsa::Ptr{CTPSA{Desc}})::Cuchar
  return ret
end


"""
Original Fortran subroutine:
ctpsa,ctpsa_r,n,t2r_,pb
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa    ! src
  type(c_ptr), value :: ctpsa_r              ! dst
  integer(c_ssz_t), value, intent(in) :: n   ! vector length
  integer(c_idx_t), intent(in) :: t2r_(*)    ! vector of index lookup
  integer(c_int), value, intent(in) :: pb    ! poisson bracket 0,1:fwd,-1:bwd
"""
"""
    mad_ctpsa_convert!(ctpsa::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)

### Input
- `ctpsa`
- `ctpsa_r`
- `n`
- `t2r_`
- `pb`
"""
function mad_ctpsa_convert!(ctpsa::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)
  @ccall MAD_TPSA.mad_ctpsa_convert(ctpsa::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)::Cvoid
end


"""
Original Fortran subroutine:
ctpsa,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa   ! src
  type(c_ptr), value :: tpsa_r              ! dst=real(src)
"""
"""
    mad_ctpsa_real!(ctpsa::Ptr{CTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `ctpsa`
- `tpsa_r`
"""
function mad_ctpsa_real!(ctpsa::Ptr{CTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_real(ctpsa::Ptr{CTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa,tpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa   ! src
  type(c_ptr), value :: tpsa_r              ! dst=imag(src)
"""
"""
    mad_ctpsa_imag!(ctpsa::Ptr{CTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})

### Input
- `ctpsa`
- `tpsa_r`
"""
function mad_ctpsa_imag!(ctpsa::Ptr{CTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_imag(ctpsa::Ptr{CTPSA{Desc}}, tpsa_r::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_re_,tpsa_im_,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_re_, tpsa_im_ ! src
  type(c_ptr), value :: ctpsa_r             ! dst=(re or 0)+i*(im or 0)
"""
"""
    mad_ctpsa_cplx!(tpsa_re_::Ptr{RTPSA{Desc}}, tpsa_im_::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `tpsa_re_`
- `tpsa_im_`
- `ctpsa_r`
"""
function mad_ctpsa_cplx!(tpsa_re_::Ptr{RTPSA{Desc}}, tpsa_im_::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_cplx(tpsa_re_::Ptr{RTPSA{Desc}}, tpsa_im_::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran function:
ctpsa,n,m_,i
bind(C)
import ; implicit none
  integer(c_ord_t) :: ord                  ! monomial order
  type(c_ptr), value, intent(in) :: ctpsa  !
  integer(c_idx_t), value, intent(in) :: i ! slot index (must be valid)
  integer(c_ssz_t), value, intent(in) :: n ! monomial length
  integer(c_ord_t) :: m_(*)                ! monomial to fill (if provided)
"""
"""
    mad_ctpsa_mono(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m_::Ptr{Cuchar}, i::Cint)::Cint

### Input
- `ctpsa`
- `n`
- `m_`
- `i`

### Output
- `ord`
"""
function mad_ctpsa_mono(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m_::Ptr{Cuchar}, i::Cint)::Cint
  ord = @ccall MAD_TPSA.mad_ctpsa_mono(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m_::Ptr{Cuchar}, i::Cint)::Cint
  return ord
end


"""
Original Fortran function:
ctpsa,n,s
bind(C)
import ; implicit none
  integer(c_idx_t) :: idx                  ! monomial index
  type(c_ptr), value, intent(in) :: ctpsa  !
  integer(c_ssz_t), value, intent(in) :: n ! string length or 0 (unknown)
  character(c_char), intent(in) :: s(*)    ! monomial as string "[0-9]*"
"""
"""
    mad_ctpsa_idxs(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring)::Cint

### Input
- `ctpsa`
- `n`
- `s`

### Output
- `idx`
"""
function mad_ctpsa_idxs(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring)::Cint
  idx = @ccall MAD_TPSA.mad_ctpsa_idxs(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring)::Cint
  return idx
end


"""
Original Fortran function:
ctpsa,n,m
bind(C)
import ; implicit none
  integer(c_idx_t) :: idx                  ! monomial index
  type(c_ptr), value, intent(in) :: ctpsa  !
  integer(c_ssz_t), value, intent(in) :: n ! monomial length
  integer(c_ord_t), intent(in) :: m(*)     ! monomial
"""
"""
    mad_ctpsa_idxm(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint

### Input
- `ctpsa`
- `n`
- `m`

### Output
- `idx`
"""
function mad_ctpsa_idxm(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint
  idx = @ccall MAD_TPSA.mad_ctpsa_idxm(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint
  return idx
end


"""
Original Fortran function:
ctpsa,n,m
bind(C)
import ; implicit none
  integer(c_idx_t) :: idx                  ! monomial index
  type(c_ptr), value, intent(in) :: ctpsa  !
  integer(c_ssz_t), value, intent(in) :: n ! monomial length
  integer(c_int), intent(in) :: m(*)       ! sparse monomial (idx,ord)
"""
"""
    mad_ctpsa_idxsm(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint

### Input
- `ctpsa`
- `n`
- `m`

### Output
- `idx`
"""
function mad_ctpsa_idxsm(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint
  idx = @ccall MAD_TPSA.mad_ctpsa_idxsm(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cint
  return idx
end


"""
Original Fortran function:
ctpsa,i,n,m_,v_
bind(C)
import ; implicit none                   ! scan for non-zero coefs starting at i
  integer(c_idx_t) :: idx                  ! next index to start searching or -1
  type(c_ptr), value, intent(in) :: ctpsa  !
  integer(c_idx_t), value, intent(in) :: i ! index to start searching
  integer(c_ssz_t), value, intent(in) :: n ! monomial length
  integer(c_ord_t) :: m_(*)                ! monomial to fill (if provided)
  real(c_cpx_t), intent(out) :: v_         ! coeff to fill (if provided)
"""
"""
    mad_ctpsa_cycle(ctpsa::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, v_::Cdouble)::Cint

### Input
- `ctpsa`
- `i`
- `n`
- `m_`
- `v_`

### Output
- `idx`
"""
function mad_ctpsa_cycle(ctpsa::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, v_::Cdouble)::Cint
  idx = @ccall MAD_TPSA.mad_ctpsa_cycle(ctpsa::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, m_::Ptr{Cuchar}, v_::Cdouble)::Cint
  return idx
end


"""
Original Fortran function:
ctpsa
bind(C)
import ; implicit none
  complex(c_cpx_t) :: val                   ! value at order 0 (index 0)
  type(c_ptr), value, intent(in) :: ctpsa
"""
"""
    mad_ctpsa_get0(ctpsa::Ptr{CTPSA{Desc}})::Cdouble

### Input
- `ctpsa`

### Output
- `val`
"""
function mad_ctpsa_get0(ctpsa::Ptr{CTPSA{Desc}})::Cdouble
  val = @ccall MAD_TPSA.mad_ctpsa_get0(ctpsa::Ptr{CTPSA{Desc}})::Cdouble
  return val
end


"""
Original Fortran function:
ctpsa,i
bind(C)
import ; implicit none
  complex(c_cpx_t) :: val                   ! value at index
  type(c_ptr), value, intent(in) :: ctpsa
  integer(c_idx_t), value, intent(in) :: i  ! slot index (must be valid)
"""
"""
    mad_ctpsa_geti(ctpsa::Ptr{CTPSA{Desc}}, i::Cint)::Cdouble

### Input
- `ctpsa`
- `i`

### Output
- `val`
"""
function mad_ctpsa_geti(ctpsa::Ptr{CTPSA{Desc}}, i::Cint)::Cdouble
  val = @ccall MAD_TPSA.mad_ctpsa_geti(ctpsa::Ptr{CTPSA{Desc}}, i::Cint)::Cdouble
  return val
end


"""
Original Fortran function:
ctpsa,n,s
bind(C)
import ; implicit none
  complex(c_cpx_t) :: val                   ! value at string monomial
  type(c_ptr), value, intent(in) :: ctpsa
  integer(c_ssz_t), value, intent(in) :: n  ! string length or 0 (unknown)
  character(c_char), intent(in) :: s(*)     ! monomial as string "[0-9]*"
"""
"""
    mad_ctpsa_gets(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring)::Cdouble

### Input
- `ctpsa`
- `n`
- `s`

### Output
- `val`
"""
function mad_ctpsa_gets(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring)::Cdouble
  val = @ccall MAD_TPSA.mad_ctpsa_gets(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring)::Cdouble
  return val
end


"""
Original Fortran function:
ctpsa,n,m
bind(C)
import ; implicit none
  complex(c_cpx_t) :: val                   ! value at monomial
  type(c_ptr), value, intent(in) :: ctpsa
  integer(c_ssz_t), value, intent(in) :: n  ! monomial length
  integer(c_ord_t), intent(in) :: m(*)      ! monomial
"""
"""
    mad_ctpsa_getm(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cdouble

### Input
- `ctpsa`
- `n`
- `m`

### Output
- `val`
"""
function mad_ctpsa_getm(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cdouble
  val = @ccall MAD_TPSA.mad_ctpsa_getm(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cdouble
  return val
end


"""
Original Fortran function:
ctpsa,n,m
bind(C)
import ; implicit none
  complex(c_cpx_t) :: val                   ! value at sparse monomial
  type(c_ptr), value, intent(in) :: ctpsa
  integer(c_ssz_t), value, intent(in) :: n  ! monomial length
  integer(c_int), intent(in) :: m(*)        ! sparse monomial (idx,ord)
"""
"""
    mad_ctpsa_getsm(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cdouble

### Input
- `ctpsa`
- `n`
- `m`

### Output
- `val`
"""
function mad_ctpsa_getsm(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cdouble
  val = @ccall MAD_TPSA.mad_ctpsa_getsm(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cdouble
  return val
end


"""
Original Fortran subroutine:
ctpsa,i,n,v
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa
  integer(c_idx_t), value, intent(in) :: i  ! slot index (must be valid)
  integer(c_ssz_t), value, intent(in) :: n  ! vector length
  complex(c_cpx_t) :: v(*)                  ! vector to fill
"""
"""
    mad_ctpsa_getv!(ctpsa::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, v::Cuchar)

### Input
- `ctpsa`
- `i`
- `n`
- `v`
"""
function mad_ctpsa_getv!(ctpsa::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, v::Cuchar)
  @ccall MAD_TPSA.mad_ctpsa_getv(ctpsa::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, v::Cuchar)::Cvoid
end


"""
Original Fortran subroutine:
ctpsa,a,b
bind(C)
import ; implicit none
  type(c_ptr), value :: ctpsa
  complex(c_cpx_t), value, intent(in) :: a, b ! ct[0] = a*ct[0]+b
"""
"""
    mad_ctpsa_set0!(ctpsa::Ptr{CTPSA{Desc}}, a::Cdouble, b::Cdouble)

### Input
- `ctpsa`
- `a`
- `b`
"""
function mad_ctpsa_set0!(ctpsa::Ptr{CTPSA{Desc}}, a::Cdouble, b::Cdouble)
  @ccall MAD_TPSA.mad_ctpsa_set0(ctpsa::Ptr{CTPSA{Desc}}, a::Cdouble, b::Cdouble)::Cvoid
end


"""
Original Fortran subroutine:
ctpsa,i,a,b
bind(C)
import ; implicit none
  type(c_ptr), value :: ctpsa
  integer(c_idx_t), value, intent(in) :: i     ! slot index (must be valid)
  complex(c_cpx_t), value, intent(in) :: a, b  ! ct[i] = a*ct[i]+b
"""
"""
    mad_ctpsa_seti!(ctpsa::Ptr{CTPSA{Desc}}, i::Cint, a::Cdouble, b::Cdouble)

### Input
- `ctpsa`
- `i`
- `a`
- `b`
"""
function mad_ctpsa_seti!(ctpsa::Ptr{CTPSA{Desc}}, i::Cint, a::Cdouble, b::Cdouble)
  @ccall MAD_TPSA.mad_ctpsa_seti(ctpsa::Ptr{CTPSA{Desc}}, i::Cint, a::Cdouble, b::Cdouble)::Cvoid
end


"""
Original Fortran subroutine:
ctpsa,n,s,a,b
bind(C)
import ; implicit none
  type(c_ptr), value :: ctpsa
  integer(c_ssz_t), value, intent(in) :: n    ! string length or 0 (unknown)
  character(c_char), intent(in) :: s(*)       ! monomial as string "[0-9]*"
  complex(c_cpx_t), value, intent(in) :: a, b ! ct[s] = a*ct[s]+b
"""
"""
    mad_ctpsa_sets!(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, a::Cdouble, b::Cdouble)

### Input
- `ctpsa`
- `n`
- `s`
- `a`
- `b`
"""
function mad_ctpsa_sets!(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, a::Cdouble, b::Cdouble)
  @ccall MAD_TPSA.mad_ctpsa_sets(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, s::Cstring, a::Cdouble, b::Cdouble)::Cvoid
end


"""
Original Fortran subroutine:
ctpsa,n,m,a,b
bind(C)
import ; implicit none
  type(c_ptr), value :: ctpsa
  integer(c_ssz_t), value, intent(in) :: n     ! monomial length
  integer(c_ord_t), intent(in) :: m(*)         ! monomial
  complex(c_cpx_t), value, intent(in) :: a, b  ! ct[m] = a*ct[m]+b
"""
"""
    mad_ctpsa_setm!(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::Cdouble, b::Cdouble)

### Input
- `ctpsa`
- `n`
- `m`
- `a`
- `b`
"""
function mad_ctpsa_setm!(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::Cdouble, b::Cdouble)
  @ccall MAD_TPSA.mad_ctpsa_setm(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::Cdouble, b::Cdouble)::Cvoid
end


"""
Original Fortran subroutine:
ctpsa,n,m,a,b
bind(C)
import ; implicit none
  type(c_ptr), value :: ctpsa
  integer(c_ssz_t), value, intent(in) :: n     ! monomial length
  integer(c_int), intent(in) :: m(*)           ! sparse monomial (idx,ord)
  complex(c_cpx_t), value, intent(in) :: a, b  ! ct[m] = a*ct[m]+b
"""
"""
    mad_ctpsa_setsm!(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::Cdouble, b::Cdouble)

### Input
- `ctpsa`
- `n`
- `m`
- `a`
- `b`
"""
function mad_ctpsa_setsm!(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::Cdouble, b::Cdouble)
  @ccall MAD_TPSA.mad_ctpsa_setsm(ctpsa::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar}, a::Cdouble, b::Cdouble)::Cvoid
end


"""
Original Fortran subroutine:
ctpsa,i,n,v
bind(C)
import ; implicit none
  type(c_ptr), value :: ctpsa
  integer(c_idx_t), value, intent(in) :: i     ! slot index (must be valid)
  integer(c_ssz_t), value, intent(in) :: n     ! vector length
  complex(c_cpx_t), intent(in) :: v(*)         ! vector to copy
"""
"""
    mad_ctpsa_setv!(ctpsa::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, v::Cuchar)

### Input
- `ctpsa`
- `i`
- `n`
- `v`
"""
function mad_ctpsa_setv!(ctpsa::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, v::Cuchar)
  @ccall MAD_TPSA.mad_ctpsa_setv(ctpsa::Ptr{CTPSA{Desc}}, i::Cint, n::Cint, v::Cuchar)::Cvoid
end


"""
Original Fortran function:
ctpsa_a,ctpsa_b,eps_
bind(C)
import ; implicit none
  logical(c_bool) :: ret                    ! true or false
  type(c_ptr), value, intent(in) :: ctpsa_a, ctpsa_b
  real(c_num_t), value, intent(in) :: eps_  ! tolerance during comparison
"""
"""
    mad_ctpsa_equ(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, eps_::Cdouble)::Cuchar

### Input
- `ctpsa_a`
- `ctpsa_b`
- `eps_`

### Output
- `ret`
"""
function mad_ctpsa_equ(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, eps_::Cdouble)::Cuchar
  ret = @ccall MAD_TPSA.mad_ctpsa_equ(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, eps_::Cdouble)::Cuchar
  return ret
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a, ctpsa_b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                       ! dst
"""
"""
    mad_ctpsa_dif!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_dif!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_dif(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a, ctpsa_b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                       ! dst
"""
"""
    mad_ctpsa_add!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_add!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_add(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a, ctpsa_b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                       ! dst
"""
"""
    mad_ctpsa_sub!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_sub!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sub(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a, ctpsa_b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                       ! dst
"""
"""
    mad_ctpsa_mul!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_mul!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_mul(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a, ctpsa_b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                       ! dst
"""
"""
    mad_ctpsa_div!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_div!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_div(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a, ctpsa_b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                       ! dst
"""
"""
    mad_ctpsa_pow!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_pow!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_pow(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,n,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a   ! src
  integer(c_int), value, intent(in) :: n      ! power (integer)
  type(c_ptr), value :: ctpsa_r               ! dst
"""
"""
    mad_ctpsa_powi!(ctpsa_a::Ptr{CTPSA{Desc}}, n::Cint, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `n`
- `ctpsa_r`
"""
function mad_ctpsa_powi!(ctpsa_a::Ptr{CTPSA{Desc}}, n::Cint, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_powi(ctpsa_a::Ptr{CTPSA{Desc}}, n::Cint, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,v,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a   ! src
  complex(c_cpx_t), value, intent(in) :: v    ! power (real)
  type(c_ptr), value :: ctpsa_r               ! dst
"""
"""
    mad_ctpsa_pown!(ctpsa_a::Ptr{CTPSA{Desc}}, v::Cuchar, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `v`
- `ctpsa_r`
"""
function mad_ctpsa_pown!(ctpsa_a::Ptr{CTPSA{Desc}}, v::Cuchar, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_pown(ctpsa_a::Ptr{CTPSA{Desc}}, v::Cuchar, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran function:
ctpsa_a,tpsa_b,eps
bind(C)
import ; implicit none
  logical(c_bool) :: ret                   ! true or false
  type(c_ptr), value, intent(in) :: ctpsa_a, tpsa_b
  real(c_num_t), value, intent(in) :: eps  ! tolerance during comparison
"""
"""
    mad_ctpsa_equt(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, eps::Cdouble)::Cuchar

### Input
- `ctpsa_a`
- `tpsa_b`
- `eps`

### Output
- `ret`
"""
function mad_ctpsa_equt(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, eps::Cdouble)::Cuchar
  ret = @ccall MAD_TPSA.mad_ctpsa_equt(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, eps::Cdouble)::Cuchar
  return ret
end


"""
Original Fortran subroutine:
ctpsa_a,tpsa_b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a, tpsa_b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                      ! dst
"""
"""
    mad_ctpsa_addt!(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `tpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_addt!(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_addt(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,tpsa_b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a, tpsa_b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                      ! dst
"""
"""
    mad_ctpsa_subt!(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `tpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_subt!(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_subt(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,ctpsa_b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a, ctpsa_b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                      ! dst
"""
"""
    mad_ctpsa_tsub!(tpsa_a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `tpsa_a`
- `ctpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_tsub!(tpsa_a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tsub(tpsa_a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,tpsa_b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a, tpsa_b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                      ! dst
"""
"""
    mad_ctpsa_dift!(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `tpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_dift!(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_dift(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,ctpsa_b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a, ctpsa_b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                      ! dst
"""
"""
    mad_ctpsa_tdif!(tpsa_a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `tpsa_a`
- `ctpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_tdif!(tpsa_a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tdif(tpsa_a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,tpsa_b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a, tpsa_b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                      ! dst
"""
"""
    mad_ctpsa_mult!(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `tpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_mult!(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_mult(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,tpsa_b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a, tpsa_b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                      ! dst
"""
"""
    mad_ctpsa_divt!(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `tpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_divt!(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_divt(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,ctpsa_b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a, ctpsa_b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                      ! dst
"""
"""
    mad_ctpsa_tdiv!(tpsa_a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `tpsa_a`
- `ctpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_tdiv!(tpsa_a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tdiv(tpsa_a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,tpsa_b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a, tpsa_b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                      ! dst
"""
"""
    mad_ctpsa_powt!(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `tpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_powt!(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_powt(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,ctpsa_b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a, ctpsa_b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                      ! dst
"""
"""
    mad_ctpsa_tpow!(tpsa_a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `tpsa_a`
- `ctpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_tpow!(tpsa_a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tpow(tpsa_a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,tpsa_b,ctpsa_r,nv
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a, tpsa_b ! src
  type(c_ptr), value :: ctpsa_r                     ! dst
  integer(c_int), value, intent(in) :: nv        ! #variables (desc%nv if 0)
"""
"""
    mad_ctpsa_poisbrat!(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, nv::Cint)

### Input
- `ctpsa_a`
- `tpsa_b`
- `ctpsa_r`
- `nv`
"""
function mad_ctpsa_poisbrat!(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, nv::Cint)
  @ccall MAD_TPSA.mad_ctpsa_poisbrat(ctpsa_a::Ptr{CTPSA{Desc}}, tpsa_b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, nv::Cint)::Cvoid
end


"""
Original Fortran subroutine:
tpsa_a,ctpsa_b,ctpsa_r,nv
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: tpsa_a, ctpsa_b ! src
  type(c_ptr), value :: ctpsa_r                     ! dst
  integer(c_int), value, intent(in) :: nv        ! #variables (desc%nv if 0)
"""
"""
    mad_ctpsa_tpoisbra!(tpsa_a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, nv::Cint)

### Input
- `tpsa_a`
- `ctpsa_b`
- `ctpsa_r`
- `nv`
"""
function mad_ctpsa_tpoisbra!(tpsa_a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, nv::Cint)
  @ccall MAD_TPSA.mad_ctpsa_tpoisbra(tpsa_a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, nv::Cint)::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a  ! src
  type(c_ptr), value :: ctpsa_r              ! dst
"""
"""
    mad_ctpsa_abs!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_abs!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_abs(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a  ! src
  type(c_ptr), value :: ctpsa_r              ! dst
"""
"""
    mad_ctpsa_arg!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_arg!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_arg(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a  ! src
  type(c_ptr), value :: ctpsa_r              ! dst
"""
"""
    mad_ctpsa_conj!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_conj!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_conj(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran function:
ctpsa_a,ctpsa_b_
bind(C)
import ; implicit none
  complex(c_cpx_t) :: nrm1                     ! sum_i |a[i]-b_[i]|
  type(c_ptr), value, intent(in) :: ctpsa_a, ctpsa_b_
"""
"""
    mad_ctpsa_nrm1(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b_::Ptr{CTPSA{Desc}})::ComplexF64

### Input
- `ctpsa_a`
- `ctpsa_b_`

### Output
- `nrm1`
"""
function mad_ctpsa_nrm1(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b_::Ptr{CTPSA{Desc}})::ComplexF64
  nrm1 = @ccall MAD_TPSA.mad_ctpsa_nrm1(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b_::Ptr{CTPSA{Desc}})::ComplexF64
  return nrm1
end


"""
Original Fortran function:
ctpsa_a,ctpsa_b_
bind(C)
import ; implicit none
  complex(c_cpx_t) :: nrm2                     ! sqrt(sum_i (a[i]-b_[i])^2)
  type(c_ptr), value, intent(in) :: ctpsa_a, ctpsa_b_
"""
"""
    mad_ctpsa_nrm2(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b_::Ptr{CTPSA{Desc}})::ComplexF64

### Input
- `ctpsa_a`
- `ctpsa_b_`

### Output
- `nrm2`
"""
function mad_ctpsa_nrm2(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b_::Ptr{CTPSA{Desc}})::ComplexF64
  nrm2 = @ccall MAD_TPSA.mad_ctpsa_nrm2(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b_::Ptr{CTPSA{Desc}})::ComplexF64
  return nrm2
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r,iv
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
  integer(c_int), value, intent(in) :: iv      ! variable index (1st order)
"""
"""
    mad_ctpsa_integ!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, iv::Cint)

### Input
- `ctpsa_a`
- `ctpsa_r`
- `iv`
"""
function mad_ctpsa_integ!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, iv::Cint)
  @ccall MAD_TPSA.mad_ctpsa_integ(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, iv::Cint)::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r,iv
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
  integer(c_int), value, intent(in) :: iv      ! variable index (1st order)
"""
"""
    mad_ctpsa_deriv!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, iv::Cint)

### Input
- `ctpsa_a`
- `ctpsa_r`
- `iv`
"""
function mad_ctpsa_deriv!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, iv::Cint)
  @ccall MAD_TPSA.mad_ctpsa_deriv(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, iv::Cint)::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r,n,m
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a   ! src
  type(c_ptr), value :: ctpsa_r               ! dst
  integer(c_ssz_t), value, intent(in) :: n    ! monomial length
  integer(c_ord_t), intent(in) :: m(*)        ! monomial
"""
"""
    mad_ctpsa_derivm!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})

### Input
- `ctpsa_a`
- `ctpsa_r`
- `n`
- `m`
"""
function mad_ctpsa_derivm!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})
  @ccall MAD_TPSA.mad_ctpsa_derivm(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, n::Cint, m::Ptr{Cuchar})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_b,ctpsa_r,nv
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a,ctpsa_b ! src
  type(c_ptr), value :: ctpsa_r                     ! dst
  integer(c_int), value, intent(in) :: nv        ! #variables (desc%nv if 0)
"""
"""
    mad_ctpsa_poisbra!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, nv::Cint)

### Input
- `ctpsa_a`
- `ctpsa_b`
- `ctpsa_r`
- `nv`
"""
function mad_ctpsa_poisbra!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, nv::Cint)
  @ccall MAD_TPSA.mad_ctpsa_poisbra(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, nv::Cint)::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,n,coef,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
  integer(c_ssz_t), value, intent(in) :: n     ! vector length
  complex(c_cpx_t), intent(in) :: coef(*)      ! vector of taylor coefs
"""
"""
    mad_ctpsa_taylor!(ctpsa_a::Ptr{CTPSA{Desc}}, n::Cint, coef::Cdouble, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `n`
- `coef`
- `ctpsa_r`
"""
function mad_ctpsa_taylor!(ctpsa_a::Ptr{CTPSA{Desc}}, n::Cint, coef::Cdouble, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_taylor(ctpsa_a::Ptr{CTPSA{Desc}}, n::Cint, coef::Cdouble, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,v,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! src and dst
  complex(c_cpx_t), value, intent(in) :: v     ! r = r+v*a
"""
"""
    mad_ctpsa_acc!(ctpsa_a::Ptr{CTPSA{Desc}}, v::Cuchar, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `v`
- `ctpsa_r`
"""
function mad_ctpsa_acc!(ctpsa_a::Ptr{CTPSA{Desc}}, v::Cuchar, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_acc(ctpsa_a::Ptr{CTPSA{Desc}}, v::Cuchar, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,v,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
  complex(c_cpx_t), value, intent(in) :: v     ! r = v*a
"""
"""
    mad_ctpsa_scl!(ctpsa_a::Ptr{CTPSA{Desc}}, v::Cuchar, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `v`
- `ctpsa_r`
"""
function mad_ctpsa_scl!(ctpsa_a::Ptr{CTPSA{Desc}}, v::Cuchar, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_scl(ctpsa_a::Ptr{CTPSA{Desc}}, v::Cuchar, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,v,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
  complex(c_cpx_t), value, intent(in) :: v     ! r = v/a
"""
"""
    mad_ctpsa_inv!(ctpsa_a::Ptr{CTPSA{Desc}}, v::Cuchar, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `v`
- `ctpsa_r`
"""
function mad_ctpsa_inv!(ctpsa_a::Ptr{CTPSA{Desc}}, v::Cuchar, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_inv(ctpsa_a::Ptr{CTPSA{Desc}}, v::Cuchar, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,v,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
  complex(c_cpx_t), value, intent(in) :: v     ! r = v/sqrt(a)
"""
"""
    mad_ctpsa_invsqrt!(ctpsa_a::Ptr{CTPSA{Desc}}, v::Cuchar, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `v`
- `ctpsa_r`
"""
function mad_ctpsa_invsqrt!(ctpsa_a::Ptr{CTPSA{Desc}}, v::Cuchar, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_invsqrt(ctpsa_a::Ptr{CTPSA{Desc}}, v::Cuchar, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_sqrt!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_sqrt!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sqrt(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_exp!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_exp!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_exp(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_log!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_log!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_log(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_s,ctpsa_c
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a     ! src
  type(c_ptr), value :: ctpsa_s, ctpsa_c        ! dst_sin, dst_cos
"""
"""
    mad_ctpsa_sincos!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_s::Ptr{CTPSA{Desc}}, ctpsa_c::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_s`
- `ctpsa_c`
"""
function mad_ctpsa_sincos!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_s::Ptr{CTPSA{Desc}}, ctpsa_c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sincos(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_s::Ptr{CTPSA{Desc}}, ctpsa_c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a     ! src
  type(c_ptr), value :: ctpsa_r                 ! dst
"""
"""
    mad_ctpsa_sin!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_sin!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sin(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_cos!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_cos!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_cos(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_tan!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_tan!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tan(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_cot!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_cot!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_cot(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_sinc!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_sinc!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sinc(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_s,ctpsa_c
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_s, ctpsa_c       ! dst_sin, dst_cos
"""
"""
    mad_ctpsa_sincosh!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_s::Ptr{CTPSA{Desc}}, ctpsa_c::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_s`
- `ctpsa_c`
"""
function mad_ctpsa_sincosh!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_s::Ptr{CTPSA{Desc}}, ctpsa_c::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sincosh(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_s::Ptr{CTPSA{Desc}}, ctpsa_c::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_sinh!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_sinh!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sinh(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_cosh!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_cosh!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_cosh(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_tanh!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_tanh!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tanh(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_coth!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_coth!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_coth(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_sinhc!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_sinhc!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_sinhc(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_asin!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_asin!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_asin(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_acos!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_acos!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_acos(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_atan!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_atan!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_atan(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_acot!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_acot!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_acot(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_asinh!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_asinh!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_asinh(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_acosh!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_acosh!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_acosh(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_atanh!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_atanh!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_atanh(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_acoth!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_acoth!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_acoth(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_erf!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_erf!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_erf(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a    ! src
  type(c_ptr), value :: ctpsa_r                ! dst
"""
"""
    mad_ctpsa_erfc!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_erfc!(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_erfc(ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,ctpsa_x,b,ctpsa_r
bind(C)
import ; implicit none
  complex(c_cpx_t), value, intent(in) :: a, b  ! coefs
  type(c_ptr), value, intent(in) :: ctpsa_x    ! src
  type(c_ptr), value :: ctpsa_r                ! dst=a*x+b
"""
"""
    mad_ctpsa_axpb!(a::Cdouble, ctpsa_x::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `a`
- `ctpsa_x`
- `b`
- `ctpsa_r`
"""
function mad_ctpsa_axpb!(a::Cdouble, ctpsa_x::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axpb(a::Cdouble, ctpsa_x::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,ctpsa_x,b,ctpsa_y,c,ctpsa_r
bind(C)
import ; implicit none
  complex(c_cpx_t), value, intent(in) :: a, b, c     ! coefs
  type(c_ptr), value, intent(in) :: ctpsa_x, ctpsa_y ! src
  type(c_ptr), value :: ctpsa_r                      ! dst=a*x+b*y+c
"""
"""
    mad_ctpsa_axpbypc!(a::Cdouble, ctpsa_x::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_y::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `a`
- `ctpsa_x`
- `b`
- `ctpsa_y`
- `c`
- `ctpsa_r`
"""
function mad_ctpsa_axpbypc!(a::Cdouble, ctpsa_x::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_y::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axpbypc(a::Cdouble, ctpsa_x::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_y::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,ctpsa_x,ctpsa_y,b,ctpsa_r
bind(C)
import ; implicit none
  complex(c_cpx_t), value, intent(in) :: a, b        ! coefs
  type(c_ptr), value, intent(in) :: ctpsa_x, ctpsa_y ! src
  type(c_ptr), value :: ctpsa_r                      ! dst=a*x*y+b
"""
"""
    mad_ctpsa_axypb!(a::Cdouble, ctpsa_x::Ptr{CTPSA{Desc}}, ctpsa_y::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `a`
- `ctpsa_x`
- `ctpsa_y`
- `b`
- `ctpsa_r`
"""
function mad_ctpsa_axypb!(a::Cdouble, ctpsa_x::Ptr{CTPSA{Desc}}, ctpsa_y::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axypb(a::Cdouble, ctpsa_x::Ptr{CTPSA{Desc}}, ctpsa_y::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,ctpsa_x,ctpsa_y,b,ctpsa_z,c,ctpsa_r
bind(C)
import ; implicit none
  complex(c_cpx_t), value, intent(in) :: a, b, c           ! coefs
  type(c_ptr), value, intent(in) :: ctpsa_x, ctpsa_y, ctpsa_z ! src
  type(c_ptr), value :: ctpsa_r                            ! dst=a*x*y+b*z+c
"""
"""
    mad_ctpsa_axypbzpc!(a::Cdouble, ctpsa_x::Ptr{CTPSA{Desc}}, ctpsa_y::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_z::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `a`
- `ctpsa_x`
- `ctpsa_y`
- `b`
- `ctpsa_z`
- `c`
- `ctpsa_r`
"""
function mad_ctpsa_axypbzpc!(a::Cdouble, ctpsa_x::Ptr{CTPSA{Desc}}, ctpsa_y::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_z::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axypbzpc(a::Cdouble, ctpsa_x::Ptr{CTPSA{Desc}}, ctpsa_y::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_z::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,ctpsa_x,ctpsa_y,b,ctpsa_u,ctpsa_v,c,ctpsa_r
bind(C)
import ; implicit none
  complex(c_cpx_t), value, intent(in) :: a, b, c         ! coefs
  type(c_ptr), value, intent(in) :: ctpsa_x, ctpsa_y, ctpsa_u, ctpsa_v ! src
  type(c_ptr), value :: ctpsa_r                          ! dst=a*x*y+b*u*v+c
"""
"""
    mad_ctpsa_axypbvwpc!(a::Cdouble, ctpsa_x::Ptr{CTPSA{Desc}}, ctpsa_y::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_u::Ptr{CTPSA{Desc}}, ctpsa_v::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `a`
- `ctpsa_x`
- `ctpsa_y`
- `b`
- `ctpsa_u`
- `ctpsa_v`
- `c`
- `ctpsa_r`
"""
function mad_ctpsa_axypbvwpc!(a::Cdouble, ctpsa_x::Ptr{CTPSA{Desc}}, ctpsa_y::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_u::Ptr{CTPSA{Desc}}, ctpsa_v::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axypbvwpc(a::Cdouble, ctpsa_x::Ptr{CTPSA{Desc}}, ctpsa_y::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_u::Ptr{CTPSA{Desc}}, ctpsa_v::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,ctpsa_x,b,ctpsa_y,c,ctpsa_z,ctpsa_r
bind(C)
import ; implicit none
  complex(c_cpx_t), value, intent(in) :: a, b, c     ! coefs
  type(c_ptr), value, intent(in) :: ctpsa_x, ctpsa_y, ctpsa_z ! src
  type(c_ptr), value :: ctpsa_r                      ! dst=a*x^2+b*y^2+c*z^2
"""
"""
    mad_ctpsa_ax2pby2pcz2!(a::Cdouble, ctpsa_x::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_y::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_z::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `a`
- `ctpsa_x`
- `b`
- `ctpsa_y`
- `c`
- `ctpsa_z`
- `ctpsa_r`
"""
function mad_ctpsa_ax2pby2pcz2!(a::Cdouble, ctpsa_x::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_y::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_z::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_ax2pby2pcz2(a::Cdouble, ctpsa_x::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_y::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_z::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_x,a,b,c,ctpsa_r
bind(C)
import ; implicit none
  complex(c_cpx_t), value, intent(in) :: a, b, c  ! coefs
  type(c_ptr), value, intent(in) :: ctpsa_x       ! src
  type(c_ptr), value :: ctpsa_r                   ! dst=a*x+sqrt(b+c*x^2)
"""
"""
    mad_ctpsa_axpsqrtbpcx2!(ctpsa_x::Ptr{CTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_x`
- `a`
- `b`
- `c`
- `ctpsa_r`
"""
function mad_ctpsa_axpsqrtbpcx2!(ctpsa_x::Ptr{CTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axpsqrtbpcx2(ctpsa_x::Ptr{CTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_x,a,b,c,ctpsa_r
bind(C)
import ; implicit none
  complex(c_cpx_t), value, intent(in) :: a, b, c  ! coefs
  type(c_ptr), value, intent(in) :: ctpsa_x       ! src
  type(c_ptr), value :: ctpsa_r                   ! dst=log(a*x+sqrt(b+c*x^2))
"""
"""
    mad_ctpsa_logaxpsqrtbpcx2!(ctpsa_x::Ptr{CTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_x`
- `a`
- `b`
- `c`
- `ctpsa_r`
"""
function mad_ctpsa_logaxpsqrtbpcx2!(ctpsa_x::Ptr{CTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_logaxpsqrtbpcx2(ctpsa_x::Ptr{CTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_x,ctpsa_y,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_x, ctpsa_y ! src
  type(c_ptr), value :: ctpsa_r                      ! dst=log(x/y)
"""
"""
    mad_ctpsa_logxdy!(ctpsa_x::Ptr{CTPSA{Desc}}, ctpsa_y::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_x`
- `ctpsa_y`
- `ctpsa_r`
"""
function mad_ctpsa_logxdy!(ctpsa_x::Ptr{CTPSA{Desc}}, ctpsa_y::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_logxdy(ctpsa_x::Ptr{CTPSA{Desc}}, ctpsa_y::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran function:
na,ctpsa_a
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na     ! vectors lengths
  type(c_ptr), intent(in) :: ctpsa_a(*)         ! src
  real(c_num_t) :: mnrm                         ! nrm
"""
"""
    mad_ctpsa_mnrm(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}})::Cdouble

### Input
- `na`
- `ctpsa_a`

### Output
- `mnrm`
"""
function mad_ctpsa_mnrm(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}})::Cdouble
  mnrm = @ccall MAD_TPSA.mad_ctpsa_mnrm(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}})::Cdouble
  return mnrm
end


"""
Original Fortran subroutine:
na,ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na       ! vectors lengths
  type(c_ptr), intent(in) :: ctpsa_a(*)           ! src
  type(c_ptr) :: ctpsa_r(*)                       ! dst
"""
"""
    mad_ctpsa_minv!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `na`
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_minv!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_minv(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
na,ctpsa_a,ctpsa_r,select
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na       ! vectors lengths
  type(c_ptr), intent(in) :: ctpsa_a(*)           ! src
  type(c_ptr) :: ctpsa_r(*)                       ! dst
  integer(c_ssz_t), intent(in) :: select(*)       ! slots to selected
"""
"""
    mad_ctpsa_pminv!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, select::Ptr{Cint})

### Input
- `na`
- `ctpsa_a`
- `ctpsa_r`
- `select`
"""
function mad_ctpsa_pminv!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, select::Ptr{Cint})
  @ccall MAD_TPSA.mad_ctpsa_pminv(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, select::Ptr{Cint})::Cvoid
end


"""
Original Fortran subroutine:
na,ctpsa_a,nb,ctpsa_b,ctpsa_r
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na, nb     ! vectors lengths
  type(c_ptr), intent(in) :: ctpsa_a(*), ctpsa_b(*) ! src
  type(c_ptr) :: ctpsa_r(*)                         ! dst[na]
"""
"""
    mad_ctpsa_compose!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, nb::Cint, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `na`
- `ctpsa_a`
- `nb`
- `ctpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_compose!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, nb::Cint, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_compose(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, nb::Cint, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
na,ctpsa_a,nb,vb,ctpsa_r
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na, nb  ! vectors lengths
  type(c_ptr), intent(in) :: ctpsa_a(*)          ! src
  complex(c_cpx_t), intent(in) :: vb(*)          ! src
  type(c_ptr) :: ctpsa_r(*)                      ! dst[na]
"""
"""
    mad_ctpsa_translate!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, nb::Cint, vb::Ptr{Cdouble}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `na`
- `ctpsa_a`
- `nb`
- `vb`
- `ctpsa_r`
"""
function mad_ctpsa_translate!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, nb::Cint, vb::Ptr{Cdouble}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_translate(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, nb::Cint, vb::Ptr{Cdouble}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
na,ctpsa_a,nb,vb,vr
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na, nb  ! vectors lengths
  type(c_ptr), intent(in) :: ctpsa_a(*)          ! src
  complex(c_cpx_t), intent(in) :: vb(*)          ! src
  complex(c_cpx_t) :: vr(*)                      ! dst[nb]
"""
"""
    mad_ctpsa_eval!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, nb::Cint, vb::Ptr{Cdouble}, vr::Ptr{Cdouble})

### Input
- `na`
- `ctpsa_a`
- `nb`
- `vb`
- `vr`
"""
function mad_ctpsa_eval!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, nb::Cint, vb::Ptr{Cdouble}, vr::Ptr{Cdouble})
  @ccall MAD_TPSA.mad_ctpsa_eval(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, nb::Cint, vb::Ptr{Cdouble}, vr::Ptr{Cdouble})::Cvoid
end


"""
Original Fortran subroutine:
na,ctpsa_a,nr,ctpsa_r,n,t2r_,pb
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na, nr  ! vectors lengths
  type(c_ptr), intent(in) :: ctpsa_a(*)          ! src
  type(c_ptr) :: ctpsa_r(*)                      ! dst
  integer(c_ssz_t), value, intent(in) :: n       ! vector length
  integer(c_idx_t), intent(in) :: t2r_(*)        ! vector of index lookup
  integer(c_int), value, intent(in) :: pb        ! poisson bracket 0,1:fwd,-1:bwd
"""
"""
    mad_ctpsa_mconv!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, nr::Cint, ctpsa_r::Ptr{CTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)

### Input
- `na`
- `ctpsa_a`
- `nr`
- `ctpsa_r`
- `n`
- `t2r_`
- `pb`
"""
function mad_ctpsa_mconv!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, nr::Cint, ctpsa_r::Ptr{CTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)
  @ccall MAD_TPSA.mad_ctpsa_mconv(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, nr::Cint, ctpsa_r::Ptr{CTPSA{Desc}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)::Cvoid
end


"""
Original Fortran subroutine:
na,ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na      ! vectors length
  type(c_ptr), value, intent(in) :: ctpsa_a      ! src
  type(c_ptr), intent(out) :: ctpsa_r(*)         ! dst
"""
"""
    mad_ctpsa_vec2fld!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `na`
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_vec2fld!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_vec2fld(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
na,ctpsa_a,ctpsa_r
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na      ! vectors length
  type(c_ptr), intent(in) :: ctpsa_a(*)          ! src
  type(c_ptr), value :: ctpsa_r                  ! dst
"""
"""
    mad_ctpsa_fld2vec!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `na`
- `ctpsa_a`
- `ctpsa_r`
"""
function mad_ctpsa_fld2vec!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_fld2vec(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
na,ctpsa_a,ctpsa_b,ctpsa_r
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na     ! vectors length
  type(c_ptr), intent(in) :: ctpsa_a(*)          ! src
  type(c_ptr), value, intent(in) :: ctpsa_b      ! src
  type(c_ptr), value :: ctpsa_r                  ! dst
"""
"""
    mad_ctpsa_fgrad!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `na`
- `ctpsa_a`
- `ctpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_fgrad!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_fgrad(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
na,ctpsa_a,ctpsa_b,ctpsa_r
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na         ! vectors length
  type(c_ptr), intent(in) :: ctpsa_a(*), ctpsa_b(*) ! src
  type(c_ptr), intent(out) :: ctpsa_r(*)            ! dst[na]
"""
"""
    mad_ctpsa_liebra!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `na`
- `ctpsa_a`
- `ctpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_liebra!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_liebra(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
na,ctpsa_a,ctpsa_b,ctpsa_r
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na         ! vectors length
  type(c_ptr), intent(in) :: ctpsa_a(*), ctpsa_b(*) ! src
  type(c_ptr), intent(out) :: ctpsa_r(*)            ! dst[na]
"""
"""
    mad_ctpsa_exppb!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `na`
- `ctpsa_a`
- `ctpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_exppb!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_exppb(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
na,ctpsa_a,ctpsa_b,ctpsa_r
bind(C)
import ; implicit none
  integer(c_ssz_t), value, intent(in) :: na        ! vectors length
  type(c_ptr), intent(in) :: ctpsa_a(*)            ! src
  type(c_ptr), intent(in), optional :: ctpsa_b(*)  ! src
  type(c_ptr), intent(out) :: ctpsa_r(*)           ! dst[na]
"""
"""
    mad_ctpsa_logpb!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `na`
- `ctpsa_a`
- `ctpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_logpb!(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_logpb(na::Cint, ctpsa_a::Ptr{CTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa,name_,eps_,nohdr_,stream_
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa     ! src
  character(c_char), intent(in) :: name_(*)   ! tpsa name (nul term. C str)
  real(c_num_t), value, intent(in) :: eps_    ! display precision, e.g. 1d-12
  integer(c_int), value, intent(in) :: nohdr_ ! discard header if not zero
  type(c_ptr), value :: stream_               ! dst=c_null_ptr => stdio
"""
"""
    mad_ctpsa_print!(ctpsa::Ptr{CTPSA{Desc}}, name_::Cstring, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid})

### Input
- `ctpsa`
- `name_`
- `eps_`
- `nohdr_`
- `stream_`
"""
function mad_ctpsa_print!(ctpsa::Ptr{CTPSA{Desc}}, name_::Cstring, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_ctpsa_print(ctpsa::Ptr{CTPSA{Desc}}, name_::Cstring, eps_::Cdouble, nohdr_::Cint, stream_::Ptr{Cvoid})::Cvoid
end


"""
Original Fortran function:
stream_
bind(C)
import ; implicit none
  type(c_ptr) :: ctpsa                        ! tpsa to read
  type(c_ptr), value, intent(in) :: stream_   ! src=c_null_ptr => stdin
"""
"""
    mad_ctpsa_scan(stream_::Ptr{Cvoid})::Ptr{CTPSA{Desc}}

### Input
- `stream_`

### Output
- `ctpsa`
"""
function mad_ctpsa_scan(stream_::Ptr{Cvoid})::Ptr{CTPSA{Desc}}
  ctpsa = @ccall MAD_TPSA.mad_ctpsa_scan(stream_::Ptr{Cvoid})::Ptr{CTPSA{Desc}}
  return ctpsa
end


"""
Original Fortran function:
kind_,name_,stream_
bind(C)
import ; implicit none
  type(c_ptr) :: desc                         ! descriptor from header
  integer(c_int), optional, intent(out) :: kind_! tpsa kind (0 real, 1 complex)
  character(c_char), optional, intent(out) :: name_(*) ! tpsa name (nul term. C str)
  type(c_ptr), value, intent(in) :: stream_   ! src=c_null_ptr => stdin
"""
"""
    mad_ctpsa_scan_hdr(kind_::Cint, name_::Cstring, stream_::Ptr{Cvoid})::Ptr{Desc{RTPSA,CTPSA}}

### Input
- `kind_`
- `name_`
- `stream_`

### Output
- `desc`
"""
function mad_ctpsa_scan_hdr(kind_::Cint, name_::Cstring, stream_::Ptr{Cvoid})::Ptr{Desc{RTPSA,CTPSA}}
  desc = @ccall MAD_TPSA.mad_ctpsa_scan_hdr(kind_::Cint, name_::Cstring, stream_::Ptr{Cvoid})::Ptr{Desc{RTPSA,CTPSA}}
  return desc
end


"""
Original Fortran subroutine:
ctpsa,stream_
bind(C)
import ; implicit none
  type(c_ptr), value :: ctpsa                 ! tpsa to read
  type(c_ptr), value, intent(in) :: stream_   ! src=c_null_ptr => stdin
"""
"""
    mad_ctpsa_scan_coef!(ctpsa::Ptr{CTPSA{Desc}}, stream_::Ptr{Cvoid})

### Input
- `ctpsa`
- `stream_`
"""
function mad_ctpsa_scan_coef!(ctpsa::Ptr{CTPSA{Desc}}, stream_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_ctpsa_scan_coef(ctpsa::Ptr{CTPSA{Desc}}, stream_::Ptr{Cvoid})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa,name_,fnam_,line_,stream_
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa    ! src
  character(c_char), intent(in) :: name_(*)  ! tpsa name (nul term. C str)
  character(c_char), intent(in) :: fnam_(*)  ! filename  (nul term. C str)
  integer(c_int), value, intent(in) :: line_ ! line number or 0
  type(c_ptr), value :: stream_              ! dst=c_null_ptr => stdio
"""
"""
    mad_ctpsa_debug!(ctpsa::Ptr{CTPSA{Desc}}, name_::Cstring, fnam_::Cstring, line_::Cint, stream_::Ptr{Cvoid})

### Input
- `ctpsa`
- `name_`
- `fnam_`
- `line_`
- `stream_`
"""
function mad_ctpsa_debug!(ctpsa::Ptr{CTPSA{Desc}}, name_::Cstring, fnam_::Cstring, line_::Cint, stream_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_ctpsa_debug(ctpsa::Ptr{CTPSA{Desc}}, name_::Cstring, fnam_::Cstring, line_::Cint, stream_::Ptr{Cvoid})::Cvoid
end





















# ------------------------------------------------------------------------------------------
# HIGHER LEVEL FUNCTIONS BELOW:

# ------------------------------------------------------------------------------------------
# DESCRIPTOR FUNCTIONS:

"""
    new_desc(nv::Integer, mo::Integer)::Ptr{Desc{RTPSA,CTPSA}}

  Creates a TPSA descriptor with the specified number of variables and maximum order. 
  The number of parameters is set to 0. 

  Input:
    nv -- Number of variables 
    mo -- Maximum order

  Output:
    A pointer to the TPSA descriptor created, with:
    Desc.nv = nv 
    Desc.mo = mo
"""
function new_desc(nv::Integer, mo::Integer)::Ptr{Desc{RTPSA,CTPSA}}
  d = @ccall MAD_TPSA.mad_desc_newv(nv::Cint,mo::Cuchar)::Ptr{Desc{RTPSA,CTPSA}}
  return d
end


"""
    new_desc(nv::Integer, mo::Integer, np_::Integer, po_::Integer)::Ptr{Desc{RTPSA,CTPSA}}

  Creates a TPSA descriptor with the specifed number of variables, maximum order,
  number of parameters, and parameter order.
  
  Input:
    nv -- Number of variables 
    mo -- Maximum order
    np_ -- Number of parameters
    po_ -- Parameter order

  Output:
    A pointer to the TPSA descriptor created, with:
    Desc.nv = nv 
    Desc.mo = mo
    Desc.np = np_
    Desc.po = po_
"""
function new_desc(nv::Integer, mo::Integer, np_::Integer, po_::Integer)::Ptr{Desc{RTPSA,CTPSA}}
  d = @ccall MAD_TPSA.mad_desc_newvp(nv::Cint, mo::Cuchar, np_::Cint, po_::Cuchar)::Ptr{Desc{RTPSA,CTPSA}}
  return d
end

"""
  new_desc(nv::Integer, mo::Integer, np_::Integer, po_::Integer, no_::Vector{<:Integer})::Ptr{Desc{RTPSA,CTPSA}}

Creates a TPSA descriptor with the specifed number of variables, maximum order,
number of parameters, parameter order, and individual variable/parameter orders 
specified in no. The first nv entries in no correspond to the variables' orders 
and the next np entries correspond the parameters' orders.

Input:
  nv -- Number of variables 
  mo -- Maximum order
  np_ -- Number of parameters
  po_ -- Parameter order
  no_ -- Vector of variable and parameter orders, in order. Must be length nv+np_ (FIGURE OUT order). 

Output:
  A pointer to the TPSA descriptor created, with:
  Desc.nv = nv 
  Desc.mo = mo
  Desc.np = np_
  Desc.po = po_
  Desc.no = no_
"""
function new_desc(nv::Integer, mo::Integer, np_::Integer, po_::Integer, no_::Vector{<:Integer})::Ptr{Desc{RTPSA,CTPSA}}
  no = convert(Vector{UInt8}, no_)
  d = @ccall MAD_TPSA.mad_desc_newvpo(nv::Cint, mo::Cuchar, np_::Cint, po_::Cuchar, no::Ptr{Cuchar})::Ptr{Desc{RTPSA,CTPSA}}
  return d
end

"""
    del!(d::Ptr{Desc{RTPSA,CTPSA}})

  Destroys the descriptor d.
  
  Input:
    d -- Descriptor to destroy

  Output:
    None.
"""
function del!(d::Ptr{Desc{RTPSA,CTPSA}})
  @ccall MAD_TPSA.mad_desc_del(d::Ptr{Desc{RTPSA,CTPSA}})::Cvoid
end

"""
    cleanup()

  Destroys all descriptors.
"""
function cleanup()
  @ccall MAD_TPSA.mad_desc_del(0::Cint)::Cvoid
end


# ------------------------------------------------------------------------------------------
# TPSA FUNCTIONS

"""
    new_TPSA(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Integer)::Ptr{RTPSA{Desc}}

  Creates a real TPSA defined by the specified descriptor and maximum order.
  If mad_tpsa_default is passed for mo, the mo defined in the descriptor is used.
  
  Input:
    d  -- Descriptor for TPSA
    mo -- Maximum order of TPSA

  Output:
    A pointer to the real TPSA created, with:
    RTPSA.d   = d 
    RTPSA.mo  = mo
    and all other members initialized to 0.
"""
function new_TPSA(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Integer)::Ptr{RTPSA{Desc}}
  @ccall MAD_TPSA.mad_tpsa_newd(d::Ptr{Desc{RTPSA,CTPSA}},mo::Cuchar)::Ptr{RTPSA{Desc}}
end

"""
    new_TPSA(t::Ptr{RTPSA{Desc}}, mo::Integer)::Ptr{RTPSA{Desc}}

  Creates a real TPSA copy of the inputted TPSA, with maximum order specified by mo.
  If mad_tpsa_same is passed for mo, the mo currently in t is used for the created TPSA.
  
  Input:
    t  -- Pointer to real TPSA to copy
    mo -- Maximum order of new TPSA

  Output:
    A pointer to the real TPSA copy created with maximum order mo.
"""
function new_TPSA(t::Ptr{RTPSA{Desc}}, mo::Integer)::Ptr{RTPSA{Desc}}
  @ccall MAD_TPSA.mad_tpsa_new(t::Ptr{RTPSA{Desc}}, mo::Cuchar)::Ptr{RTPSA{Desc}}
end

"""
    set_TPSA(t::Ptr{RTPSA{Desc}}, i::Integer, n::Integer, v::Vector{<:Float64})

  Sets the coefficients of the TPSA in indices i:i+n to those in v. That is,
  t.coefs[i:i+n] = v. The coefficients are sorted by order. v must be length n.
  
  Input:
    t -- Pointer to real TPSA
    i -- Starting index of coefficients in TPSA to set
    n -- Number of coefficients to set in TPSA
    v -- Vector values to set coefficients in TPSA. 

  Output:
    Sets the coefficients in the TPSA t accordingly. 
"""
function set_TPSA!(t::Ptr{RTPSA{Desc}}, i::Integer, n::Integer, v::Vector{<:Float64})
  @ccall MAD_TPSA.mad_tpsa_setv(t::Ptr{RTPSA{Desc}}, i::Cint, n::Cuint, v::Ptr{Cdouble})::Cvoid
end


# ONly 1 function for setting needed
function set_TPSA!(t::Ptr{RTPSA{Desc}}, a::Float64, b::Float64)
  @ccall MAD_TPSA.mad_tpsa_set0(t::Ptr{RTPSA{Desc}}, a::Cdouble, b::Cdouble)::Cvoid
end

"""

    print_TPSA(t::Ptr{RTPSA{Desc}}, name::AbstractString, eps_::Real = 0.0, nohdr_::Bool = false, filename::AbstractString = "", mode::AbstractString="w+")

  Prints the TPSA coefficients to stdout with precision eps_. If nohdr_ is not zero, 
  the header is not printed. 
"""
function print_TPSA(t::Ptr{RTPSA{Desc}}, name::AbstractString, eps_::Real = 0.0, nohdr_::Bool = false, filename::AbstractString = "", mode::AbstractString="w+")
  if filename=="" # print to stdout
    @ccall MAD_TPSA.mad_tpsa_print(t::Ptr{RTPSA{Desc}}, name::Cstring, eps_::Cdouble,nohdr_::Cint,0::Cint)::Cvoid
  else
    fp = @ccall fopen(filename::Cstring, mode::Cstring)::Ptr{Cvoid}
    @ccall MAD_TPSA.mad_tpsa_print(t::Ptr{RTPSA{Desc}}, name::Cstring, eps_::Cdouble,nohdr_::Cint,fp::Ptr{Cvoid})::Cvoid
    @ccall fclose(fp::Ptr{Cvoid})::Cvoid
  end
end

"""
    set_name(t::Ptr{RTPSA{Desc}}, nam::AbstractString)

  Set the name of a TPSA.

  Input:
    t   -- Source TPSA
    nam -- String of new name for TPSA

  Output:
    Sets the TPSA name (RTPSA.nam).
"""
function set_name!(t::Ptr{RTPSA{Desc}}, nam::AbstractString)
  @ccall MAD_TPSA.mad_tpsa_setnam(t::Ptr{RTPSA{Desc}}, nam::Cstring)::Cvoid
end

"""
NEEDS DOCUMENTATION
"""
function desc_maxlen(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Integer)::Int64
  ret = @ccall MAD_TPSA.mad_desc_maxlen(d::Ptr{Desc{RTPSA,CTPSA}}, mo::Cuchar)::Int64
  return ret
end


"""
    sin!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

  Sets c = sin(a) in place. Aliasing is supported (i.e. a == c).

  Input:
    a -- Source TPSA 
    c -- Destination TPSA

  Output:
    Sets the TPSA c = sin(a).
"""
function sin!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_sin(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end

"""
    asin!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

  Sets c = asin(a) in place. Aliasing is supported (i.e. a == c).

  Input:
    a -- Source TPSA 
    c -- Destination TPSA

  Output:
    Sets the TPSA c = asin(a).
"""
function asin!(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall "libmad_tpsa".mad_tpsa_asin(a::Ptr{RTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid 
end

"""
    del!(t::Ptr{RTPSA{Desc}})

  Destroys the TPSA at t.

  Input:
    t -- TPSA to destroy

  Output:
    None.
"""
function del!(t::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_tpsa_del(t::Ptr{RTPSA{Desc}})::Cvoid
end



