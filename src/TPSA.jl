module TPSA
include("Monomial.jl")
include("Descriptor.jl")
include("RealTPSA.jl")
include("ComplexTPSA.jl")
using .Descriptor
using .RealTPSA
using .ComplexTPSA
using Printf
#import Base: sin
export Desc, RTPSA, CTPSA, new_desc,new_TPSA,set_TPSA!,print_TPSA,sin!,del!,asin!,set_name!,cleanup,desc_maxlen,MAD_TPSA_DEFAULT, MAD_TPSA_SAME, mad_desc_newv

const MAD_TPSA = :("libmad_tpsa")
const MAD_TPSA_DEFAULT::Cuchar = 255
const MAD_TPSA_SAME::Cuchar = 254

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



# ------------------------------------------------------------------------------------------
# mad_ctpsa:


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
ctpsa,c
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa   ! src
  type(c_ptr), value :: c              ! dst=real(src)
"""
"""
    mad_ctpsa_real!(ctpsa::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

### Input
- `ctpsa`
- `c`
"""
function mad_ctpsa_real!(ctpsa::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_real(ctpsa::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa,c
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa   ! src
  type(c_ptr), value :: c              ! dst=imag(src)
"""
"""
    mad_ctpsa_imag!(ctpsa::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}})

### Input
- `ctpsa`
- `c`
"""
function mad_ctpsa_imag!(ctpsa::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_imag(ctpsa::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}})::Cvoid
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
ctpsa_a,b,eps
bind(C)
import ; implicit none
  logical(c_bool) :: ret                   ! true or false
  type(c_ptr), value, intent(in) :: ctpsa_a, b
  real(c_num_t), value, intent(in) :: eps  ! tolerance during comparison
"""
"""
    mad_ctpsa_equt(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, eps::Cdouble)::Cuchar

### Input
- `ctpsa_a`
- `b`
- `eps`

### Output
- `ret`
"""
function mad_ctpsa_equt(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, eps::Cdouble)::Cuchar
  ret = @ccall MAD_TPSA.mad_ctpsa_equt(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, eps::Cdouble)::Cuchar
  return ret
end


"""
Original Fortran subroutine:
ctpsa_a,b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a, b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                      ! dst
"""
"""
    mad_ctpsa_addt!(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `b`
- `ctpsa_r`
"""
function mad_ctpsa_addt!(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_addt(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a, b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                      ! dst
"""
"""
    mad_ctpsa_subt!(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `b`
- `ctpsa_r`
"""
function mad_ctpsa_subt!(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_subt(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,ctpsa_b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: a, ctpsa_b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                      ! dst
"""
"""
    mad_ctpsa_tsub!(a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `a`
- `ctpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_tsub!(a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tsub(a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a, b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                      ! dst
"""
"""
    mad_ctpsa_dift!(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `b`
- `ctpsa_r`
"""
function mad_ctpsa_dift!(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_dift(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,ctpsa_b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: a, ctpsa_b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                      ! dst
"""
"""
    mad_ctpsa_tdif!(a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `a`
- `ctpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_tdif!(a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tdif(a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a, b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                      ! dst
"""
"""
    mad_ctpsa_mult!(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `b`
- `ctpsa_r`
"""
function mad_ctpsa_mult!(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_mult(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a, b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                      ! dst
"""
"""
    mad_ctpsa_divt!(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `b`
- `ctpsa_r`
"""
function mad_ctpsa_divt!(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_divt(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,ctpsa_b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: a, ctpsa_b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                      ! dst
"""
"""
    mad_ctpsa_tdiv!(a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `a`
- `ctpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_tdiv!(a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tdiv(a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a, b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                      ! dst
"""
"""
    mad_ctpsa_powt!(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `ctpsa_a`
- `b`
- `ctpsa_r`
"""
function mad_ctpsa_powt!(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_powt(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,ctpsa_b,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: a, ctpsa_b  ! lhs, rhs
  type(c_ptr), value :: ctpsa_r                      ! dst
"""
"""
    mad_ctpsa_tpow!(a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `a`
- `ctpsa_b`
- `ctpsa_r`
"""
function mad_ctpsa_tpow!(a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_tpow(a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
ctpsa_a,b,ctpsa_r,nv
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: ctpsa_a, b ! src
  type(c_ptr), value :: ctpsa_r                     ! dst
  integer(c_int), value, intent(in) :: nv        ! #variables (desc%nv if 0)
"""
"""
    mad_ctpsa_poisbrat!(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, nv::Cint)

### Input
- `ctpsa_a`
- `b`
- `ctpsa_r`
- `nv`
"""
function mad_ctpsa_poisbrat!(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, nv::Cint)
  @ccall MAD_TPSA.mad_ctpsa_poisbrat(ctpsa_a::Ptr{CTPSA{Desc}}, b::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, nv::Cint)::Cvoid
end


"""
Original Fortran subroutine:
a,ctpsa_b,ctpsa_r,nv
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: a, ctpsa_b ! src
  type(c_ptr), value :: ctpsa_r                     ! dst
  integer(c_int), value, intent(in) :: nv        ! #variables (desc%nv if 0)
"""
"""
    mad_ctpsa_tpoisbra!(a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, nv::Cint)

### Input
- `a`
- `ctpsa_b`
- `ctpsa_r`
- `nv`
"""
function mad_ctpsa_tpoisbra!(a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, nv::Cint)
  @ccall MAD_TPSA.mad_ctpsa_tpoisbra(a::Ptr{RTPSA{Desc}}, ctpsa_b::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}}, nv::Cint)::Cvoid
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
a,cx,b,ctpsa_r
bind(C)
import ; implicit none
  complex(c_cpx_t), value, intent(in) :: a, b  ! coefs
  type(c_ptr), value, intent(in) :: cx    ! src
  type(c_ptr), value :: ctpsa_r                ! dst=a*x+b
"""
"""
    mad_ctpsa_axpb!(a::Cdouble, cx::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `a`
- `cx`
- `b`
- `ctpsa_r`
"""
function mad_ctpsa_axpb!(a::Cdouble, cx::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axpb(a::Cdouble, cx::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,cx,b,cy,c,ctpsa_r
bind(C)
import ; implicit none
  complex(c_cpx_t), value, intent(in) :: a, b, c     ! coefs
  type(c_ptr), value, intent(in) :: cx, cy ! src
  type(c_ptr), value :: ctpsa_r                      ! dst=a*x+b*y+c
"""
"""
    mad_ctpsa_axpbypc!(a::Cdouble, cx::Ptr{CTPSA{Desc}}, b::Cdouble, cy::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `a`
- `cx`
- `b`
- `cy`
- `c`
- `ctpsa_r`
"""
function mad_ctpsa_axpbypc!(a::Cdouble, cx::Ptr{CTPSA{Desc}}, b::Cdouble, cy::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axpbypc(a::Cdouble, cx::Ptr{CTPSA{Desc}}, b::Cdouble, cy::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,cx,cy,b,ctpsa_r
bind(C)
import ; implicit none
  complex(c_cpx_t), value, intent(in) :: a, b        ! coefs
  type(c_ptr), value, intent(in) :: cx, cy ! src
  type(c_ptr), value :: ctpsa_r                      ! dst=a*x*y+b
"""
"""
    mad_ctpsa_axypb!(a::Cdouble, cx::Ptr{CTPSA{Desc}}, cy::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `a`
- `cx`
- `cy`
- `b`
- `ctpsa_r`
"""
function mad_ctpsa_axypb!(a::Cdouble, cx::Ptr{CTPSA{Desc}}, cy::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axypb(a::Cdouble, cx::Ptr{CTPSA{Desc}}, cy::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,cx,cy,b,ctpsa_z,c,ctpsa_r
bind(C)
import ; implicit none
  complex(c_cpx_t), value, intent(in) :: a, b, c           ! coefs
  type(c_ptr), value, intent(in) :: cx, cy, ctpsa_z ! src
  type(c_ptr), value :: ctpsa_r                            ! dst=a*x*y+b*z+c
"""
"""
    mad_ctpsa_axypbzpc!(a::Cdouble, cx::Ptr{CTPSA{Desc}}, cy::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_z::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `a`
- `cx`
- `cy`
- `b`
- `ctpsa_z`
- `c`
- `ctpsa_r`
"""
function mad_ctpsa_axypbzpc!(a::Cdouble, cx::Ptr{CTPSA{Desc}}, cy::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_z::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axypbzpc(a::Cdouble, cx::Ptr{CTPSA{Desc}}, cy::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_z::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,cx,cy,b,ctpsa_u,ctpsa_v,c,ctpsa_r
bind(C)
import ; implicit none
  complex(c_cpx_t), value, intent(in) :: a, b, c         ! coefs
  type(c_ptr), value, intent(in) :: cx, cy, ctpsa_u, ctpsa_v ! src
  type(c_ptr), value :: ctpsa_r                          ! dst=a*x*y+b*u*v+c
"""
"""
    mad_ctpsa_axypbvwpc!(a::Cdouble, cx::Ptr{CTPSA{Desc}}, cy::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_u::Ptr{CTPSA{Desc}}, ctpsa_v::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `a`
- `cx`
- `cy`
- `b`
- `ctpsa_u`
- `ctpsa_v`
- `c`
- `ctpsa_r`
"""
function mad_ctpsa_axypbvwpc!(a::Cdouble, cx::Ptr{CTPSA{Desc}}, cy::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_u::Ptr{CTPSA{Desc}}, ctpsa_v::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axypbvwpc(a::Cdouble, cx::Ptr{CTPSA{Desc}}, cy::Ptr{CTPSA{Desc}}, b::Cdouble, ctpsa_u::Ptr{CTPSA{Desc}}, ctpsa_v::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
a,cx,b,cy,c,ctpsa_z,ctpsa_r
bind(C)
import ; implicit none
  complex(c_cpx_t), value, intent(in) :: a, b, c     ! coefs
  type(c_ptr), value, intent(in) :: cx, cy, ctpsa_z ! src
  type(c_ptr), value :: ctpsa_r                      ! dst=a*x^2+b*y^2+c*z^2
"""
"""
    mad_ctpsa_ax2pby2pcz2!(a::Cdouble, cx::Ptr{CTPSA{Desc}}, b::Cdouble, cy::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_z::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `a`
- `cx`
- `b`
- `cy`
- `c`
- `ctpsa_z`
- `ctpsa_r`
"""
function mad_ctpsa_ax2pby2pcz2!(a::Cdouble, cx::Ptr{CTPSA{Desc}}, b::Cdouble, cy::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_z::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_ax2pby2pcz2(a::Cdouble, cx::Ptr{CTPSA{Desc}}, b::Cdouble, cy::Ptr{CTPSA{Desc}}, c::Ptr{RTPSA{Desc}}, ctpsa_z::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
cx,a,b,c,ctpsa_r
bind(C)
import ; implicit none
  complex(c_cpx_t), value, intent(in) :: a, b, c  ! coefs
  type(c_ptr), value, intent(in) :: cx       ! src
  type(c_ptr), value :: ctpsa_r                   ! dst=a*x+sqrt(b+c*x^2)
"""
"""
    mad_ctpsa_axpsqrtbpcx2!(cx::Ptr{CTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `cx`
- `a`
- `b`
- `c`
- `ctpsa_r`
"""
function mad_ctpsa_axpsqrtbpcx2!(cx::Ptr{CTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_axpsqrtbpcx2(cx::Ptr{CTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
cx,a,b,c,ctpsa_r
bind(C)
import ; implicit none
  complex(c_cpx_t), value, intent(in) :: a, b, c  ! coefs
  type(c_ptr), value, intent(in) :: cx       ! src
  type(c_ptr), value :: ctpsa_r                   ! dst=log(a*x+sqrt(b+c*x^2))
"""
"""
    mad_ctpsa_logaxpsqrtbpcx2!(cx::Ptr{CTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `cx`
- `a`
- `b`
- `c`
- `ctpsa_r`
"""
function mad_ctpsa_logaxpsqrtbpcx2!(cx::Ptr{CTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_logaxpsqrtbpcx2(cx::Ptr{CTPSA{Desc}}, a::Cdouble, b::Cdouble, c::Ptr{RTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
end


"""
Original Fortran subroutine:
cx,cy,ctpsa_r
bind(C)
import ; implicit none
  type(c_ptr), value, intent(in) :: cx, cy ! src
  type(c_ptr), value :: ctpsa_r                      ! dst=log(x/y)
"""
"""
    mad_ctpsa_logxdy!(cx::Ptr{CTPSA{Desc}}, cy::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})

### Input
- `cx`
- `cy`
- `ctpsa_r`
"""
function mad_ctpsa_logxdy!(cx::Ptr{CTPSA{Desc}}, cy::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_logxdy(cx::Ptr{CTPSA{Desc}}, cy::Ptr{CTPSA{Desc}}, ctpsa_r::Ptr{CTPSA{Desc}})::Cvoid
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



end