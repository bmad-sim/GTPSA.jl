"""
    mad_ctpsa_mnrm(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}})::Cdouble

???

### Input
- `na`
- `ma`

### Output
- `nrm`
"""
function mad_ctpsa_mnrm(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}})::Cdouble
  nrm = @ccall MAD_TPSA.mad_ctpsa_mnrm(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}})::Cdouble
  return nrm
end


"""
    mad_ctpsa_minv!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})

???

### Input
- `na`
- `ma`
- `mc`
"""
function mad_ctpsa_minv!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})
  @ccall MAD_TPSA.mad_ctpsa_minv(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})::Cvoid
end


"""
    mad_ctpsa_pminv!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}}, select::Ptr{Cint})

???

### Input
- `na`
- `ma`
- `mc`
- `select`
"""
function mad_ctpsa_pminv!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}}, select::Ptr{Cint})
  @ccall MAD_TPSA.mad_ctpsa_pminv(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}}, select::Ptr{Cint})::Cvoid
end


"""
    mad_ctpsa_compose!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})

???

### Input
- `na`
- `ma`
- `nb`
- `mb`
- `mc`
"""
function mad_ctpsa_compose!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})
  @ccall MAD_TPSA.mad_ctpsa_compose(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, mb::Ptr{Ptr{CTPSA{Desc}}}, mc::Ptr{Ptr{CTPSA{Desc}}})::Cvoid
end


"""
    mad_ctpsa_translate!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, tb::Ptr{ComplexF64}, mc::Ptr{CTPSA{Desc}})

???

### Input
- `na`
- `ma`
- `nb`
- `tb`
- `mc`
"""
function mad_ctpsa_translate!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, tb::Ptr{ComplexF64}, mc::Ptr{CTPSA{Desc}})
  @ccall MAD_TPSA.mad_ctpsa_translate(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, tb::Ptr{ComplexF64}, mc::Ptr{CTPSA{Desc}})::Cvoid
end


"""
    mad_ctpsa_eval!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, tb::Ptr{ComplexF64}, tc::Ptr{ComplexF64})

???

### Input
- `na`
- `ma`
- `nb`
- `tb`
- `tc`
"""
function mad_ctpsa_eval!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, tb::Ptr{ComplexF64}, tc::Ptr{ComplexF64})
  @ccall MAD_TPSA.mad_ctpsa_eval(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nb::Cint, tb::Ptr{ComplexF64}, tc::Ptr{ComplexF64})::Cvoid
end


"""
    mad_ctpsa_mconv!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nc::Cint, mc::Ptr{Ptr{CTPSA{Desc}}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)

???

### Input
- `na`
- `ma`
- `nc`
- `mc`
- `n`
- `t2r_`
- `pb`
"""
function mad_ctpsa_mconv!(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nc::Cint, mc::Ptr{Ptr{CTPSA{Desc}}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)
  @ccall MAD_TPSA.mad_ctpsa_mconv(na::Cint, ma::Ptr{Ptr{CTPSA{Desc}}}, nc::Cint, mc::Ptr{Ptr{CTPSA{Desc}}}, n::Cint, t2r_::Ptr{Cint}, pb::Cint)::Cvoid
end