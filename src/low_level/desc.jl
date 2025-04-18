"""
    `Desc`

This is a 1-to-1 struct for the C definition `desc` (descriptor) in GTPSA. Descriptors include all 
information about the TPSA, including the number of variables/parameters and their orders, lookup tables for 
the monomials, monomial indexing function, and pre-allocated permanent temporaries for fast evaluation.

### Fields
- `id::Cint`                   -- Index in list of registered descriptors
- `nn::Cint`                   -- Number of variables + number of parameters, `nn = nv+np <= 100000`
- `nv::Cint`                   -- Number of variables
- `np::Cint`                   -- Number of parameters
- `mo::Cuchar`                 -- Max order of both variables AND parameters
- `po::Cuchar`                 -- Max order of parameters
- `sh::Cuchar`                 -- shared with id or -1
- `no::Ptr{Cuchar}`            -- Array of orders of each variable (first `nv` entries) and parameters (last `np` entries), length `nn`. Note: In C this is `const`

- `uno::Cint`                  -- User provided array of orders of each variable/parameter (with `mad_desc_newvpo`)
- `nth::Cint`                  -- Max number of threads or 1
- `nc::Cuint`                  -- Number of coefficients (max length of TPSA)
- `pmul::Cuint`                -- Threshold for parallel mult (0 = disable)
- `pcomp::Cuint`               -- Threshold for parallel compose (0 = disable)

- `shared::Ptr{Cint}`          -- counter of shared desc (all tables below except prms)
- `monos::Ptr{Cuchar}`         -- 'Matrix' storing the monomials (sorted by variable)
- `ords::Ptr{Cuchar}`          -- Order of each monomial of `To`
- `prms::Ptr{Cuchar}`          -- Order of parameters in each monomial of `To` (zero = no parameters)
- `To::Ptr{Ptr{Cuchar}}`       -- Table by orders - pointers to monomials, sorted by order
- `Tv::Ptr{Ptr{Cuchar}}`       -- Table by vars - pointers to monomials, sorted by variable
- `ocs::Ptr{Ptr{Cuchar}}`      -- `ocs[t,i]` -> `o` in mul, compute `o` on thread `t 3 <= o <= mo` aterminated with 0

- `ord2idx::Ptr{Cint}`         -- Order to polynomial start index in `To` (i.e. in TPSA `coef`)
- `tv2to::Ptr{Cint}`           -- Lookup `tv`->`to`
- `to2tv::Ptr{Cint}`           -- Lookup `to`->`tv`
- `H::Ptr{Cint}`               -- Indexing matrix in `Tv`
- `L::Ptr{Ptr{Cint}}`          -- Multiplication indexes `L[oa,ob]`->`L_ord` `L_ord[ia,ib]`->`ic`
- `L_idx::Ptr{Ptr{Ptr{Cint}}}` -- `L_idx[oa,ob]`->`[start] [split] [end]` idxs in `L`

- `size::Culonglong`           -- Bytes used by `desc`. `Unsigned Long Int`: In 32 bit system is `Int32` but 64 bit is `Int64`. Using `Culonglong` assuming 64 bit

- `dst_n::Cdouble`             -- density count
- `dst_mu::Cdouble`            -- density mean
- `dst_var::Cdouble`           -- density variance

- `t::Ptr{Ptr{Cvoid}}`         -- Temporary array contains 8 pointers to `TPS{Float64}`s already initialized
- `ct::Ptr{Ptr{Cvoid}}`        -- Temporary array contains 8 pointers to `TPS{ComplexF64}`s already initialized
- `ti::Ptr{Cint}`              -- idx of tmp used by each thread (length = # threads)
- `cti::Ptr{Cint}`             -- idx of tmp used by each thread (length = # threads)                                                                                              
"""
struct Desc
  id::Cint                   
  nn::Cint                   
  nv::Cint                   
  np::Cint                   
  mo::Cuchar                 
  po::Cuchar     
  sh::Cuchar            
  no::Ptr{Cuchar}            

  uno::Cint                  
  nth::Cint                  
  nc::Cuint     
  pmul::Cuint
  pcomp::Cuint              

  shared::Ptr{Cint}
  monos::Ptr{Cuchar}         
  ords::Ptr{Cuchar}          
  prms::Ptr{Cuchar}          
  To::Ptr{Ptr{Cuchar}}       
  Tv::Ptr{Ptr{Cuchar}}       
  ocs::Ptr{Ptr{Cuchar}}      

  ord2idx::Ptr{Cint}         
  tv2to::Ptr{Cint}           
  to2tv::Ptr{Cint}           
  H::Ptr{Cint}               
  L::Ptr{Ptr{Cint}}          
  L_idx::Ptr{Ptr{Ptr{Cint}}} 

  size::Culonglong           

  dst_n::Cdouble
  dst_mu::Cdouble
  dst_var::Cdouble

  t::Ptr{Ptr{Cvoid}}         
  ct::Ptr{Ptr{Cvoid}}        
  ti::Ptr{Cint}              
  cti::Ptr{Cint}             
end

"""
    mad_desc_newv(nv::Cint, mo::Cuchar)::Ptr{Desc}

Creates a TPSA descriptor with the specified number of variables and maximum order. 
The number of parameters is set to 0.

### Input
- `nv`  -- Number of variables in the TPSA
- `mo`  -- Maximum order of TPSA, `mo = max(1, mo)`

### Output
- `ret` -- Descriptor with the specified number of variables and maximum order
"""
function mad_desc_newv(nv::Cint, mo::Cuchar)::Ptr{Desc}
  ret = @ccall MAD_TPSA.mad_desc_newv(nv::Cint, mo::Cuchar)::Ptr{Desc}
  return ret
end


"""
    mad_desc_newvp(nv::Cint, mo::Cuchar, np_::Cint, po_::Cuchar)::Ptr{Desc}

Creates a TPSA descriptor with the specifed number of variables, maximum order, number of 
parameters, and parameter order.

### Input
- `nv`  -- Number of variables
- `mo`  -- Maximum order of TPSA INCLUDING PARAMETERS, `mo = max(1, mo)`
- `np_` -- (Optional) Number of parameters, default is 0
- `po_` -- (Optional) Order of parameters, `po = max(1, po_)`

### Output
- `ret` -- Descriptor with the specified `nv`, `mo`, `np`, and `po`
"""
function mad_desc_newvp(nv::Cint, mo::Cuchar, np_::Cint, po_::Cuchar)::Ptr{Desc}
  ret = @ccall MAD_TPSA.mad_desc_newvp(nv::Cint, mo::Cuchar, np_::Cint, po_::Cuchar)::Ptr{Desc}
  return ret
end


"""
    mad_desc_newvpo(nv::Cint, mo::Cuchar, np_::Cint, po_::Cuchar, no_)::Ptr{Desc}

Creates a TPSA descriptor with the specifed number of variables, maximum order for both variables and parameters, number of parameters, 
parameter order, and individual variable/parameter orders specified in `no`. The first `nv` entries in `no` 
correspond to the variables' orders and the next `np` entries correspond the parameters' orders.

### Input
- `nv`   -- Number of variables
- `mo`   -- Maximum order of TPSA (`mo = max(mo , no[0 :nn-1])`, `nn = nv+np`)
- `np_`  -- (Optional) Number of parameters, default is 0
- `po_`  -- (Optional) Order of parameters (`po = max(po_, no[nv:nn-1])`, `po <= mo`)
- `no_`  -- (Optional) Array of orders of variables and parameters

### Output
- `ret` -- Descriptor with the specified `nv`, `mo`, `np`, `po`, `no`.
"""
function mad_desc_newvpo(nv::Cint, mo::Cuchar, np_::Cint, po_::Cuchar, no_)::Ptr{Desc}
  typeof(no_) == Ptr{Nothing} || eltype(no_) == Cuchar || error("no_ must have eltype Cuchar if provided!")
  ret = @ccall MAD_TPSA.mad_desc_newvpo(nv::Cint, mo::Cuchar, np_::Cint, po_::Cuchar, no_::Ptr{Cuchar})::Ptr{Desc}
  return ret
end


"""
    mad_desc_del!(d_::Ptr{Desc})

Calls the destructor for the passed descriptor.

"""
function mad_desc_del!(d_::Ptr{Desc})
  @ccall MAD_TPSA.mad_desc_del(d_::Ptr{Desc})::Cvoid
end


"""
    mad_desc_getnv!(d::Ptr{Desc}, mo_::Ref{Cuchar}, np_::Ref{Cint}, po_::Ref{Cuchar}::Cint

Returns the number of variables in the descriptor, and sets the passed `mo_`, `np_`, and `po_` to the maximum 
order, number of parameters, and parameter order respectively.

### Input
- `d` -- Descriptor

### Output
- `mo_` -- (Optional) Maximum order of the descriptor
- `np_` -- (Optional) Number of parameters of the descriptor
- `po_` -- (Optional) Parameter order of the descriptor
- `ret` -- Number of variables in TPSA
"""
function mad_desc_getnv!(d::Ptr{Desc}, mo_::Ref{Cuchar}, np_::Ref{Cint}, po_::Ref{Cuchar})::Cint
  ret = @ccall MAD_TPSA.mad_desc_getnv(d::Ptr{Desc}, mo_::Ptr{Cuchar}, np_::Ptr{Cint}, po_::Ptr{Cuchar})::Cint
  return ret
end


"""
    mad_desc_maxord(d::Ptr{Desc}, nn::Cint, no_)::Cuchar

Sets the order of the variables and parameters of the TPSA to those specified in `no_` and 
returns the maximum order of the TPSA.

### Input
- `d`   -- Descriptor
- `nn`  -- Number of variables + number of parameters, `no_[1..nn]`
- `no_` -- (Optional) Orders of parameters to be filled if provided

### Output
- `ret`  -- Maximum order of TPSA
"""
function mad_desc_maxord(d::Ptr{Desc}, nn::Cint, no_)::Cuchar
  typeof(no_) == Ptr{Nothing} || eltype(no_) == Cuchar || error("no_ must have eltype Cuchar if provided!")
  ret = @ccall MAD_TPSA.mad_desc_maxord(d::Ptr{Desc}, nn::Cint, no_::Ptr{Cuchar})::Cuchar
  return ret
end


"""
    mad_desc_maxlen(d::Ptr{Desc}, mo::Cuchar)::Cint

Gets the maximum length of the TPSA given an order. 

### Input
- `d`   -- Descriptor
- `mo`  -- Order (`ordlen(maxord) == maxlen`)

### Output
- `ret` -- monomials in `0..order`
"""
function mad_desc_maxlen(d::Ptr{Desc}, mo::Cuchar)::Cint
  ret = @ccall MAD_TPSA.mad_desc_maxlen(d::Ptr{Desc}, mo::Cuchar)::Cint
  return ret
end


"""
    mad_desc_isvalids(d::Ptr{Desc}, n::Cint, s::Cstring)::Bool

Checks if monomial as string `s` is valid given maximum order of descriptor.

### Input
- `d`  -- Descriptor
- `n`  -- Monomial string length
- `s`  -- Monomial as string "[0-9]*"

### Output
- `ret` -- True if valid, false if invalid
"""
function mad_desc_isvalids(d::Ptr{Desc}, n::Cint, s::Cstring)::Bool
  ret = @ccall MAD_TPSA.mad_desc_isvalids(d::Ptr{Desc}, n::Cint, s::Cstring)::Bool
  return ret
end


"""
    mad_desc_isvalidm(d::Ptr{Desc}, n::Cint, m)::Bool

Checks if monomial as byte array `m` is valid given maximum order of descriptor.

### Input
- `d`  -- Descriptor
- `n`  -- Length of monomial
- `m`  -- Monomial as byte array

### Output
- `ret` -- True if valid, false if invalid
"""
function mad_desc_isvalidm(d::Ptr{Desc}, n::Cint, m)::Bool
  eltype(m) == Cuchar || error("m must have eltype Cuchar !")
  ret = @ccall MAD_TPSA.mad_desc_isvalidm(d::Ptr{Desc}, n::Cint, m::Ptr{Cuchar})::Bool
  return ret
end


"""
    mad_desc_isvalidsm(d::Ptr{Desc}, n::Cint, m)::Bool

Checks the monomial as sparse monomial `m` (monomial stored as sequence of integers with each pair 
`[(i,o)]` such that `i` = index, `o` = order) is valid given the maximum order of the descriptor.

### Input
- `d`   -- Descriptor
- `n`   -- Length of monomial
- `m`   -- Sparse monomial `[(idx, ord)]`

### Output
- `ret` -- True if valid, false if invalid
"""
function mad_desc_isvalidsm(d::Ptr{Desc}, n::Cint, m)::Bool
    eltype(m) == Cint || error("m must have eltype Cint !")
  ret = @ccall MAD_TPSA.mad_desc_isvalidsm(d::Ptr{Desc}, n::Cint, m::Ptr{Cint})::Bool
  return ret
end


"""
    mad_desc_idxs(d::Ptr{Desc}, n::Cint, s::Cstring)::Cint

Returns the index of the monomial as string `s` in the descriptor, or -1 if the monomial is invalid.

### Input
- `d`   -- Descriptor
- `n`   -- String length or 0 if unknown
- `s`   -- Monomial as string "[0-9]*"

### Output
- `ret` -- Monomial index or -1 if invalid monomial
"""
function mad_desc_idxs(d::Ptr{Desc}, n::Cint, s::Cstring)::Cint
  ret = @ccall MAD_TPSA.mad_desc_idxs(d::Ptr{Desc}, n::Cint, s::Cstring)::Cint
  return ret
end


"""
    mad_desc_idxm(d::Ptr{Desc}, n::Cint, m)::Cint

Returns the index of the monomial as byte array `m` in the descriptor, or -1 if the monomial is invalid.

### Input
- `d` -- Descriptor
- `n`    -- Monomial length
- `m`    -- Monomial as byte array

### Output
- `ret`  -- Monomial index or -1 if invalid
"""
function mad_desc_idxm(d::Ptr{Desc}, n::Cint, m)::Cint
  eltype(m) == Cuchar || error("m must have eltype Cuchar !")
  ret = @ccall MAD_TPSA.mad_desc_idxm(d::Ptr{Desc}, n::Cint, m::Ptr{Cuchar})::Cint
  return ret
end


"""
    mad_desc_idxsm(d::Ptr{Desc}, n::Cint, m)::Cint

Returns the index of the monomial as sparse monomial `m`, indexed as `[(i,o)]`, in the descriptor, or -1 if the monomial is invalid.

### Input
- `d`   -- Descriptor
- `n`   -- Monomial length
- `m`   -- Sparse monomial `[(idx,ord)]`

### Output
- `ret` -- Monomial index or -1 if invalid
"""
function mad_desc_idxsm(d::Ptr{Desc}, n::Cint, m)::Cint
  eltype(m) == Cint || error("m must have eltype Cint !")
  ret = @ccall MAD_TPSA.mad_desc_idxsm(d::Ptr{Desc}, n::Cint, m::Ptr{Cint})::Cint
  return ret
end


"""
    mad_desc_nxtbyvar(d::Ptr{Desc}, n::Cint, m)::Cint

Returns the next monomial after monomial `m` in the TPSA when sorted by variable.

### Input
- `d`   -- Descriptor
- `n`   -- Monomial length
- `m`   -- Monomial as byte array

### Output
- `idx` -- Monomial index or -1 if no valid next monomial
"""
function mad_desc_nxtbyvar(d::Ptr{Desc}, n::Cint, m)::Cint
  eltype(m) == Cuchar || error("m must have eltype Cuchar !")
  idx = @ccall MAD_TPSA.mad_desc_nxtbyvar(d::Ptr{Desc}, n::Cint, m::Ptr{Cuchar})::Cint
  return idx
end


"""
    mad_desc_nxtbyord(d::Ptr{Desc}, n::Cint, m)::Cint

Returns the next monomial after monomial `m` in the TPSA when sorted by order.

### Input
- `d`   -- Descriptor
- `n`   -- Monomial length
- `m`   -- Monomial as byte array

### Output
- `idx` -- Monomial index or -1 if no valid next monomial
"""
function mad_desc_nxtbyord(d::Ptr{Desc}, n::Cint, m)::Cint
  eltype(m) == Cuchar || error("m must have eltype Cuchar !")
  idx = @ccall MAD_TPSA.mad_desc_nxtbyord(d::Ptr{Desc}, n::Cint, m::Ptr{Cuchar})::Cint
  return idx
end


"""
    mad_desc_mono!(d::Ptr{Desc}, i::Cint, n::Cint, m_, p_)::Cuchar

Returns the order of the monomial at index `i`, and if `n` and `m_` are provided, then will also fill `m_` 
with the monomial at this index. Also will optionally return the order of the parameters in the monomial 
if `p_` is provided

### Input
- `d`   -- Descriptor
- `i`   -- Slot index (must be valid)
- `n`   -- Monomial length (must be provided if `m_` is to be filled)

### Output
- `ret` -- Monomial order at slot index
- `m_`  -- (Optional) Monomial to fill if provided
- `p_`  -- (Optional) Order of parameters in monomial if provided
"""
function mad_desc_mono!(d::Ptr{Desc}, i::Cint, n::Cint, m_, p_)::Cuchar
  typeof(m_) == Ptr{Nothing} || eltype(m_) == Cuchar || error("m_ must have eltype Cuchar if provided!")
  typeof(p_) == Ptr{Nothing} || eltype(p_) == Cuchar || error("p_ must have eltype Cuchar if provided!")
  ret = @ccall MAD_TPSA.mad_desc_mono(d::Ptr{Desc}, i::Cint, n::Cint, m_::Ptr{Cuchar}, p_::Ptr{Cuchar})::Cuchar
  return ret
end

"""
    mad_desc_info(d::Ptr{Desc}, fp::Ptr{Cvoid})

For debugging.

### Input
- `d`  -- Descriptor to debug
- `fp` -- File to write to. If null, will write to `stdout`
"""
function mad_desc_info(d::Ptr{Desc}, fp_::Ptr{Cvoid})
  @ccall MAD_TPSA.mad_desc_info(d::Ptr{Desc}, fp_::Ptr{Cvoid})::Cvoid
end


"""
    mad_desc_paropsth!(d::Ptr{Desc}, mult_, comp_)

Sets the parallelised operations thresholds for multiplication (`mult_`) and/or
composition (`comp_`). Will return in `mult_` and/or `comp_` the previous threshold.

### Input
- `mult_` -- (Optional) `Ptr{Cint}` to new multiplication OMP parallelization threshold
- `comp_` -- (Optional) `Ptr{Cint}` to new composition OMP parallelization threshold

### Output
- `mult_` -- (Optional) old multiplication parallelization threshold
- `comp_` -- (Optional) old composition parallelization threshold
"""
function mad_desc_paropsth!(d::Ptr{Desc}, mult_, comp_)
  @ccall MAD_TPSA.mad_desc_paropsth!(d::Ptr{Desc}, mult_::Ptr{Cint}, comp_::Ptr{Cint})::Cvoid
end