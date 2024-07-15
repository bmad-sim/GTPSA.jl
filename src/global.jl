const MAD_TPSA::String = :("libgtpsa")
const MAD_TPSA_DEFAULT::Cuchar = 255
const MAD_TPSA_SAME::Cuchar = 254
const MAD_DESC_CURR::Ptr{Desc} = C_NULL
const DESC_MAX_TMP::Int = 8
const NAMSZ::Int = 16 

# Global non-constants (types MUST be specified)
desc_current::Descriptor = Descriptor(MAD_DESC_CURR)   # Current Descriptor
show_eps::Float64 =  0.0                               # Print epsilon
show_sparse::Bool = false                              # Use sparse monomial print
show_header::Bool = false                              # Print a header above each TPS