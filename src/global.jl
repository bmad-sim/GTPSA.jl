const MAD_TPSA::String = :("libgtpsa")
const MAD_TPSA_DEFAULT::Cuchar = 255
const MAD_TPSA_SAME::Cuchar = 254
const MAD_DESC_CURR::Ptr{Desc} = C_NULL
const DESC_MAX_TMP::Int = 8

# Global non-constants (types MUST be specified)
desc_current::Descriptor = Descriptor(MAD_DESC_CURR)   # Current Descriptor
show_eps::Float64 =  0.0                               # Print epsilon
show_sparse::Bool = false                              # Use sparse monomial print
show_header::Bool = false                              # Print a header above each TPS

"""
    setGTPSA!(a::AbstractString, val)

Function to set global variables in GTPSA. Options for `a` are:

- `desc_current::Descriptor` -- defines the `Descriptor` to use when that information is not explicitly (or implicitly in a TPS copy constructor) available, e.g. when calling `TPS(a)` where `a` is not a TPS. This is set each time a new `Descriptor` is defined
- `show_eps::Float64`        -- defines the value below which the absolute value of a monomial coefficient is NOT printed
- `show_sparse::Bool`        -- specifies whether the sparse monomial format is used for printing. This is useful for GTPSAs containing a large number of variables and parameters
- `show_header::Bool`        -- specifies whether or not to print the GTPSA `Descriptor` information above each TPS output
"""
function setGTPSA!(a::AbstractString, val)
  a == "desc_current" && (return (GTPSA.desc_current = val))
  a == "show_eps"     && (return (GTPSA.show_eps = val))
  a == "show_sparse"  && (return (GTPSA.show_sparse = val))
  a == "show_header"  && (return (GTPSA.show_header = val))
  error("Global variable \"$(a)\" does not exist!")
end

"""
    getGTPSA(a::AbstractString)

Function to get global variables in GTPSA. Options for `a` are:

- `desc_current::Descriptor` -- defines the `Descriptor` to use when that information is not explicitly (or implicitly in a TPS copy constructor) available, e.g. when calling `TPS(a)` where `a` is not a TPS. This is set each time a new `Descriptor` is defined
- `show_eps::Float64`        -- defines the value below which the absolute value of a monomial coefficient is NOT printed
- `show_sparse::Bool`        -- specifies whether the sparse monomial format is used for printing. This is useful for GTPSAs containing a large number of variables and parameters
- `show_header::Bool`        -- specifies whether or not to print the GTPSA `Descriptor` information above each TPS output
"""
function getGTPSA(a::AbstractString)
  a == "desc_current" && (return GTPSA.desc_current)
  a == "show_eps"     && (return GTPSA.show_eps)
  a == "show_sparse"  && (return GTPSA.show_sparse)
  a == "show_header"  && (return GTPSA.show_header)
  error("Global variable \"$(a)\" does not exist!")
end