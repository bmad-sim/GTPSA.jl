# --- clear ---
clear!(t::NewTPS{Float64})    = mad_tpsa_clear!(t)
clear!(t::NewTPS{ComplexF64}) = mad_ctpsa_clear!(t)

"""
    norm(t1::TPS)::Float64

Calculates the 1-norm of the `TPS`, which is the sum of 
the `abs` of all coefficients.
"""
norm(t1::NewTPS{Float64}) = mad_tpsa_nrm(t1)
norm(t1::NewTPS{ComplexF64}) = mad_ctpsa_nrm(t1)
