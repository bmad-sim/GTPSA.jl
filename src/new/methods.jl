# --- clear ---
clear!(t::NewTPS{Float64})    = mad_tpsa_clear!(t)
clear!(t::NewTPS{ComplexF64}) = mad_ctpsa_clear!(t)