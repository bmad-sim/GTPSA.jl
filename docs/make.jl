using Pkg
using Documenter, GTPSA

cp(joinpath(@__DIR__, "..", "README.md"), joinpath(@__DIR__, "src", "index.md"); force=true)

makedocs(
  sitename="GTPSA.jl",
  authors = "Matt Signorelli",
  format=Documenter.HTMLWriter.HTML(size_threshold = nothing),
  pages = 
  [
    "Home" => "index.md",
    "Quickstart Guide" => "quickstart.md",
    "Manual" => ["man/a_toc.md",
                 "man/b_definitions.md", 
                 "man/c_descriptor.md", 
                 "man/d_tps.md",
                 "man/e_varsparams.md",
                 "man/f_monoindex.md",
                 "man/g_mono.md",
                 "man/h_gjh.md",
                 "man/i_slice.md",
                 "man/j_methods.md",
                 "man/k_fastgtpsa.md",
                 "man/l_io.md",
                 "man/m_global.md",
                 "man/n_all.md"],
    "For Developers" => "devel.md"
  ]
)

deploydocs(; repo = "github.com/bmad-sim/GTPSA.jl.git")
