using Pkg
using Documenter, GTPSA

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
                 "man/e_complextps.md",
                 "man/f_varsparams.md",
                 "man/g_monoindex.md",
                 "man/h_mono.md",
                 "man/i_gjh.md",
                 "man/j_slice.md",
                 "man/k_methods.md",
                 "man/l_fastgtpsa.md",
                 "man/m_io.md",
                 "man/n_global.md",
                 "man/o_all.md"],
    "For Developers" => "devel.md"
  ]
)

deploydocs(; repo = "github.com/bmad-sim/GTPSA.jl.git")
