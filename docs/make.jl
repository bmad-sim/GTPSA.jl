using Pkg
using Documenter, GTPSA

makedocs(
  sitename="GTPSA.jl",
  authors = "Matt Signorelli",
  format=Documenter.HTMLWriter.HTML(size_threshold = nothing),
  pages = 
  [
    "Home" => "index.md",
    "Usage" => "usage.md",
    "Advanced Usage" => "advanced.md",
    "Code Index" => "code.md",
    "For Developers" => "devel.md"
  ]
)

deploydocs(; repo = "github.com/bmad-sim/GTPSA.jl.git")
