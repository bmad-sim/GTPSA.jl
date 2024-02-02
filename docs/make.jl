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
    "Manual" => ["man/definitions.md", 
                 "man/descriptor.md", 
                 "man/tps.md"],
                 #"man/partials.md",
                 #"man/methods.md",
                 #"man/fastgtpsa.md",
                 #"man/io.md",
                 #"man/all.md"],
    "For Developers" => "devel.md"
  ]
)

deploydocs(; repo = "github.com/bmad-sim/GTPSA.jl.git")
