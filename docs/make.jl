using Pkg
Pkg.activate("../../TPSA")
Pkg.instantiate()
using TPSA
using Documenter, TPSA

makedocs(
  sitename="TPSA.jl",
  authors = "Matt Signorelli",
  format=Documenter.HTMLWriter.HTML(size_threshold = nothing),
  pages = 
  [
    "Home" => "index.md",
    "Setup for Development" => "setup.md",
    "Low-Level Functions" => 
    Any[
      "Descriptor" => "low_level/desc.md",
      "Monomial" => "low_level/mono.md",
      "Real TPSA" => "low_level/rtpsa.md",
      "Complex TPSA" => "low_level/ctpsa.md",
    ]
  ]
)