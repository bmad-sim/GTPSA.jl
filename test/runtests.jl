using Test, JET
using BenchmarkTools: @benchmark, BenchmarkTools
using SpecialFunctions
using GTPSA
import GTPSA: Desc

BenchmarkTools.DEFAULT_PARAMETERS.gctrial = false
BenchmarkTools.DEFAULT_PARAMETERS.evals = 2
BenchmarkTools.DEFAULT_PARAMETERS.samples = 1

include("test_dynamic.jl")
include("test_static.jl")

@testset "Compare with MAD" begin
  include("compare_MAD.jl")
  expected_out = """mad_mono.h downloaded.
  Comparing mad_mono.h to mono.jl...
  mad_desc.h downloaded.
  Comparing mad_desc.h to desc.jl...
  mad_tpsa.h downloaded.
  Comparing mad_tpsa.h to rtpsa.jl...
  mad_ctpsa.h downloaded.
  Comparing mad_ctpsa.h to ctpsa.jl...
  mad_ctpsa_cplx found in GTPSA.jl, but not MAD_TPSA!
  mad_ctpsa_cplx found in GTPSA.jl, but not MAD_TPSA!
  mad_ctpsa_cplx found in GTPSA.jl, but not MAD_TPSA!
  """
  @test compare_MAD() == expected_out
end