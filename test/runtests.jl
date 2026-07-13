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
  mad_tpsa_sincoshmq: Number of C variables different from number of Julia variables! Skipping variable check...
  mad_ctpsa.h downloaded.
  Comparing mad_ctpsa.h to ctpsa.jl...
  mad_ctpsa_sincoshmq: Number of C variables different from number of Julia variables! Skipping variable check...
  mad_ctpsa_cplx found in GTPSA.jl, but not MAD_TPSA!
  mad_ctpsa_cplx found in GTPSA.jl, but not MAD_TPSA!
  mad_ctpsa_cplx found in GTPSA.jl, but not MAD_TPSA!
  """
  # sincoshmq in the header file (which this compare script reads) does not have a space between 
  # tpsa_t* and "a", so it looks like tpsa_t*a, tpsa_t *b, ... which messes up the variable count. 
  # so everything is ok, perhaps the script can be modified to account for this in the future but 
  # for now let it go
  @test compare_MAD() == expected_out
end