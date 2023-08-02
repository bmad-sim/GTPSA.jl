using TPSA
using Test

@testset "TPSA.jl" begin
  @test TPSA.greet_TPSA() == "Hello TPSA!"
  @test TPSA.greet_TPSA() != "Hello world!"
end
