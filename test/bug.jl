using Test

@testset "Bug" begin
  println("preallocating")
  w = Vector{Vector{Float64}}(undef, 248)
  for i=1:length(w)
    w[i] = [0.]
  end
  println("preallocated")
  @test w[1][1] == 0.
end