include("tests.jl")
using .TPSA
using Test

@testset "tests.jl" begin
  @test String(read("test0.txt")) == String(read("../gtpsa_c/gtpsa_ex0.txt"));
  @test String(read("test1.txt")) == String(read("../gtpsa_c/gtpsa_ex1.txt"));
  @test String(read("test2.txt")) == String(read("../gtpsa_c/gtpsa_ex2.txt"));
  @test String(read("test3.txt")) == String(read("../gtpsa_c/gtpsa_ex3.txt"));
  @test String(read("test4.txt")) == String(read("../gtpsa_c/gtpsa_ex4.txt"));
  @test String(read("test5.txt")) == String(read("../gtpsa_c/gtpsa_ex5.txt"));
end
