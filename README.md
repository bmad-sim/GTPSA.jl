# TPSA

[![Build Status](https://github.com/bmad-sim/TPSA.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/bmad-sim/TPSA.jl/actions/workflows/CI.yml?query=branch%3Amain)
## Setup for Development
First, the shared C library for the GTPSA must be compiled for Julia to call the C functions. In any directory:
```
git clone https://github.com/mattsignorelli/gtpsa.git
```
Then, build the shared C library of the GTPSA:
```
cd gtpsa
cmake .
make
```
LAPACK is required for the GTPSA shared library to work (and on some machines, compile) properly. The CMake file will look for LAPACK on your machine and link it to libgtpsa. To ensure this has been properly done, check that liblapack shows up with ```ldd libgtpsa.so```. Once ```libgtpsa.so``` is made, the path to the shared library must be added to the ```$LD_LIBRARY_PATH``` environment variable. This can be done by adding ```export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:<path_to_parent_dir_of_libgtpsa.so>``` to your ```.bashrc``` file.

Next, in your ```.julia/dev/``` directory, clone the repository with:
```
git clone https://github.com/bmad-sim/TPSA.jl.git
```
Finally, while in the ```dev``` directory, the Julia package can be used in the Julia REPL with:
```
using Pkg
Pkg.activate("TPSA")
using TPSA
```
For Julia programs outside ```.julia/dev```, the full path to the ```TPSA``` directory from the current directory should be specified. 
