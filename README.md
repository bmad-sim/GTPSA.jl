# TPSA

[![Build Status](https://github.com/bmad-sim/TPSA.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/bmad-sim/TPSA.jl/actions/workflows/CI.yml?query=branch%3Amain)
## Setup for Development
First, the shared C library for MAD_TPSA must be compiled for Julia to call the C functions. Follow the corresponding instructions depending on your machine:
### Linux C Library Compilation
In any directory:
```
git clone https://github.com/mattsignorelli/mad_tpsa.git
cd mad_tpsa
cmake .
make
```
```libmad_tpsa.so``` will be built and placed in the subdirectory ```build/lib```. LAPACK is required for the shared library to work (and on some machines, compile) properly. The CMake file will look for LAPACK on your machine and link it to ```libmad_tpsa```. To ensure this has been properly done, check that ```liblapack``` shows up with ```ldd libmad_tpsa.so``` in the ```build/lib``` subdirectory. Finally, the path to ```libmad_tpsa.so``` must be added to the ```$LD_LIBRARY_PATH``` environment variable. This can be done by adding ```export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:<full_path_to_mad_tpsa/build/lib>``` to your ```.bashrc``` file.

### Mac Shared C Library Compilation
In any directory:
```
git clone https://github.com/mattsignorelli/mad_tpsa.git
cd mad_tpsa
cmake .
make
```
```libmad_tpsa.dylib``` will be built and placed in the subdirectory ```build/lib```. LAPACK is required for the shared library to work (and on some machines, compile) properly. The CMake file will look for LAPACK on your machine and link it to ```libmad_tpsa```. To ensure this has been properly done, check that ```liblapack``` shows up with ```ldd libmad_tpsa.dylib``` in the ```build/lib``` subdirectory. Finally, the path to ```libmad_tpsa.dylib``` must be added to the ```$DYLD_FALLBACK_LIBRARY_PATH``` environment variable. This can be done by adding ```export DYLD_FALLBACK_LIBRARY_PATH=$DYLD_FALLBACK_LIBRARY_PATH:<full_path_to_mad_tpsa/build/lib>``` to your ```.bashrc``` file.

### Using/Developing TPSA.jl
After compiling ```libmad_tpsa``` on your machine, in your ```.julia/dev/``` directory, clone the ```TPSA.jl``` repository with:
```
git clone https://github.com/bmad-sim/TPSA.jl.git
```
Finally, while in the ```dev``` directory, the Julia package can be used in the Julia REPL with:
```
using Pkg
Pkg.activate("TPSA.jl")
Pkg.instantiate()
using TPSA
```
For Julia programs outside ```.julia/dev```, the full path to the ```TPSA.jl``` directory from the current directory should be specified. 
