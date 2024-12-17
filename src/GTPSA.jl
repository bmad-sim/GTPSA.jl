module GTPSA


import Base:  +,
              -,
              *,
              /,
              ^,
              ∘,
              inv,
              atan,
              hypot,
              abs   ,
              sqrt  ,
              exp   ,
              log   ,
              sin   ,
              cos   ,
              tan   ,
              csc   ,
              sec   ,
              cot   ,
              sinc  ,
              sinh  ,
              cosh  ,
              tanh  ,
              csch  ,
              sech  ,
              coth  ,
              asin  ,
              acos  ,
              atan  ,
              acsc  ,
              asec  ,
              acot  ,
              asinh ,
              acosh ,
              atanh ,
              acsch ,
              asech ,
              acoth ,
              zero  ,
              zeros ,
              one   ,
              ones  ,
              real  ,
              imag  ,
              conj  ,
              angle ,
              complex,
              Complex,
              promote_rule,
              getindex,
              setindex!,
              ==,
              <,
              >,
              <=,
              >=,
              !=,
              isequal,
              isless,
              isinf,
              isnan,
              show,
              copy!,
              lastindex,
              firstindex,
              rand,
              unsafe_convert,
              eps,
              floatmin,
              floatmax,
              delete!

import LinearAlgebra: norm, mul!
import SpecialFunctions: erf, erfc

using GTPSA_jll, Printf, PrettyTables

export  
  # Layer 2 structs + functions NOT in Base:
  Descriptor,
  TPS,
  TPS64,
  ComplexTPS64,
  unit  ,
  sincu,
  sinhc ,
  sinhcu,
  asinc ,
  asincu,
  asinhc,
  asinhcu,
  erf   ,
  erfc  ,
  norm,
  polar,
  rect, 
  clear!,
  mul!,

  # Monomial as TPS creators:
  vars,
  params,
  complexvars,
  complexparams,
  mono,
  complexmono,

  # Convenience getters:
  #gradient,
  #gradient!,
  #jacobian,
  #jacobian!,
  #jacobiant,
  #jacobiant!,
  #hessian,
  #hessian!,
  

  # Methods:
  complex!,
  compose, compose!,
  cycle!,
  evaluate, evaluate!,
  fgrad, fgrad!,
  integ, ∫, integ!, ∫!,
  deriv, ∂, deriv!, ∂!,
  getord, getord!,
  cutord, cutord!,
  clearord, clearord!,
  translate, translate!,
  par,
  scalar,
  setTPS!,
  numcoefs,
  normTPS,
  
  # Temporaries:
  @FastGTPSA,
  @FastGTPSA!

include("low_level/mono.jl")      # All functions in mad_mono.c (monomial utility functions, unused by GTPSA.jl)
include("low_level/desc.jl")      # All functions in mad_desc.c (Descriptor functions)
include("descriptor.jl")          # Descriptor struct and constructors
include("tps.jl")                 # TPS struct definition/ctors and promotion rules/type operators
include("fastgtpsa/temptps.jl")   # TempTPS (temporary TPS for @FastGTPSA internal use) struct definition and promotion rules
include("low_level/rtpsa.jl")     # All functions in mad_tpsa.c  (TPS{Float64} functions)
include("low_level/ctpsa.jl")     # All functions in mad_ctpsa.c (TPS{ComplexF64} functions)
include("low.jl")                 # Interface to low-level TPS operators (cycle!, getsm, ...)
include("utils.jl")               # Utility functions 
include("inplace_operators.jl")   # In place (!) operators defined for TPS and TempTPS
include("operators.jl")           # TPS math overloaded operators/functions 
include("fastgtpsa/fastgtpsa.jl") # Definition of the @FastGTPSA macro
include("fastgtpsa/operators.jl") # TempTPS special math operators/functions
include("global.jl")              # Global variables
include("getset.jl")              # Indexing/slicing TPS, par, convenience getters (gradient, jacobian, hessian)
include("ctors.jl")               # Convenience constructors (vars, params, mono)
include("show.jl")                # Output
include("methods.jl")             # Higher-level TPS functions (setTPS!, clear!, derivatives, integrals, evaluate, etc)


end
