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
              show,
              copy!,
              lastindex,
              firstindex,
              rand,
              unsafe_convert,
              eltype

import LinearAlgebra: norm, mul!
import SpecialFunctions: erf, erfc

using GTPSA_jll, Printf, PrettyTables

export  
  # Layer 2 structs + functions NOT in Base:
  Descriptor,
  TPS,
  ComplexTPS,
  unit  ,
  sinhc ,
  asinc ,
  asinhc,
  erf   ,
  erfc  ,
  norm,
  polar,
  rect, 
  clear!,
  complex!,
  add!,
  sub!,
  mul!,
  div!,

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
  evaluate, evaluate!,
  compose, compose!,
  integ, ∫, integ!, ∫!,
  deriv, ∂, deriv!, ∂!,
  getord, getord!,
  cutord, cutord!,
  clearord, clearord!,
  translate, translate!,
  par,
  scalar,
  setTPS!,
  lengthTPS,
  
  # Temporaries:
  @FastGTPSA,
  ±,
  ∓,
  ⨰,
  ⨱,
  ⤊,
  __t_inv, __t_atan, __t_abs, __t_sqrt, __t_exp, __t_log, __t_sin, __t_cos, __t_tan, __t_csc, __t_sec, __t_cot, __t_sinc, __t_sinh, __t_cosh,
  __t_tanh, __t_csch, __t_sech, __t_coth, __t_asin, __t_acos, __t_atan, __t_acsc, __t_asec, __t_acot, __t_asinh, __t_acosh, __t_atanh, __t_acsch, 
  __t_asech, __t_acoth, __t_real, __t_imag, __t_conj, __t_angle, __t_complex, __t_sinhc, __t_asinc, __t_asinhc, __t_erf, __t_erfc, __t_norm,
  __t_polar, __t_rect,


  NewTPS,
  ComplexNewTPS

include("low.jl")           # Low level, 1-to-1 Julia-to-C code including C struct definitions
include("descriptor.jl")    # Descriptor struct and constructors
include("global.jl")        # Global variables
include("tps.jl")           # TPS/ComplexTPS structs and constructors
include("utils.jl")         # Utility functions including changing Descriptor of TPS
include("ctors.jl")         # Convenience constructors (vars, params, mono)
include("getset.jl")        # Indexing/slicing TPS, par, convenience getters (gradient, jacobian, hessian)
include("show.jl")          # Output
include("operators.jl")     # TPS math overloaded operators/functions
include("methods.jl")       # Higher-level TPS functions (derivatives, integrals, evaluate, etc)
include("fast_gtpsa.jl")    # Everything for the @FastGTPSA macro

include("new/tps.jl")
include("new/low.jl")
include("new/operators.jl")
include("new/getset.jl")
include("new/show.jl")
include("new/methods.jl")

end
