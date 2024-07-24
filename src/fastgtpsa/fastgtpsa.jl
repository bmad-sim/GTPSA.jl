"""
    @FastGTPSA(expr)

Macro to speed up evaluation of mathematical expressions containing TPSs.
The temporaries generated during evaluation of the expression are drawn 
from a thread-safe buffer, reducing the number of heap allocations to 
2 (which is for a single TPS) for the result. @FastGTPSA is completely 
transparent to all other types, so it can be prepended to expressions 
while still maintaining type-generic code.

# Example
```julia-repl
julia> using GTPSA, BenchmarkTools

julia> d = Descriptor(3,5); x = vars(d);

julia> @btime \$x[1]^3*sin(\$x[2])/log(2+\$x[3])-exp(\$x[1]*\$x[2])*im;
  1.654 μs (20 allocations: 5.88 KiB)

julia> @btime @FastGTPSA \$x[1]^3*sin(\$x[2])/log(2+\$x[3])-exp(\$x[1]*\$x[2])*im;
  1.363 μs (2 allocations: 960 bytes)
```
"""
macro FastGTPSA(expr)
  return :(to_TPS($(to_temp_form(munge_expr(esc(expr))))))
end 

"""
    @FastGTPSA!(result, expr)

Macro to speed up evaluation of mathematical expressions containing TPSs.
The temporaries generated during evaluation of the expression are drawn 
from a thread-safe buffer, and `result` is set equal to the result. With 
this macro, the number of heap allocations during expression evaluation is 
0, however it is not type-generic as `result` must be an allocated `TPS`.

# Example
```julia-repl
julia> using GTPSA, BenchmarkTools

julia> d = Descriptor(3,5); x = vars(d);

julia> t = ComplexTPS64();

julia> @btime \$x[1]^3*sin(\$x[2])/log(2+\$x[3])-exp(\$x[1]*\$x[2])*im;
  1.654 μs (20 allocations: 5.88 KiB)

julia> @btime @FastGTPSA!(\$t, \$x[1]^3*sin(\$x[2])/log(2+\$x[3])-exp(\$x[1]*\$x[2])*im);
  1.321 μs (0 allocations: 0 bytes)
```
"""
macro FastGTPSA!(result, expr)
  return :(to_TPS!($(esc(result)), $(to_temp_form(munge_expr(esc(expr))))))
end 

function to_temp_form(expr)
  fcns = [:unit, :sqrt, :exp, :log, :sin, :cos, :tan, :cot, :sinh, :cosh, :tanh, :inv, :coth, 
          :asin, :acos, :atan, :acot, :asinh, :acosh, :atanh, :acoth, :erf, :erfc, :sinc, 
          :sinhc, :asinc, :asinhc, :csc, :csch, :acsc, :acsch, :sec, :sech, :asec, :asech, 
          :conj, :rect, :real, :imag, :angle, :abs, :atan, :polar, :complex, :zero, :one,
          :norm, :normTPS, :+, :-, :*, :/, :^]
  if expr.head == :.
    pkg = expr.args[1]
    if pkg == :GTPSA && expr.args[end].value in fcns # Only change is function is in GTPSA
      str = "__t_" * string(expr.args[end].value)
      expr.args[end] = QuoteNode(Symbol(str))
    end
    return expr
  end
  for i in eachindex(expr.args)
    if expr.args[i] isa Expr && expr.args[i].args[1] in fcns
      to_temp_form(expr.args[i])
    elseif expr.args[i] == :+
      expr.args[i] = :±  # \pm  (allowed as unary operator)
    elseif expr.args[i] == :-
      expr.args[i] = :∓  # \mp  (allowed as unary operator)
    elseif expr.args[i] == :*
      expr.args[i] = :⨰  # \dottimes
    elseif expr.args[i] == :/
      expr.args[i] = :⨱  # \timesbar
    elseif expr.args[i] == :^
      expr.args[i] = :⤊  # \Uuparrow
    elseif expr.args[i] in fcns
      str = "__t_" * string(expr.args[i])
      expr.args[i] = Symbol(str)
    end
  end

  return expr
end

function munge_expr(expr::Expr; inplace::Bool = false)
  if !inplace; expr = deepcopy(expr); end

  # Munge :(+, a, b, c, d) to be :(+, (+, a, b, c), d)
  if expr.head == :call && (expr.args[1] == :+ || expr.args[1] == :*) && length(expr.args) > 3
    # println("Found +*")
    stub = deepcopy(expr)
    pop!(stub.args)  # d removed in above exprample
    expr.args = [expr.args[1], stub, expr.args[end]]
  end

  # Recursively call this routine for each element in args array if the arg is an Expr.
  for arg in expr.args
    # println("In: " * string(arg))
    if typeof(arg) == Expr; arg = munge_expr(arg, inplace = true); end
  end

  return expr
end

function to_TPS(t1::TempTPS{T}) where {T}
  t = TPS{T}(getdesc(t1).desc, getmo(t1)) #get_and_zero_mo!(t1))
  copy!(t,t1)
  rel_temp!(t1)
  return t
end

function to_TPS!(t::TPS{T},t1::TempTPS{T}) where {T}
  copy!(t,t1)
  rel_temp!(t1)
  return t
end

# Fallback for non-TPS types
to_TPS(a) = a


#=
# --- mul ---
for t = ((TPS,TPS),(TPS,Number),(Number,TPS),(TempTPS,TempTPS),(TempTPS,TPS),(TPS,TempTPS),(TempTPS,Number),(Number,TempTPS))
@eval begin
function ⨰(t1::$t[1], t2::$t[2])
  t = get_out(t1,t2)
  mul!(t, t1, t2)
  rel_op!(t1, t2)
  return t
end
end
end
⨰(a, b) = (@inline; *(a,b)) # Fall back for non TPS types

# --- add ---
for t = ((TPS,TPS),(TPS,Number),(Number,TPS),(TempTPS,TempTPS),(TempTPS,TPS),(TPS,TempTPS),(TempTPS,Number),(Number,TempTPS))
@eval begin
function ±(t1::$t[1], t2::$t[2])
  t = get_out(t1,t2)
  add!(t, t1, t2)
  rel_op!(t1, t2)
  return t
end
end
end
±(a, b) =(@inline; +(a,b)) # Fall back for non TPS types

# --- sub ---
for t = ((TPS,TPS),(TPS,Number),(Number,TPS),(TempTPS,TempTPS),(TempTPS,TPS),(TPS,TempTPS),(TempTPS,Number),(Number,TempTPS))
@eval begin
function ∓(t1::$t[1], t2::$t[2])
  t = get_out(t1,t2)
  sub!(t, t1, t2)
  rel_op!(t1, t2)
  return t
end
end
end
∓(a, b) = (@inline; -(a,b)) # Fall back for non TPS types


# --- div ---
for t = ((TPS,TPS),(TPS,Number),(Number,TPS),(TempTPS,TempTPS),(TempTPS,TPS),(TPS,TempTPS),(TempTPS,Number),(Number,TempTPS))
@eval begin
function ⨱(t1::$t[1], t2::$t[2])
  t = get_out(t1,t2)
  div!(t, t1, t2)
  rel_op!(t1, t2)
  return t
end
end
end
⨱(a, b) = (@inline; /(a,b)) # Fall back for non TPS types

# --- pow ---
for t = ((TPS,TPS),(TPS,Number),(Number,TPS),(TempTPS,TempTPS),(TempTPS,TPS),(TPS,TempTPS),(TempTPS,Number),(Number,TempTPS))
@eval begin
function ⤊(t1::$t[1], t2::$t[2])
  t = get_out(t1,t2)
  pow!(t, t1, t2)
  rel_op!(t1, t2)
  return t
end
end
end

⤊(a,b) = (@inline; ^(a,b)) # Fall back for non TPS types


# Now, all unary operators have been defined properly with ! syntax
# So all we have to do is define the out of place operators 
# but always return temporaries.

# The way it works if basically all in-place operators in operators.jl
# can also work for temporary types. But the out of place operators 
# must have special definition __t_ and the arithmetic too.


function __t_inv(ctpsa1::TempTPS{ComplexF64})
  mad_ctpsa_inv!(ctpsa1, convert(ComplexF64, 1), ctpsa1)
  return ctpsa1
end

function __t_inv(t1::TPS{Float64})
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_inv!(t1, convert(Cdouble, 1), tpsa)
  return tpsa
end

function __t_inv(tpsa1::TempTPS{Float64})
  mad_tpsa_inv!(tpsa1, convert(Cdouble, 1), tpsa1)
  return tpsa1
end


function __t_inv(ct1::TPS{ComplexF64})
  ctpsa = TempTPS{ComplexF64}(ct1)
  mad_ctpsa_inv!(ct1, convert(ComplexF64, 1), ctpsa)
  return ctpsa
end


# --- Unary ---
# TPS{Float64}:
function ±(t1::TPS{Float64})::TPS{Float64}
  return t1
end

function ±(tpsa::TempTPS{Float64})::TempTPS{Float64}
  return tpsa
end

function ∓(t1::TPS{Float64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_scl!(t1, -1., tpsa)
  return tpsa
end

function ∓(tpsa::TempTPS{Float64})::TempTPS{Float64}
  mad_tpsa_scl!(tpsa, -1., tpsa)
  return tpsa 
end

# TPS{ComplexF64}:
function ±(ct1::TPS{ComplexF64})::TPS{ComplexF64}
  return ct1
end

function ±(ctpsa::TempTPS{ComplexF64})::TempTPS{ComplexF64}
  return ctpsa
end

function ∓(ct1::TPS{ComplexF64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(ct1)
  mad_ctpsa_scl!(ct1, convert(ComplexF64, -1), ctpsa)
  return ctpsa
end

function ∓(ctpsa::TempTPS{ComplexF64})::TempTPS{ComplexF64}
  mad_ctpsa_scl!(ctpsa, convert(ComplexF64, -1), ctpsa)
  return ctpsa
end

# -- zero --

function __t_zero(ct1::TPS{ComplexF64})::TempTPS{ComplexF64}
  tpsa = TempTPS{ComplexF64}(ct1)
  mad_ctpsa_clear!(tpsa)
  return tpsa
end

function __t_zero(ct1::TempTPS{ComplexF64})::TempTPS{ComplexF64}
  mad_ctpsa_clear!(ct1)
  return ct1
end

function __t_zero(t1::TPS{Float64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_clear!(tpsa)
  return tpsa
end

function __t_zero(t1::TempTPS{Float64})::TempTPS{Float64}
  mad_tpsa_clear!(t1)
  return t1
end


# -- one ---
function __t_one(ct1::TPS{ComplexF64})::TempTPS{ComplexF64}
  tpsa = TempTPS{ComplexF64}(ct1)
  mad_ctpsa_clear!(tpsa)
  mad_ctpsa_seti!(tpsa, Cint(0), ComplexF64(0), ComplexF64(1))
  return tpsa
end

function __t_one(tpsa::TempTPS{ComplexF64})::TempTPS{ComplexF64}
  mad_ctpsa_clear!(tpsa)
  mad_ctpsa_seti!(tpsa, Cint(0), ComplexF64(0), ComplexF64(1))
  return tpsa
end

function __t_one(t1::TPS{Float64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_clear!(tpsa)
  mad_tpsa_seti!(tpsa, Cint(0), Float64(0), Float64(1))
  return tpsa
end

function __t_one(tpsa::TempTPS{Float64})::TempTPS{Float64}
  mad_tpsa_clear!(tpsa)
  mad_tpsa_seti!(tpsa, Cint(0), Float64(0), Float64(1))
  return tpsa
end


function __t_real(ct1::TPS{ComplexF64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(ct1)
  mad_ctpsa_real!(ct1, tpsa)
  return tpsa
end

function __t_imag(ct1::TPS{ComplexF64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(ct1)
  mad_ctpsa_imag!(ct1, tpsa)
  return tpsa
end

function __t_real(t1::TPS{Float64})::TPS{Float64}
  return t1
end

function __t_imag(t1::TPS{Float64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_setval!(tpsa, 0.0)
  return tpsa
end

# Temps:

function __t_real(ctpsa::TempTPS{ComplexF64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(ctpsa)
  mad_ctpsa_real!(ctpsa, tpsa)
  rel_temp!(ctpsa)
  return tpsa
end

function __t_imag(ctpsa::TempTPS{ComplexF64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(ctpsa)
  mad_ctpsa_imag!(ctpsa, tpsa)
  rel_temp!(ctpsa)
  return tpsa
end

function __t_real(tpsa::TempTPS{Float64})::TempTPS{Float64}
  return tpsa
end

function __t_imag(tpsa::TempTPS{Float64})::TempTPS{Float64}
  mad_tpsa_setval!(tpsa, 0.0)
  return tpsa
end


# Fallbacks for regular types
±(a) = +a
∓(a) = -a
__t_real(a) = real(a)
__t_imag(a) = imag(a)
__t_zero(a) = zero(a)
__t_one(a) = one(a)



# --- atan, norm ---
# TPS{Float64}:
function __t_atan(t1::TPS{Float64}, t2::TPS{Float64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_atan2!(t1, t2, tpsa)
  return tpsa
end

function __t_atan(t1::TPS{Float64}, a::Real)::TempTPS{Float64}
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_setval!(tpsa, convert(Float64, a))
  mad_tpsa_atan2!(t1, tpsa, tpsa)
  return tpsa
end

function __t_atan(a::Real, t1::TPS{Float64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_setval!(tpsa, convert(Float64, a))
  mad_tpsa_atan2!(tpsa,t1, tpsa)
  return tpsa
end

function __t_atan(tpsa1::TempTPS{Float64}, tpsa2::TempTPS{Float64})::TempTPS{Float64}
  mad_tpsa_atan2!(tpsa1, tpsa2, tpsa1)
  rel_temp!(tpsa2)
  return tpsa1
end

function __t_atan(tpsa1::TempTPS{Float64}, a::Real)::TempTPS{Float64}
  tpsa2 = TempTPS{Float64}(tpsa1)
  mad_tpsa_setval!(tpsa2, convert(Float64, a))
  mad_tpsa_atan2!(tpsa1, tpsa2, tpsa1)
  rel_temp!(tpsa2)
  return tpsa1
end

function __t_atan(a::Real, tpsa1::TempTPS{Float64})::TempTPS{Float64}
  tpsa2 = TempTPS{Float64}(tpsa1)
  mad_tpsa_setval!(tpsa2, convert(Float64, a))
  mad_tpsa_atan2!(tpsa2,tpsa1, tpsa1)
  rel_temp!(tpsa2)
  return tpsa1
end

function __t_atan(t1::TPS{Float64}, tpsa1::TempTPS{Float64})::TempTPS{Float64}
  mad_tpsa_atan2!(t1, tpsa1, tpsa1)
  return tpsa1
end

function __t_atan(tpsa1::TempTPS{Float64}, t1::TPS{Float64})::TempTPS{Float64}
  mad_tpsa_atan2!(tpsa1, t1, tpsa1)
  return tpsa1
end


__t_atan(a,b) = (@inline; atan(a,b))

function __t_norm(tpsa1::TempTPS{Float64})::Float64
  nrm = mad_tpsa_nrm(tpsa1)
  rel_temp!(tpsa1)
  return nrm 
end

function __t_norm(ctpsa1::TempTPS{ComplexF64})::Float64
  nrm = mad_ctpsa_nrm(ctpsa1)
  rel_temp!(ctpsa1)
  return nrm 
end

__t_norm(a) = (@inline; norm(a))


# --- rest of unary functions ---
# TPS{Float64}:
macro FUNT(F)
  F1 = Symbol("__t_" * F)
  fn = Symbol("mad_tpsa_" * F * "!")
  quote
      function $(esc(F1))(t1::TPS{Float64})::TempTPS{Float64}
        tpsa = TempTPS{Float64}(t1)
        $(esc(fn))(t1, tpsa)
        return tpsa
      end

      function $(esc(F1))(tpsa1::TempTPS{Float64})::TempTPS{Float64}
        $(esc(fn))(tpsa1, tpsa1)
        return tpsa1
      end

      # Fallback
      $(esc(F1))(a) = (@inline; $(esc(Symbol(F)))(a))
  end
end

@FUNT("abs"  )
@FUNT("unit"  )
@FUNT("sqrt"  )
@FUNT("exp"  )
@FUNT("log"  )
@FUNT("sin"  )
@FUNT("cos"  )
@FUNT("tan"  )
@FUNT("cot"  )
@FUNT("sinh"  )
@FUNT("cosh"  )
@FUNT("tanh"  )
@FUNT("coth"  )
@FUNT("asin"  )
@FUNT("acos"  )
@FUNT("atan"  )
@FUNT("acot"  )
@FUNT("asinh")
@FUNT("acosh" )
@FUNT("atanh" )
@FUNT("acoth" )
@FUNT("erf"  )
@FUNT("erfc"  )

# sinc in Julia has different definition than GTPSA
# In Julia: sinc(x) = sin(pi*x)/(pi*x)
# in C GTPSA: sinc(x) = sin(x)/x
# To make sinc agree:
function __t_sinc(t1::TPS{Float64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_scl!(t1, convert(Cdouble, pi), tpsa)
  mad_tpsa_sinc!(tpsa, tpsa)
  return tpsa
end

function __t_sinc(tpsa1::TempTPS{Float64})::TempTPS{Float64}
  mad_tpsa_scl!(tpsa1, convert(Cdouble, pi), tpsa1)
  mad_tpsa_sinc!(tpsa1, tpsa1)
  return tpsa1
end

__t_sinc(a) = (@inline; sinc(a))

# asinc is not in Julia, but in C is asinc(x) = asin(x)*x
# To give similiar behavior, define asinc(x) = asin(pi*x)*(pi⨰x)
function __t_asinc(t1::TPS{Float64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_scl!(t1, convert(Cdouble, pi), tpsa)
  mad_tpsa_asinc!(tpsa, tpsa)
  return tpsa
end

function __t_asinc(tpsa1::TempTPS{Float64})::TempTPS{Float64}
  mad_tpsa_scl!(tpsa1, convert(Cdouble, pi), tpsa1)
  mad_tpsa_asinc!(tpsa1, tpsa1)
  return tpsa1
end

# asinc undefined for not TPSA types (not in Base)

function __t_sinhc(t1::TPS{Float64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_scl!(t1, convert(Cdouble, pi), tpsa)
  mad_tpsa_sinhc!(tpsa, tpsa)
  return tpsa
end

function __t_sinhc(tpsa1::TempTPS{Float64})::TempTPS{Float64}
  mad_tpsa_scl!(tpsa1, convert(Cdouble, pi), tpsa1)
  mad_tpsa_sinhc!(tpsa1, tpsa1)
  return tpsa1
end

# sinhc undefined for not TPSA types (not in Base)

# asinhc is not in Julia, but in C is asinc(x) = asin(x)*x
# To give similiar behavior, define asinc(x) = asin(pi*x)*(pi*x)
function __t_asinhc(t1::TPS{Float64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_scl!(t1, convert(Cdouble, pi), tpsa)
  mad_tpsa_asinhc!(tpsa, tpsa)
  return tpsa
end

function __t_asinhc(tpsa1::TempTPS{Float64})::TempTPS{Float64}
  mad_tpsa_scl!(tpsa1, convert(Cdouble, pi), tpsa1)
  mad_tpsa_asinhc!(tpsa1, tpsa1)
  return tpsa1
end

# asinhc undefined for not TPSA types (not in Base)

# These functions are not implemented in the GTPSA C library, so they 
# are implemented below
function __t_csc(t1::TPS{Float64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_sin!(t1, tpsa)
  mad_tpsa_inv!(tpsa, 1.0, tpsa)
  return tpsa
end

function __t_csc(tpsa1::TempTPS{Float64})::TempTPS{Float64}
  mad_tpsa_sin!(tpsa1, tpsa1)
  mad_tpsa_inv!(tpsa1, 1.0, tpsa1)
  return tpsa1
end

__t_csc(a) = (@inline; csc(a))

function __t_sec(t1::TPS{Float64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_cos!(t1, tpsa)
  mad_tpsa_inv!(tpsa, 1.0, tpsa)
  return tpsa
end

function __t_sec(tpsa1::TempTPS{Float64})::TempTPS{Float64}
  mad_tpsa_cos!(tpsa1, tpsa1)
  mad_tpsa_inv!(tpsa1, 1.0, tpsa1)
  return tpsa1
end

__t_sec(a) = (@inline; sec(a))

function __t_csch(t1::TPS{Float64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_sinh!(t1, tpsa)
  mad_tpsa_inv!(tpsa, 1.0, tpsa)
  return tpsa
end

function __t_csch(tpsa1::TempTPS{Float64})::TempTPS{Float64}
  mad_tpsa_sinh!(tpsa1, tpsa1)
  mad_tpsa_inv!(tpsa1, 1.0, tpsa1)
  return tpsa1
end

__t_csch(a) = (@inline; csch(a))

function __t_sech(t1::TPS{Float64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_cosh!(t1, tpsa)
  mad_tpsa_inv!(tpsa, 1.0, tpsa)
  return tpsa
end

function __t_sech(tpsa1::TempTPS{Float64})::TempTPS{Float64}
  mad_tpsa_cosh!(tpsa1, tpsa1)
  mad_tpsa_inv!(tpsa1, 1.0, tpsa1)
  return tpsa1
end

__t_sech(a) = (@inline; sech(a))

function __t_acsc(t1::TPS{Float64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_inv!(t1, 1.0, tpsa)
  mad_tpsa_asin!(tpsa, tpsa)
  return tpsa
end

function __t_acsc(tpsa1::TempTPS{Float64})::TempTPS{Float64}
  mad_tpsa_inv!(tpsa1, 1.0, tpsa1)
  mad_tpsa_asin!(tpsa1, tpsa1)
  return tpsa1
end

__t_acsc(a) = (@inline; acsc(a))

function __t_asec(t1::TPS{Float64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_inv!(t1, 1.0, tpsa)
  mad_tpsa_acos!(tpsa, tpsa)
  return tpsa
end

function __t_asec(tpsa1::TempTPS{Float64})::TempTPS{Float64}
  mad_tpsa_inv!(tpsa1, 1.0, tpsa1)
  mad_tpsa_acos!(tpsa1, tpsa1)
  return tpsa1
end

__t_asec(a) = (@inline; asec(a))

function __t_acsch(t1::TPS{Float64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_inv!(t1, 1.0, tpsa)
  mad_tpsa_asinh!(tpsa, tpsa)
  return tpsa
end

function __t_acsch(tpsa1::TempTPS{Float64})::TempTPS{Float64}
  mad_tpsa_inv!(tpsa1, 1.0, tpsa1)
  mad_tpsa_asinh!(tpsa1, tpsa1)
  return tpsa1
end

__t_acsch(a) = (@inline; acsch(a))

function __t_asech(t1::TPS{Float64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_inv!(t1, 1.0, tpsa)
  mad_tpsa_acosh!(tpsa, tpsa)
  return tpsa
end

function __t_asech(tpsa1::TempTPS{Float64})::TempTPS{Float64}
  mad_tpsa_inv!(tpsa1, 1.0, tpsa1)
  mad_tpsa_acosh!(tpsa1, tpsa1)
  return tpsa1
end

__t_asech(a) = (@inline; asech(a))

# TPS{ComplexF64}:
# --- rest of unary functions ---
macro FUNCT(F)
  F1 = Symbol("__t_" * F)
  fn = Symbol("mad_ctpsa_" * F * "!")
  quote
      function $(esc(F1))(ct1::TPS{ComplexF64})::TempTPS{ComplexF64}
        ctpsa = TempTPS{ComplexF64}(ct1)
        $(esc(fn))(ct1, ctpsa)
        return ctpsa
      end

      function $(esc(F1))(ctpsa1::TempTPS{ComplexF64})::TempTPS{ComplexF64}
        $(esc(fn))(ctpsa1, ctpsa1)
        return ctpsa1
      end

      # Fallback already defined in previous macro so not needed here
      # (esc(F1))(a) = $(esc(Symbol(F)))(a)
  end
end

function __t_abs(ct1::TPS{ComplexF64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(ct1)
  mad_ctpsa_cabs!(ct1, tpsa)
  return tpsa
end

function __t_abs(ctpsa1::TempTPS{ComplexF64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(ctpsa1)
  mad_ctpsa_cabs!(ctpsa1, tpsa)
  rel_temp!(ctpsa1)
  return tpsa
end

function __t_conj(ct1::TPS{ComplexF64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(ct1)
  mad_ctpsa_conj!(ct1, ctpsa)
  return ctpsa
end

function __t_conj(t1::TPS{Float64})::TPS{Float64}
  return t1
end

function __t_conj(tpsa1::TempTPS{Float64})::TempTPS{Float64}
  return tpsa1
end

__t_conj(a) = (@inline; conj(a))

function __t_angle(ct1::TPS{ComplexF64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(ct1)
  mad_ctpsa_carg!(ct1, tpsa)
  return tpsa
end

function __t_angle(ctpsa1::TempTPS{ComplexF64})::TempTPS{Float64}
  tpsa = TempTPS{Float64}(ctpsa1)
  mad_ctpsa_carg!(ctpsa1, tpsa)
  rel_temp!(ctpsa1)
  return tpsa
end

function __t_angle(t1::TPS{Float64})::TempTPS{Float64}
  ctpsa = TempTPS{ComplexF64}(t1)
  tpsa = TempTPS{Float64}(t1)
  mad_ctpsa_cplx!(t1, Base.unsafe_convert(Ptr{TempTPS{Float64}}, C_NULL), ctpsa)
  mad_ctpsa_carg!(ctpsa, tpsa)
  rel_temp!(ctpsa)
  return tpsa
end

function __t_angle(tpsa1::TempTPS{Float64})::TempTPS{Float64}
  ctpsa = TempTPS{ComplexF64}(tpsa1)
  mad_ctpsa_cplx!(tpsa1, Base.unsafe_convert(Ptr{TempTPS{Float64}}, C_NULL), ctpsa)
  mad_ctpsa_carg!(ctpsa, tpsa1)
  rel_temp!(ctpsa)
  return tpsa1
end

__t_angle(a) = (@inline; angle(a))

function __t_complex(t1::TPS{Float64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(t1)
  mad_ctpsa_cplx!(t1, Base.unsafe_convert(Ptr{TempTPS{Float64}}, C_NULL), ctpsa)
  return ctpsa
end

function __t_complex(tpsa1::TempTPS{Float64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(tpsa1)
  mad_ctpsa_cplx!(tpsa1, Base.unsafe_convert(Ptr{TempTPS{Float64}}, C_NULL), ctpsa)
  rel_temp!(tpsa1)
  return ctpsa
end

function __t_complex(ct1::TPS{ComplexF64})::TPS{ComplexF64}
  return ct1
end

function __t_complex(ctpsa1::TempTPS{ComplexF64})::TempTPS{ComplexF64}
  return ctpsa1
end

__t_complex(a) = (@inline; complex(a))

function __t_complex(t1::TPS{Float64}, t2::TPS{Float64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(t1)
  mad_ctpsa_cplx!(t1, t2, ctpsa)
  return ctpsa
end

function __t_complex(tpsa1::TempTPS{Float64}, t1::TPS{Float64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(t1)
  mad_ctpsa_cplx!(tpsa1, t1, ctpsa)
  rel_temp!(tpsa1)
  return ctpsa
end

function __t_complex(t1::TPS{Float64}, tpsa1::TempTPS{Float64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(t1)
  mad_ctpsa_cplx!(t1, tpsa1, ctpsa)
  rel_temp!(tpsa1)
  return ctpsa
end

function __t_complex(tpsa1::TempTPS{Float64}, tpsa2::TempTPS{Float64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(tpsa1)
  mad_ctpsa_cplx!(tpsa1, tpsa2, ctpsa)
  rel_temp!(tpsa2)
  rel_temp!(tpsa1)
  return ctpsa
end

function __t_complex(t1::TPS{Float64}, a::Real)::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(t1)
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_setval!(tpsa, convert(Float64, a))
  mad_ctpsa_cplx!(t1, tpsa, ctpsa)
  rel_temp!(tpsa)
  return ctpsa
end

function __t_complex(tpsa1::TempTPS{Float64}, a::Real)::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(tpsa1)
  tpsa2 = TempTPS{Float64}(tpsa1)
  mad_tpsa_setval!(tpsa2, convert(Float64, a))
  mad_ctpsa_cplx!(tpsa1, tpsa2, ctpsa)
  rel_temp!(tpsa2)
  return ctpsa
end

function __t_complex(a::Real,t1::TPS{Float64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(t1)
  tpsa = TempTPS{Float64}(t1)
  mad_tpsa_setval!(tpsa, convert(Float64, a))
  mad_ctpsa_cplx!(tpsa, t1, ctpsa)
  rel_temp!(tpsa)
  return ctpsa
end

function __t_complex(a::Real, tpsa1::TempTPS{Float64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(tpsa1)
  tpsa2 = TempTPS{Float64}(tpsa1)
  mad_tpsa_setval!(tpsa2, convert(Float64, a))
  mad_ctpsa_cplx!(tpsa2, tpsa1, ctpsa)
  rel_temp!(tpsa2)
  return ctpsa
end

__t_complex(a,b) = (@inline; complex(a,b))

function __t_polar(ct1::TPS{ComplexF64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(ct1)
  mad_ctpsa_polar!(ct1, ctpsa)
  return ctpsa
end

function __t_polar(ctpsa1::TempTPS{ComplexF64})::TempTPS{ComplexF64}
  mad_ctpsa_polar!(ctpsa1, ctpsa1)
  return ctpsa1
end

function __t_polar(t1::TPS{Float64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(t1)
  mad_ctpsa_cplx!(t1, Base.unsafe_convert(Ptr{TempTPS{Float64}}, C_NULL), ctpsa)
  mad_ctpsa_polar!(ctpsa, ctpsa)
  return ctpsa
end

function __t_polar(tpsa1::TempTPS{Float64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(tpsa1)
  mad_ctpsa_cplx!(tpsa1, Base.unsafe_convert(Ptr{TempTPS{Float64}}, C_NULL), ctpsa)
  rel_temp!(tpsa1)
  mad_ctpsa_polar!(ctpsa, ctpsa)
  return ctpsa
end

__t_polar(a) = (@inline; polar(a))

function __t_rect(ct1::TPS{ComplexF64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(ct1)
  mad_ctpsa_rect!(ct1, ctpsa)
  return ctpsa
end

function __t_rect(ctpsa1::TempTPS{ComplexF64})::TempTPS{ComplexF64}
  mad_ctpsa_rect!(ctpsa1, ctpsa1)
  return ctpsa1
end

function __t_rect(t1::TPS{Float64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(t1)
  mad_ctpsa_cplx!(t1, Base.unsafe_convert(Ptr{TempTPS{Float64}}, C_NULL), ctpsa)
  mad_ctpsa_rect!(ctpsa, ctpsa)
  return ctpsa
end

function __t_rect(tpsa1::TempTPS{Float64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(tpsa1)
  mad_ctpsa_cplx!(tpsa1, Base.unsafe_convert(Ptr{TempTPS{Float64}}, C_NULL), ctpsa)
  rel_temp!(tpsa1)
  mad_ctpsa_rect!(ctpsa, ctpsa)
  return ctpsa
end

__t_rect(a) = (@inline; rect(a))


@FUNCT("unit"  )
@FUNCT("sqrt"  )
@FUNCT("exp"  )
@FUNCT("log"  )
@FUNCT("sin"  )
@FUNCT("cos"  )
@FUNCT("tan"  )
@FUNCT("cot"  )
@FUNCT("sinh"  )
@FUNCT("cosh"  )
@FUNCT("tanh"  )
@FUNCT("coth"  )
@FUNCT("asin"  )
@FUNCT("acos"  )
@FUNCT("atan"  )
@FUNCT("acot"  )
@FUNCT("asinh" )
@FUNCT("acosh" )
@FUNCT("atanh" )
@FUNCT("acoth" )
@FUNCT("erf"  )
@FUNCT("erfc"  )

# sinc in Julia has different definition than GTPSA
# In Julia: sinc(x) = sin(pi*x)/(pi*x)
# in C GTPSA: sinc(x) = sin(x)/x
# To make sinc agree:
function __t_sinc(ct1::TPS{ComplexF64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(ct1)
  mad_ctpsa_scl!(ct1, convert(ComplexF64, pi), ctpsa)
  mad_ctpsa_sinc!(ctpsa, ctpsa)
  return ctpsa
end

function __t_sinc(ctpsa1::TempTPS{ComplexF64})::TempTPS{ComplexF64}
  mad_ctpsa_scl!(ctpsa1, convert(ComplexF64, pi), ctpsa1)
  mad_ctpsa_sinc!(ctpsa1, ctpsa1)
  return ctpsa1
end

# asinc is not in Julia, but in C is asinc(x) = asin(x)*x
# To give similiar behavior, define asinc(x) = asin(pi*x)*(pi⨰x)
function __t_asinc(ct1::TPS{ComplexF64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(ct1)
  mad_ctpsa_scl!(ct1, convert(ComplexF64, pi), ctpsa)
  mad_ctpsa_asinc!(ctpsa, ctpsa)
  return ctpsa
end

function __t_asinc(ctpsa1::TempTPS{ComplexF64})::TempTPS{ComplexF64}
  mad_ctpsa_scl!(ctpsa1, convert(ComplexF64, pi), ctpsa1)
  mad_ctpsa_asinc!(ctpsa1, ctpsa1)
  return ctpsa1
end

# asinc undefined for not TPSA types (not in Base)

function __t_sinhc(ct1::TPS{ComplexF64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(ct1)
  mad_ctpsa_scl!(ct1, convert(ComplexF64, pi), ctpsa)
  mad_ctpsa_sinhc!(ctpsa, ctpsa)
  return ctpsa
end

function __t_sinhc(ctpsa1::TempTPS{ComplexF64})::TempTPS{ComplexF64}
  mad_ctpsa_scl!(ctpsa1, convert(ComplexF64, pi), ctpsa1)
  mad_ctpsa_sinhc!(ctpsa1, ctpsa1)
  return ctpsa1
end

# sinhc undefined for not TPSA types (not in Base)

# asinhc is not in Julia, but in C is asinc(x) = asin(x)*x
# To give similiar behavior, define asinc(x) = asin(pi*x)*(pi*x)
function __t_asinhc(ct1::TPS{ComplexF64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(ct1)
  mad_ctpsa_scl!(ct1, convert(ComplexF64, pi), ctpsa)
  mad_ctpsa_asinhc!(ctpsa, ctpsa)
  return ctpsa
end

function __t_asinhc(ctpsa1::TempTPS{ComplexF64})::TempTPS{ComplexF64}
  mad_ctpsa_scl!(ctpsa1, convert(ComplexF64, pi), ctpsa1)
  mad_ctpsa_asinhc!(ctpsa1, ctpsa1)
  return ctpsa1
end

# asinhc undefined for not TPSA types (not in Base)

# These functions are not implemented in the GTPSA C library, so they 
# are implemented below
function __t_csc(ct1::TPS{ComplexF64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(ct1)
  mad_ctpsa_sin!(ct1, ctpsa)
  mad_ctpsa_inv!(ctpsa, convert(ComplexF64, 1.0), ctpsa)
  return ctpsa
end

function __t_csc(ctpsa1::TempTPS{ComplexF64})::TempTPS{ComplexF64}
  mad_ctpsa_sin!(ctpsa1, ctpsa1)
  mad_ctpsa_inv!(ctpsa1, convert(ComplexF64, 1.0), ctpsa1)
  return ctpsa1
end

function __t_sec(ct1::TPS{ComplexF64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(ct1)
  mad_ctpsa_cos!(ct1, ctpsa)
  mad_ctpsa_inv!(ctpsa, convert(ComplexF64, 1.0), ctpsa)
  return ctpsa
end

function __t_sec(ctpsa1::TempTPS{ComplexF64})::TempTPS{ComplexF64}
  mad_ctpsa_cos!(ctpsa1, ctpsa1)
  mad_ctpsa_inv!(ctpsa1, convert(ComplexF64, 1.0), ctpsa1)
  return ctpsa1
end

function __t_csch(ct1::TPS{ComplexF64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(ct1)
  mad_ctpsa_sinh!(ct1, ctpsa)
  mad_ctpsa_inv!(ctpsa, convert(ComplexF64, 1.0), ctpsa)
  return ctpsa
end

function __t_csch(ctpsa1::TempTPS{ComplexF64})::TempTPS{ComplexF64}
  mad_ctpsa_sinh!(ctpsa1, ctpsa1)
  mad_ctpsa_inv!(ctpsa1, convert(ComplexF64, 1.0), ctpsa1)
  return ctpsa1
end

function __t_sech(ct1::TPS{ComplexF64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(ct1)
  mad_ctpsa_cosh!(ct1, ctpsa)
  mad_ctpsa_inv!(ctpsa, convert(ComplexF64, 1.0), ctpsa)
  return ctpsa
end

function __t_sech(ctpsa1::TempTPS{ComplexF64})::TempTPS{ComplexF64}
  mad_ctpsa_cosh!(ctpsa1, ctpsa1)
  mad_ctpsa_inv!(ctpsa1, convert(ComplexF64, 1.0), ctpsa1)
  return ctpsa1
end

function __t_acsc(ct1::TPS{ComplexF64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(ct1)
  mad_ctpsa_inv!(ct1, convert(ComplexF64, 1.0), ctpsa)
  mad_ctpsa_asin!(ctpsa, ctpsa)
  return ctpsa
end

function __t_acsc(ctpsa1::TempTPS{ComplexF64})::TempTPS{ComplexF64}
  mad_ctpsa_inv!(ctpsa1, convert(ComplexF64, 1.0), ctpsa1)
  mad_ctpsa_asin!(ctpsa1, ctpsa1)
  return ctpsa1
end

function __t_asec(ct1::TPS{ComplexF64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(ct1)
  mad_ctpsa_inv!(ct1, convert(ComplexF64, 1.0), ctpsa)
  mad_ctpsa_acos!(ctpsa, ctpsa)
  return ctpsa
end

function __t_asec(ctpsa1::TempTPS{ComplexF64})::TempTPS{ComplexF64}
  mad_ctpsa_inv!(ctpsa1, convert(ComplexF64, 1.0), ctpsa1)
  mad_ctpsa_acos!(ctpsa1, ctpsa1)
  return ctpsa1
end

function __t_acsch(ct1::TPS{ComplexF64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(ct1)
  mad_ctpsa_inv!(ct1, convert(ComplexF64, 1.0), ctpsa)
  mad_ctpsa_asinh!(ctpsa, ctpsa)
  return ctpsa
end

function __t_acsch(ctpsa1::TempTPS{ComplexF64})::TempTPS{ComplexF64}
  mad_ctpsa_inv!(ctpsa1, convert(ComplexF64, 1.0), ctpsa1)
  mad_ctpsa_asinh!(ctpsa1, ctpsa1)
  return ctpsa1
end

function __t_asech(ct1::TPS{ComplexF64})::TempTPS{ComplexF64}
  ctpsa = TempTPS{ComplexF64}(ct1)
  mad_ctpsa_inv!(ct1, convert(ComplexF64, 1.0), ctpsa)
  mad_ctpsa_acosh!(ctpsa, ctpsa)
  return ctpsa
end

function __t_asech(ctpsa1::TempTPS{ComplexF64})::TempTPS{ComplexF64}
  mad_ctpsa_inv!(ctpsa1, convert(ComplexF64, 1.0), ctpsa1)
  mad_ctpsa_acosh!(ctpsa1, ctpsa1)
  return ctpsa1
end

__t_hypot(a,b) = (@inline; hypot(a,b))
__t_hypot(a,b,c) = (@inline; hypot(a,b,c))
=#