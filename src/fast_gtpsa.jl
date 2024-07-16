"""
    FastGTPSA(expr)

Macro speed up evaluation of mathematical expressions containing TPSs.
@FastGTPSA is completely transparent to all other types, so it can be prepended 
to expressions while still maintaining type-generic code.

# Example
```julia-repl
julia> using GTPSA, BenchmarkTools

julia> d = Descriptor(3,5); x = vars(d);

julia> @btime \$x[1]^3*sin(\$x[2])/log(2+\$x[3])-exp(\$x[1]*\$x[2])*im;
  2.114 μs (10 allocations: 160 bytes)

julia> @btime @FastGTPSA \$x[1]^3*sin(\$x[2])/log(2+\$x[3])-exp(\$x[1]*\$x[2])*im;
  1.744 μs (1 allocation: 16 bytes)
```
"""
macro FastGTPSA(expr)
  return :(to_TPS($(to_temp_form(esc(expr)))))
end 

function to_TPS(tpsa::Ptr{TPS{Float64}})::TPS{Float64}
  t = TPS{Float64}(mad_tpsa_desc(tpsa), MAD_TPSA_DEFAULT)
  mad_tpsa_copy!(tpsa,t)
  rel_temp!(tpsa)
  return t
end

function to_TPS(ctpsa::Ptr{TPS{ComplexF64}})::TPS{ComplexF64}
  ct = TPS{ComplexF64}(mad_ctpsa_desc(ctpsa), MAD_TPSA_DEFAULT)
  mad_ctpsa_copy!(ctpsa,ct)
  rel_temp!(ctpsa)
  return ct
end

# Fallback for non-TPSA types
to_TPS(a) = (@inline; a)

function to_temp_form(expr)
  fcns = [:inv, :atan, :abs, :sqrt, :exp, :log, :sin, :cos, :tan, :csc, :sec, :cot, :sinc, :sinh, :cosh,
          :tanh, :csch, :sech, :coth, :asin, :acos, :atan, :acsc, :asec, :acot, :asinh, :acosh, :atanh, :acsch, 
          :asech, :acoth, :real, :imag, :conj, :angle, :complex, :sinhc, :asinc, :asinhc, :erf, :erfc, :norm,
          :polar, :rect, :+, :-, :*, :/, :^, :zero, :one]
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

get_rtemp!(t1::Union{TPS{ComplexF64},TPS{Float64}}) = get_rtemp_low!(unsafe_load(getdesc(t1).desc))
function get_rtemp!(t1::Ptr{TPS{Float64}})
  desc = mad_tpsa_desc(t1)
  get_rtemp_low!(unsafe_load(desc))
end
function get_rtemp!(t1::Ptr{TPS{ComplexF64}})
  desc = mad_ctpsa_desc(t1)
  get_rtemp_low!(unsafe_load(desc))
end

function get_rtemp_low!(desc::Desc)::Ptr{TPS{Float64}}
  tmpidx = unsafe_load(desc.ti, Threads.threadid())
  if tmpidx == DESC_MAX_TMP
    # Run out of temporaries... no choice but to throw error 
    # Release this thread's temporaries and give warning to run cleartemps!()
    unsafe_store!(desc.cti, Cint(0), Threads.threadid())
    error("Permanent temporaries buffer out of memory (max $DESC_MAX_TMP). To use @FastGTPSA, please split expression into subexpressions, and if this Julia run is not terminated, GTPSA.cleartemps!(d::Descriptor=GTPSA.desc_current) must be executed.")
  end
  idx = (Threads.threadid()-1)*DESC_MAX_TMP+tmpidx+1
  #println("threadid = ", Threads.threadid(), ", getting temp t[", idx-1,"], incrementing ti[", Threads.threadid()-1, "] = ", tmpidx, "->", tmpidx+1)
  t = unsafe_load(Base.unsafe_convert(Ptr{Ptr{TPS{Float64}}}, desc.t), idx)
  #@assert unsafe_load(Base.unsafe_convert(Ptr{Ptr{TPS{Float64}}}, desc.t), idx) == t
  # Increment tmp idx in Descriptor
  unsafe_store!(desc.ti, tmpidx+Cint(1), Threads.threadid())
  return t
end


get_ctemp!(t1::Union{TPS{ComplexF64},TPS{Float64}}) = get_ctemp_low!(unsafe_load(getdesc(t1).desc))
function get_ctemp!(t1::Ptr{TPS{Float64}})
  desc = mad_tpsa_desc(t1)
  get_ctemp_low!(unsafe_load(desc))
end
function get_ctemp!(t1::Ptr{TPS{ComplexF64}})
  desc = mad_ctpsa_desc(t1)
  get_ctemp_low!(unsafe_load(desc))
end

function get_ctemp_low!(desc::Desc)::Ptr{TPS{ComplexF64}}
  tmpidx = unsafe_load(desc.cti, Threads.threadid())
  if tmpidx == DESC_MAX_TMP
    # Run out of temporaries... no choice but to throw error
    unsafe_store!(desc.cti, Cint(0), Threads.threadid())
    error("Permanent temporaries buffer out of memory (max $DESC_MAX_TMP). To use @FastGTPSA, please split expression into subexpressions, and if this Julia run is not terminated, GTPSA.cleartemps!(d::Descriptor=GTPSA.desc_current) must be executed.")
  end
  idx = (Threads.threadid()-1)*DESC_MAX_TMP+tmpidx+1
  ct = unsafe_load(Base.unsafe_convert(Ptr{Ptr{TPS{ComplexF64}}}, desc.ct), idx)
  # Increment tmp idx in Descriptor
  unsafe_store!(desc.cti, tmpidx+Cint(1), Threads.threadid())
  return ct
end

function rel_temp!(tpsa::Ptr{TPS{Float64}})
  desc = unsafe_load(mad_tpsa_desc(tpsa))
  tmpidx = unsafe_load(desc.ti, Threads.threadid())
  #println("decrementing ti[", Threads.threadid()-1, "] = ", tmpidx, "->", tmpidx-1)
  # Decrement tmp idx in Descriptor
  #idx = (Threads.threadid()-1)*DESC_MAX_TMP+tmpidx
  #println(idx)
  #println(unsafe_load(Base.unsafe_convert(Ptr{Ptr{TPS{Float64}}}, desc.t), idx), " ?= ", tpsa)
 # @assert unsafe_load(Base.unsafe_convert(Ptr{Ptr{TPS{Float64}}}, desc.t), idx) == tpsa
  
  unsafe_store!(desc.ti, tmpidx-Cint(1), Threads.threadid())
  return
end

function rel_temp!(ctpsa::Ptr{TPS{ComplexF64}})
  desc = unsafe_load(mad_ctpsa_desc(ctpsa))
  tmpidx = unsafe_load(desc.cti, Threads.threadid())
  # Decrement tmp idx in Descriptor
  unsafe_store!(desc.cti, tmpidx-Cint(1), Threads.threadid())
  return
end

function cleartemps!(d::Descriptor=GTPSA.desc_current)
  desc = unsafe_load(d.desc)
  for i = 1:Threads.nthreads()
    unsafe_store!(desc.ti, Cint(0), i)
    unsafe_store!(desc.cti, Cint(0), i)
  end
  return
end


function checktemps(d::Descriptor=GTPSA.desc_current)
  desc = unsafe_load(d.desc)
  for i=1:desc.nth
    unsafe_load(desc.ti, i) == 0 || return false
    unsafe_load(desc.cti, i) == 0 || return false
  end
  return true
end

# --- Unary ---
# TPS{Float64}:
function ±(t1::TPS{Float64})::TPS{Float64}
  return t1
end

function ±(tpsa::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  return tpsa
end

function ∓(t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_scl!(t1, -1., tpsa)
  return tpsa
end

function ∓(tpsa::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_scl!(tpsa, -1., tpsa)
  return tpsa 
end

# TPS{ComplexF64}:
function ±(ct1::TPS{ComplexF64})::TPS{ComplexF64}
  return ct1
end

function ±(ctpsa::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  return ctpsa
end

function ∓(ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_scl!(ct1, convert(ComplexF64, -1), ctpsa)
  return ctpsa
end

function ∓(ctpsa::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_scl!(ctpsa, convert(ComplexF64, -1), ctpsa)
  return ctpsa
end

# -- zero --

function __t_zero(ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  tpsa = get_ctemp!(ct1)
  mad_ctpsa_clear!(tpsa)
  return tpsa
end

function __t_zero(ct1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_clear!(ct1)
  return ct1
end

function __t_zero(t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_clear!(tpsa)
  return tpsa
end

function __t_zero(t1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_clear!(t1)
  return t1
end


# -- one ---
function __t_one(ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  tpsa = get_ctemp!(ct1)
  mad_ctpsa_clear!(tpsa)
  mad_ctpsa_seti!(tpsa, Cint(0), ComplexF64(0), ComplexF64(1))
  return tpsa
end

function __t_one(tpsa::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_clear!(tpsa)
  mad_ctpsa_seti!(tpsa, Cint(0), ComplexF64(0), ComplexF64(1))
  return tpsa
end

function __t_one(t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_clear!(tpsa)
  mad_tpsa_seti!(tpsa, Cint(0), Float64(0), Float64(1))
  return tpsa
end

function __t_one(tpsa::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_clear!(tpsa)
  mad_tpsa_seti!(tpsa, Cint(0), Float64(0), Float64(1))
  return tpsa
end


function __t_real(ct1::TPS{ComplexF64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(ct1)
  mad_ctpsa_real!(ct1, tpsa)
  return tpsa
end

function __t_imag(ct1::TPS{ComplexF64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(ct1)
  mad_ctpsa_imag!(ct1, tpsa)
  return tpsa
end

function __t_real(t1::TPS{Float64})::TPS{Float64}
  return t1
end

function __t_imag(t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_setval!(tpsa, 0.0)
  return tpsa
end

# Temps:

function __t_real(ctpsa::Ptr{TPS{ComplexF64}})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(ctpsa)
  mad_ctpsa_real!(ctpsa, tpsa)
  rel_temp!(ctpsa)
  return tpsa
end

function __t_imag(ctpsa::Ptr{TPS{ComplexF64}})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(ctpsa)
  mad_ctpsa_imag!(ctpsa, tpsa)
  rel_temp!(ctpsa)
  return tpsa
end

function __t_real(tpsa::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  return tpsa
end

function __t_imag(tpsa::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
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

# --- add ---
# TPS{Float64}:
function ±(t1::TPS{Float64}, t2::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_add!(t1, t2, tpsa)
  return tpsa
end

function ±(t1::TPS{Float64}, a::Real)::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_copy!(t1, tpsa)
  mad_tpsa_seti!(tpsa, Cint(0),1., convert(Float64,a))
  return tpsa
end

function ±(a::Real, t1::TPS{Float64})::Ptr{TPS{Float64}}
  return t1 ± a
end

function ±(tpsa1::Ptr{TPS{Float64}}, tpsa2::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_add!(tpsa1, tpsa2, tpsa1)
  rel_temp!(tpsa2)
  return tpsa1
end

function ±(t1::TPS{Float64}, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_add!(t1, tpsa1, tpsa1)
  return tpsa1
end

function ±(tpsa1::Ptr{TPS{Float64}},t1::TPS{Float64})::Ptr{TPS{Float64}}
  return t1 ± tpsa1
end

function ±(tpsa1::Ptr{TPS{Float64}}, a::Real)::Ptr{TPS{Float64}}
  mad_tpsa_seti!(tpsa1, Cint(0), 1., convert(Float64,a))
  return tpsa1
end

function ±(a::Real, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  return tpsa1 ± a
end

# TPS{ComplexF64}:
function ±(ct1::TPS{ComplexF64}, ct2::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_add!(ct1, ct2, ctpsa)
  return ctpsa
end

function ±(ct1::TPS{ComplexF64}, a::Number)::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_copy!(ct1, ctpsa)
  mad_ctpsa_seti!(ctpsa, Cint(0), convert(ComplexF64,1), convert(ComplexF64,a))
  return ctpsa
end

function ±(a::Number, ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  return ct1 ± a
end

function ±(ctpsa1::Ptr{TPS{ComplexF64}}, ctpsa2::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_add!(ctpsa1, ctpsa2, ctpsa1)
  rel_temp!(ctpsa2)
  return ctpsa1
end

function ±(ct1::TPS{ComplexF64}, ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_add!(ct1, ctpsa1, ctpsa1)
  return ctpsa1
end

function ±(ctpsa1::Ptr{TPS{ComplexF64}}, ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  return  ct1 ± ctpsa1
end

function ±(ctpsa1::Ptr{TPS{ComplexF64}}, a::Number)::Ptr{TPS{ComplexF64}}
  mad_ctpsa_seti!(ctpsa1, Cint(0), convert(ComplexF64,1), convert(ComplexF64,a))
  return ctpsa1
end

function ±(a::Number, ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  return ctpsa1 ± a
end


# TPS{Float64} to TPS{ComplexF64} promotion:
function ±(ct1::TPS{ComplexF64}, t1::TPS{Float64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_addt!(ct1, t1, ctpsa)
  return ctpsa
end

function ±(t1::TPS{Float64}, ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  return ct1 ± t1
end

function ±(t1::TPS{Float64}, a::Complex)::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(t1)
  mad_ctpsa_cplx!(t1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  mad_ctpsa_seti!(ctpsa, Cint(0),convert(ComplexF64, 1), convert(ComplexF64, a))
  return ctpsa
end

function ±(a::Complex, t1::TPS{Float64})::Ptr{TPS{ComplexF64}}
  return t1 ± a
end

function ±(tpsa1::Ptr{TPS{Float64}}, a::Complex)::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(tpsa1)
  mad_ctpsa_cplx!(tpsa1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  mad_ctpsa_seti!(ctpsa, Cint(0),convert(ComplexF64, 1), convert(ComplexF64, a))
  rel_rtemp!(tpsa1)
  return ctpsa
end

function ±(a::Complex, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{ComplexF64}}
  return tpsa1 ± a
end

function ±(ctpsa1::Ptr{TPS{ComplexF64}}, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_addt!(ctpsa1, tpsa1, ctpsa1)
  rel_temp!(tpsa1)
  return ctpsa1
end

function ±(tpsa1::Ptr{TPS{Float64}},ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  return ctpsa1 ± tpsa1
end

function ±(ctpsa1::Ptr{TPS{ComplexF64}}, t1::TPS{Float64})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_addt!(ctpsa1, t1, ctpsa1)
  return ctpsa1
end

function ±(t1::TPS{Float64},ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  return  ctpsa1 ± t1
end

# All other types should just be +
±(a, b) =(@inline; +(a,b))
±(a, b, c, xs...) = (@inline; Base.afoldl(±, (±)((±)(a,b),c), xs...))


# --- sub ---
# TPS{Float64}:
function ∓(t1::TPS{Float64}, t2::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_sub!(t1, t2, tpsa)
  return tpsa
end

function ∓(t1::TPS{Float64}, a::Real)::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_copy!(t1, tpsa)
  mad_tpsa_seti!(tpsa, Cint(0), 1., convert(Float64, -a))
  return tpsa
end

function ∓(a::Real, t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_scl!(t1, -1., tpsa)
  mad_tpsa_seti!(tpsa, Cint(0), 1., convert(Float64, a))
  return tpsa
end

function ∓(tpsa1::Ptr{TPS{Float64}}, tpsa2::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_sub!(tpsa1, tpsa2, tpsa1)
  rel_temp!(tpsa2)
  return tpsa1
end

function ∓(t1::TPS{Float64}, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_sub!(t1, tpsa1, tpsa1)
  return tpsa1
end

function ∓(tpsa1::Ptr{TPS{Float64}}, t1::TPS{Float64})::Ptr{TPS{Float64}}
  mad_tpsa_sub!(tpsa1, t1, tpsa1)
  return tpsa1
end

function ∓(tpsa1::Ptr{TPS{Float64}}, a::Real)::Ptr{TPS{Float64}}
  mad_tpsa_seti!(tpsa1, Cint(0), 1., convert(Float64, -a))
  return tpsa1
end

function ∓(a::Real, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_scl!(tpsa1, -1., tpsa1)
  mad_tpsa_seti!(tpsa1, Cint(0), 1., convert(Float64, a))
  return tpsa1
end

# TPS{ComplexF64}:
function ∓(ct1::TPS{ComplexF64}, ct2::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_sub!(ct1, ct2, ctpsa)
  return ctpsa
end

function ∓(ct1::TPS{ComplexF64}, a::Number)::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_copy!(ct1, ctpsa)
  mad_ctpsa_seti!(ctpsa, Cint(0), convert(ComplexF64, 1.), convert(ComplexF64, -a))
  return ctpsa
end

function ∓(a::Number, ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_scl!(ct1, convert(ComplexF64,-1.), ctpsa)
  mad_ctpsa_seti!(ctpsa, Cint(0), convert(ComplexF64,1.), convert(ComplexF64, a))
  return ctpsa
end

function ∓(ctpsa1::Ptr{TPS{ComplexF64}}, ctpsa2::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_sub!(ctpsa1, ctpsa2, ctpsa1)
  rel_temp!(ctpsa2)
  return ctpsa1
end

function ∓(ct1::TPS{ComplexF64}, ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_sub!(ct1, ctpsa1, ctpsa1)
  return ctpsa1
end

function ∓(ctpsa1::Ptr{TPS{ComplexF64}}, ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_sub!(ctpsa1, ct1, ctpsa1)
  return ctpsa1
end

function ∓(ctpsa1::Ptr{TPS{ComplexF64}}, a::Number)::Ptr{TPS{ComplexF64}}
  mad_ctpsa_seti!(ctpsa1, Cint(0), convert(ComplexF64,1.), convert(ComplexF64, -a))
  return ctpsa1
end

function ∓(a::Number, ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_scl!(ctpsa1, convert(ComplexF64, -1.), ctpsa1)
  mad_ctpsa_seti!(ctpsa1, Cint(0), convert(ComplexF64, 1.), convert(ComplexF64, a))
  return ctpsa1
end

# TPS{Float64} to TPS{ComplexF64} promotion:
function ∓(ct1::TPS{ComplexF64}, t1::TPS{Float64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_subt!(ct1, t1, ctpsa)
  return ctpsa
end

function ∓(t1::TPS{Float64}, ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_tsub!(t1, ct1, ctpsa)
  return ctpsa
end

function ∓(t1::TPS{Float64}, a::Complex)::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(t1)
  mad_ctpsa_cplx!(t1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  mad_ctpsa_seti!(ctpsa, Cint(0), convert(ComplexF64, 1), convert(ComplexF64, -a))
  return ctpsa
end

function ∓(a::Complex, t1::TPS{Float64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(t1)
  mad_ctpsa_cplx!(t1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  mad_ctpsa_scl!(ctpsa, convert(ComplexF64, -1), ctpsa)
  mad_ctpsa_seti!(ctpsa,Cint(0), convert(ComplexF64, 1), convert(ComplexF64,a))
  return ctpsa
end

function ∓(ctpsa1::Ptr{TPS{ComplexF64}}, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_subt!(ctpsa1, tpsa1, ctpsa1)
  rel_temp!(tpsa1)
  return ctpsa1
end

function ∓(tpsa1::Ptr{TPS{Float64}}, ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_tsub!(tpsa1, ctpsa1, ctpsa1)
  rel_temp!(tpsa1)
  return ctpsa1
end

function ∓(tpsa1::Ptr{TPS{Float64}}, a::Complex)::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(tpsa1)
  mad_ctpsa_cplx!(tpsa1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  rel_temp!(tpsa1)
  mad_ctpsa_seti!(ctpsa, Cint(0), convert(ComplexF64, 1), convert(ComplexF64, -a))
  return ctpsa
end

function ∓(a::Complex, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(tpsa1)
  mad_ctpsa_cplx!(tpsa1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  rel_temp!(tpsa1)
  mad_ctpsa_scl!(ctpsa, convert(ComplexF64, -1), ctpsa)
  mad_ctpsa_seti!(ctpsa, Cint(0), convert(ComplexF64, 1), convert(ComplexF64,a))
  return ctpsa
end
function ∓(ctpsa1::Ptr{TPS{ComplexF64}}, t1::TPS{Float64})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_subt!(ctpsa1, t1, ctpsa1)
  return ctpsa1
end

function ∓(t1::TPS{Float64}, ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_tsub!(t1, ctpsa1, ctpsa1)
  return ctpsa1
end

# All other types should just be -
∓(a, b) = (@inline; -(a,b))
# afoldl only for +,*

# --- mul ---
# TPS{Float64}:
function ⨰(t1::TPS{Float64}, t2::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_mul!(t1, t2, tpsa) 
  return tpsa
end

function ⨰(t1::TPS{Float64}, a::Real)::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_scl!(t1, convert(Float64, a), tpsa)
  return tpsa
end

function ⨰(a::Real, t1::TPS{Float64})::Ptr{TPS{Float64}}
  return t1 ⨰ a
end

function ⨰(tpsa1::Ptr{TPS{Float64}}, tpsa2::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_mul!(tpsa1, tpsa2, tpsa1)
  rel_temp!(tpsa2) 
  return tpsa1
end

function ⨰(t1::TPS{Float64}, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_mul!(t1, tpsa1, tpsa1)
  return tpsa1
end

function ⨰(tpsa1::Ptr{TPS{Float64}}, t1::TPS{Float64})::Ptr{TPS{Float64}}
  return t1 ⨰ tpsa1
end

function ⨰(tpsa1::Ptr{TPS{Float64}}, a::Real)::Ptr{TPS{Float64}}
  mad_tpsa_scl!(tpsa1, convert(Float64, a), tpsa1)
  return tpsa1
end

function ⨰(a::Real, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  return tpsa1 ⨰ a
end

# TPS{ComplexF64}:
function ⨰(ct1::TPS{ComplexF64}, ct2::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_mul!(ct1, ct2, ctpsa) 
  return ctpsa
end

function ⨰(ct1::TPS{ComplexF64}, a::Number)::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_scl!(ct1, convert(ComplexF64, a), ctpsa)
  return ctpsa
end

function ⨰(a::Number, ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  return ct1 ⨰ a
end

function ⨰(ctpsa1::Ptr{TPS{ComplexF64}}, ctpsa2::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_mul!(ctpsa1, ctpsa2, ctpsa1)
  rel_temp!(ctpsa2) 
  return ctpsa1
end

function ⨰(ct1::TPS{ComplexF64}, ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_mul!(ct1, ctpsa1, ctpsa1)
  return ctpsa1
end

function ⨰(ctpsa1::Ptr{TPS{ComplexF64}}, ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  return ct1 ⨰ ctpsa1
end

function ⨰(ctpsa1::Ptr{TPS{ComplexF64}}, a::Number)::Ptr{TPS{ComplexF64}}
  mad_ctpsa_scl!(ctpsa1, convert(ComplexF64, a), ctpsa1)
  return ctpsa1
end

function ⨰(a::Number, ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  return ctpsa1 ⨰ a
end

# TPS{Float64} to TPS{ComplexF64} promotion:
function ⨰(ct1::TPS{ComplexF64}, t1::TPS{Float64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_mult!(ct1, t1, ctpsa)
  return ctpsa
end

function ⨰(t1::TPS{Float64}, ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  return ct1 ⨰ t1
end

function ⨰(t1::TPS{Float64}, a::Complex)::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(t1)
  mad_ctpsa_cplx!(t1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  mad_ctpsa_scl!(ctpsa, convert(ComplexF64,a), ctpsa)
  return ctpsa
end

function ⨰(a::Complex, t1::TPS{Float64})::Ptr{TPS{ComplexF64}}
  return t1 ⨰ a
end

function ⨰(ctpsa1::Ptr{TPS{ComplexF64}}, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_mult!(ctpsa1, tpsa1, ctpsa1)
  rel_temp!(tpsa1)
  return ctpsa1
end

function ⨰(tpsa1::Ptr{TPS{Float64}}, ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  return ctpsa1 ⨰ tpsa1
end

function ⨰(tpsa1::Ptr{TPS{Float64}}, a::Complex)::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(tpsa1)
  mad_ctpsa_cplx!(tpsa1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  rel_temp!(tpsa1)
  mad_ctpsa_scl!(ctpsa, convert(ComplexF64,a), ctpsa)
  return ctpsa
end

function ⨰(a::Complex, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{ComplexF64}}
  return tpsa1 ⨰ a
end

function ⨰(ctpsa1::Ptr{TPS{ComplexF64}}, t1::TPS{Float64})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_mult!(ctpsa1, t1, ctpsa1)
  return ctpsa1
end

function ⨰(t1::TPS{Float64}, ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  return ctpsa1 ⨰ t1
end

# Fallbacks
# All other types should just be *
⨰(a, b) = (@inline; *(a,b))
⨰(a, b, c, xs...) = (@inline; Base.afoldl(⨰, (⨰)((⨰)(a,b),c), xs...))

# --- div ---
# TPS{Float64}:
function ⨱(t1::TPS{Float64}, t2::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_div!(t1, t2, tpsa)
  return tpsa
end

function ⨱(t1::TPS{Float64}, a::Real)::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_scl!(t1, convert(Float64, 1/a), tpsa)
  return tpsa
end

function ⨱(a::Real, t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_inv!(t1, convert(Float64,a), tpsa)
  return tpsa
end

function ⨱(tpsa1::Ptr{TPS{Float64}}, tpsa2::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_div!(tpsa1, tpsa2, tpsa1)
  rel_temp!(tpsa2)
  return tpsa1
end

function ⨱(tpsa1::Ptr{TPS{Float64}}, a::Real)::Ptr{TPS{Float64}}
  mad_tpsa_scl!(tpsa1, convert(Float64, 1/a), tpsa1)
  return tpsa1
end

function ⨱(a::Real, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_inv!(tpsa1, convert(Float64, a), tpsa1)
  return tpsa1
end

function ⨱(tpsa1::Ptr{TPS{Float64}}, t1::TPS{Float64})::Ptr{TPS{Float64}}
  mad_tpsa_div!(tpsa1, t1, tpsa1)
  return tpsa1
end

function ⨱(t1::TPS{Float64},tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_div!(t1, tpsa1, tpsa1)
  return tpsa1
end


# TPS{ComplexF64}:
function ⨱(ct1::TPS{ComplexF64}, ct2::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_div!(ct1, ct2, ctpsa)
  return ctpsa
end

function ⨱(ct1::TPS{ComplexF64}, a::Number)::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_scl!(ct1, convert(ComplexF64, 1/a), ctpsa)
  return ctpsa
end

function ⨱(a::Number, ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_inv!(ct1, convert(ComplexF64,a), ctpsa)
  return ctpsa
end

function ⨱(ctpsa1::Ptr{TPS{ComplexF64}}, ctpsa2::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_div!(ctpsa1, ctpsa2, ctpsa1)
  rel_temp!(ctpsa2)
  return ctpsa1
end

function ⨱(ctpsa1::Ptr{TPS{ComplexF64}}, a::Number)::Ptr{TPS{ComplexF64}}
  mad_ctpsa_scl!(ctpsa1, convert(ComplexF64, 1/a), ctpsa1)
  return ctpsa1
end

function ⨱(a::Number, ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_inv!(ctpsa1, convert(ComplexF64, a), ctpsa1)
  return ctpsa1
end

function ⨱(ctpsa1::Ptr{TPS{ComplexF64}}, ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_div!(ctpsa1, ct1, ctpsa1)
  return ctpsa1
end

function ⨱(ct1::TPS{ComplexF64},ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_div!(ct1, ctpsa1, ctpsa1)
  return ctpsa1
end

# TPS{Float64} to TPS{ComplexF64} promotion:
function ⨱(ct1::TPS{ComplexF64}, t1::TPS{Float64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_divt!(ct1, t1, ctpsa)
  return ctpsa
end

function ⨱(t1::TPS{Float64}, ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_tdiv!(t1, ct1, ctpsa)
  return ctpsa
end

function ⨱(t1::TPS{Float64}, a::Complex)::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(t1)
  mad_ctpsa_cplx!(t1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  mad_ctpsa_scl!(ctpsa, convert(ComplexF64, 1/a), ctpsa)
  return ctpsa
end

function ⨱(a::Complex, t1::TPS{Float64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(t1)
  mad_ctpsa_cplx!(t1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  mad_ctpsa_inv!(ctpsa, convert(ComplexF64, a), ctpsa)
  return ctpsa
end

function ⨱(ctpsa1::Ptr{TPS{ComplexF64}}, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_divt!(ctpsa1, tpsa1, ctpsa1)
  rel_temp!(tpsa1)
  return ctpsa1
end

function ⨱(tpsa1::Ptr{TPS{Float64}}, ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_tdiv!(tpsa1, ctpsa1, ctpsa1)
  rel_temp!(tpsa1)
  return ctpsa1
end

function ⨱(tpsa1::Ptr{TPS{Float64}}, a::Complex)::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(tpsa1)
  mad_ctpsa_cplx!(tpsa1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  rel_temp!(tpsa1)
  mad_ctpsa_scl!(ctpsa, convert(ComplexF64, 1/a), ctpsa)
  return ctpsa
end

function ⨱(a::Complex, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(tpsa1)
  mad_ctpsa_cplx!(tpsa1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  rel_temp!(tpsa1)
  mad_ctpsa_inv!(ctpsa, convert(ComplexF64, a), ctpsa)
  return ctpsa
end

function ⨱(ctpsa1::Ptr{TPS{ComplexF64}}, t1::TPS{Float64})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_divt!(ctpsa1, t1, ctpsa1)
  return ctpsa1
end

function ⨱(t1::TPS{Float64}, ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_tdiv!(t1, ctpsa1, ctpsa1)
  return ctpsa1
end


# Fallbacks
# All other types should just be /
⨱(a, b) = (@inline; /(a,b))

# --- pow ---
# TPS{Float64}:
function ⤊(t1::TPS{Float64}, t2::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_pow!(t1, t2, tpsa)
  return tpsa
end

function ⤊(t1::TPS{Float64}, i::Integer)::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_powi!(t1, convert(Cint, i), tpsa)
  return tpsa
end

function ⤊(t1::TPS{Float64}, a::Real)::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_pown!(t1, convert(Float64, a), tpsa)
  return tpsa
end

function ⤊(a::Real, t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_scl!(t1, convert(Float64, log(a)),  tpsa)
  mad_tpsa_exp!(tpsa, tpsa)
  return tpsa
end

function __t_inv(t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_inv!(t1, convert(Cdouble, 1), tpsa)
  return tpsa
end

function ⤊(tpsa1::Ptr{TPS{Float64}}, tpsa2::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_pow!(tpsa1, tpsa2, tpsa1)
  rel_temp!(tpsa2)
  return tpsa1
end

function ⤊(tpsa1::Ptr{TPS{Float64}}, i::Integer)::Ptr{TPS{Float64}}
  mad_tpsa_powi!(tpsa1, convert(Cint, i), tpsa1)
  return tpsa1
end

function ⤊(tpsa1::Ptr{TPS{Float64}}, a::Real)::Ptr{TPS{Float64}}
  mad_tpsa_pown!(tpsa1, convert(Float64, a), tpsa1)
  return tpsa1
end

function ⤊(a::Real,tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_scl!(tpsa1, convert(Float64, log(a)),  tpsa1)
  mad_tpsa_exp!(tpsa1, tpsa1)
  return tpsa1
end

function __t_inv(tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_inv!(tpsa1, convert(Cdouble, 1), tpsa1)
  return tpsa1
end

function ⤊(t1::TPS{Float64}, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_pow!(t1, tpsa1, tpsa1)
  return tpsa1
end

function ⤊(tpsa1::Ptr{TPS{Float64}},t1::TPS{Float64})::Ptr{TPS{Float64}}
  mad_tpsa_pow!(tpsa1, t1, tpsa1)
  return tpsa1
end

# TPS{ComplexF64}:
function ⤊(ct1::TPS{ComplexF64}, ct2::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_pow!(ct1, ct2, ctpsa)
  return ctpsa
end

function ⤊(ct1::TPS{ComplexF64}, i::Integer)::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_powi!(ct1, convert(Cint, i), ctpsa)
  return ctpsa
end

function ⤊(ct1::TPS{ComplexF64}, a::Number)::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_pown!(ct1, convert(ComplexF64, a), ctpsa)
  return ctpsa
end

function ⤊(a::Number, ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_scl!(ct1, convert(ComplexF64, log(a)),  ctpsa)
  mad_ctpsa_exp!(ctpsa, ctpsa)
  return ctpsa
end

function __t_inv(ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_inv!(ct1, convert(ComplexF64, 1), ctpsa)
  return ctpsa
end

function ⤊(ctpsa1::Ptr{TPS{ComplexF64}}, ctpsa2::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_pow!(ctpsa1, ctpsa2, ctpsa1)
  rel_temp!(ctpsa2)
  return ctpsa1
end

function ⤊(ctpsa1::Ptr{TPS{ComplexF64}}, i::Integer)::Ptr{TPS{ComplexF64}}
  mad_ctpsa_powi!(ctpsa1, convert(Cint, i), ctpsa1)
  return ctpsa1
end

function ⤊(ctpsa1::Ptr{TPS{ComplexF64}}, a::Number)::Ptr{TPS{ComplexF64}}
  mad_ctpsa_pown!(ctpsa1, convert(ComplexF64, a), ctpsa1)
  return ctpsa1
end

function ⤊(a::Number,ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_scl!(ctpsa1, convert(ComplexF64, log(a)),  ctpsa1)
  mad_ctpsa_exp!(ctpsa1, ctpsa1)
  return ctpsa1
end

function __t_inv(ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_inv!(ctpsa1, convert(ComplexF64, 1), ctpsa1)
  return ctpsa1
end

function ⤊(ct1::TPS{ComplexF64}, ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_pow!(ct1, ctpsa1, ctpsa1)
  return ctpsa1
end

function ⤊(ctpsa1::Ptr{TPS{ComplexF64}},ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_pow!(ctpsa1, ct1, ctpsa1)
  return ctpsa1
end

# TPS{Float64} to TPS{ComplexF64} promotion:
function ⤊(ct1::TPS{ComplexF64}, t1::TPS{Float64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_powt!(ct1, t1, ctpsa)
  return ctpsa
end

function ⤊(t1::TPS{Float64}, ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_tpow!(t1, ct1, ctpsa)
  return ctpsa
end

function ⤊(t1::TPS{Float64}, a::Complex)::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(t1)
  mad_ctpsa_cplx!(t1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  mad_ctpsa_pown!(ctpsa, convert(ComplexF64, a), ctpsa)
  return ctpsa
end

function ⤊(a::Complex, t1::TPS{Float64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(t1)
  mad_ctpsa_cplx!(t1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  mad_ctpsa_scl!(ctpsa, convert(ComplexF64, log(a)), ctpsa)
  mad_ctpsa_exp!(ctpsa, ctpsa)
  return ctpsa
end

function ⤊(ctpsa1::Ptr{TPS{ComplexF64}}, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_powt!(ctpsa1, tpsa1, ctpsa1)
  rel_temp!(tpsa1)
  return ctpsa1
end

function ⤊(tpsa1::Ptr{TPS{Float64}}, ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_tpow!(tpsa1, ctpsa1, ctpsa1)
  rel_temp!(tpsa1)
  return ctpsa1
end

function ⤊(tpsa1::Ptr{TPS{Float64}}, a::Complex)::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(tpsa1)
  mad_ctpsa_cplx!(tpsa1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  rel_temp!(tpsa1)
  mad_ctpsa_pown!(ctpsa, convert(ComplexF64, a), ctpsa)
  return ctpsa
end

function ⤊(a::Complex, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(tpsa1)
  mad_ctpsa_cplx!(tpsa1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  rel_temp!(tpsa1)
  mad_ctpsa_scl!(ctpsa, convert(ComplexF64, log(a)), ctpsa)
  mad_ctpsa_exp!(ctpsa, ctpsa)
  return ctpsa
end

function ⤊(ctpsa1::Ptr{TPS{ComplexF64}}, t1::TPS{Float64})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_powt!(ctpsa1, t1, ctpsa1)
  return ctpsa1
end

function ⤊(t1::TPS{Float64}, ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_tpow!(t1, ctpsa1, ctpsa1)
  return ctpsa1
end

⤊(a,b) = (@inline; ^(a,b))

# --- atan, norm ---
# TPS{Float64}:
function __t_atan(t1::TPS{Float64}, t2::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_atan2!(t1, t2, tpsa)
  return tpsa
end

function __t_atan(t1::TPS{Float64}, a::Real)::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_setval!(tpsa, convert(Float64, a))
  mad_tpsa_atan2!(t1, tpsa, tpsa)
  return tpsa
end

function __t_atan(a::Real, t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_setval!(tpsa, convert(Float64, a))
  mad_tpsa_atan2!(tpsa,t1, tpsa)
  return tpsa
end

function __t_atan(tpsa1::Ptr{TPS{Float64}}, tpsa2::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_atan2!(tpsa1, tpsa2, tpsa1)
  rel_temp!(tpsa2)
  return tpsa1
end

function __t_atan(tpsa1::Ptr{TPS{Float64}}, a::Real)::Ptr{TPS{Float64}}
  tpsa2 = get_rtemp!(tpsa1)
  mad_tpsa_setval!(tpsa2, convert(Float64, a))
  mad_tpsa_atan2!(tpsa1, tpsa2, tpsa1)
  rel_temp!(tpsa2)
  return tpsa1
end

function __t_atan(a::Real, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  tpsa2 = get_rtemp!(tpsa1)
  mad_tpsa_setval!(tpsa2, convert(Float64, a))
  mad_tpsa_atan2!(tpsa2,tpsa1, tpsa1)
  rel_temp!(tpsa2)
  return tpsa1
end

function __t_atan(t1::TPS{Float64}, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_atan2!(t1, tpsa1, tpsa1)
  return tpsa1
end

function __t_atan(tpsa1::Ptr{TPS{Float64}}, t1::TPS{Float64})::Ptr{TPS{Float64}}
  mad_tpsa_atan2!(tpsa1, t1, tpsa1)
  return tpsa1
end


__t_atan(a,b) = (@inline; atan(a,b))

function __t_norm(tpsa1::Ptr{TPS{Float64}})::Float64
  nrm = mad_tpsa_nrm(tpsa1)
  rel_temp!(tpsa1)
  return nrm 
end

function __t_norm(ctpsa1::Ptr{TPS{ComplexF64}})::Float64
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
      function $(esc(F1))(t1::TPS{Float64})::Ptr{TPS{Float64}}
        tpsa = get_rtemp!(t1)
        $(esc(fn))(t1, tpsa)
        return tpsa
      end

      function $(esc(F1))(tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
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
function __t_sinc(t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_scl!(t1, convert(Cdouble, pi), tpsa)
  mad_tpsa_sinc!(tpsa, tpsa)
  return tpsa
end

function __t_sinc(tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_scl!(tpsa1, convert(Cdouble, pi), tpsa1)
  mad_tpsa_sinc!(tpsa1, tpsa1)
  return tpsa1
end

__t_sinc(a) = (@inline; sinc(a))

# asinc is not in Julia, but in C is asinc(x) = asin(x)*x
# To give similiar behavior, define asinc(x) = asin(pi*x)*(pi⨰x)
function __t_asinc(t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_scl!(t1, convert(Cdouble, pi), tpsa)
  mad_tpsa_asinc!(tpsa, tpsa)
  return tpsa
end

function __t_asinc(tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_scl!(tpsa1, convert(Cdouble, pi), tpsa1)
  mad_tpsa_asinc!(tpsa1, tpsa1)
  return tpsa1
end

# asinc undefined for not TPSA types (not in Base)

function __t_sinhc(t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_scl!(t1, convert(Cdouble, pi), tpsa)
  mad_tpsa_sinhc!(tpsa, tpsa)
  return tpsa
end

function __t_sinhc(tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_scl!(tpsa1, convert(Cdouble, pi), tpsa1)
  mad_tpsa_sinhc!(tpsa1, tpsa1)
  return tpsa1
end

# sinhc undefined for not TPSA types (not in Base)

# asinhc is not in Julia, but in C is asinc(x) = asin(x)*x
# To give similiar behavior, define asinc(x) = asin(pi*x)*(pi*x)
function __t_asinhc(t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_scl!(t1, convert(Cdouble, pi), tpsa)
  mad_tpsa_asinhc!(tpsa, tpsa)
  return tpsa
end

function __t_asinhc(tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_scl!(tpsa1, convert(Cdouble, pi), tpsa1)
  mad_tpsa_asinhc!(tpsa1, tpsa1)
  return tpsa1
end

# asinhc undefined for not TPSA types (not in Base)

# These functions are not implemented in the GTPSA C library, so they 
# are implemented below
function __t_csc(t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_sin!(t1, tpsa)
  mad_tpsa_inv!(tpsa, 1.0, tpsa)
  return tpsa
end

function __t_csc(tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_sin!(tpsa1, tpsa1)
  mad_tpsa_inv!(tpsa1, 1.0, tpsa1)
  return tpsa1
end

__t_csc(a) = (@inline; csc(a))

function __t_sec(t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_cos!(t1, tpsa)
  mad_tpsa_inv!(tpsa, 1.0, tpsa)
  return tpsa
end

function __t_sec(tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_cos!(tpsa1, tpsa1)
  mad_tpsa_inv!(tpsa1, 1.0, tpsa1)
  return tpsa1
end

__t_sec(a) = (@inline; sec(a))

function __t_csch(t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_sinh!(t1, tpsa)
  mad_tpsa_inv!(tpsa, 1.0, tpsa)
  return tpsa
end

function __t_csch(tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_sinh!(tpsa1, tpsa1)
  mad_tpsa_inv!(tpsa1, 1.0, tpsa1)
  return tpsa1
end

__t_csch(a) = (@inline; csch(a))

function __t_sech(t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_cosh!(t1, tpsa)
  mad_tpsa_inv!(tpsa, 1.0, tpsa)
  return tpsa
end

function __t_sech(tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_cosh!(tpsa1, tpsa1)
  mad_tpsa_inv!(tpsa1, 1.0, tpsa1)
  return tpsa1
end

__t_sech(a) = (@inline; sech(a))

function __t_acsc(t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_inv!(t1, 1.0, tpsa)
  mad_tpsa_asin!(tpsa, tpsa)
  return tpsa
end

function __t_acsc(tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_inv!(tpsa1, 1.0, tpsa1)
  mad_tpsa_asin!(tpsa1, tpsa1)
  return tpsa1
end

__t_acsc(a) = (@inline; acsc(a))

function __t_asec(t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_inv!(t1, 1.0, tpsa)
  mad_tpsa_acos!(tpsa, tpsa)
  return tpsa
end

function __t_asec(tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_inv!(tpsa1, 1.0, tpsa1)
  mad_tpsa_acos!(tpsa1, tpsa1)
  return tpsa1
end

__t_asec(a) = (@inline; asec(a))

function __t_acsch(t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_inv!(t1, 1.0, tpsa)
  mad_tpsa_asinh!(tpsa, tpsa)
  return tpsa
end

function __t_acsch(tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  mad_tpsa_inv!(tpsa1, 1.0, tpsa1)
  mad_tpsa_asinh!(tpsa1, tpsa1)
  return tpsa1
end

__t_acsch(a) = (@inline; acsch(a))

function __t_asech(t1::TPS{Float64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(t1)
  mad_tpsa_inv!(t1, 1.0, tpsa)
  mad_tpsa_acosh!(tpsa, tpsa)
  return tpsa
end

function __t_asech(tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
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
      function $(esc(F1))(ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
        ctpsa = get_ctemp!(ct1)
        $(esc(fn))(ct1, ctpsa)
        return ctpsa
      end

      function $(esc(F1))(ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
        $(esc(fn))(ctpsa1, ctpsa1)
        return ctpsa1
      end

      # Fallback already defined in previous macro so not needed here
      # (esc(F1))(a) = $(esc(Symbol(F)))(a)
  end
end

function __t_abs(ct1::TPS{ComplexF64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(ct1)
  mad_ctpsa_cabs!(ct1, tpsa)
  return tpsa
end

function __t_abs(ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(ctpsa1)
  mad_ctpsa_cabs!(ctpsa1, tpsa)
  rel_temp!(ctpsa1)
  return tpsa
end

function __t_conj(ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_conj!(ct1, ctpsa)
  return ctpsa
end

function __t_conj(t1::TPS{Float64})::TPS{Float64}
  return t1
end

function __t_conj(tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  return tpsa1
end

__t_conj(a) = (@inline; conj(a))

function __t_angle(ct1::TPS{ComplexF64})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(ct1)
  mad_ctpsa_carg!(ct1, tpsa)
  return tpsa
end

function __t_angle(ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{Float64}}
  tpsa = get_rtemp!(ctpsa1)
  mad_ctpsa_carg!(ctpsa1, tpsa)
  rel_temp!(ctpsa1)
  return tpsa
end

function __t_angle(t1::TPS{Float64})::Ptr{TPS{Float64}}
  ctpsa = get_ctemp!(t1)
  tpsa = get_rtemp!(t1)
  mad_ctpsa_cplx!(t1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  mad_ctpsa_carg!(ctpsa, tpsa)
  rel_temp!(ctpsa)
  return tpsa
end

function __t_angle(tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{Float64}}
  ctpsa = get_ctemp!(tpsa1)
  mad_ctpsa_cplx!(tpsa1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  mad_ctpsa_carg!(ctpsa, tpsa1)
  rel_temp!(ctpsa)
  return tpsa1
end

__t_angle(a) = (@inline; angle(a))

function __t_complex(t1::TPS{Float64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(t1)
  mad_ctpsa_cplx!(t1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  return ctpsa
end

function __t_complex(tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(tpsa1)
  mad_ctpsa_cplx!(tpsa1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  rel_temp!(tpsa1)
  return ctpsa
end

function __t_complex(ct1::TPS{ComplexF64})::TPS{ComplexF64}
  return ct1
end

function __t_complex(ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  return ctpsa1
end

__t_complex(a) = (@inline; complex(a))

function __t_complex(t1::TPS{Float64}, t2::TPS{Float64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(t1)
  mad_ctpsa_cplx!(t1, t2, ctpsa)
  return ctpsa
end

function __t_complex(tpsa1::Ptr{TPS{Float64}}, t1::TPS{Float64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(t1)
  mad_ctpsa_cplx!(tpsa1, t1, ctpsa)
  rel_temp!(tpsa1)
  return ctpsa
end

function __t_complex(t1::TPS{Float64}, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(t1)
  mad_ctpsa_cplx!(t1, tpsa1, ctpsa)
  rel_temp!(tpsa1)
  return ctpsa
end

function __t_complex(tpsa1::Ptr{TPS{Float64}}, tpsa2::Ptr{TPS{Float64}})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(tpsa1)
  mad_ctpsa_cplx!(tpsa1, tpsa2, ctpsa)
  rel_temp!(tpsa2)
  rel_temp!(tpsa1)
  return ctpsa
end

function __t_complex(t1::TPS{Float64}, a::Real)::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(t1)
  tpsa = get_rtemp!(t1)
  mad_tpsa_setval!(tpsa, convert(Float64, a))
  mad_ctpsa_cplx!(t1, tpsa, ctpsa)
  rel_temp!(tpsa)
  return ctpsa
end

function __t_complex(tpsa1::Ptr{TPS{Float64}}, a::Real)::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(tpsa1)
  tpsa2 = get_rtemp!(tpsa1)
  mad_tpsa_setval!(tpsa2, convert(Float64, a))
  mad_ctpsa_cplx!(tpsa1, tpsa2, ctpsa)
  rel_temp!(tpsa2)
  return ctpsa
end

function __t_complex(a::Real,t1::TPS{Float64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(t1)
  tpsa = get_rtemp!(t1)
  mad_tpsa_setval!(tpsa, convert(Float64, a))
  mad_ctpsa_cplx!(tpsa, t1, ctpsa)
  rel_temp!(tpsa)
  return ctpsa
end

function __t_complex(a::Real, tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(tpsa1)
  tpsa2 = get_rtemp!(tpsa1)
  mad_tpsa_setval!(tpsa2, convert(Float64, a))
  mad_ctpsa_cplx!(tpsa2, tpsa1, ctpsa)
  rel_temp!(tpsa2)
  return ctpsa
end

__t_complex(a,b) = (@inline; complex(a,b))

function __t_polar(ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_polar!(ct1, ctpsa)
  return ctpsa
end

function __t_polar(ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_polar!(ctpsa1, ctpsa1)
  return ctpsa1
end

function __t_polar(t1::TPS{Float64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(t1)
  mad_ctpsa_cplx!(t1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  mad_ctpsa_polar!(ctpsa, ctpsa)
  return ctpsa
end

function __t_polar(tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(tpsa1)
  mad_ctpsa_cplx!(tpsa1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  rel_temp!(tpsa1)
  mad_ctpsa_polar!(ctpsa, ctpsa)
  return ctpsa
end

__t_polar(a) = (@inline; polar(a))

function __t_rect(ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_rect!(ct1, ctpsa)
  return ctpsa
end

function __t_rect(ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_rect!(ctpsa1, ctpsa1)
  return ctpsa1
end

function __t_rect(t1::TPS{Float64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(t1)
  mad_ctpsa_cplx!(t1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
  mad_ctpsa_rect!(ctpsa, ctpsa)
  return ctpsa
end

function __t_rect(tpsa1::Ptr{TPS{Float64}})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(tpsa1)
  mad_ctpsa_cplx!(tpsa1, Base.unsafe_convert(Ptr{TPS{Float64}}, C_NULL), ctpsa)
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
function __t_sinc(ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_scl!(ct1, convert(ComplexF64, pi), ctpsa)
  mad_ctpsa_sinc!(ctpsa, ctpsa)
  return ctpsa
end

function __t_sinc(ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_scl!(ctpsa1, convert(ComplexF64, pi), ctpsa1)
  mad_ctpsa_sinc!(ctpsa1, ctpsa1)
  return ctpsa1
end

# asinc is not in Julia, but in C is asinc(x) = asin(x)*x
# To give similiar behavior, define asinc(x) = asin(pi*x)*(pi⨰x)
function __t_asinc(ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_scl!(ct1, convert(ComplexF64, pi), ctpsa)
  mad_ctpsa_asinc!(ctpsa, ctpsa)
  return ctpsa
end

function __t_asinc(ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_scl!(ctpsa1, convert(ComplexF64, pi), ctpsa1)
  mad_ctpsa_asinc!(ctpsa1, ctpsa1)
  return ctpsa1
end

# asinc undefined for not TPSA types (not in Base)

function __t_sinhc(ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_scl!(ct1, convert(ComplexF64, pi), ctpsa)
  mad_ctpsa_sinhc!(ctpsa, ctpsa)
  return ctpsa
end

function __t_sinhc(ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_scl!(ctpsa1, convert(ComplexF64, pi), ctpsa1)
  mad_ctpsa_sinhc!(ctpsa1, ctpsa1)
  return ctpsa1
end

# sinhc undefined for not TPSA types (not in Base)

# asinhc is not in Julia, but in C is asinc(x) = asin(x)*x
# To give similiar behavior, define asinc(x) = asin(pi*x)*(pi*x)
function __t_asinhc(ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_scl!(ct1, convert(ComplexF64, pi), ctpsa)
  mad_ctpsa_asinhc!(ctpsa, ctpsa)
  return ctpsa
end

function __t_asinhc(ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_scl!(ctpsa1, convert(ComplexF64, pi), ctpsa1)
  mad_ctpsa_asinhc!(ctpsa1, ctpsa1)
  return ctpsa1
end

# asinhc undefined for not TPSA types (not in Base)

# These functions are not implemented in the GTPSA C library, so they 
# are implemented below
function __t_csc(ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_sin!(ct1, ctpsa)
  mad_ctpsa_inv!(ctpsa, convert(ComplexF64, 1.0), ctpsa)
  return ctpsa
end

function __t_csc(ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_sin!(ctpsa1, ctpsa1)
  mad_ctpsa_inv!(ctpsa1, convert(ComplexF64, 1.0), ctpsa1)
  return ctpsa1
end

function __t_sec(ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_cos!(ct1, ctpsa)
  mad_ctpsa_inv!(ctpsa, convert(ComplexF64, 1.0), ctpsa)
  return ctpsa
end

function __t_sec(ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_cos!(ctpsa1, ctpsa1)
  mad_ctpsa_inv!(ctpsa1, convert(ComplexF64, 1.0), ctpsa1)
  return ctpsa1
end

function __t_csch(ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_sinh!(ct1, ctpsa)
  mad_ctpsa_inv!(ctpsa, convert(ComplexF64, 1.0), ctpsa)
  return ctpsa
end

function __t_csch(ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_sinh!(ctpsa1, ctpsa1)
  mad_ctpsa_inv!(ctpsa1, convert(ComplexF64, 1.0), ctpsa1)
  return ctpsa1
end

function __t_sech(ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_cosh!(ct1, ctpsa)
  mad_ctpsa_inv!(ctpsa, convert(ComplexF64, 1.0), ctpsa)
  return ctpsa
end

function __t_sech(ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_cosh!(ctpsa1, ctpsa1)
  mad_ctpsa_inv!(ctpsa1, convert(ComplexF64, 1.0), ctpsa1)
  return ctpsa1
end

function __t_acsc(ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_inv!(ct1, convert(ComplexF64, 1.0), ctpsa)
  mad_ctpsa_asin!(ctpsa, ctpsa)
  return ctpsa
end

function __t_acsc(ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_inv!(ctpsa1, convert(ComplexF64, 1.0), ctpsa1)
  mad_ctpsa_asin!(ctpsa1, ctpsa1)
  return ctpsa1
end

function __t_asec(ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_inv!(ct1, convert(ComplexF64, 1.0), ctpsa)
  mad_ctpsa_acos!(ctpsa, ctpsa)
  return ctpsa
end

function __t_asec(ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_inv!(ctpsa1, convert(ComplexF64, 1.0), ctpsa1)
  mad_ctpsa_acos!(ctpsa1, ctpsa1)
  return ctpsa1
end

function __t_acsch(ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_inv!(ct1, convert(ComplexF64, 1.0), ctpsa)
  mad_ctpsa_asinh!(ctpsa, ctpsa)
  return ctpsa
end

function __t_acsch(ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_inv!(ctpsa1, convert(ComplexF64, 1.0), ctpsa1)
  mad_ctpsa_asinh!(ctpsa1, ctpsa1)
  return ctpsa1
end

function __t_asech(ct1::TPS{ComplexF64})::Ptr{TPS{ComplexF64}}
  ctpsa = get_ctemp!(ct1)
  mad_ctpsa_inv!(ct1, convert(ComplexF64, 1.0), ctpsa)
  mad_ctpsa_acosh!(ctpsa, ctpsa)
  return ctpsa
end

function __t_asech(ctpsa1::Ptr{TPS{ComplexF64}})::Ptr{TPS{ComplexF64}}
  mad_ctpsa_inv!(ctpsa1, convert(ComplexF64, 1.0), ctpsa1)
  mad_ctpsa_acosh!(ctpsa1, ctpsa1)
  return ctpsa1
end

__t_hypot(a,b) = (@inline; hypot(a,b))
__t_hypot(a,b,c) = (@inline; hypot(a,b,c))
