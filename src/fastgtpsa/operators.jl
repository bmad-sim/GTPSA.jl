# --- helper functions for manual memory management ---
"""
    get_out(t1, t2)

Takes 2 input arguments where at least 1 is a TPS/TempTPS
and returns what the output TempTPS should be. This could 
either be `t1` or `t2`, or a "new" temporary from the buffer.
"""
function get_out(t1::T1, t2::T2) where {T1,T2}
  # If both are TempTPS's and of same type, put result into t1
  # If promotion is occuring, then put result into promoted one
  if T1 <: TempTPS && T2 <: TempTPS
    if T1 == TempTPS{ComplexF64}
      return t1
    elseif T2 == TempTPS{ComplexF64}
      return t2
    else
      return t1
    end
  elseif T2 <: TempTPS
    # If only t2 is a TempTPS, put the result into t2 unless promotion is occuring
    outtype = promote_type(T1,T2)
    if T2 == outtype
      return t2
    else
      # this means t2 is real but t1 is complex, get a complex temporary
      return (outtype)(t2)
    end    
  elseif T1 <: TempTPS
    # If only t1 is a TempTPS, put the result into t1 unless promotion is occuring
    outtype = promote_type(T1,T2)
    if T1 == outtype
      return t1
    else
      return (outtype)(t1)
    end
  else
    # None are TempTPS, so depending on output type we'll need to get one
    outtype = promote_type(TempTPS{Float64},T1,T2)
    if T1 <: TPS
      return (outtype)(t1)
    else
      return (outtype)(t2)
    end
  end
end

"""
    rel_op!(t1, t2)

This should be called after an operation with `t1` and `t2`
where at least 1 is a TPS/TempTPS. Releases the TempTPS if necessary.
"""
function rel_op!(t1::T1, t2::T2) where {T1,T2}
  # If both are TempTPS's and of same type, release t2
  # If promotion is occuring, then release real type
  if T1 <: TempTPS && T2 <: TempTPS
    if T1 == TempTPS{ComplexF64}
      rel_temp!(t2)
    elseif T2 == TempTPS{ComplexF64}
      rel_temp!(t1)
    else
      rel_temp!(t2)
    end
  elseif T2 <: TempTPS
    # If only t2 is a TempTPS, then release if promotion
    outtype = promote_type(T1,T2)
    if T2 != outtype
      rel_temp!(t2)
    end    
  elseif T1 <: TempTPS
    # If only t1 is a TempTPS, then release if promotion
    outtype = promote_type(T1,T2)
    if T1 != outtype
      rel_temp!(t1)
    end
  end
end

# --- zero/one ---
__t_zero(t1::TPS)    = (t = TempTPS{numtype(t1)}(t1); clear!(t); return t) 
__t_zero(t::TempTPS) = (clear!(t); return t) 

__t_one(t1::TPS)    = (t = TempTPS{numtype(t1)}(t1); clear!(t); seti!(t, 0, 0, 1); return t) 
__t_one(t::TempTPS) = (clear!(t); seti!(t, 0, 0, 1); return t) 

__t_zero(a) = zero(a)
__t_one(a) = one(a)

# --- Basic unary ---
±(t1::TPS)    = t1
±(t::TempTPS) = t
±(a) = +a

∓(t1::TPS)    = (t = TempTPS{numtype(t1)}(t1); mul!(t, -1, t1); return t)
∓(t::TempTPS) = (mul!(t, -1, t); return t)
∓(b) = -b

__t_norm(t1::TempTPS) = __t_abs(t1)
__t_norm(a) = norm(a)

# --- Arithmetic +(±),-(∓),*(⨰),/(⨱),^(⤊) ---
for t = ((TPS,TPS),(TPS,Number),(Number,TPS),(TempTPS,TempTPS),(TempTPS,TPS),(TPS,TempTPS),(TempTPS,Number),(Number,TempTPS))
@eval begin
function ±(t1::$t[1], t2::$t[2])
  t = get_out(t1,t2)
  add!(t, t1, t2)
  rel_op!(t1, t2)
  return t
end

function ∓(t1::$t[1], t2::$t[2])
  t = get_out(t1,t2)
  sub!(t, t1, t2)
  rel_op!(t1, t2)
  return t
end

function ⨰(t1::$t[1], t2::$t[2])
  t = get_out(t1,t2)
  mul!(t, t1, t2)
  rel_op!(t1, t2)
  return t
end

function ⨱(t1::$t[1], t2::$t[2])
  t = get_out(t1,t2)
  div!(t, t1, t2)
  rel_op!(t1, t2)
  return t
end
end
end

for t = ((TPS,TPS),(TPS,Number),(Number,TPS),(Integer,TPS),(TPS,Integer),
         (TempTPS,TempTPS),(TempTPS,TPS),(TPS,TempTPS),(TempTPS,Number),
         (Number,TempTPS),(Integer,TempTPS),(TempTPS,Integer))
@eval begin
function ⤊(t1::$t[1], t2::$t[2])
  t = get_out(t1,t2)
  pow!(t, t1, t2)
  rel_op!(t1, t2)
  return t
end
end
end

±(a,b) = +(a,b)
∓(a,b) = -(a,b)
⨰(a,b) = *(a,b)
⨱(a,b) = a/b
⤊(a,b) = ^(a,b)

# --- Rest of unary functions ---
for t = (:unit, :sqrt, :exp, :log, :sin, :cos, :tan, :cot, :sinh, :cosh, :tanh, :inv,
  :coth, :asin, :acos, :atan, :acot, :asinh, :acosh, :atanh, :acoth, :erf, :erfc, :sinc, :sincu,
  :sinhc, :sinhcu, :asinc, :asincu, :asinhc, :asinhcu, :csc, :csch, :acsc, :acsch, :sec, :sech, 
  :asec, :asech, :conj, :rect, :log10)
@eval begin
($(Symbol("__t_",t)))(t1::TPS)     = (t = TempTPS{numtype(t1)}(t1); $(Symbol(t,:!))(t, t1); return t)
($(Symbol("__t_",t)))(t::TempTPS)  = ($(Symbol(t,:!))(t, t); return t)
($(Symbol("__t_",t)))(a) = $t(a)
end
end

# --- atan2 ---

__t_atan(t1::TPS{Float64},     t2::TPS{Float64})     = (t = TempTPS{Float64}(t1); atan!(t, t1, t2); return t)
__t_atan(t1::TempTPS{Float64}, t2::TPS{Float64})     = (atan!(t1, t1, t2); return t1)
__t_atan(t1::TPS{Float64},     t2::TempTPS{Float64}) = (atan!(t2, t1, t2); return t2)
__t_atan(t1::TempTPS{Float64}, t2::TempTPS{Float64}) = (atan!(t1, t1, t2); rel_temp!(t2); return t1)

function __t_atan(t1::TPS{Float64}, a::Real)
  t = TempTPS{Float64}(t1)
  clear!(t)
  seti!(t,0,0,a)
  atan!(t, t1, t)
  return t
end
function __t_atan(a::Real, t1::TPS{Float64})
  t = TempTPS{Float64}(t1)
  clear!(t)
  seti!(t,0,0,a)
  atan!(t, t, t1)
  return t
end

function __t_atan(t1::TempTPS{Float64}, a::Real)
  t = TempTPS{Float64}(t1)
  clear!(t)
  seti!(t,0,0,a)
  atan!(t1, t1, t)
  rel_temp!(t)
  return t1
end
function __t_atan(a::Real, t1::TempTPS{Float64})
  t = TempTPS{Float64}(t1)
  clear!(t)
  seti!(t,0,0,a)
  atan!(t1, t, t1)
  rel_temp!(t)
  return t1
end


__t_atan(a,b) = atan(a,b)

# --- Unary functions that return TempTPS{Float64} ---
for t = (:real,  :imag, :angle, :abs)
@eval begin
($(Symbol("__t_",t)))(t1::TPS)     = (t = TempTPS{real(numtype(t1))}(t1); $(Symbol(t,:!))(t, t1); return t)
function ($(Symbol("__t_",t)))(t1::TempTPS)
  if numtype(t1) <: Real
    $(Symbol(t,:!))(t1, t1)
    return t1
  else
    t = TempTPS{real(numtype(t1))}(t1)
    $(Symbol(t,:!))(t, t1)
    rel_temp!(t1)  # Release the complex temporary
    return t
  end
end

($(Symbol("__t_",t)))(a) = $t(a) 

end
end

# --- Unary functions that return TPS{ComplexF64} --- 
__t_polar(t1::TPS) = (t = TempTPS{complex(numtype(t1))}(t1); polar!(t,t1); return t)
function __t_polar(t1::TempTPS)
  if numtype(t1) <: Complex
    polar!(t1, t1)
    return t1
  else
    t = TempTPS{complex(numtype(t1))}(t1)
    polar!(t, t1)
    rel_temp!(t1)  # Release the real temporary
    return t
  end
end

__t_polar(a) = polar(a)

__t_complex(t1::TPS{ComplexF64})     = t1 
__t_complex(t1::TempTPS{ComplexF64}) = t1 

__t_complex(t1::TPS{Float64})     = (t = TempTPS{complex(numtype(t1))}(t1); complex!(t,tre=t1); return t);
__t_complex(t1::TempTPS{Float64}) = (t = TempTPS{complex(numtype(t1))}(t1); complex!(t,tre=t1); rel_temp!(t1); return t);

function __t_complex(tre::RealTPS, tim::RealTPS)
  t = TempTPS{ComplexF64}(tre)
  complex!(t, tre=tre, tim=tim)
  if tim isa TempTPS
    rel_temp!(tim)
  end
  if tre isa TempTPS
    rel_temp!(tre)
  end
  return t
end

function __t_complex(tre::RealTPS, tim::Real)
  t = TempTPS{ComplexF64}(tre)
  complex!(t, tre=tre, tim=tim)
  if tre isa TempTPS
    rel_temp!(tre)
  end
  return t
end

function __t_complex(tre::Real, tim::RealTPS)
  t = TempTPS{ComplexF64}(tim)
  complex!(t, tre=tre, tim=tim)
  if tim isa TempTPS
    rel_temp!(tim)
  end
  return t
end

__t_complex(a) = complex(a)
__t_complex(a,b) = complex(a,b)

# hypot not supported by @FastGTPSA yet

# normTPS too
__t_normTPS(t1::TempTPS{Float64})    = (nrm = mad_tpsa_nrm(t1); rel_temp!(t1); return nrm)
__t_normTPS(t1::TempTPS{ComplexF64}) = (nrm = mad_ctpsa_nrm(t1); rel_temp!(t1); return nrm)

__t_normTPS(a) = normTPS(a)


