"""
    @FastGTPSA(expr_or_block)

Macro to speed up evaluation of mathematical expressions containing TPSs.
The temporaries generated during evaluation of the expression are drawn 
from a thread-safe buffer, reducing the number of heap allocations to 
2 (which is for a single TPS) for the result. @FastGTPSA is completely 
transparent to all other types, so it can be prepended to expressions 
while still maintaining type-generic code.

```julia-repl
julia> using GTPSA, BenchmarkTools

julia> d = Descriptor(3,7); x = vars(d);

julia> t = @btime \$x[1]^3*sin(\$x[2])/log(2+\$x[3])-exp(\$x[1]*\$x[2])*im;
  3.458 μs (20 allocations: 11.88 KiB)

julia> t = @btime @FastGTPSA \$x[1]^3*sin(\$x[2])/log(2+\$x[3])-exp(\$x[1]*\$x[2])*im;
  3.172 μs (2 allocations: 1.94 KiB)
```

`@FastGTPSA` can also be prepended to a block of code, in which case it 
is applied after every `=` sign in the block: 

```julia-repl
julia> @btime @FastGTPSA begin
       t1 = \$x[1]^3*sin(\$x[2])/log(2+\$x[3])-exp(\$x[1]*\$x[2])*im;
       t2 = \$x[1]^3*sin(\$x[2])/log(2+\$x[3])-exp(\$x[1]*\$x[2])*im;
       a = t1+t2
       L = 5+3*exp(7)
       end
  6.317 μs (6 allocations: 5.81 KiB)
```
"""
macro FastGTPSA(expr_or_block)
  if expr_or_block.head == :block 
    for i in eachindex(expr_or_block.args)
      if !(expr_or_block.args[i] isa LineNumberNode) && expr_or_block.args[i].head == :(=)
        expr_or_block.args[i] = :($(expr_or_block.args[i].args[1]) = GTPSA.to_TPS($(to_temp_form(munge_expr(expr_or_block.args[i].args[2])))))
      end
    end
    return esc(expr_or_block)
  else
    return :(to_TPS($(to_temp_form(munge_expr(esc(expr_or_block))))))
  end
end 

"""
    @FastGTPSA!(expr_or_block)

Macro to speed up evaluation of mathematical expressions that may contain
assignment to pre-allocated `TPS`s. The temporaries generated during 
evaluation of the expression are drawn from a thread-safe buffer. With this 
macro, the number of heap allocations during evaluation of expressions 
containing `TPS`s is 0, and it is fully transparent to non-TPS types.

This macro supports assignment using `=`, `+=`, `-=`, `*=`, `/=`, and `^=`.

**Important:** The symbols must be defined prior to calling the macro 
regardless of whether or not the type is a `TPS`:

```julia-repl
julia> using GTPSA, BenchmarkTools

julia> d = Descriptor(3,7); x = vars(d); 

julia> t = ComplexTPS64(); # Pre-allocate

julia> @btime @FastGTPSA! \$t = \$x[1]^3*sin(\$x[2])/log(2+\$x[3])-exp(\$x[1]*\$x[2])*im;
  2.972 μs (0 allocations: 0 bytes)

julia> @btime @FastGTPSA! \$t ^= \$x[1]^3*sin(\$x[2])/log(2+\$x[3])-exp(\$x[1]*\$x[2])*im;
  6.550 μs (0 allocations: 0 bytes)

julia> y = rand(3); z = 2; # transparent to non-TPS types 

julia>  @btime @FastGTPSA! \$z = \$y[1]^3*sin(\$y[2])/log(2+\$y[3])-exp(\$y[1]*\$y[2])*im;
  11.344 ns (0 allocations: 0 bytes)
```

Like `@FastGTPSA`, `@FastGTPSA!` can prepended to a block of code, in 
which case it is applied before every line in the block containing assignment:

```julia-repl
julia> t1 = zero(ComplexTPS64); t2 = zero(ComplexTPS64); z = 0; 

julia> @btime @FastGTPSA! begin
       \$t1 = \$x[1]^3*sin(\$x[2])/log(2+\$x[3])-exp(\$x[1]*\$x[2])*im;
       \$t2 -= \$x[1]^3*sin(\$x[2])/log(2+\$x[3])-exp(\$x[1]*\$x[2])*im;
       \$z += 7
       end
  5.965 μs (0 allocations: 0 bytes)
```
"""
macro FastGTPSA!(expr_or_block)
  if expr_or_block.head == :block 
    for i in eachindex(expr_or_block.args)
      if !(expr_or_block.args[i] isa LineNumberNode)
        if expr_or_block.args[i].head == :(=)
          op! = nothing
        elseif expr_or_block.args[i].head == :(+=)
          op! = add!
        elseif expr_or_block.args[i].head == :(-=)
          op! = sub!
        elseif expr_or_block.args[i].head == :(*=)
          op! = mul!
        elseif expr_or_block.args[i].head == :(/=)
          op! = div!
        elseif expr_or_block.args[i].head == :(^=)
          op! = pow!
        else
          continue
        end
        expr_or_block.args[i] = :($(expr_or_block.args[i].args[1]) isa TPS ? $(expr_or_block.args[i].args[1]) = GTPSA.to_TPS!($(expr_or_block.args[i].args[1]), $(to_temp_form(munge_expr(expr_or_block.args[i].args[2]))), $op!) : $(expr_or_block.args[i])) 
      end
    end
    return esc(expr_or_block)
  else

    if expr_or_block.head == :(=)
      op! = nothing
    elseif expr_or_block.head == :(+=)
      op! = add!
    elseif expr_or_block.head == :(-=)
      op! = sub!
    elseif expr_or_block.head == :(*=)
      op! = mul!
    elseif expr_or_block.head == :(/=)
      op! = div!
    elseif expr_or_block.head == :(^=)
      op! = pow!
    else
      return :($expr_or_block)
    end

    return :( $(esc(expr_or_block.args[1])) isa TPS ?  $(esc(expr_or_block.args[1])) = to_TPS!($(esc(expr_or_block.args[1])), $(to_temp_form(munge_expr(esc(expr_or_block.args[2])))),$op!) : $(esc(expr_or_block)))  
  end
end 

to_temp_form(not_expr) = not_expr
function to_temp_form(expr::Expr)
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
      expr.args[i] = :(GTPSA.:±)  # \pm  (allowed as unary operator)
    elseif expr.args[i] == :-
      expr.args[i] = :(GTPSA.:∓)  # \mp  (allowed as unary operator)
    elseif expr.args[i] == :*
      expr.args[i] = :(GTPSA.:⨰)  # \dottimes
    elseif expr.args[i] == :/
      expr.args[i] = :(GTPSA.:⨱)  # \timesbar
    elseif expr.args[i] == :^
      expr.args[i] = :(GTPSA.:⤊)  # \Uuparrow
    elseif expr.args[i] in fcns
      str = "__t_" * string(expr.args[i])
      expr.args[i] = :(GTPSA.$(Symbol(str)))
    end
  end

  return expr
end

munge_expr(not_expr) = not_expr
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

function to_TPS!(t::S, t1::T, op!)  where {S,T}
  if !(S <: TPS)
    if T <: Union{TPS,TempTPS} 
      try error("Incorrect destination type: received $(typeof(a)), require $(TPS{T})") finally GTPSA.cleartemps!(getdesc(b)) end
    else
      if isnothing(op!)
        return t1
      else
        if op! == add!
          return t+t1
        elseif op! == sub!
          return t-t1
        elseif op! == mul!
          return t*t1
        elseif op! == div!
          return t/t1
        else
          return t^t1
        end
      end
    end
  end

  if isnothing(op!)
    if t1 isa TPS || t1 isa TempTPS
      copy!(t,t1)
    else
      clear!(t)
      seti!(t, 0, 0, t1)
    end
  else
    (op!)(t,t,t1)
  end

  if t1 isa TempTPS
    rel_temp!(t1)
  end
  return t
end




#to_TPS!(t::TPS{T}, b::Union{TPS{T},TempTPS{T}}, op!) where {T}




# Fallback for non-TPS types
to_TPS(a) = a
#to_TPS!(a, b::Union{TPS,TempTPS}, op!) = try error("Incorrect destination type: received $(typeof(a)), require $(TPS{T})") finally GTPSA.cleartemps!(getdesc(b)) end
#to_TPS!(a,b,op!) = b

