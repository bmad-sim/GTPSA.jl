# 1. Apply other macros to expression first
function apply_macro(exp::Expr)
  if exp isa Expr && exp.head == :macrocall
      exp.args[3] = apply_macro(exp.args[3])
      return macroexpand(@__MODULE__, exp, recursive = false)
  else
      return exp
  end
end

# 2. Change broadcasted arithmetic .+ -> (+). 
# this is basically converting expression with .+ to those 
# generated by using @. (yes they are different)
function change_dots(expr::Expr)
  i=1
  while i <= length(expr.args)
    if expr.args[i] isa Expr
      change_dots(expr.args[i])
      i += 1
    else
      if expr.args[i] == :.+
        expr.head = :.
        expr.args = [:+, Expr(:tuple, expr.args[i+1:end]...)]
      elseif expr.args[i] == :.-
        expr.head = :.
        expr.args = [:-, Expr(:tuple, expr.args[i+1:end]...)]
      elseif expr.args[i] == :.*
        expr.head = :.
        expr.args = [:*, Expr(:tuple, expr.args[i+1:end]...)]
      elseif expr.args[i] == :./
        expr.head = :.
        expr.args = [:/, Expr(:tuple, expr.args[i+1:end]...)]
      elseif expr.args[i] == :.^
        expr.head = :.
        expr.args = [:^, Expr(:tuple, expr.args[i+1:end]...)]
      else
        i += 1
      end
    end
  end
  return expr
end

# 3. Munge expression :(+, a, b, c, d) to be :(+, (+, (+, a, b), c), d)
# so temporaries push/pop handled correctly
function munge_expr(expr::Expr; inplace::Bool = false)
  if !inplace; expr = deepcopy(expr); end

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
munge_expr(not_expr) = not_expr

# 4. Change functions symbols to temporary types
function change_functions(expr::Expr)
  fcns = [:unit, :sqrt, :exp, :log, :sin, :cos, :tan, :cot, :sinh, :cosh, :tanh, :inv, :coth, 
          :asin, :acos, :atan, :acot, :asinh, :acosh, :atanh, :acoth, :erf, :erfc, :sinc, 
          :sinhc, :asinc, :asinhc, :csc, :csch, :acsc, :acsch, :sec, :sech, :asec, :asech, 
          :conj, :rect, :real, :imag, :angle, :abs, :atan, :polar, :complex, :zero, :one,
          :norm, :normTPS]

  function map_to_temp(fun::Symbol)
      if fun == :+
        return :(GTPSA.:±)  # \pm  (allowed as unary operator)
      elseif fun == :-
        return :(GTPSA.:∓)  # \mp  (allowed as unary operator)
      elseif fun == :*
        return :(GTPSA.:⨰)  # \dottimes
      elseif fun == :/
        return :(GTPSA.:⨱)  # \timesbar
      elseif fun == :^
        return :(GTPSA.:⤊)  # \Uuparrow
      elseif fun in fcns
        return :(GTPSA.$(Symbol("__t_" * string(fun))))
      else
        return fun
      end
  end

  # Check if we are calling a function in GTPSA module (with qualified GTPSA. ) and change if so
  if expr.head == :. && expr.args[2] isa QuoteNode && expr.args[1] == :GTPSA && expr.args[end].value in fcns
    expr.args[end] = QuoteNode(map_to_temp(expr.args[end].value))
    return expr
  end

  for i in eachindex(expr.args)
    if expr.args[i] isa Expr
      change_functions(expr.args[i])
    elseif expr.args[i] isa Symbol
      expr.args[i] = map_to_temp(expr.args[i])
    end
  end

  return expr
end


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
    block = expr_or_block
    for i in eachindex(block.args)
      if !(block.args[i] isa LineNumberNode) && block.args[i].head == :(=)
        expr = block.args[i].args[2]
        expr = apply_macro(expr)
        expr = change_dots(expr)
        expr = munge_expr(expr)
        expr = change_functions(expr)
        block.args[i] = :($(block.args[i].args[1]) = GTPSA.to_TPS.($expr))
      end
    end
    return esc(block)
  else
    expr = expr_or_block
    expr = apply_macro(esc(expr))
    expr = change_dots(expr)
    expr = munge_expr(expr)
    expr = change_functions(expr)
    return :(to_TPS.($expr))
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
    block = expr_or_block
    for i in eachindex(block.args)
      if !(block.args[i] isa LineNumberNode)
        expr = block.args[i]
        rhs = expr.args[2]
        
        if expr.head == :(=)
          op! = nothing
        elseif expr.head == :(+=)
          op! = add!
        elseif expr.head == :(-=)
          op! = sub!
        elseif expr.head == :(*=)
          op! = mul!
        elseif expr.head == :(/=)
          op! = div!
        elseif expr.head == :(^=)
          op! = pow!
        else
          return :($expr)
        end

        rhs = apply_macro(esc(rhs))
        rhs = change_dots(rhs)
        rhs = munge_expr(rhs)
        rhs = change_functions(rhs)
        outvar = esc(expr.args[1])

        block.args[i] = :( $outvar isa TPS ?  to_TPS!($outvar, $rhs, $op!) : $(esc(expr)))
      end
    end
    return block
  else
    expr = expr_or_block
    rhs = expr.args[2]
    
    if expr.head == :(=)
      op! = nothing
    elseif expr.head == :(+=)
      op! = add!
    elseif expr.head == :(-=)
      op! = sub!
    elseif expr.head == :(*=)
      op! = mul!
    elseif expr.head == :(/=)
      op! = div!
    elseif expr.head == :(^=)
      op! = pow!
    else
      return :($expr)
    end

    rhs = apply_macro(esc(rhs))
    rhs = change_dots(rhs)
    rhs = munge_expr(rhs)
    rhs = change_functions(rhs)
    outvar = esc(expr.args[1])

    return :( $outvar isa TPS ?  to_TPS!($outvar, $rhs, $op!) : $(esc(expr)))
  end
end 

function to_TPS(t1::TempTPS{T}) where {T}
  t = TPS{T}(getdesc(t1).desc, getmo(t1)) #get_and_zero_mo!(t1))
  copy!(t,t1)
  rel_temp!(t1)
  return t
end

function to_TPS!(t::TPS, t1, op!)
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

