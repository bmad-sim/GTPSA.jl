# Julia script to compare current GTPSA.jl with latest
# MAD_TPSA code from the MAD Git repo:
# https://github.com/MethodicalAcceleratorDesign/MAD
using Downloads

const io_out = IOBuffer()

struct FunctionInfo
  name::String
  ret::String
  vars::Vector{String}
  types::Vector{String}
end

function compare(fun_decs_c, fun_decs_jl)
  funs_c = []
  funs_jl = []

  for fun in fun_decs_c
    #println(io_out, "Getting info for $(fun)")
    name, ret, vars, types = get_c_fun_info(fun)
    fun_info = FunctionInfo(name, ret, vars, types)
    push!(funs_c, fun_info)
  end

  for fun in fun_decs_jl
    name, ret, vars, types = get_jl_fun_info(fun)
    fun_info = FunctionInfo(name, ret, vars, types)
    push!(funs_jl, fun_info)
  end

  names_jl = [x.name for x in funs_jl]
  #println.(names_jl)
  used = BitArray(undef, length(names_jl))
  used .= 0
  for fun_c in funs_c
    # println(io_out, "Checking $(fun_c.name)")
    if isempty(findall(x->x==fun_c.name, names_jl))
      println(io_out, "$(fun_c.name) not found in GTPSA.jl!")
      continue
    end
    idx_jl = findall(x->x==fun_c.name, names_jl)[1]
    fun_jl = funs_jl[idx_jl]
    used[idx_jl] = 1
    types_c_to_jl = c_to_jl_type.(fun_c.types)
    if (c_to_jl_type(fun_c.ret,true) != fun_jl.ret)
      println(io_out, "$(fun_c.name): Different returns types! C: $(fun_c.ret) => $(c_to_jl_type(fun_c.ret,true)) not equal to Julia $(fun_jl.ret)")
    end
    if (length(fun_c.types) != length(fun_jl.types))
      println(io_out, "$(fun_c.name): Number of C variables different from number of Julia variables! Skipping variable check...")
    else
      for i = 1:length(fun_c.types)
        if "$(fun_c.vars[i])::$(types_c_to_jl[i])" != "$(fun_jl.vars[i])::$(fun_jl.types[i])"
          println(io_out, "$(fun_c.name): Variable in C $(fun_c.types[i]) $(fun_c.vars[i]) => $(fun_c.vars[i])::$(types_c_to_jl[i]) not equal to Julia $(fun_jl.vars[i])::$(fun_jl.types[i])")
        end
      end
    end
  end

  idxs_leftover = findall(x->x==0, used)
  for leftover in idxs_leftover
    fun_c = funs_jl[leftover]
    println(io_out, "$(fun_c.name) found in GTPSA.jl, but not MAD_TPSA!")
  end
end


function c_to_jl_type(type_c, return_type=false)
  type_jl = ""
  ptr = findfirst("*", type_c)
  if !isnothing(ptr) && !occursin("tpsa_t", type_c) 
    type_jl = type_jl * "Ptr{"
  end

  if occursin("ord_t", type_c)
    type_jl = type_jl * "Cuchar"
  elseif occursin("int", type_c)
    type_jl = type_jl * "Cint"
  elseif occursin("num_t", type_c)
    type_jl = type_jl * "Cdouble"
  elseif occursin("cpx_t", type_c)
    type_jl = type_jl * "ComplexF64"
  elseif occursin("ssz_t", type_c)
    type_jl = type_jl * "Cint"
  elseif occursin("log_t", type_c)
    type_jl = type_jl * "Bool"
  elseif occursin("str_t", type_c)
    type_jl = type_jl * "Cstring"
  elseif occursin("idx_t", type_c)
    type_jl = type_jl * "Cint"
  elseif occursin("char", type_c)
    type_jl = type_jl * "Cuchar"
  elseif occursin("desc_t", type_c)
    type_jl = type_jl * "Desc"
  elseif occursin("ctpsa_t**", type_c)
    type_jl = type_jl * "Ptr{TPS{ComplexF64}}"
  elseif occursin("ctpsa_t", type_c)
    return_type ? type_jl = type_jl * "Ptr{TPS{ComplexF64}}" : type_jl = type_jl * "Ref{TPS{ComplexF64}}"
  elseif occursin("tpsa_t**", type_c)
    type_jl = type_jl * "Ptr{TPS{Float64}}"
  elseif occursin("tpsa_t", type_c)
    return_type ? type_jl = type_jl * "Ptr{TPS{Float64}}" : type_jl = type_jl * "Ref{TPS{Float64}}"
  elseif occursin("FILE", type_c)
    type_jl = type_jl * "Cvoid"
  elseif occursin("void", type_c)
    type_jl = type_jl * "Cvoid"
  else
    println(io_out, "ERROR TYPE NOT FOUND! type_c = $(type_c)")
  end
  if !isnothing(ptr) && !occursin("tpsa_t", type_c) 
    type_jl = type_jl * "}"
  end

  return type_jl
end

function get_jl_fun_info(fun)
  name = strip(replace(fun[1:findnext("(", fun, 1)[1]-1], "!" => ""))
  fun = strip(fun[findnext("(", fun, 1)[1]+1:end])
  if occursin("::", fun[findlast(")", fun)[1]:end])
    return_type = fun[findlast("::", fun)[1]+2:end]
    fun = fun[1:findlast("::", fun)[1]-1]
  else
    return_type = "Cvoid"
  end

  vars = []
  types = []
  while length(fun) > 1
    if occursin(r"([,](?![^{]*\}))", fun)
      curVarEnd = findfirst(r"([,](?![^{]*\}))", fun)[1]
    else
      curVarEnd = findfirst(")", fun)[1]
    end

    # Pointers in type will be added after parsing variable name
    var = strip(fun[1:findfirst("::", fun[1:curVarEnd])[1] - 1])
    type = strip(fun[findfirst("::", fun[1:curVarEnd])[1] + 2:curVarEnd-1])

    push!(vars, var)
    push!(types, type)
    fun = strip(fun[curVarEnd+1:end])
  end

  return name, return_type, vars, types
end

function get_c_fun_info(fun)
  return_type = fun[1:findnext(" ", fun, 1)[1]-1]
  fun = fun[findnext(" ", fun, 1)[1]+1:end]
  name = strip(fun[1:findnext("(", fun, 1)[1]-1])
  fun = strip(fun[findnext("(", fun, 1)[1]+1:end])
  vars = []
  types = []
  while length(fun) > 2
    if occursin(",", fun)
      curVarEnd = findfirst(",", fun)[1]
    else
      curVarEnd = findfirst(")", fun)[1]
      if occursin("...", fun)
        type = types[end]
        push!(types, type)
        push!(vars, "...")
        fun = strip(fun[curVarEnd+1:end])
        continue
      end
    end
    
    if isnothing(findfirst(" ", fun[1:curVarEnd]))
      fun = strip(fun[curVarEnd+1:end])
      continue
    end
    # Pointers in type will be added after parsing variable name
    type = fun[1:findfirst(" ", fun[1:curVarEnd])[1]-1]

    # For simplicity, if types have [], make them * because Julia doesn't care
    while occursin("[", type)
      type = replace(var, r"\[[^\]]*\]" => "*")
    end

    var = strip(fun[findfirst(" ", fun[1:curVarEnd])[1]+1:curVarEnd-1])
  
    # Now if variable contains any [] or *, these need to each be appended to type
    # and removed from variable
    while occursin("*", var)
      var = replace(var, "*" => "", count=1)
      type = type * "*"
    end
  
    while occursin("[", var)
      var = replace(var, r"\[[^\]]*\]" => "", count=1)
      type = type * "*"
    end
  
    push!(vars, strip(var))
    push!(types, type)
    fun = strip(fun[curVarEnd+1:end])
  end

  return name, return_type, vars, types
end

function get_jl_function_declarations(str)
  fun_decs::Vector{String} = []

  # Remove multiline comments
  i = 1
  while occursin("\"\"\"", str)
    len = length(str)
    start_index = findnext("\"\"\"", str, i)[1] - 1
    end_index = findnext("\"\"\"", str, start_index+4)[3] + 1
    str = str[1:start_index] * str[end_index:len]
    i = start_index + 1
  end

  # Remove multiline comments
  i = 1
  while occursin("#=", str)
    len = length(str)
    start_index = findnext("#=", str, i)[1] - 1
    end_index = findnext("=#", str, i)[2] + 1
    str = str[1:start_index] * str[end_index:len]
    i = start_index + 1
  end

  # Remove single line comments
  i = 1
  while occursin("#", str)
    len = length(str)
    start_index = findnext("#", str, i)[1] - 1
    end_index = findnext("\n", str, start_index+1)[1]
    str = str[1:start_index] * str[end_index:len]
    i = start_index+1
  end

  # Get function declarations
  lines = readlines(IOBuffer(str))
  i = 1
  while i < length(lines)
    line = lines[i]

    # Check if line contains a ccall declaration
    if occursin("MAD_TPSA.", line) # && !occursin("ccall", line)
      full_line = strip(line)
      line = line
      while (!occursin(")", full_line))
        i += 1
        full_line = full_line * strip(lines[i])
      end
        push!(fun_decs, full_line)
    end
    i += 1
  end

  # Remove everything up to ccall
  fun_decs = map(x->x[1+findfirst("MAD_TPSA.",x)[end]:end], fun_decs)
  #fun_decs = replace.(fun_decs, " " => "" )

  return fun_decs
end

function get_c_function_declarations(str)
  fun_decs::Vector{String} = []

  # Remove multiline comments
  i = 1
  while occursin("/*", str)
    len = length(str)
    start_index = findnext("/*", str, i)[1] - 1
    end_index = findnext("*/", str, i)[2] + 1
    str = str[1:start_index] * str[end_index:len]
    i = start_index + 1
  end

  # Remove single line comments
  i = 1
  while occursin("//", str)
    len = length(str)
    start_index = findnext("//", str, i)[1] - 1
    end_index = findnext("\n", str, start_index+1)[1]
    str = str[1:start_index] * str[end_index:len]
    i = start_index+1
  end

  # Remove preprocessor statements
  i = 1
  while occursin("#", str)
    len = length(str)
    start_index = findnext("#", str, i)[1] - 1
    end_index = findnext("\n", str, start_index+1)[1]
    str = str[1:start_index] * str[end_index:len]
    i = start_index+1
  end

  # Get function declarations
  lines = readlines(IOBuffer(str))
  i = 1
  while i < length(lines)
    line = lines[i]

    # Check if line contains a function declaration
    if occursin("(", line)
      full_line = strip(line)
      line = line
      while (!occursin(")", full_line))
        i += 1
        full_line = full_line * strip(lines[i])
      end
        push!(fun_decs, full_line)
    end
    i += 1
  end

  # Remove "const" in function declarations
  fun_decs = replace.(fun_decs, "const " => "")

  return fun_decs
end

function compare_MAD()
  io = IOBuffer()

  try
    Downloads.download("https://raw.githubusercontent.com/MethodicalAcceleratorDesign/MAD-NG/dev/src/mad_mono.h", io)
    println(io_out, "mad_mono.h downloaded.")
  catch e
    println(io_out, "Error downloading mad_mono.h")
    showerror(stdout, e)
  end

  str = String(take!(io))
  fun_decs_c  = get_c_function_declarations(str)

  str = read("../src/low_level/mono.jl", String)
  fun_decs_jl = get_jl_function_declarations(str)
  println(io_out, "Comparing mad_mono.h to mono.jl...")
  compare(fun_decs_c, fun_decs_jl)


  try
    Downloads.download("https://raw.githubusercontent.com/MethodicalAcceleratorDesign/MAD-NG/dev/src/mad_desc.h", io)
    println(io_out, "mad_desc.h downloaded.")
  catch e
    println(io_out, "Error downloading mad_desc.h")
    showerror(stdout, e)
  end

  str = String(take!(io))
  fun_decs_c  = get_c_function_declarations(str)

  str = read("../src/low_level/desc.jl", String)
  fun_decs_jl = get_jl_function_declarations(str)
  println(io_out, "Comparing mad_desc.h to desc.jl...")
  compare(fun_decs_c, fun_decs_jl)


  try
    Downloads.download("https://raw.githubusercontent.com/MethodicalAcceleratorDesign/MAD-NG/dev/src/mad_tpsa.h", io)
    println(io_out, "mad_tpsa.h downloaded.")
  catch e
    println(io_out, "Error downloading mad_tpsa.h")
    showerror(stdout, e)
  end

  str = String(take!(io))
  fun_decs_c  = get_c_function_declarations(str)

  str = read("../src/low_level/rtpsa.jl", String)
  fun_decs_jl = get_jl_function_declarations(str)
  println(io_out, "Comparing mad_tpsa.h to rtpsa.jl...")
  compare(fun_decs_c, fun_decs_jl)

  try
    Downloads.download("https://raw.githubusercontent.com/MethodicalAcceleratorDesign/MAD-NG/dev/src/mad_ctpsa.h", io)
    println(io_out, "mad_ctpsa.h downloaded.")
  catch e
    println(io_out, "Error downloading mad_ctpsa.h")
    showerror(stdout, e)
  end

  str = String(take!(io))
  fun_decs_c  = get_c_function_declarations(str)

  str = read("../src/low_level/ctpsa.jl", String)
  fun_decs_jl = get_jl_function_declarations(str)
  println(io_out, "Comparing mad_ctpsa.h to ctpsa.jl...")
  compare(fun_decs_c, fun_decs_jl)

  return String(take!(io_out))
end

#end=#
