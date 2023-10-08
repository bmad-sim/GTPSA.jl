# Julia script to compare current TPSA.jl with latest
# MAD_TPSA code from the MAD Git repo:
# https://github.com/MethodicalAcceleratorDesign/MAD
using Downloads

function get_function_declarations(str)
  fun_decs::Vector{String} = []

  # Remove multiline comments
  i = 1
  while occursin("/*", str)
    len = length(str)
    start_index = findnext("/*", str, i)[1] - 1
    end_index = findnext("*/", str, i)[2] + 1
    str = str[1:start_index] * str[end_index:len]
    i = end_index
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
        println(full_line)
    end
    i += 1
  end
  return fun_decs
end


io = IOBuffer()

try
  Downloads.download("https://raw.githubusercontent.com/MethodicalAcceleratorDesign/MAD/dev/src/mad_desc.h", io)
  println("mad_desc.h downloaded.")
catch e
  println("Error downloading mad_desc.h")
  showerror(stdout, e)
end

try
  Downloads.download("https://raw.githubusercontent.com/MethodicalAcceleratorDesign/MAD/dev/src/mad_tpsa.h", io)
  println("mad_tpsa.h downloaded.")
catch e
  println("Error downloading mad_tpsa.h")
  showerror(stdout, e)
end

str = String(take!(io))
fun_decs = get_function_declarations(str)