
# --- print ---
function show_GTPSA_info(io::IO, desc::Desc)
  nv = desc.nv
  np = desc.np
  nn = desc.nn
  no_ = unsafe_wrap(Vector{Cuchar}, desc.no, nn)
  no = convert(Vector{Int}, no_)
  if nv > 0
    @printf(io, "%-18s %i\n", "# Variables: ", nv)
    if all(no[1] .== no[1:nv])
      @printf(io, "%-18s %i\n", "Variable order: ", no[1])
    else
      @printf(io, "%-18s", "Variable orders: ")
      print(io, no[1:nv])
      print(io, "\n")
    end
  end
  if np > 0
    @printf(io, "%-18s %i\n", "# Parameters: ", np)
    if all(no[nv+1] .== no[nv+1:end])
      @printf(io, "%-18s %i\n", "Parameter order: ", no[nv+1])
    else
      @printf(io, "%-18s", "Parameter orders: ")
      print(io, no[nv+1:end])
      print(io, "\n")
    end
  end
end

function show(io::IO, d::Descriptor)
  println(io, "GTPSA Descriptor")
  println(io, "-----------------------")
  desc = unsafe_load(d.desc)
  show_GTPSA_info(io, desc)
end

struct MonoDisplay
  varidxs::Vector{Int}
  varords::Vector{Int}
  paramidxs::Vector{Int}
  paramords::Vector{Int}
end

function show(io::IO, m::MonoDisplay)
  subscript(i) = join(Char(0x2080 + d) for d in reverse!(digits(i)))
  function superscript(i)
    if i < 0
        c = [Char(0x207B)]
    else
        c = []
    end
    for d in reverse(digits(abs(i)))
        if d == 0 push!(c, Char(0x2070)) end
        if d == 1 push!(c, Char(0x00B9)) end
        if d == 2 push!(c, Char(0x00B2)) end
        if d == 3 push!(c, Char(0x00B3)) end
        if d > 3 push!(c, Char(0x2070+d)) end
    end
    return join(c)
  end
  for i=1:length(m.varidxs)
    varidx = m.varidxs[i]
    varord = m.varords[i]
    print(io, "(x" * subscript(varidx) * ")" * superscript(varord) * " ")
  end
  for i=1:length(m.paramidxs)
    paramidx = m.paramidxs[i]
    paramord = m.paramords[i]
    print(io, "(k" * subscript(paramidx) * ")" * superscript(paramord) * " ")
  end
end

function format(t::TPS; coloffset=0, max_nn=-1)
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d))
  nv = desc.nv
  np = desc.np
  nn = desc.nn

  if max_nn == -1
    max_nn = nn
  end

  v = Ref{Cdouble}()
  mono = Vector{UInt8}(undef, nn)

  if !GTPSA.show_sparse
    out = Matrix{Any}(undef, 0, (coloffset+1+1+1+1+1+max_nn)) # Coefficient, order, spacing, exponents
    idx = Cint(-1)
    idx = mad_tpsa_cycle!(t.tpsa, idx, nn, mono, v)
    while idx >= 0
      order = Int(sum(mono))
      if abs(v[]) > GTPSA.show_eps
        if np > 0
          out = vcat(out, Any[repeat([nothing], coloffset)... v[] nothing order nothing convert(Vector{Int}, mono[1:nv])... " |" convert(Vector{Int}, mono[nv+1:end])... repeat([nothing], max_nn-nn)...])
        else
          out = vcat(out, Any[repeat([nothing], coloffset)... v[] nothing order nothing convert(Vector{Int}, mono)... nothing repeat([nothing], max_nn-nn)...])
        end
      end
      idx = mad_tpsa_cycle!(t.tpsa, idx, nn, mono, v)
    end
    if size(out)[1] == 0
      if np > 0
        out = vcat(out, Any[repeat([nothing], coloffset)... 0.0 nothing Int(0) nothing zeros(Int,nv)... " |" zeros(Int,np)... repeat([nothing], max_nn-nn)...])
      else
        out = vcat(out, Any[repeat([nothing], coloffset)... 0.0 nothing Int(0) nothing zeros(Int,nn)... nothing repeat([nothing], max_nn-nn)...])
      end
    end
    formatters = (ft_printf("%23.16le", [coloffset+1]), ft_printf("%2i", coloffset+2:length(out[1,:])), ft_nonothing)
  else
    out = Matrix{Any}(nothing, 0, (coloffset+1+1+1+1+1))
    idx = Cint(-1)
    idx = mad_tpsa_cycle!(t.tpsa, idx, nn, mono, v)
    while idx >= 0
      order = Int(sum(mono))
      varidxs = Vector{Int}(undef,0)
      varords = Vector{Int}(undef,0)
      paramidxs = Vector{Int}(undef,0)
      paramords = Vector{Int}(undef,0)
      # Create variable pairs
      for vp_idx in findall(x->x>0x0, mono)
        if vp_idx > nv
          push!(paramidxs, vp_idx-nv)
          push!(paramords, Int(mono[vp_idx]))
        else
          push!(varidxs, vp_idx)
          push!(varords, Int(mono[vp_idx]))
        end
      end
      if iszero(varords) && iszero(paramords)
        mono_display=1
      else
        mono_display = MonoDisplay(varidxs, varords, paramidxs, paramords)
      end
      if abs(v[]) > GTPSA.show_eps
        out = vcat(out, Any[repeat([nothing], coloffset)... v[] nothing order nothing mono_display])
      end
      idx = mad_tpsa_cycle!(t.tpsa, idx, nn, mono, v)
    end
    if size(out)[1] == 0
      out = vcat(out, Any[repeat([nothing], coloffset)... 0.0 nothing Int(0) nothing 1])
    end
    formatters = (ft_printf("%23.16le", [coloffset+1]), ft_printf("%2i", coloffset+3), ft_nonothing)
  end
  return out, formatters
end

function show(io::IO, t::TPS)
  out, formatters = format(t)
  println(io, "TPS:")
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d))
  extralines = 0
  if GTPSA.show_header
    println(io, "-----------------------")
    show_GTPSA_info(io, desc)
    println(io, "-----------------------")
    extralines = 2 + (desc.nv > 0 ? 2 : 0) + (desc.np > 0 ? 2 : 0)
  end
  # Check if sparse monomial or exponent:
  if GTPSA.show_sparse
    println(io, " Coefficient                Order   Monomial")
  else
    println(io, " Coefficient                Order   Exponent")
  end
  # Remove two lines from display size
  pretty_table(io, out,tf=tf_borderless,formatters=formatters,show_header=false, alignment=:l,display_size=(displaysize(io)[1]-4-extralines,displaysize(io)[2]),vlines=[])
end


function format(t::ComplexTPS; coloffset=0, max_nn=-1)
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d))
  nv = desc.nv
  np = desc.np
  nn = desc.nn
  if max_nn == -1
    max_nn = nn
  end

  v = Ref{ComplexF64}()
  mono = Vector{UInt8}(undef, nn)

  # sparse monomial or order output
  if !GTPSA.show_sparse
    out = Matrix{Any}(undef, 0, (coloffset+1+1+1+1+1+1+max_nn)) # First col is coefficient, rest are orders
    idx = Cint(-1)
    idx = mad_ctpsa_cycle!(t.tpsa, idx, nn, mono, v)
    while idx >= 0
      order = Int(sum(mono))
      if abs(v[]) > GTPSA.show_eps
        if np > 0
          out = vcat(out, Any[repeat([nothing], coloffset)... real(v[]) imag(v[]) nothing order nothing convert(Vector{Int}, mono[1:nv])... " |" convert(Vector{Int}, mono[nv+1:end])... repeat([nothing], max_nn-nn)...])
        else
          out = vcat(out, Any[repeat([nothing], coloffset)... real(v[]) imag(v[]) nothing order nothing convert(Vector{Int}, mono)... nothing repeat([nothing], max_nn-nn)...])
        end
      end
      idx = mad_ctpsa_cycle!(t.tpsa, idx, nn, mono, v)
    end
    if size(out)[1] == 0
      if np > 0
        out = vcat(out, Any[repeat([nothing], coloffset)... 0.0 0.0 nothing Int(0) nothing zeros(Int,nv)... " |" zeros(Int,np)... repeat([nothing], max_nn-nn)...])
      else
        out = vcat(out, Any[repeat([nothing], coloffset)... 0.0 0.0 nothing Int(0) nothing zeros(Int,nn)... nothing repeat([nothing], max_nn-nn)...])
      end
    end
    formatters = (ft_printf("%23.16le", [coloffset+1]),ft_printf("%23.16le", [coloffset+2]), ft_printf("%2i", coloffset+3:length(out[1,:])), ft_nonothing)
  else
    out = Matrix{Any}(nothing, 0, (coloffset+1+1+1+1+1+1))
    idx = Cint(-1)
    idx = mad_ctpsa_cycle!(t.tpsa, idx, nn, mono, v)
    while idx >= 0
      order = Int(sum(mono))
      varidxs = Vector{Int}(undef,0)
      varords = Vector{Int}(undef,0)
      paramidxs = Vector{Int}(undef,0)
      paramords = Vector{Int}(undef,0)
      # Create variable pairs
      for vp_idx in findall(x->x>0x0, mono)
        if vp_idx > nv
          push!(paramidxs, vp_idx-nv)
          push!(paramords, Int(mono[vp_idx]))
        else
          push!(varidxs, vp_idx)
          push!(varords, Int(mono[vp_idx]))
        end
      end
      if iszero(varords) && iszero(paramords)
        mono_display=1
      else
        mono_display = MonoDisplay(varidxs, varords, paramidxs, paramords)
      end
      if abs(v[]) > GTPSA.show_eps
        out = vcat(out, Any[repeat([nothing], coloffset)... real(v[]) imag(v[]) nothing order nothing mono_display])
      end
      idx = mad_ctpsa_cycle!(t.tpsa, idx, nn, mono, v)
    end
    if size(out)[1] == 0
      out = vcat(out, Any[repeat([nothing], coloffset)... 0.0 0.0 nothing Int(0) nothing 1])
    end
    formatters = (ft_printf("%23.16le", [coloffset+1]),ft_printf("%23.16le", [coloffset+2]), ft_printf("%2i", coloffset+4), ft_nonothing)
  end
  return out, formatters
end

function show(io::IO, t::ComplexTPS)
  out, formatters = format(t)
  println(io, "ComplexTPS:")
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d))
  extralines = 0
  if GTPSA.show_header
    println(io, "-----------------------")
    show_GTPSA_info(io, desc)
    println(io, "-----------------------")
    extralines = 2 + (desc.nv > 0 ? 2 : 0) + (desc.np > 0 ? 2 : 0)
  end
  if GTPSA.show_sparse
    println(io, " Real                     Imag                       Order   Monomial")
  else
    println(io, " Real                     Imag                       Order   Exponent")
  end
  pretty_table(io, out,tf=tf_borderless,formatters=formatters,show_header=false, alignment=:l,display_size=(displaysize(io)[1]-4-extralines,displaysize(io)[2]),vlines=[])
end

show(io::IO, m::Vector{<:Union{TPS,ComplexTPS}}) = show_vec(io, m)
show(io::IO, ::MIME"text/plain", m::Vector{<:Union{TPS,ComplexTPS}}) = show_vec(io, m)

function show_vec(io::IO, m::Vector{<:Union{TPS,ComplexTPS}})
  N = length(m)
  lines_used=Ref{Int}(0)
  if N < 1
    print(io, "$(eltype(m))[]")
    return
  end
  println(io, N, "-element $(typeof(m)):")
  lines_used[] += 1
  for i in eachindex(m)
    if !isassigned(m, i)
      println(io, "\n\tAtleast one $(eltype(m)) is undefined!")
      return
    end
  end
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(m[1].tpsa).d))
  diffdescs = false
  for i in eachindex(m)
    if desc != unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(m[i].tpsa).d))
      println(io, "WARNING: Atleast one $(eltype(m)) has a different Descriptor!")
      diffdescs = true
      lines_used[] += 1
    end
  end
  if GTPSA.show_header
    if diffdescs
      println(io, "Cannot show GTPSA header (non-unique Descriptor).")
      lines_used[] += 1
    else
      println(io, "-----------------------")
      show_GTPSA_info(io, desc)
      println(io, "-----------------------")
      lines_used[] += 2 + (desc.nv > 0 ? 2 : 0) + (desc.np > 0 ? 2 : 0)
    end
  end
  show_map!(io, m, lines_used)
end

# WARNING: only_vars should ONLY be set by developers who know what they're doing!
# Same for varnames!
function show_map!(io::IO, m::Vector{<:Union{TPS,ComplexTPS}}, lines_used::Ref=Ref{Int}(0), only_vars=false, varnames=1:length(m))
  N = only_vars ? min(unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(m[1].tpsa).d)).nv,length(m)) : length(m)
  length(varnames)== N  || error("invalid varnames length")
  tf_GTPSA = TextFormat(up_right_corner     = '-',
                       up_left_corner      = '-',
                       bottom_left_corner  = ' ',
                       bottom_right_corner = ' ',
                       up_intersection     = '-',
                       left_intersection   = '-',
                       right_intersection  = ' ',
                       middle_intersection = ' ',
                       bottom_intersection = ' ',
                       column              = ' ',
                       row                 = '-',
                       hlines              = [])#,
                       #vlines             =[]);


  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(m[1].tpsa).d))
  max_nn = desc.nv + desc.np
  for i in eachindex(m)
    curdesc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(m[i].tpsa).d))
    if only_vars && (desc.nv != curdesc.nv)
      error("Cannot use only_vars = true for vector with TPSs having different number of variables")
    end
    max_nn = max(max_nn, curdesc.nv+curdesc.np)
  end
  hlines = Int[0]
  out, formatters = format(m[1], coloffset=1, max_nn=max_nn)

  out[:,1] .= varnames[1]
  for i=2:N
    push!(hlines, length(out[:,1]))
    tmpout, __ = format(m[i],coloffset=1, max_nn=max_nn)
    tmpout[:,1] .= varnames[i]
    out = vcat(out, tmpout)
  end
  # Check if sparse monomial or exponent:
  !get(io, :limit, false) || lines_used[] < displaysize(io)[1]-5 || (println(io, "\t⋮"); return)
  cols = length(out[1,:])
  if GTPSA.show_sparse
    if eltype(m) == TPS
      println(io, "  Out  Coefficient                Order   Monomial")
    else # ComplexTPS
      println(io, "  Out  Real                     Imag                       Order   Monomial")
    end
  else
    if eltype(m) == TPS
      println(io, "  Out  Coefficient                Order   Exponent")
      cols += 105
    else
      println(io, "  Out  Real                     Imag                       Order   Exponent")
      cols += 108
    end
  end
  lines_used[] += 1
  !get(io, :limit, false) || lines_used[] < displaysize(io)[1]-5 || (println(io, "\t⋮"); return)
  pretty_table(io, out,tf=tf_GTPSA,formatters=(ft_printf("%3i:",1), formatters...),show_header=false, alignment=:l, hlines=hlines, body_hlines_format=('-','-','-','-'),display_size=(displaysize(io)[1]-2-lines_used[],displaysize(io)[2]),vlines=[])
  lines_used[] += length(out[:,1])+N # each border is a line
  if cols >= displaysize(io)[2] #&& !(lines_used[]+1 < displaysize(io)[1]-5) 
    lines_used[] += 1
  end
end
