
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

function format(t::TPS; coloffset=0)
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d))
  nv = desc.nv
  np = desc.np
  nn = desc.nn
  v = Ref{Cdouble}()
  mono = Vector{UInt8}(undef, nn)

  if !GTPSA.show_sparse
    if np > 0
      out = Matrix{Any}(undef, 0, (coloffset+1+1+1+1+1+nn)) # Coefficient, order, spacing, exponents
    else
      out = Matrix{Any}(undef, 0, (coloffset+1+1+1+1+nn)) # Coefficient, order, spacing, exponents
    end
    idx = Cint(-1)
    idx = mad_tpsa_cycle!(t.tpsa, idx, nn, mono, v)
    while idx >= 0
      order = Int(sum(mono))
      if abs(v[]) > GTPSA.show_eps
        if np > 0
          out = vcat(out, Any[repeat([nothing], coloffset)... v[] nothing order nothing convert(Vector{Int}, mono[1:nv])... " |" convert(Vector{Int}, mono[nv+1:end])...])
        else
          out = vcat(out, Any[repeat([nothing], coloffset)... v[] nothing order nothing convert(Vector{Int}, mono)...])
        end
      end
      idx = mad_tpsa_cycle!(t.tpsa, idx, nn, mono, v)
    end
    if size(out)[1] == 0
      if np > 0
        out = vcat(out, Any[repeat([nothing], coloffset)... 0.0 nothing Int(0) nothing zeros(Int,nv)... " |" zeros(Int,np)...])
      else
        out = vcat(out, Any[repeat([nothing], coloffset)... 0.0 nothing Int(0) nothing zeros(Int,nn)...])
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
  # Check if sparse monomial or exponent:
  if GTPSA.show_sparse
    println(io, " Coefficient                Order   Monomial")
  else
    println(io, " Coefficient                Order   Exponent")
  end
  # Remove two lines from display size
  pretty_table(io, out,tf=tf_borderless,formatters=formatters,show_header=false, alignment=:l,display_size=(displaysize(io)[1]-4,displaysize(io)[2]),vlines=[])
end


function format(t::ComplexTPS; coloffset=0)
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d))
  nv = desc.nv
  np = desc.np
  nn = desc.nn
  v = Ref{ComplexF64}()
  mono = Vector{UInt8}(undef, nn)

  # sparse monomial or order output
  if !GTPSA.show_sparse
    if np > 0
      out = Matrix{Any}(undef, 0, (coloffset+1+1+1+1+1+1+nn)) # First col is coefficient, rest are orders
    else
      out = Matrix{Any}(undef, 0, (coloffset+1+1+1+1+1+nn)) # First col is coefficient, rest are orders
    end
    idx = Cint(-1)
    idx = mad_ctpsa_cycle!(t.tpsa, idx, nn, mono, v)
    while idx >= 0
      order = Int(sum(mono))
      if abs(v[]) > GTPSA.show_eps
        if np > 0
          out = vcat(out, Any[repeat([nothing], coloffset)... real(v[]) imag(v[]) nothing order nothing convert(Vector{Int}, mono[1:nv])... " |" convert(Vector{Int}, mono[nv+1:end])...])
        else
          out = vcat(out, Any[repeat([nothing], coloffset)... real(v[]) imag(v[]) nothing order nothing convert(Vector{Int}, mono)...])
        end
      end
      idx = mad_ctpsa_cycle!(t.tpsa, idx, nn, mono, v)
    end
    if size(out)[1] == 0
      if np > 0
        out = vcat(out, Any[repeat([nothing], coloffset)... 0.0 0.0 nothing Int(0) nothing zeros(Int,nv)... " |" zeros(Int,np)...])
      else
        out = vcat(out, Any[repeat([nothing], coloffset)... 0.0 0.0 nothing Int(0) nothing zeros(Int,nn)...])
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
  nn = desc.nn
  if GTPSA.show_sparse
    println(io, " Real                     Imag                       Order   Monomial")
  else
    println(io, " Real                     Imag                       Order   Exponent")
  end
  pretty_table(io, out,tf=tf_borderless,formatters=formatters,show_header=false, alignment=:l,display_size=(displaysize(io)[1]-4,displaysize(io)[2]),vlines=[])
end

show(io::IO, m::Vector{<:Union{TPS,ComplexTPS}}) = show_map(io, m)
show(io::IO, ::MIME"text/plain", m::Vector{<:Union{TPS,ComplexTPS}}) = show_map(io, m)

function show_map(io::IO, m::Vector{TPS})
  N = length(m)
  if N < 1
    print(io, "TPS[]")
    return
  end
  println(io, N, "-element Vector{TPS}:")
  for i in eachindex(m)
    if !isassigned(m, i)
      println(io, "\n\tAtleast one TPS in the Vector is undefined!")
      return
    end
  end
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
  hlines = Int[0]
  out, formatters = format(m[1], coloffset=1)
  out[:,1] .= 1
  for i=2:N
    push!(hlines, length(out[:,1]))
    tmpout, __ = format(m[i],coloffset=1)
    tmpout[:,1] .= i
    out = vcat(out, tmpout)
  end
  # Check if sparse monomial or exponent:
  if GTPSA.show_sparse
    println(io, "  Out  Coefficient                Order   Monomial")
  else
    println(io, "  Out  Coefficient                Order   Exponent")
  end
  pretty_table(io, out,tf=tf_GTPSA,formatters=(ft_printf("%3i:",1), formatters...),show_header=false, alignment=:l, hlines=hlines, body_hlines_format=('-','-','-','-'),display_size=(displaysize(io)[1]-4,displaysize(io)[2]),vlines=[])
end

function show_map(io::IO,  m::Vector{ComplexTPS})  
  N = length(m)
  if N < 1
    print(io, "ComplexTPS[]")
    return
  end
  println(io, N, "-element Vector{ComplexTPS}:")
  for i in eachindex(m)
    if !isassigned(m, i)
      println(io, "\n\tAtleast one ComplexTPS in the Vector is undefined!")
      return
    end
  end
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
                       #vlines              = :all);
  hlines = Int[0]
  out, formatters = format(m[1], coloffset=1)
  out[:,1] .= 1
  for i=2:N
    push!(hlines, length(out[:,1]))
    tmpout, __ = format(m[i],coloffset=1)
    tmpout[:,1] .= i
    out = vcat(out, tmpout)
  end
  # Check if sparse monomial or exponent:
  if GTPSA.show_sparse
    println(io, "  Out  Real                     Imag                       Order   Monomial")
  else
    println(io, "  Out  Real                     Imag                       Order   Exponent")
  end
  
  pretty_table(io, out,tf=tf_GTPSA,formatters=(ft_printf("%3i:",1), formatters...),show_header=false, alignment=:l, hlines=hlines, body_hlines_format=('-','-','-','-'),display_size=(displaysize(io)[1]-4,displaysize(io)[2]),vlines=[])
end
