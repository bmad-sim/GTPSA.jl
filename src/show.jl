
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
  EPS = 0 # 1e-100

  # If nn > 6 (6 chosen arbitrarily), use sparse monomial format for print
  if nn <= 6
    out = Matrix{Any}(undef, 0, (coloffset+1+1+1+nn)) # Coefficient, order, spacing, exponents
    idx = Cint(-1)
    idx = mad_tpsa_cycle!(t.tpsa, idx, nn, mono, v)
    while idx >= 0
      order = Int(sum(mono))
      if abs(v[]) > EPS
        out = vcat(out, Any[repeat([nothing], coloffset)... v[] order nothing convert(Vector{Int}, mono)...])
      end
      idx = mad_tpsa_cycle!(t.tpsa, idx, nn, mono, v)
    end
    if size(out)[1] == 0
      out = vcat(out, Any[repeat([nothing], coloffset)... 0.0 Int(0) nothing zeros(Int,nn)...])
    end
    formatters = (ft_printf("%23.16le", [coloffset+1]), ft_printf("%2i", coloffset+2:coloffset+3+nn), ft_nonothing)
  else
    out = Matrix{Any}(nothing, 0, (coloffset+1+1+1+1))
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
      if abs(v[]) > EPS
        out = vcat(out, Any[repeat([nothing], coloffset)... v[] order nothing mono_display])
      end
      idx = mad_tpsa_cycle!(t.tpsa, idx, nn, mono, v)
    end
    if size(out)[1] == 0
      out = vcat(out, Any[repeat([nothing], coloffset)... 0.0 Int(0) nothing 1])
    end
    formatters = (ft_printf("%23.16le", [coloffset+1]), ft_printf("%2i", coloffset+2), ft_nonothing)
  end
  return out, formatters
end

function show(io::IO, t::TPS)
  out, formatters = format(t)
  println(io, "TPS:")
  # Check if sparse monomial or exponent:
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d))
  nn = desc.nn
  if nn > 6
    println(io, "  Coefficient              Order     Monomial")
  else
    println(io, "  Coefficient              Order     Exponent")
  end
  # Remove two lines from display size
  pretty_table(io, out,tf=tf_borderless,formatters=formatters,show_header=false, alignment=:l,display_size=(displaysize(io)[1]-4,displaysize(io)[2]))
end

function format(t::ComplexTPS; coloffset=0)
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d))
  nv = desc.nv
  np = desc.np
  nn = desc.nn
  v = Ref{ComplexF64}()
  mono = Vector{UInt8}(undef, nn)
  EPS = 0 # 1e-100

  # If nn > 6 (6 chosen arbitrarily), use sparse monomial format for print
  if nn <= 6
    out = Matrix{Any}(undef, 0, (coloffset+1+1+1+1+nn)) # First col is coefficient, rest are orders
    idx = Cint(-1)
    idx = mad_ctpsa_cycle!(t.tpsa, idx, nn, mono, v)
    while idx >= 0
      order = Int(sum(mono))
      if abs(v[]) > EPS
        out = vcat(out, Any[repeat([nothing], coloffset)... real(v[]) imag(v[]) order nothing convert(Vector{Int}, mono)...])
      end
      idx = mad_ctpsa_cycle!(t.tpsa, idx, nn, mono, v)
    end
    if size(out)[1] == 0
      out = vcat(out, Any[repeat([nothing], coloffset)... 0.0 0.0 Int(0) nothing zeros(Int,nn)...])
    end
    formatters = (ft_printf("%23.16le", [coloffset+1]),ft_printf("%23.16le", [coloffset+2]), ft_printf("%2i", coloffset+3:coloffset+4+nn), ft_nonothing)
  else
    out = Matrix{Any}(nothing, 0, (coloffset+1+1+1+1+1))
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
      if abs(v[]) > EPS
        out = vcat(out, Any[repeat([nothing], coloffset)... real(v[]) imag(v[]) order nothing mono_display])
      end
      idx = mad_ctpsa_cycle!(t.tpsa, idx, nn, mono, v)
    end
    if size(out)[1] == 0
      out = vcat(out, Any[repeat([nothing], coloffset)... 0.0 0.0 Int(0) nothing 1])
    end
    formatters = (ft_printf("%23.16le", [coloffset+1]),ft_printf("%23.16le", [coloffset+2]), ft_printf("%2i", coloffset+3), ft_nonothing)
  end
  return out, formatters
end

function show(io::IO, t::ComplexTPS)
  out, formatters = format(t)
  println(io, "ComplexTPS:")
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(t.tpsa).d))
  nn = desc.nn
  if nn > 6
    println(io, "  Real                      Imag                     Order     Monomial")
  else
    println(io, "  Real                      Imag                     Order     Exponent")
  end
  pretty_table(io, out,tf=tf_borderless,formatters=formatters,show_header=false, alignment=:l,display_size=(displaysize(io)[1]-4,displaysize(io)[2]))
end

function show(io::IO, ::MIME"text/plain", m::Vector{TPS})
  for i in eachindex(m)
    if !isassigned(m, i)
      show(io, m)
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
  N = length(m)
  if N < 1
    print(io, m)
    return
  end
  hlines = Int[0]
  out, formatters = format(m[1], coloffset=1)
  out[:,1] .= 1
  for i=2:N
    push!(hlines, length(out[:,1]))
    tmpout, __ = format(m[i],coloffset=1)
    tmpout[:,1] .= i
    out = vcat(out, tmpout)
  end
  println(io, N, "-element Vector{TPS}:")
  # Check if sparse monomial or exponent:
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(m[1].tpsa).d))
  nn = desc.nn
  if nn > 6
    println(io, "  Out   Coefficient              Order     Monomial")
  else
    println(io, "  Out   Coefficient              Order     Exponent")
  end
  pretty_table(io, out,tf=tf_GTPSA,formatters=(ft_printf("%2i:",1), formatters...),show_header=false, alignment=:l, hlines=hlines, body_hlines_format=('-','-','-','-'),display_size=(displaysize(io)[1]-4,displaysize(io)[2]))
end

function show(io::IO, ::MIME"text/plain", m::Vector{ComplexTPS})
  for i in eachindex(m)
    if !isassigned(m, i)
      show(io, m)
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
  N = length(m)
  if N < 1
    print(io, m)
    return
  end
  hlines = Int[0]
  out, formatters = format(m[1], coloffset=1)
  out[:,1] .= 1
  for i=2:N
    push!(hlines, length(out[:,1]))
    tmpout, __ = format(m[i],coloffset=1)
    tmpout[:,1] .= i
    out = vcat(out, tmpout)
  end
  println(io, N, "-element Vector{ComplexTPS}:")
  # Check if sparse monomial or exponent:
  desc = unsafe_load(Base.unsafe_convert(Ptr{Desc}, unsafe_load(m[1].tpsa).d))
  nn = desc.nn
  if nn > 6
    println(io, "  Out   Real                      Imag                     Order     Monomial")
  else
    println(io, "  Out   Real                      Imag                     Order     Exponent")
  end
  
  pretty_table(io, out,tf=tf_GTPSA,formatters=(ft_printf("%2i:",1), formatters...),show_header=false, alignment=:l, hlines=hlines, body_hlines_format=('-','-','-','-'),display_size=(displaysize(io)[1]-4,displaysize(io)[2]))
end
