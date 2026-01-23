function show(io::IO, d::Descriptor)
  d.desc == C_NULL && (println(io, "Null"); return)
  print(io, "Descriptor(")
  desc = unsafe_load(d.desc)
  mo = desc.mo
  nv = desc.nv
  np = desc.np
  nn = desc.nn
  po = desc.po
  uno = desc.uno
  no_ = unsafe_wrap(Vector{Cuchar}, desc.no, nn)
  no = convert(Vector{Int}, no_)

  if np != 0 || uno != 0
    print(io, "NV=$nv, MO=$mo, NP=$np, PO=$po")
  else
    print(io, "NV=$nv, MO=$mo")
  end

  if uno != 0
    # print variables
    print(io, ", NO=[")
    for i in 1:2:nv-1
      if i == 1
        @printf(io, " %hhu %hhu", no[i], no[i+1])
      else
        @printf(io, "  %hhu %hhu", no[i], no[i+1])
      end
    end
    if mod(nv, 2) != 0
      @printf(io, "  %hhu", no[nv])
    end

    for i in nv+1:nv+np
      if no[i] != po
        @printf(io, "  %d^%hhu", i, no[i])
      end
    end
    print(io, "]")
    # FOR(i,nv,nv+np) if (ords[i] != po) fprintf(stream, "  %d^%hhu", i+1, ords[i]);
  end
  print(io, ")")
  return
end

function format_tps_contents(t::TPS{T,D}) where {T,D}
  tmp, __ = mktemp()
  fout = open(tmp, "w+")
  redirect_stdout(fout) do
    if T == Float64
      GTPSA.mad_tpsa_print(t, "", GTPSA.show_eps, Int32(1), C_NULL)
    else
      GTPSA.mad_ctpsa_print(t, "", GTPSA.show_eps, Int32(1), C_NULL)
    end
    Libc.flush_cstdio()
  end
  seek(fout, 1) # Ignore first line:
  str = read(fout, String)
  close(fout)

  # now get rid of the first 7 spaces in the first column
  lines = split(str, '\n')
  lines = map(lines) do line
    line[9:end]
  end
  return lines
end

function show(io::IO, t::TPS{T,D}) where {T,D}
  print(io, typeof(t))
  print(io, ":\n")
  extralines = 0
  if D == Dynamic
    println(io, getdesc(t))
    extralines += 1
  end

  lines = format_tps_contents(t)

  oversized=false
  if get(io, :limit, false) && length(lines) > displaysize(io)[1]-extralines-5
    oversized=true 
    lines = lines[1:(displaysize(io)[1]-extralines-5)]
  end
  print(io, join(lines, '\n'))
  if oversized
    print(io, "\n       ... (Output truncated)")
  end
  return
end

function show_vec(io, m)
  T = eltype(m)
  D = desctype(T)
  N = length(m)
  lines_used = 0
  if N < 1
    print(io,  eltype(m), "[]")
    return
  end
  println(io, N, "-element ", typeof(m), ":")
  lines_used += 1
  for i in eachindex(m)
    if !isassigned(m, i)
      println(io, "\n\tAtleast one $(eltype(m)) is undefined!")
      return
    end
  end
  desc = first(m).d
  diffdescs = false
  for i in eachindex(m)
    if !diffdescs && desc != m[i].d
      println(io, "WARNING: Atleast one $(eltype(m)) has a different Descriptor!")
      diffdescs = true
      lines_used += 1
    end
  end
  if !diffdescs && D == Dynamic
    println(io, Descriptor(desc))
    lines_used += 1
  end
  tpsouts = Any[]
  coef_str = "INDEX  COEFFICIENT             ORDER   EXPONENTS"
  println(io, coef_str)
  lines_used += 1
  oversized=false
  #@show lines_used
  for i in eachindex(m)
    lines_used += 1 # For the line ----
    if diffdescs # for the Descriptor
      lines_used += 1
    end
    t = m[i]
    lines = format_tps_contents(t)
    lines = map(lines[2:end-1]) do line
      @sprintf(" %2i:  %s", i, line)
    end

    if get(io, :limit, false) && lines_used + length(lines) > displaysize(io)[1]-4
      oversized=true
      #@show lines_used
      #@show lines_used-(displaysize(io)[1]-6)
      #@show length(lines)
     ## @show min(lines_used-(displaysize(io)[1]-6), length(lines))
      lines = lines[1:min(displaysize(io)[1]-4-lines_used, length(lines))] #lines_used-displaysize(io)[1]-4]
      if !isempty(lines) 
        push!(tpsouts, join(lines, '\n')) 
      end
      break
    else
      lines_used += length(lines)
    end
    push!(tpsouts, join(lines, '\n')) 
  end
  
  line_length = findmax(tpsouts) do tpsout
    findmax(length.(split(tpsout, '\n')))[1]
  end
  
  for (tps,tpsout) in zip(m,tpsouts)
    println(io, repeat('-', max(line_length[1], length(coef_str))))
    if diffdescs
      println(io, Descriptor(tps.d))
      #println(io, Descriptor(desc))
    end
    println(io, tpsout)
  end
  if oversized
    print(io, "       ... (Output truncated)")
    return
  end
  return
end

show(io::IO, m::AbstractArray{<:TPS{T}}) where {T} = show_vec(io, m)
show(io::IO, ::MIME"text/plain", m::AbstractArray{<:TPS{T}}) where {T} = show_vec(io, m)
