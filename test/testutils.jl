filter_asm(io::IOBuffer) = filter(l -> !startswith(l,";") && !isempty(l) && !startswith(l,"\tnopw") && !startswith(l,"\t.text"), split(String(take!(io)),"\n"))
asm_stat(ss::Array{SubString{String},1}) =
  ( total = length(ss)
  , movs = length(filter(l -> occursin("mov",l),ss))
  , mov  = length(filter(l -> startswith(l,"\tmov"),ss))
  , vmov = length(filter(l -> startswith(l,"\tvmov"),ss))
  )
display_asm_stat_io(io::IOBuffer) = display(asm_stat(filter_asm(io)))
io = IOBuffer()
structinfo(T) = [(fieldoffset(T,i), fieldname(T,i), fieldtype(T,i)) for i = 1:fieldcount(T)];

function mkptr(x::T) where T
  r = Ref{T}(x)
  return (r, Ptr{T}(pointer_from_objref(r)))
end
