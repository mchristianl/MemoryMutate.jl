using MemoryMutate

expr = :(a->b->c->d)

arr = zeros(Float64,10)
ptr = pointer(arr)
arr_ptr = convert(Ptr{Float64},pointer_from_objref(arr)) # is ≈64 Bytes below ptr
# because (?) in most architectures heap "grows upwards" where stack "grows downwards"
# Why stack grows down https://gist.github.com/cpq/8598782

ptr2 = ptr + (3-1)*sizeof(Float64)
view = unsafe_wrap(Array{Float64,1},ptr2,(1,); own=false)
view[1] = 2.0

function setat(ptr::Ptr{T},i::Int64,x::T) where T
    ptr2 = ptr + (i-1)*sizeof(T)
    view = unsafe_wrap(Array{T,1},ptr2,(1,); own=false)
    view[1] = x
end

################################################################################

struct G2; x :: Float32;               end # struct G2 { const float x; };
struct F2; x :: Float32; g ::     G2 ; end # struct F2 { const float x; const G2  g; };
struct E2; x :: Float32; f ::     F2 ; end # struct E2 { const float x; const F2  g; };
struct D2; x :: Float32; e :: Ptr{E2}; end # struct D2 {       float x;       E2  g; };
struct C2; x :: Float32; d ::     D2 ; end # struct C2 { const float x; const D2& g; };
struct B2; x :: Float32; c ::     C2 ; end # struct B2 { const float x; const C2& g; };
struct A2; x :: Float32; b ::     B2 ; end # struct A2 {       float x;       B2& g; };

g2 =     G2(6.0f0)     # NOTE: we cannot change g2 and f2, since they aren't allocated on the heap
f2 =     F2(5.0f0,g2)  #
e2 = Ref(E2(4.0f0,f2)) # here, we copy f2 (and it's inner g2) to the heap -> these ones, e2[].f, e2[].f.g and e2[].f.g.x, can be changed
d2 =     D2(3.0f0,pointer_from_objref(e2))
c2 =     C2(2.0f0,d2)
b2 =     B2(1.0f0,c2)
a2 =     A2(0.0f0,b2)

test201(a::A2,v::Float32) = @mem  a.b.c.d->e.f.g.x = v

v = 9f0; @assert e2[].f.g.x != v; test201(a2,v); @assert e2[].f.g.x == v;
