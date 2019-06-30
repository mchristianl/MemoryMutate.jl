using MemoryMutate

include("testutils.jl")

################################################################################

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

arr = Float32[0f0,1f0,2f0,3f0,4f0]
arrptr = pointer(arr)
v = 0f0; @assert arr[2] != v; @mem arrptr[2] = v; @assert arr[2] == v;


################################################################################

        struct G2; x :: Float32;               end # struct G2 { const float x; };
        struct F2; x :: Float32; g ::     G2 ; end # struct F2 { const float x; const G2  g; };
        struct E2; x :: Float32; f ::     F2 ; end # struct E2 { const float x; const F2  g; };
        struct D2; x :: Float32; e :: Ptr{E2}; end # struct D2 {       float x;       E2  g; };
        struct C2; x :: Float32; d ::     D2 ; end # struct C2 { const float x; const D2& g; };
        struct B2; x :: Float32; c ::     C2 ; end # struct B2 { const float x; const C2& g; };
mutable struct A2; x :: Float32; b ::     B2 ; end # struct A2 {       float x;       B2& g; };

g2    =     G2(6.0f0)     # NOTE: we cannot change g2 and f2, since they aren't allocated on the heap
f2    =     F2(5.0f0,g2)  #
e2    = Ref(E2(4.0f0,f2)) # here, we copy f2 (and it's inner g2) to the heap -> these ones, e2[].f, e2[].f.g and e2[].f.g.x, can be changed
e2ptr = pointer_from_objref(e2)
d2    =     D2(3.0f0,e2ptr)
c2    =     C2(2.0f0,d2)
b2    =     B2(1.0f0,c2)
a2    =     A2(0.0f0,b2)

test201(a::A2,v::Float32) = @mem  a.b.c.d.e->f.g.x = v
test202(a::A2,v::G2)      = @mem  a.b.c.d.e->f.g = v
test203(a::A2,v::Float32) = @mem  a.b.c.d.e->f.x = v
test204(a::A2,v::F2)      = @mem  a.b.c.d.e->f = v
test205(a::A2,v::Float32) = @mem  a.b.c.d.e->x = v
test206(a::A2,v::Ptr{E2}) = @mem  a.b.c.d.e = v
test207(a::A2,v::Float32) = @mem  a.b.c.d.x = v
test208(a::A2,v::D2)      = @mem  a.b.c.d = v
test209(a::A2,v::Float32) = @mem  a.b.c.x = v
test210(a::A2,v::C2)      = @mem  a.b.c = v
test211(a::A2,v::Float32) = @mem  a.b.x = v
test212(a::A2,v::B2)      = @mem  a.b = v
test213(a::A2,v::Float32) = @mem  a.x = v
test214(a::A2,v::A2)      = @mem  a = v

v  = 0f0
v += 1f0;                                @assert unsafe_load(a2.b.c.d.e).f.g.x  != v;           test201(a2,v);           @assert unsafe_load(a2.b.c.d.e).f.g.x  == v;
v += 1f0;                                @assert unsafe_load(a2.b.c.d.e).f.g    != G2(v);       test202(a2,G2(v));       @assert unsafe_load(a2.b.c.d.e).f.g    == G2(v);
v += 1f0;                                @assert unsafe_load(a2.b.c.d.e).f.x    != v;           test203(a2,v);           @assert unsafe_load(a2.b.c.d.e).f.x    == v;
v += 1f0;                                @assert unsafe_load(a2.b.c.d.e).f      != F2(v,g2);    test204(a2,F2(v,g2));    @assert unsafe_load(a2.b.c.d.e).f      == F2(v,g2);
v += 1f0;                                @assert unsafe_load(a2.b.c.d.e).x      != v;           test205(a2,v);           @assert unsafe_load(a2.b.c.d.e).x      == v;
v += 1f0; e2b, e2bptr = mkptr(E2(v,f2)); @assert             a2.b.c.d.e         != e2bptr;      test206(a2,e2bptr);      @assert             a2.b.c.d.e         == e2bptr;
v += 1f0;                                @assert             a2.b.c.d.x         != v;           test207(a2,v);           @assert             a2.b.c.d.x         == v;
v += 1f0;                                @assert             a2.b.c.d           != D2(v,e2ptr); test208(a2,D2(v,e2ptr)); @assert             a2.b.c.d           == D2(v,e2ptr);
v += 1f0;                                @assert             a2.b.c.x           != v;           test209(a2,v);           @assert             a2.b.c.x           == v;
v += 1f0;                                @assert             a2.b.c             != C2(v,d2);    test210(a2,C2(v,d2));    @assert             a2.b.c             == C2(v,d2);
v += 1f0;                                @assert             a2.b.x             != v;           test211(a2,v);           @assert             a2.b.x             == v;
v += 1f0;                                @assert             a2.b               != B2(v,c2);    test212(a2,B2(v,c2));    @assert             a2.b               == B2(v,c2);
v += 1f0;                                @assert             a2.x               != v;           test213(a2,v);           @assert             a2.x               == v;
v += 1f0;                                @assert             a2                 != A2(v,b2);    test214(a2,A2(v,b2));  # @assert             a2                 == A2(v,b2); # # call by "sharing"

code_native(io,test201,(A2,Float32)); display_asm_stat_io(io) # (total = 3, movs = 2, mov = 1, vmov = 1)
code_native(io,test202,(A2,G2));      display_asm_stat_io(io) # (total = 6, movs = 4, mov = 3, vmov = 1)
code_native(io,test203,(A2,Float32)); display_asm_stat_io(io) # (total = 3, movs = 2, mov = 1, vmov = 1)
code_native(io,test204,(A2,F2));      display_asm_stat_io(io) # (total = 6, movs = 5, mov = 3, vmov = 2)
code_native(io,test205,(A2,Float32)); display_asm_stat_io(io) # (total = 4, movs = 2, mov = 1, vmov = 1)
code_native(io,test206,(A2,Ptr{E2})); display_asm_stat_io(io) # (total = 4, movs = 2, mov = 2, vmov = 0)
code_native(io,test207,(A2,Float32)); display_asm_stat_io(io) # (total = 2, movs = 1, mov = 0, vmov = 1)
code_native(io,test208,(A2,D2));      display_asm_stat_io(io) # (total = 6, movs = 5, mov = 1, vmov = 4)
code_native(io,test209,(A2,Float32)); display_asm_stat_io(io) # (total = 2, movs = 1, mov = 0, vmov = 1)
code_native(io,test210,(A2,C2));      display_asm_stat_io(io) # (total = 10, movs = 9, mov = 5, vmov = 4)
code_native(io,test211,(A2,Float32)); display_asm_stat_io(io) # (total = 2, movs = 1, mov = 0, vmov = 1)
code_native(io,test212,(A2,B2));      display_asm_stat_io(io) # (total = 8, movs = 5, mov = 1, vmov = 4)
code_native(io,test213,(A2,Float32)); display_asm_stat_io(io) # (total = 2, movs = 1, mov = 0, vmov = 1)
code_native(io,test214,(A2,A2));      display_asm_stat_io(io) # (total = 3, movs = 2, mov = 2, vmov = 0)

################################################################################

struct G3; x :: Float32;               end
struct F3; x :: Float32; g ::     G3 ; end
struct E3; x :: Float32; f ::     F3 ; end
struct D3; x :: Float32; e :: Ptr{E3}; end
struct C3; x :: Float32; d ::     D3 ; end
struct B3; x :: Float32; c ::     C3 ; end
struct A3; x :: Float32; b ::     B3 ; end

g3    =     G3(6.0f0)     # NOTE: we cannot change g2 and f2, since they aren't allocated on the heap
f3    =     F3(5.0f0,g3)  #
e3    = Ref(E3(4.0f0,f3)) # here, we copy f2 (and it's inner g2) to the heap -> these ones, e2[].f, e2[].f.g and e2[].f.g.x, can be changed
e3ptr = Ptr{E3}(pointer_from_objref(e3))
d3    =     D3(3.0f0,e3ptr)
c3    =     C3(2.0f0,d3)
b3    =     B3(1.0f0,c3)
a3    = Ref(A3(0.0f0,b3))
a3ptr = Ptr{A3}(pointer_from_objref(a3))

@assert unsafe_load(unsafe_load(a3ptr).b.c.d.e).f.g.x == @mem a3ptr->b.c.d.e->f.g.x
@assert unsafe_load(unsafe_load(a3ptr).b.c.d.e).f.g   == @mem a3ptr->b.c.d.e->f.g
@assert unsafe_load(unsafe_load(a3ptr).b.c.d.e).f     == @mem a3ptr->b.c.d.e->f
@assert unsafe_load(unsafe_load(a3ptr).b.c.d.e)       == @mem a3ptr->b.c.d.e[]
@assert             unsafe_load(a3ptr).b.c.d.e        == @mem a3ptr->b.c.d.e
@assert             unsafe_load(a3ptr).b.c.d          == @mem a3ptr->b.c.d
@assert             unsafe_load(a3ptr).b.c            == @mem a3ptr->b.c
@assert             unsafe_load(a3ptr).b              == @mem a3ptr->b
@assert             unsafe_load(a3ptr)                == @mem a3ptr[]

test301(a::Ptr{A3},v::Float32) = @mem  a->b.c.d.e->f.g.x = v
test302(a::Ptr{A3},v::G3)      = @mem  a->b.c.d.e->f.g = v
test303(a::Ptr{A3},v::Float32) = @mem  a->b.c.d.e->f.x = v
test304(a::Ptr{A3},v::F3)      = @mem  a->b.c.d.e->f = v
test305(a::Ptr{A3},v::Float32) = @mem  a->b.c.d.e->x = v
test306(a::Ptr{A3},v::Ptr{E3}) = @mem  a->b.c.d.e = v
test307(a::Ptr{A3},v::Float32) = @mem  a->b.c.d.x = v
test308(a::Ptr{A3},v::D3)      = @mem  a->b.c.d = v
test309(a::Ptr{A3},v::Float32) = @mem  a->b.c.x = v
test310(a::Ptr{A3},v::C3)      = @mem  a->b.c = v
test311(a::Ptr{A3},v::Float32) = @mem  a->b.x = v
test312(a::Ptr{A3},v::B3)      = @mem  a->b = v
test313(a::Ptr{A3},v::Float32) = @mem  a->x = v
test314(a::Ptr{A3},v::A3)      = @mem  a[] = v

v  = 0f0
v += 1f0;                                @assert unsafe_load(unsafe_load(a3ptr).b.c.d.e).f.g.x  != v;           test301(a3ptr,v);           @assert unsafe_load(unsafe_load(a3ptr).b.c.d.e).f.g.x  == v;
v += 1f0;                                @assert unsafe_load(unsafe_load(a3ptr).b.c.d.e).f.g    != G3(v);       test302(a3ptr,G3(v));       @assert unsafe_load(unsafe_load(a3ptr).b.c.d.e).f.g    == G3(v);
v += 1f0;                                @assert unsafe_load(unsafe_load(a3ptr).b.c.d.e).f.x    != v;           test303(a3ptr,v);           @assert unsafe_load(unsafe_load(a3ptr).b.c.d.e).f.x    == v;
v += 1f0;                                @assert unsafe_load(unsafe_load(a3ptr).b.c.d.e).f      != F3(v,g3);    test304(a3ptr,F3(v,g3));    @assert unsafe_load(unsafe_load(a3ptr).b.c.d.e).f      == F3(v,g3);
v += 1f0;                                @assert unsafe_load(unsafe_load(a3ptr).b.c.d.e).x      != v;           test305(a3ptr,v);           @assert unsafe_load(unsafe_load(a3ptr).b.c.d.e).x      == v;
v += 1f0; e3b, e3bptr = mkptr(E3(v,f3)); @assert             unsafe_load(a3ptr).b.c.d.e         != e3bptr;      test306(a3ptr,e3bptr);      @assert             unsafe_load(a3ptr).b.c.d.e         == e3bptr;
v += 1f0;                                @assert             unsafe_load(a3ptr).b.c.d.x         != v;           test307(a3ptr,v);           @assert             unsafe_load(a3ptr).b.c.d.x         == v;
v += 1f0;                                @assert             unsafe_load(a3ptr).b.c.d           != D3(v,e3ptr); test308(a3ptr,D3(v,e3ptr)); @assert             unsafe_load(a3ptr).b.c.d           == D3(v,e3ptr);
v += 1f0;                                @assert             unsafe_load(a3ptr).b.c.x           != v;           test309(a3ptr,v);           @assert             unsafe_load(a3ptr).b.c.x           == v;
v += 1f0;                                @assert             unsafe_load(a3ptr).b.c             != C3(v,d3);    test310(a3ptr,C3(v,d3));    @assert             unsafe_load(a3ptr).b.c             == C3(v,d3);
v += 1f0;                                @assert             unsafe_load(a3ptr).b.x             != v;           test311(a3ptr,v);           @assert             unsafe_load(a3ptr).b.x             == v;
v += 1f0;                                @assert             unsafe_load(a3ptr).b               != B3(v,c3);    test312(a3ptr,B3(v,c3));    @assert             unsafe_load(a3ptr).b               == B3(v,c3);
v += 1f0;                                @assert             unsafe_load(a3ptr).x               != v;           test313(a3ptr,v);           @assert             unsafe_load(a3ptr).x               == v;
v += 1f0; a3b, a3bptr = mkptr(A3(v,b3)); @assert             unsafe_load(a3ptr)                 != A3(v,b3);    test314(a3ptr,A3(v,b3));    @assert             unsafe_load(a3ptr)                 == A3(v,b3);

code_native(io,test301,(Ptr{A3},Float32)); display_asm_stat_io(io) # (total = 3, movs = 2, mov = 1, vmov = 1)
code_native(io,test302,(Ptr{A3},G3));      display_asm_stat_io(io) # (total = 6, movs = 4, mov = 3, vmov = 1)
code_native(io,test303,(Ptr{A3},Float32)); display_asm_stat_io(io) # (total = 3, movs = 2, mov = 1, vmov = 1)
code_native(io,test304,(Ptr{A3},F3));      display_asm_stat_io(io) # (total = 6, movs = 5, mov = 3, vmov = 2)
code_native(io,test305,(Ptr{A3},Float32)); display_asm_stat_io(io) # (total = 4, movs = 2, mov = 1, vmov = 1)
code_native(io,test306,(Ptr{A3},Ptr{E3})); display_asm_stat_io(io) # (total = 4, movs = 2, mov = 2, vmov = 0)
code_native(io,test307,(Ptr{A3},Float32)); display_asm_stat_io(io) # (total = 2, movs = 1, mov = 0, vmov = 1)
code_native(io,test308,(Ptr{A3},D3));      display_asm_stat_io(io) # (total = 6, movs = 5, mov = 1, vmov = 4)
code_native(io,test309,(Ptr{A3},Float32)); display_asm_stat_io(io) # (total = 2, movs = 1, mov = 0, vmov = 1)
code_native(io,test310,(Ptr{A3},C3));      display_asm_stat_io(io) # (total = 10, movs = 9, mov = 5, vmov = 4)
code_native(io,test311,(Ptr{A3},Float32)); display_asm_stat_io(io) # (total = 2, movs = 1, mov = 0, vmov = 1)
code_native(io,test312,(Ptr{A3},B3));      display_asm_stat_io(io) # (total = 8, movs = 5, mov = 1, vmov = 4)
code_native(io,test313,(Ptr{A3},Float32)); display_asm_stat_io(io) # (total = 2, movs = 1, mov = 0, vmov = 1)
code_native(io,test314,(Ptr{A3},A3));      display_asm_stat_io(io) # (total = 7, movs = 5, mov = 3, vmov = 2)
