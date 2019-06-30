module MemoryMutate
  using StaticArrays

  # TODO:
  #  undefined symbols weird error
  #    changed `throw` to `error`
  #    but this issue might be raised by `@assert`
  #  allow C-like syntax a->b->c in @mem and @ptr
  #    allow @mem to have no rhs then
  # we could, for non-bitstypes, allow `@mem a = x` to implement `unsafe_store!(Ptr{A}(pointer_from_objref(a)), x)`
  # Another reason to support this is, that when a C function is given a pointer (e.g. because it takes it's argument by reference) which resides inside of an allocated bitstype struct …
  #   put in another way: when we want to pass a C-reference/pointer which refers to a part of an (immutable) bitstype, this has to be a pointer for Julia
  #   because casting it to a RefValue would require type (tag) information at that memory location, which is not present (i.e. you cannot "cast" a Ptr{T} into a RefValue{T})
  #     there is unsafe_pointer_to_objref, but this requires (!) the type (tag) information to be present, so (in best case) this will segfault
  # hand-in-hand might also goes the GC-preserving of such @ptr statements
  #   so we might want to have something like
  #     ptr = @ptr base a.b.c.d.e
  #     @GC.preserve base f(ptr)
  #   the idea is, that @ptr stores the rightmost "Julia-GC-allocated" Julia-object into `base` that we can use then to preserve in `@GC.preserve`
  #     currently we need to manually guarantee that the pointers "lifetime" extends for the statement using it
  # for the next version we might change some of the internals to better handle the cases where fieldoffsets cannot be statically determined
  #   Nash 2016 - Inference Convergence Algorithm in Julia
  #     https://juliacomputing.com/blog/2016/04/04/inference-convergence.html
  #   Optionally-generated functions
  #     https://docs.julialang.org/en/v1/manual/metaprogramming/index.html#Optionally-generated-functions-1
  #   we also might want to "fuse" all such fieldknowledge-collecting functions using
  #     for (n,fA) in enumerate(fieldnames(T))
  #       push!(exprs, :( f == $(QuoteNode(fA)) && ($sym_idx = … something where f ≡ fA … ) ))
  #     end
  #   into a single one
  #   in order to do so, we could use a naming scheme for the generated variables of the different "levels", using `parentN` and `childN` for more clarity
  # the GMSH API shows how to pass allocated memory to Julia (does it?)
  #    function getElements(dim = -1, tag = -1)
  #      api_elementTypes_ = Ref{Ptr{Cint}}()
  #      api_elementTypes_n_ = Ref{Csize_t}()
  #      api_elementTags_ = Ref{Ptr{Ptr{Cint}}}()
  #      …
  #      ierr = Ref{Cint}()
  #      ccall((:gmshModelMeshGetElements, gmsh.lib), Nothing,
  #            (Ptr{Ptr{Cint}}, Ptr{Csize_t}, Ptr{Ptr{Ptr{Cint}}}, Ptr{Ptr{Csize_t}}, Ptr{Csize_t}, Ptr{Ptr{Ptr{Cint}}}, Ptr{Ptr{Csize_t}}, Ptr{Csize_t}, Cint, Cint, Ptr{Cint}),
  #            api_elementTypes_, api_elementTypes_n_, api_elementTags_, api_elementTags_n_, api_elementTags_nn_, api_nodeTags_, api_nodeTags_n_, api_nodeTags_nn_, dim, tag, ierr)
  #      …
  #      elementTypes = unsafe_wrap(Array, api_elementTypes_[], api_elementTypes_n_[], own=true)
  #      tmp_api_elementTags_ = unsafe_wrap(Array, api_elementTags_[], api_elementTags_nn_[], own=true)
  #      tmp_api_elementTags_n_ = unsafe_wrap(Array, api_elementTags_n_[], api_elementTags_nn_[], own=true)
  #      …
  #      return elementTypes, elementTags, nodeTags
  #    end
  # the CImGui.jl (C/C++) wrapper has an @c macro which seems to perform what we do with @ptr
  #   @c CImGui.ShowDemoWindow(&show_demo_window)
  #  i.e.
  #    @macroexpand @c f(&x)
  #      cref     = Ref(x)
  #      cref_ret = f(cref)
  #      x        = cref[]
  #      cref_ret
  #  it is also using
  #   @cstatic f=Cfloat(0.0) counter=Cint(0) begin … @c CImGui.SliderFloat("float", &f, 0, 1) … end
  #  i.e.
  #   @macroexpand @cstatic f=Cfloat(0.0) begin; foo; end
  #   let global static_f
  #       local f = static_f
  #      begin
  #        foo
  #      end
  #      static_f = f
  #      f
  #    end

  export @mem
  export @yolo
  export @ptr
  export @voidptr
  export @typedptr

  # TODO: may fuse fieldindex_generated, fieldisbitstype_generated and fieldisimmutable_generated into a single function
  # TODO: spread a few assertions with descriptive error messages

  # statically unroll the cases for all fieldnames of T
  @generated function (fieldindex_generated(v::T, f::Symbol)::UInt64) where T
    # inside the generated function macro, parameters and their type are the same, e.g. v == T
    if T <: Ptr; T = T.parameters[1]; end
    sym_idx = gensym()
    exprs = [:( $sym_idx = UInt64(0) )]
    for (n,fA) in enumerate(fieldnames(T))
      push!(exprs, :( f == $(QuoteNode(fA)) && ($sym_idx = UInt64($n)) ))
    end
    push!(exprs, :( return $sym_idx ))
    return Expr(:block,exprs...)
  end # generates a Core.Compiler.Const((:x, :b), false) and @code_warntype is okay with that

  isbitstypeandnotapointer(T::Type) = isbitstype(T) && ~(T <: Ptr)

  # statically unroll the cases for all fieldnames of T
  @generated function fieldisbitstype_generated(::T,f::Symbol) where T
    # inside the generated function macro, parameters and their type are the same, e.g. v == T
    sym_bit = gensym()
    exprs = [:( $sym_bit = false )]
    for (n,fA) in enumerate(fieldnames(T))
      push!(exprs, :( f == $(QuoteNode(fA)) && ($sym_bit = isbitstypeandnotapointer($(fieldtypes(T)[n]))) ))
    end
    push!(exprs, :( return $sym_bit ))
    return Expr(:block,exprs...)
  end

  # statically unroll the cases for all fieldnames of T
  @generated function fieldisimmutable_generated(::T,f::Symbol) where T
    # inside the generated function macro, parameters and their type are the same, e.g. v == T
    sym_imm = gensym()
    exprs = [:( $sym_imm = false )]
    for (n,fA) in enumerate(fieldnames(T))
      push!(exprs, :( f == $(QuoteNode(fA)) && ($sym_imm = $(!fieldtypes(T)[n].mutable)) ))
    end
    push!(exprs, :( return $sym_imm ))
    return Expr(:block,exprs...)
  end

  fieldisbitstype_static(::T, f::Symbol) where T = isbitstype(fieldtype(T,f))
  refisbitstype_static(::Ref{T}) where T = isbitstype(T)
  refisbitstype_static(::NTuple{N,T}) where {N,T} = isbitstype(T)
  refisbitstype_static(::SArray{E,T,N,M}) where {E,T,N,M} = isbitstype(T)
  ismutable_static(::T) where T = T.mutable
  isreference_static(::T) where T = T <: Ref # note, that we have Ptr <: Ref
  (fieldoffset_static(::T, i::UInt64)::Int64) where T = fieldoffset(T,i)
  (fieldoffset_static(::Ptr{T}, i::UInt64)::Int64) where T = fieldoffset(T,i)

  pointer_or_pointer_from_objref(x::T) where T = pointer_from_objref(x)
  pointer_or_pointer_from_objref(x::Ptr{T}) where T = x

  @inline @generated function unsafe_store_generated2(::Val{mode},::T,f::S,base::B,off::Int64,rhs::R,ptr::P,::Val{accumulatePointers},::Val{writeReferences},::Val{reallocateImmutableRHSpointer}) where {mode,T,S<:Union{Symbol,Nothing},B,R,P<:Union{Ptr,Nothing},accumulatePointers,writeReferences,reallocateImmutableRHSpointer}
    exprs = []
    sym_tmp = gensym("tmp")
    fnames = []
    ftypes = []
    if T <: SArray || T <: NTuple
      fnames = [:nothing]
      ftypes = [T.parameters[2]]
    elseif T <: Ptr
      fnames = fieldnames(T.parameters[1])
      ftypes = fieldtypes(T.parameters[1])
    else
      fnames = fieldnames(T)
      ftypes = fieldtypes(T)
    end
    if B <: Ptr && S <: Nothing # write directly to the pointer
      mode == :assignment && push!(exprs, :( @GC.preserve base unsafe_store!(base+off,rhs) ) )
      mode == :pointer    && push!(exprs, :(                                 base+off      ) )
    else # write to the designated place
      for (n,(fname,ftype)) in enumerate(zip(fnames,ftypes))
        action = :()
        if B.mutable || B <: Ptr # B is mutable, so `pointer_from_objref(base)` is valid
          if isbitstype(ftype) # write a bitstype into the memory of `base`
            mode == :assignment && ( action = :( @GC.preserve base unsafe_store!(reinterpret(Ptr{$(ftype)},pointer_or_pointer_from_objref(base)+off),rhs) ) )
            mode == :pointer    && ( action = :(                                 reinterpret(Ptr{$(ftype)},pointer_or_pointer_from_objref(base)+off)      ) )
          elseif writeReferences # write a reference into the memory of `base`
            # if the field we want to write to is not a bitstype, then we end up with it's parent as the `base`
            # in that case, we are at the second-to-last level, where the normal assignment can be used
            # so we do not see this case
            mode == :assignment && (
              action = :( @assert isa(rhs,$(ftype)) "The value to assign is of type '$($(R))', but the field to assign it to is of type '$($(ftype))'";
                @GC.preserve base unsafe_store!(reinterpret(Ptr{Ptr{Nothing}},pointer_or_pointer_from_objref(base)+off),pointer_from_objref(rhs)) ) )
            mode == :pointer    && ( action = :(reinterpret(Ptr{Ptr{Nothing}},pointer_or_pointer_from_objref(base)+off)                           ) )
          else
            # we do not see this case either
            action = :(error("""
              From type '$($T)' the field '$($(QuoteNode(fname)))' is of type '$($ftype)' and it is located in type '$($(B))' at offset $(off).
              The field type '$($ftype)' is a non-bitstype. You need to set 'writeReferences = true' to enable this operation.
              """))
          end
        elseif accumulatePointers # B is immutable, so we cannot use `pointer_from_objref(base)` and use an accumulated pointer instead
          if P != Nothing # if we have such pointer, we can use it
            if isbitstype(ftype) # write a bitstype into the memory at `ptr`
              mode == :assignment && ( action = :( @GC.preserve base unsafe_store!(reinterpret(Ptr{$(ftype)},ptr),rhs) ) )
              mode == :pointer    && ( action = :(                                 reinterpret(Ptr{$(ftype)},ptr)      ) )
            elseif writeReferences # write a reference into the memory at `ptr`
              if ftype.mutable # `rhs` is already allocated and `pointer_from_objref(rhs)` is valid
                mode == :assignment && (
                  action = :( @assert isa(rhs,$(ftype)) "The value to assign is of type '$($(R))', but the field to assign it to is of type '$($(ftype))'";
                    @GC.preserve base unsafe_store!(reinterpret(Ptr{Ptr{Nothing}},ptr),pointer_from_objref(rhs)) ) )
                mode == :pointer    && ( action = :(reinterpret(Ptr{Ptr{Nothing}},ptr) ) )
              elseif reallocateImmutableRHSpointer # we need to allocate `rhs` with `Ref`
                mode == :assignment && (
                  action = :( @assert isa(rhs,$(ftype)) "The value to assign is of type '$($(R))', but the field to assign it to is of type '$($(ftype))'";
                    $sym_tmp = Ref(rhs);
                    @GC.preserve base $sym_tmp unsafe_store!(reinterpret(Ptr{Ptr{Nothing}},ptr),unsafe_load(reinterpret(Ptr{Ptr{Nothing}},pointer_from_objref($sym_tmp)))) ) )
                mode == :pointer && ( action = :(            reinterpret(Ptr{Ptr{Nothing}},ptr)                                                                            ) )
              else
                action = :(error("""
                The given right hand side is of type '$($(R))' which is an immutable non-isbitstype.
                You need to set 'reallocateImmutableRHSpointer = true' to enable this operation.
                """))
              end
            else
              action = :(error("""
              From type '$($T)' the field '$($(QuoteNode(fname)))' is of type '$($(ftype))' and it is located in type '$($(B))' at offset $(off).
              This is a non-bitstype. You need to set 'writeReferences = true' to enable this operation.
              """))
            end
          else
            action = :(error("""There is neither a reference, nor a mutable present in the assigment to be used as a base pointer."""))
          end
        else
          action = :(error("""
            From type '$($T)' the field '$($(QuoteNode(fname)))' is of type '$($(ftype))' and it is located in type '$($(B))' at offset $(off).
            The base type '$($(B))' is immutable. You need to set 'accumulatePointers = true' to enable this operation.
            """))
        end
        if T <: SArray || T <: NTuple
          push!(exprs,:( $action )) # NOTE: this is set up to be just executed once
        else
          push!(exprs,:( f == $(QuoteNode(fname)) && return ($action) ))
        end
      end
    end
    # push!(exprs,:( error("the field '$($(QuoteNode(fname)))' was not found in type '$($(T))'") ))
    return Expr(:block,exprs...)
    # return :(println($exprs))
  end
  # fallback for assigning references
  @inline unsafe_store_generated2(::Val{:assignment}, ::Ref,::Nothing,base,basebase,off::Int64,rhs,_,_,_) = (base[] = rhs) # @assert isa(base,Ref), @assert off == 0
  @inline unsafe_store_generated2(::Val{:pointer}, ::Ref{T},::Nothing,base,basebase,off::Int64,rhs,_,_,_) where T = reinterpret(Ptr{T},pointer_from_objref(base)) # @assert isa(base,Ref), @assert off == 0

  # unroll the cases for all fieldnames of T
  @generated function unsafe_store_generated(::T,f::Symbol,base,off::Int64,rhs) where T
    exprs = []
    for (n,fA) in enumerate(fieldnames(T))
      push!(exprs,
        :( f == $(QuoteNode(fA))
        && ( @assert $(isbitstype(fieldtypes(T)[n])) """
              From type '$($T)', the field '$($(QuoteNode(fA)))' to be set is itself of type '$($(fieldtypes(T)[n]))', which is not bitstype.
              This means that the value of field '$($(QuoteNode(fA)))' is an immutable reference
              and it is opaque to us (is it?) how that is represented in memory (a pointer, for sure) and whether other objects rely on this to be constant.
              If it would be of type 'Ref{$($(fieldtypes(T)[n]))}' we'd  had a chance to replace it in the memory of it's parent '$($T)'.
              """
           ; @GC.preserve base unsafe_store!(reinterpret(Ptr{$(fieldtypes(T)[n])},pointer_from_objref(base)+off),rhs)
           )
        ))
    end
    return Expr(:block,exprs...)
  end
  # fallback for assigning references
  unsafe_store_generated(::T,::Nothing,base,off::Int64,rhs) where T = (base[] = rhs) # @assert isa(base,Ref), @assert off == 0

  setfield_or_deref(::Val{:assignment},x::T,f::Symbol,rhs) where T = setfield!(x,f,rhs)
  setfield_or_deref(::Val{:assignment},x::T,::Nothing,rhs) where T = (x[] = rhs)
  setfield_or_deref(::Val{:pointer},x::T,f::Symbol,rhs) where T = reinterpret(Ptr{fieldtype(T,f)},pointer_from_objref(x)+fieldoffset(T,fieldindex_generated(x,f))) # TODO: check whether this optimizes statically
  setfield_or_deref(::Val{:pointer},x::Ref{T},::Nothing,rhs) where T = reinterpret(Ptr{T},pointer_from_objref(x))
  fieldtype_static(::T, f::Symbol) where T = fieldtype(T,f)

  function cumprod_SimpleVector(v :: Core.SimpleVector)
    c = [1]
    for n = 1:length(v)-1
      push!(c,c[end]*v[n])
    end
    return c
  end

  indexoffset_static(t,prev::Int64) = Int64(0)
  # @generated function (indexoffset_static(t::Array{T,N}, indices...) :: UInt64) where {T,N}
  #   sym = gensym()
  #   return :( $sym = strides(t); $() )
  # end
  @generated function (indexoffset_static(t::SArray{E,T,N,M}, prev::Int64, indices...) :: Int64) where {E,T,N,M}
    sym = gensym()
    dims = E.parameters
    strides = MemoryMutate.cumprod_SimpleVector(dims)
    elsize = T.isbitstype ? sizeof(T) : sizeof(Ptr{Nothing}) # nonbitstypes are stored as "hidden references"/pointers
    index = Expr(:call,:+,[:( (indices[$i]-1)*($s) ) for (i,s) in enumerate(strides)]...)
    return T.isbitstype ? :( prev + $index * $elsize ) : :( $index * $elsize ) # if T is not a bitstype, then the array is not a bitstype and we will receive a pointer to the beginning of the array so the previous index has to be discarded
  end
  @generated function (indexoffset_static(t::NTuple{N,T}, prev::Int64, indices...) :: Int64) where {N,T}
    elsize = T.isbitstype ? sizeof(T) : sizeof(Ptr{Nothing}) # nonbitstypes are stored as "hidden references"/pointers
    index = :( indices[1]-1 )
    return T.isbitstype ? :( prev + $index * $elsize ) : :( $index * $elsize ) # if T is not a bitstype, then the array is not a bitstype and we will receive a pointer to the beginning of the array so the previous index has to be discarded
  end
  @generated function (indexoffset_static(t::Ptr{T}, prev::Int64, indices...) :: Int64) where {T}
    # @assert prev == 0
    elsize = T.isbitstype ? sizeof(T) : sizeof(Ptr{Nothing}) # nonbitstypes are stored as "hidden references"/pointers
    index = :( indices[1]-1 )
    return :( $index * $elsize )
  end

  @generated function fieldpointer_static(basebase,base::T,off::Int64,prev::Ptr)::Ptr{Nothing} where T
    return (
        T <: Ptr
      ? :(base+off)
      : T.mutable
      ? :( pointer_from_objref(base)+off )
      : T.isbitstype
      ? :( prev+off )
      : (T <: SArray || T <: MArray) # TODO: MArray? at least throw an error
      ? :( @GC.preserve basebase unsafe_load(unsafe_load(reinterpret(Ptr{Ptr{Ptr{Nothing}}},prev)))+off ) # sizeof(SArray{Tuple{N},SomeNonIsBitsType,1,N}) == 8 ⇒ it seems that SArray of non-isbits types allocates a single, intermediate array (we perform double dereferencing here)
      : :( @GC.preserve basebase unsafe_load(reinterpret(Ptr{Ptr{Nothing}},prev))+off )
      )
  end
  @generated function fieldpointer_static(basebase,base::T,off::Int64,::Nothing) where T
    return T <: Ptr ? :(base+off) : T.mutable ? :( pointer_from_objref(base)+off ) : :( nothing )
  end


  pointer_from_objref_typed(x :: T) where T = reinterpret(Ptr{T},pointer_from_objref(x))

  # see Tree.agda
  #   :(a.b.c.d->e.f.g.x = v) == :( ((a.b).c).d -> (((e.f).g).x = v) )
  #     true
  #   leftBalance(:( ((a.b).c).d -> (((e.f).g).x = v) ))
  #     :(((((((a.b).c).d->e)).f).g).x = v)
  function leftBalance(expr)
    # println()
    # println(expr)
    # dump(expr)
    if expr isa Expr
      if expr.head == :(->)
        # a -> b seems to (always?) produce an intermediate :block with a preceeding LineNumberNode that we are filtering out
        inner = expr.args[2] isa Expr && expr.args[2].head == :block && expr.args[2].args[1] isa LineNumberNode ? expr.args[2].args[2] : expr.args[2]
        if inner isa Expr
          if inner.head == :(->)
            # println("leftBalance (e ⇒ (f ⇒ g))     = leftBalance ((e ⇒ f) ⇒ g)")
            # a -> b seems to (always?) produce an intermediate :block with a preceeding LineNumberNode that we are filtering out
            innerinner = inner.args[2] isa Expr && inner.args[2].head == :block && inner.args[2].args[1] isa LineNumberNode ? inner.args[2].args[2] : inner.args[2]
            return leftBalance(Expr(:(->), Expr(:(->), expr.args[1], inner.args[1]), innerinner))
          else
            # println("leftBalance (e ⇒ E H [])      = (e ⇒ E H [])")
            #   unimplemented / does not occur
            # println("leftBalance (e ⇒ E H (a ∷ x)) = E H (leftBalance (leftBalance e ⇒ a) ∷ (map leftBalance x))")
            if inner.head == :call
              #   return Expr(inner.head, inner.args[1], leftBalance(Expr(:(->),expr.args[1],inner.args[2])), map(leftBalance,inner.args[3:end])... )
              # println("inner.head        = $(inner.head)")
              # println("inner.args[1]     = $(inner.args[1])")
              # println("expr.args[1]      = $(expr.args[1])")
              # println("inner.args[2]     = $(inner.args[2])")
              # println("inner.args[3:end] = $(inner.args[3:end])")
              # println()
              return Expr(inner.head, inner.args[1],            (Expr(:(->),leftBalance(expr.args[1]),inner.args[2])), map(leftBalance,inner.args[3:end])... )
            # elseif inner.head == :(.)
            #   return Expr(inner.head, inner.args[1], leftBalance(Expr(:(->),expr.args[1],inner.args[2])), map(leftBalance,inner.args[3:end])... )
            else
              # println("inner.head        = $(inner.head)")
              # println("expr.args[1]      = $(expr.args[1])")
              # println("inner.args[1]     = $(inner.args[1])")
              # println("inner.args[3:end] = $(inner.args[2:end])")
              # println()
              return Expr(inner.head,                leftBalance(Expr(:(->),leftBalance(expr.args[1]),inner.args[1])), map(leftBalance,inner.args[2:end])... )
            end
          end
        else
          # println("leftBalance (e ⇒ S)           = e ⇒ S")
          return expr
        end
      else
        # println("leftBalance (E H x)           = E H (map leftBalance x)")
        if expr.head == :call
          return Expr(expr.head, expr.args[1], map(leftBalance,expr.args[2:end])...)
        else
          return Expr(expr.head, map(leftBalance,expr.args)...)
        end
      end
    else
      # println("leftBalance S                 = S")
      return expr
    end
  end


  # # because pointer_from_objref is prohibited on immutables, we collect the pointers manually, beginning from the last occuring mutable
  # const accumulatePointers = true
  # # when in an immutable (non-isbits) type we want to set a field of non-isbits type, this is represented as a pointer and we update the pointer value then
  # const writeReferences = true
  # # when in an immutable (non-isbits) type we want to set a field of immutable non-isbits type, again this is a pointer, but also pointer_from_objref is now prohibited on this right-hand-side.
  # # We use unsafe_load(reinterpret(Ptr{Ptr{Nothing}},Ref(rhs))) to obtain a pointer in that case.
  # const reallocateImmutableRHSpointer = true

  derefPtrPtr(x::T) where T = x
  derefPtrPtr(x::Ptr{Ptr{T}}) where T = unsafe_load(x)
  PtrOrNothing(x::T) where T = nothing
  PtrOrNothing(x::Ptr{T}) where T = Ptr{Nothing}(x)

  @inline getfieldorpointer(x::T,f::Symbol,idx::UInt64) where {T} = getfield(x,f)
  @generated function getfieldorpointer(x::Ptr{T},f::Symbol,idx::UInt64) where {T}
    sym_res = gensym()
    exprs = [:( $sym_res = false )]
    for (n,fA) in enumerate(fieldnames(T))
      push!(exprs, :( f == $(QuoteNode(fA)) && ($sym_res = $derefPtrPtr(Ptr{$(fieldtypes(T)[n])}(x+$(fieldoffset(T,n))))  ) ))
    end
    push!(exprs, :( return $sym_res ))
    return Expr(:block,exprs...)
  end

  # structinfo(T) = [(fieldoffset(T,i), fieldname(T,i), fieldtype(T,i)) for i = 1:getfieldcount(T)];
  function mem_helper(expr, mode :: Symbol = :assignment, accumulatePointers :: Bool = false, writeReferences :: Bool = false, reallocateImmutableRHSpointer :: Bool = false, followPointers :: Bool = false)
    if followPointers
      expr = leftBalance(expr)
    end
    if mode == :assignment
      @assert expr.head == :(=) "Expression for mutating must be an assignment (=)." # unless we use a->b->c
      RHS = expr.args[2]
      cur = expr.args[1]
    end
    if mode == :pointer
      RHS = nothing
      cur = expr
    end

    # level[n] contains an expression for the current level of access, most likely just a symbol but could be an arbitrary expression as well, or `nothing` if the dereference `[]` occured
    levels = []
    cases = []
    while isa(cur,Expr)
      if cur.head == :(.)
        pushfirst!(levels,cur.args[2])
        pushfirst!(cases,:getfield)
        cur = cur.args[1]
      elseif cur.head == :call && cur.args[1] == :getfield # getproperty … is what is called by .
        pushfirst!(levels,cur.args[3])
        pushfirst!(cases,:getfield)
        cur = cur.args[2]
      elseif cur.head == :ref
        pushfirst!(cases,:getindex)
        pushfirst!(levels,cur.args[2:end])
        cur = cur.args[1]
      elseif followPointers && cur isa Expr && cur.head == :(->)
        pushfirst!(cases,:getfield)
        # e.g. in a.b->c.d we have, that a and c aren't QuoteNodes anymore, which is okay for a, but noch for c
        if cur.args[2] isa Symbol; pushfirst!(levels,QuoteNode(cur.args[2]))
        else                     ; pushfirst!(levels,          cur.args[2] )
        end
        cur = cur.args[1] # LineNumberNodes are already filtered out by leftBalance
      else
        break
      end
    end
    pushfirst!(levels,cur)
    pushfirst!(cases,:getfield)
    length(levels) <= 1 && return mode == :pointer ? esc(:($pointer_from_objref_typed($expr))) : esc(expr)
    # println(collect(zip(levels,cases)))

    # for each level, generate symbols to hold
    # TODO: name these `parent` and `child` for more clarity
    sym_val = [] # every level produces the parent-value for the next level
    sym_ptr = [] # the cumulated pointer from the first mutable/reference
    # sym_typ = [] # the type of `val`. It turned out, that it's necessary to use generic or generated functions to obtain compiler constants for the type's properties instead of using the type directly. That is why we introduce `fld`, `idx`, `bit`, `ref` and `mut`.
    sym_ref = [] # a Bool, whether the type of `val` is a reference type
    sym_mut = [] # a Bool, whether the type of `val` is mutable
    sym_fld = [] # this is a symbol (the "value" of the next level's "expression", `level[n+1]`), to be used to produce the `val` of the next level or `nothing` to signal to apply dereferencing `[]`
    sym_idx = [] # if there is a `fld` symbol, then this is it's numerical index fieldnames-table of the type of `val`. `idx` is zero if the symbol does not occur as a field
    sym_bit = [] # if there is a `fld` symbol, then this is a Bool, whether the type of `val`.`fld` is a bitstype
    sym_off = [] # if there is a `fld` symbol, then this is a UInt64 which accumulates the offsets of previous levels and "resets" every time we hit a non-bitstype
    for m = length(levels):-1:1
      pushfirst!(sym_val,gensym("val$(m)"))
      pushfirst!(sym_ptr,gensym("ptr$(m)"))
      # pushfirst!(sym_typ,gensym("typ$(m)"))
      pushfirst!(sym_fld,gensym("fld$(m)"))
      pushfirst!(sym_idx,gensym("idx$(m)"))
      pushfirst!(sym_bit,gensym("bit$(m)"))
      pushfirst!(sym_ref,gensym("ref$(m)"))
      pushfirst!(sym_mut,gensym("mut$(m)"))
      pushfirst!(sym_off,gensym("off$(m)"))
    end

    # setting up the expressions to hold compiler constants: for the first level
    expr_val = [:( $(sym_val[1]) = $(levels[1])                       )]
    expr_mut = [:( $(sym_mut[1]) = $ismutable_static($(sym_val[1]))   )]
    expr_ref = [:( $(sym_ref[1]) = $isreference_static($(sym_val[1])) )]
    expr_fld = []
    expr_idx = []
    expr_bit = []
    expr_off = []
    if cases[2] == :getindex # the next level is a dereference `[]`
      expr_fld = [:( $(sym_fld[1]) = nothing                              )]
      # expr_idx = [:( $(sym_idx[1]) = ()                                   )]
      expr_idx = [:( $(sym_idx[1]) = $(Expr(:tuple,levels[2]...))         )]
      expr_bit = [:( $(sym_bit[1]) = $refisbitstype_static($(sym_val[1])) )]
      # expr_off = [:( $(sym_off[1]) = UInt64(0)                            )]
      expr_off = [:( $(sym_off[1]) = $indexoffset_static($(sym_val[1]),0,$(sym_idx[1])...)      )]
    else # the next level provides a symbol
      expr_fld = [:( $(sym_fld[1]) = $(levels[2])                                             )]
      expr_idx = [:( $(sym_idx[1]) = $fieldindex_generated($(sym_val[1]),$(sym_fld[1]))       )]
      expr_bit = [:( $(sym_bit[1]) = $fieldisbitstype_generated($(sym_val[1]), $(sym_fld[1])) )]
      expr_off = [:( $(sym_off[1]) = $fieldoffset_static($(sym_val[1]), $(sym_idx[1]))        )] # TODO: sym_idx becomes 0 when it is not a field of the corresp. struct
    end
    expr_ptr = [:( $(sym_ptr[1]) = $(sym_mut[1]) ? pointer_from_objref($(sym_val[1])) + $(sym_off[1]) : $PtrOrNothing($(sym_val[1])) )]

    # setting up the expressions to hold compiler constants: for the following levels
    for n=2:length(levels)-1 # every level, we "get" a value: either by dereferencing or by accessing a field
      if cases[n] == :getindex # the current level's value is obtained via dereferencing `[]` the previous level's value
        push!(expr_val,:( $(sym_val[n]) = getindex($(sym_val[n-1]), $(sym_idx[n-1])...) )) # do the dereferencing `[]` to obtain a value
      else # the current level's value is obtained via a symbol from the previous level's value
        # push!(expr_val,:( $(sym_val[n]) = getfield($(sym_val[n-1]), $(sym_fld[n-1])) ))
        push!(expr_val,:( $(sym_val[n]) = $getfieldorpointer($(sym_val[n-1]), $(sym_fld[n-1]), $(sym_idx[n-1])) ))
      end
        push!(expr_mut,:( $(sym_mut[n]) = $ismutable_static($(sym_val[n]))     ))
        push!(expr_ref,:( $(sym_ref[n]) = $isreference_static($(sym_val[n-1])) ))
      if cases[n+1] == :getindex # the next level is a dereference `[]`
        push!(expr_fld,:( $(sym_fld[n]) = nothing                              ))
        # push!(expr_idx,:( $(sym_idx[n]) = ()                                   ))
        push!(expr_idx,:( $(sym_idx[n]) = $(Expr(:tuple,levels[n+1]...))       ))
        push!(expr_bit,:( $(sym_bit[n]) = $refisbitstype_static($(sym_val[n])) ))
        # push!(expr_off,:( $(sym_off[n]) = UInt64(0)                            ))
        push!(expr_off,:( $(sym_off[n]) = $indexoffset_static($(sym_val[n]),$(sym_off[n-1]),$(sym_idx[n])...) ))
      else # the next level provides a symbol
        push!(expr_fld,:( $(sym_fld[n]) = $(levels[n+1]) ))
        push!(expr_idx,:( $(sym_idx[n]) = $fieldindex_generated($(sym_val[n]),$(sym_fld[n])) ))
        push!(expr_bit,:( $(sym_bit[n]) = $fieldisbitstype_generated($(sym_val[n]), $(sym_fld[n])) ))
        push!(expr_off,:( $(sym_off[n]) = $fieldoffset_static($(sym_val[n]), $(sym_idx[n])) + ($(sym_bit[n-1]) ? $(sym_off[n-1]) : Int64(0)) )) # TODO: sym_idx becomes 0 when it is not a field of the corresp. struct
      end
      push!(expr_ptr,:( $(sym_ptr[n]) = $fieldpointer_static($(sym_val[n-1]),$(sym_val[n]),$(sym_off[n]),$(sym_ptr[n-1])) ))
    end

    # https://github.com/JuliaLang/julia/issues/10578
    #   "Comments can be passed through using the Expr(:meta, ...) functionality we have now."
    #   I am using LineNumberNode instead
    exprs = []
    for n=1:length(levels)-1
      level_str = replace(repr(levels[n]),"\n" => "")
      push!(exprs,LineNumberNode(-1,"level$(n) $(level_str)"))
      push!(exprs,expr_val[n])
      push!(exprs,expr_fld[n])
      push!(exprs,expr_idx[n])
      # push!(exprs,expr_chk[n])
      push!(exprs,expr_bit[n])
      push!(exprs,expr_mut[n])
      push!(exprs,expr_off[n])
      push!(exprs,expr_ptr[n])
    end

    # obtain the result
    push!(exprs,LineNumberNode(-1,"result"))
    sym_rhs = gensym("RHS")
    push!(exprs,:( $sym_rhs = $RHS ))

    #=
    generate nested `if` statements based on the previously obtained compiler constants to catch all the cases
      If we are at level[n], then
      - this level's value is obtained from the previous level's value via dereferencing or via a symbol,
      - whether this value is a bitstype was already detected in the previous level and it is bit[n-1]
      - and if this level's value is of a bitstype, then we assume it to be inlined into the parent's struct and therefore the parent's value or one it's ancestors have to be used as a base pointer.
       - … because pointer_from_objref will fail on bitstypes
      so it becomes
      - bit[n-1] ⇒ is this level's field a bitstype? Then use the previous expression from level[n-1] instead.
      - val[n] ⇒ Else, use this as base and perform store operation
    =#

    # the first level: use the first level's value as the base to assign at the second-to-last accumulated offset the `rhs` of the second-to-last field's type in the second-to-last value
    # expr_str = :(
    #     $(LineNumberNode(-1,"@val1+off$(length(levels)-1) <- val$(length(levels)-1)"))
    #   ; $unsafe_store_generated($(sym_val[length(levels)-1]),$(sym_fld[length(levels)-1]),$(sym_val[1]),$(sym_off[length(levels)-1]),$sym_rhs)
    #   )
    expr_str = :(
      $(LineNumberNode(-1,"@val1+off$(length(levels)-1) <- val$(length(levels)-1)"))
      ; $unsafe_store_generated2($(Val(mode)),$(sym_val[length(levels)-1]),$(sym_fld[length(levels)-1]),$(sym_val[1]),$(sym_off[length(levels)-1]),$sym_rhs,$(sym_ptr[1]),$(Val(accumulatePointers)),$(Val(writeReferences)),$(Val(reallocateImmutableRHSpointer)))
      )
    if length(levels) == 2 # if the first level is the second-to-last level AND it is mutable, we can use the normal assignment
      expr_rec = :(
            $(sym_mut[1]) # is the current level's value of mutable?
          ? $setfield_or_deref($(Val(mode)),$(sym_val[1]),$(sym_fld[1]),$sym_rhs)
          : $expr_str # use unsafe_store! on pointer_from_objref for the first level
          )
    else # directly use unsafe_store! on pointer_from_objref for the first level
      expr_rec = expr_str
    end

    # the remaining levels: use the current level's value as the base to assign at the second-to-last accumulated offset the `rhs` of the second-to-last field's type in the second-to-last value
    for n=2:length(levels)-1
      # expr_str = :(
      #     $(LineNumberNode(-1,"@val$(n)+off$(length(levels)-1) <- val$(length(levels)-1)"))
      #   ; $unsafe_store_generated($(sym_val[length(levels)-1]), $(sym_fld[length(levels)-1]), $(sym_val[n]), $(sym_off[length(levels)-1]),$sym_rhs)
      #   )
      expr_str = :(
          $(LineNumberNode(-1,"@val$(n)+off$(length(levels)-1) <- val$(length(levels)-1)"))
          ; $unsafe_store_generated2($(Val(mode)),$(sym_val[length(levels)-1]), $(sym_fld[length(levels)-1]), $(sym_val[n]), $(sym_off[length(levels)-1]),$sym_rhs,$(sym_ptr[n]),$(Val(accumulatePointers)),$(Val(writeReferences)),$(Val(reallocateImmutableRHSpointer)))
        )
      if n == length(levels)-1 # if the current level is the second-to-last level AND it is mutable, we can use the normal assignment
        expr_rec = :( $(sym_bit[n-1]) # is the current level's value of bitstype?
          ? $expr_rec # use one of the previous level's as a base pointer
          : ( $(sym_mut[n]) # is the current level's value mutable?
            ? $setfield_or_deref($(Val(mode)),$(sym_val[length(levels)-1]),$(sym_fld[length(levels)-1]),$sym_rhs)
            : $expr_str # use unsafe_store! on pointer_from_objref fot the current level
            )
          )
      else
        expr_rec = :( $(sym_bit[n-1]) # is the current level's value of bitstype?
          ? $expr_rec # use one of the previous level's as a base pointer
          : $expr_str # use unsafe_store! on pointer_from_objref fot the current level
          )
      end
    end
    push!(exprs,:( $expr_rec ))

    # unsafe_store! returns a pointer, but we want the assignment (=) to return the right hand side
    mode == :assignment && push!(exprs,:( $sym_rhs ))

    return esc(Expr(:block,exprs...))
  end

  macro mem(expr)
    return mem_helper(expr,:assignment,false,false,false,true)
  end
  macro yolo(expr)
    return mem_helper(expr,:assignment,true,true,true,true)
  end
  macro ptr(expr)
    return mem_helper(expr,:pointer,true,true,true,true)
  end
  macro voidptr(expr)
    return :(reinterpret(Ptr{Nothing},$(mem_helper(expr,:pointer,true,true,true,true))))
  end
  macro typedptr(type,expr)
    return :(reinterpret(Ptr{$type},$(mem_helper(expr,:pointer,true,true,true,true))))
  end
end
