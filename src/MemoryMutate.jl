module MemoryMutate
  export @mem

  # TODO: may fuse fieldindex_generated, fieldisbitstype_generated and fieldisimmutable_generated into a single function
  # TODO: spread a few assertions with descriptive error messages

  # statically unroll the cases for all fieldnames of T
  @generated function (fieldindex_generated(v::T, f::Symbol)::UInt64) where T
    # inside the generated function macro, parameters and their type are the same, e.g. v == T
    sym_idx = gensym()
    exprs = [:( $sym_idx = UInt64(0) )]
    for (n,fA) in enumerate(fieldnames(T))
      push!(exprs, :( f == $(QuoteNode(fA)) && ($sym_idx = UInt64($n)) ))
    end
    push!(exprs, :( return $sym_idx ))
    return Expr(:block,exprs...)
  end # generates a Core.Compiler.Const((:x, :b), false) and @code_warntype is okay with that

  # statically unroll the cases for all fieldnames of T
  @generated function fieldisbitstype_generated(::T,f::Symbol) where T
    # inside the generated function macro, parameters and their type are the same, e.g. v == T
    sym_bit = gensym()
    exprs = [:( $sym_bit = false )]
    for (n,fA) in enumerate(fieldnames(T))
      push!(exprs, :( f == $(QuoteNode(fA)) && ($sym_bit = isbitstype($(fieldtypes(T)[n]))) ))
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
  ismutable_static(::T) where T = T.mutable
  isreference_static(::T) where T = T <: Ref
  fieldoffset_static(::T, i::UInt64) where T = fieldoffset(T,i)


  # TODO: one would have to recover the pointers, beginning with the first
  const accumulatePointers = true
  const writeReferences = true
  const reallocateImmutableRHSpointer = true
  @generated function unsafe_store_generated2(::T,f::Symbol,base::B,off::UInt64,rhs::R,ptr::P) where {T,B,R,P<:Union{Ptr{Nothing},Nothing}}
    exprs = []
    sym_tmp = gensym("tmp")
    for (n,fA) in enumerate(fieldnames(T))
      action = :()
      if B.mutable # B is mutable, so `pointer_from_objref(base)` is valid
        if isbitstype(fieldtypes(T)[n]) # write a bitstype into the memory of `base`
          action = :( @GC.preserve base unsafe_store!(reinterpret(Ptr{$(fieldtypes(T)[n])},pointer_from_objref(base)+off),rhs) )
        elseif (@__MODULE__).writeReferences # write a reference into the memory of `base`
          # if the field we want to write to is not a bitstype, then we end up with it's parent as the `base`
          # in that case, we are at the second-to-last level, where the normal assignment can be used
          # so we do not see this case
          action = :( @assert isa(rhs,$(fieldtypes(T)[n])) "The value to assign is of type '$($(R))', but the field to assign it to is of type '$($(fieldtypes(T)[n]))'";
            @GC.preserve base unsafe_store!(reinterpret(Ptr{Ptr{Nothing}},pointer_from_objref(base)+off),pointer_from_objref(rhs)) )
        else
          # we do not see this case either
          action = :(throw("""
            From type '$($T)' the field '$($(QuoteNode(fA)))' is of type '$(fieldtypes(T)[n])' and it is located in type '$($(B))' at offset $(off).
            The field type '$(fieldtypes(T)[n])' is a non-bitstype. You need to set 'writeReferences = true' to enable this operation.
            """))
        end
      elseif accumulatePointers # B is immutable, so we cannot use `pointer_from_objref(base)` and use an accumulated pointer instead
        if P != Nothing # if we have such pointer, we can use it
          if isbitstype(fieldtypes(T)[n]) # write a bitstype into the memory at `ptr`
            action = :( @GC.preserve base unsafe_store!(reinterpret(Ptr{$(fieldtypes(T)[n])},ptr),rhs) )
          elseif writeReferences # write a reference into the memory at `ptr`
            if fieldtypes(T)[n].mutable # `rhs` is already allocated and `pointer_from_objref(rhs)` is valid
              action = :( @assert isa(rhs,$(fieldtypes(T)[n])) "The value to assign is of type '$($(R))', but the field to assign it to is of type '$($(fieldtypes(T)[n]))'";
                @GC.preserve base unsafe_store!(reinterpret(Ptr{Ptr{Nothing}},ptr),pointer_from_objref(rhs)) )
            elseif reallocateImmutableRHSpointer # we need to allocate `rhs` with `Ref`
              action = :( @assert isa(rhs,$(fieldtypes(T)[n])) "The value to assign is of type '$($(R))', but the field to assign it to is of type '$($(fieldtypes(T)[n]))'";
                $sym_tmp = Ref(rhs);
                @GC.preserve base $sym_tmp unsafe_store!(reinterpret(Ptr{Ptr{Nothing}},ptr),unsafe_load(reinterpret(Ptr{Ptr{Nothing}},pointer_from_objref($sym_tmp)))) )
            else
              action = :(throw("""
              The given right hand side is of type '$($(R))' which is an immutable non-isbitstype.
              You need to set 'reallocateImmutableRHSpointer = true' to enable this operation.
              """))
            end
          else
            action = :(throw("""
            From type '$($T)' the field '$($(QuoteNode(fA)))' is of type '$($(fieldtypes(T)[n]))' and it is located in type '$($(B))' at offset $(off).
            This is a non-bitstype. You need to set 'writeReferences = true' to enable this operation.
            """))
          end
        else
          action = :(throw("""There is neither a reference, nor a mutable present in the assigment to be used as a base pointer."""))
        end
      else
        action = :(throw("""
          From type '$($T)' the field '$($(QuoteNode(fA)))' is of type '$($(fieldtypes(T)[n]))' and it is located in type '$($(B))' at offset $(off).
          The base type '$($(B))' is immutable. You need to set 'accumulatePointers = true' to enable this operation.
          """))
      end
      push!(exprs,:( f == $(QuoteNode(fA)) && ($action) ))
    end
    return Expr(:block,exprs...)
  end
  # fallback for assigning references
  unsafe_store_generated2(::T,::Nothing,base,basebase,off::UInt64,rhs) where T = (base[] = rhs) # @assert isa(base,Ref), @assert off == 0

  # unroll the cases for all fieldnames of T
  @generated function unsafe_store_generated(::T,f::Symbol,base,off::UInt64,rhs) where T
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
  unsafe_store_generated(::T,::Nothing,base,off::UInt64,rhs) where T = (base[] = rhs) # @assert isa(base,Ref), @assert off == 0

  setfield_or_deref(x::T,f::Symbol,rhs) where T = setfield!(x,f,rhs)
  setfield_or_deref(x::T,::Nothing,rhs) where T = (x[] = rhs)
  fieldtype_static(::T, f::Symbol) where T = fieldtype(T,f)

  @generated function fieldpointer_static(basebase,base::T,off::UInt64,prev::Ptr{Nothing})::Ptr{Nothing} where T
    return T.mutable ? :( pointer_from_objref(base)+off ) : :( @GC.preserve basebase unsafe_load(reinterpret(Ptr{Ptr{Nothing}},prev))+off )
  end
  @generated function fieldpointer_static(basebase,base::T,off::UInt64,::Nothing) where T
    return T.mutable ? :( pointer_from_objref(base)+off ) : :( nothing )
  end

  # structinfo(T) = [(fieldoffset(T,i), fieldname(T,i), fieldtype(T,i)) for i = 1:fieldcount(T)];
  macro mem(expr)
    @assert expr.head == :(=) "Expression for mutating must be an assignment (=)."
    RHS = expr.args[2]

    # level[n] contains an expression for the current level of access, most likely just a symbol but could be an arbitrary expression as well, or `nothing` if the dereference `[]` occured
    levels = []
    cur = expr.args[1]
    while isa(cur,Expr)
      if cur.head == :(.)
        pushfirst!(levels,cur.args[2])
        cur = cur.args[1]
      elseif cur.head == :call && cur.args[1] == :getfield # getproperty … is what is called by .
        pushfirst!(levels,cur.args[3])
        cur = cur.args[2]
      elseif cur.head == :ref
        pushfirst!(levels,nothing)
        cur = cur.args[1]
      else
        break
      end
    end
    pushfirst!(levels,cur)
    length(levels) <= 1 && return esc(expr)

    # for each level, generate symbols to hold
    sym_val = [] # every level produces the parent-value for the next level
    sym_ptr = [] # the cumulated pointer from the first mutable/reference
    # sym_typ = [] # the type of `val`. It turned out, that it's necessary to use generic or generated functions to obtain compiler constants for the type's properties instead of using the type directly. That is why we introduce `fld`, `idx`, `bit`, `ref` and `mut`.
    sym_ref = [] # a Bool, whether the type of `val` is mutable
    sym_mut = [] # a Bool, whether the type of `val` is a reference type
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
    if levels[2] == nothing # the next level is a dereference `[]`
      expr_fld = [:( $(sym_fld[1]) = nothing                              )]
      expr_idx = [:( ;                                                    )]
      expr_bit = [:( $(sym_bit[1]) = $refisbitstype_static($(sym_val[1])) )]
      expr_off = [:( $(sym_off[1]) = UInt64(0)                            )]
    else # the next level provides a symbol
      expr_fld = [:( $(sym_fld[1]) = $(levels[2])                                             )]
      expr_idx = [:( $(sym_idx[1]) = $fieldindex_generated($(sym_val[1]),$(sym_fld[1]))       )]
      expr_bit = [:( $(sym_bit[1]) = $fieldisbitstype_generated($(sym_val[1]), $(sym_fld[1])) )]
      expr_off = [:( $(sym_off[1]) = $fieldoffset_static($(sym_val[1]), $(sym_idx[1]))        )]
    end
    expr_ptr = [:( $(sym_ptr[1]) = $(sym_mut[1]) ? pointer_from_objref($(sym_val[1])) + $(sym_off[1]) : nothing )]

    # setting up the expressions to hold compiler constants: for the following levels
    for n=2:length(levels)-1 # every level, we "get" a value: either by dereferencing or by accessing a field
      if levels[n] == nothing # the current level's value is obtained via dereferencing `[]` the previous level's value
        push!(expr_val,:( $(sym_val[n]) = $(sym_val[n-1])[] )) # do the dereferencing `[]` to obtain a value
      else # the current level's value is obtained via a symbol from the previous level's value
        push!(expr_val,:( $(sym_val[n]) = getfield($(sym_val[n-1]), $(sym_fld[n-1])) )) # TODO: do so with `getfield`
      end
        push!(expr_mut,:( $(sym_mut[n]) = $ismutable_static($(sym_val[n]))     ))
        push!(expr_ref,:( $(sym_ref[n]) = $isreference_static($(sym_val[n-1])) ))
      if levels[n+1] == nothing # the next level is a dereference `[]`
        push!(expr_fld,:( $(sym_fld[n]) = nothing                              ))
        push!(expr_idx,:( ;                                                    ))
        push!(expr_bit,:( $(sym_bit[n]) = $refisbitstype_static($(sym_val[n])) ))
        push!(expr_off,:( $(sym_off[n]) = UInt64(0)                            ))
      else # the next level provides a symbol
        push!(expr_fld,:( $(sym_fld[n]) = $(levels[n+1]) ))
        push!(expr_idx,:( $(sym_idx[n]) = $fieldindex_generated($(sym_val[n]),$(sym_fld[n])) ))
        push!(expr_bit,:( $(sym_bit[n]) = $fieldisbitstype_generated($(sym_val[n]), $(sym_fld[n])) ))
        push!(expr_off,:( $(sym_off[n]) = $fieldoffset_static($(sym_val[n]), $(sym_idx[n])) + ($(sym_bit[n-1]) ? $(sym_off[n-1]) : UInt64(0)) ))
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
      ; $unsafe_store_generated2($(sym_val[length(levels)-1]),$(sym_fld[length(levels)-1]),$(sym_val[1]),$(sym_off[length(levels)-1]),$sym_rhs,$(sym_ptr[1]))
      )
    if length(levels) == 2 # if the first level is the second-to-last level AND it is mutable, we can use the normal assignment
      expr_rec = :(
            $(sym_mut[1]) # is the current level's value of mutable?
          ? $setfield_or_deref($(sym_val[1]),$(sym_fld[1]),$sym_rhs)
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
          ; $unsafe_store_generated2($(sym_val[length(levels)-1]), $(sym_fld[length(levels)-1]), $(sym_val[n]), $(sym_off[length(levels)-1]),$sym_rhs,$(sym_ptr[n]))
        )
      if n == length(levels)-1 # if the current level is the second-to-last level AND it is mutable, we can use the normal assignment
        expr_rec = :( $(sym_bit[n-1]) # is the current level's value of bitstype?
          ? $expr_rec # use one of the previous level's as a base pointer
          : ( $(sym_mut[n]) # is the current level's value mutable?
            ? $setfield_or_deref($(sym_val[length(levels)-1]),$(sym_fld[length(levels)-1]),$sym_rhs)
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
    push!(exprs,:( $sym_rhs ))

    return esc(Expr(:block,exprs...))
  end
end
