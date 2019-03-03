
## MemoryMutate

WARNING: This is a proof of conept and it's underlying assumptions need additional validation.

A macro `@mutate` is provided for multi-_level_ assignments `a.b.c.d.e = v` that uses `fieldoffset` to calculate the memory offset of the to-be-assigned field relative to the rightmost mutable _level_ and uses `pointer_from_objref` on that rightmost mutable _level_, adds this offset to the obtained pointer and uses `unsafe_store!` to write the value to that memory location. It compiles down to a few `mov` assembly instructions, if the types and symbols are statically determinable.

This is a **different** approach then replacing the whole immutable of the right-most mutable with a new, modified one, as in

1. Julep: setfield! for mutable references to immutables
  https://github.com/JuliaLang/julia/issues/17115
  - _we propose making it possible to have setfield! modify fields inside of immutable objects that are wrapped in mutable objects_
  - _To support this proposal, the setfield! function will get a multi-arg form, with the following behaviors:_
    _setfield!(x, a, b, c, value) mutates the right most mutable object to change the value of its fields to be_
    _equivalent to copying the immutable objects and updating the referenced field._
  - _tl;dr The syntax:_
    _x.a.b.c = 3_
    _would now be valid, as long as at least one of the referenced fields is mutable._
2. WIP: Make mutating immutables easier
  https://github.com/JuliaLang/julia/pull/21912
  - proposes an @ operator as in `x@a = 2` for an immutable `x`
  - _The ways this works is that under the hood, it creates a new immutable object with the specified field modified and then assigns it back to the appropriate place._
    _Syntax wise, everything to the left of the @ is what's being assigned to, everything to the right of the @ is what is to be modified._

There are two important justifications to make on which this approach relies:
1. That immutables, allocated with `Ref` must not be assumed to stay constant when using `unsafe_store!`.
2. That the memory layout of bitstypes is exaclty what we achieve with `fieldoffset`.

Furthermore, although the assignment becomes just a few `mov` assembly instructions, it is not quite clear which optimizations might break by using `unsafe_store!`.

### Use case

In order to wrap C/C++ libraries in Julia, it is necessary to represent the C-structs as Julia types of the same layout.
That can be achieved for nested C-structs only with nested immutable Julia types (bitstypes).
More precisely, the most top level Julia type can be a mutable one, but all contained Julia types need to be immutable (bitstypes).

The [Julia documentation](https://docs.julialang.org/en/v1/manual/calling-c-and-fortran-code/#Passing-Pointers-for-Modifying-Inputs-1) writes

> Because C doesn't support multiple return values, often C functions will take pointers to data that the function will modify.

Consider [the following C/C++ function](https://github.com/ValveSoftware/openvr/blob/master/headers/openvr.h#L3625)

```C++
/** Computes the overlay-space pixel coordinates of where the ray intersects the overlay with the
* specified settings. Returns false if there is no intersection. */
bool ComputeOverlayIntersection(
	uint64_t ulOverlayHandle,
	const VROverlayIntersectionParams_t *pParams,
	VROverlayIntersectionResults_t *pResults
);
```

Here `pResults` should hold a pointer to a `VROverlayIntersectionResults_t` C-struct that is defined in the following way

```C++
enum ETrackingUniverseOrigin
{
	…
};

struct HmdVector3_t
{
	float v[3];
};

struct HmdVector2_t
{
	float v[2];
};

struct VROverlayIntersectionParams_t
{
	HmdVector3_t vSource;
	HmdVector3_t vDirection;
	ETrackingUniverseOrigin eOrigin;
};

struct VROverlayIntersectionResults_t
{
	HmdVector3_t vPoint;
	HmdVector3_t vNormal;
	HmdVector2_t vUVs;
	float fDistance;
};
```

In order to provide C-layout compatible Julia structs, we might use

```julia
const ETrackingUniverseOrigin = UInt32 # sizeof(ETrackingUniverseOrigin) == 4

struct HmdVector3_t # sizeof(HmdVector3_t) == 12
  v :: SArray{Tuple{3},Float32,1,3}
end

struct HmdVector2_t # sizeof(HmdVector2_t) == 8
  v :: SArray{Tuple{2},Float32,1,2}
end

struct VROverlayIntersectionParams_t # sizeof(VROverlayIntersectionParams_t) == 28
  vSource :: HmdVector3_t
  vDirection :: HmdVector3_t
  eOrigin :: ETrackingUniverseOrigin
end

struct VROverlayIntersectionResults_t # sizeof(VROverlayIntersectionResults_t) == 36
  vPoint :: HmdVector3_t
  vNormal :: HmdVector3_t
  vUVs :: HmdVector2_t
  fDistance :: Float32
end
```

A `Ref{VROverlayIntersectionResults_t}` will be passed to Julia's `ccall` and the C/C++ function will alter the underlying immutable - it will mutate the immutable in an arbitrary C-style way.

Therefore we assume that immutables `x`, allocated on the heap via `Ref(x)` are not assumed to be constant anymore by Julia and one can modify them in a C-style way, i.e. modify any parts of the immutables memory.

### What this module provides

Consider the Julia immutable types `Pconst` and `Cconst`

```julia
struct Pconst # sizeof(Pconst) == 8
  x :: Float32
  y :: Float32
end

struct Cconst # sizeof(Cconst) == 12
  r :: Float32
  g :: Float32
  b :: Float32
end
```

and the mutable type `Vmutbl`

```julia
mutable struct Vmutbl # sizeof(Vmutbl) == 20
  p :: Pconst
  c :: Cconst
end
```

for

```julia
p = Pconst(1f0,2f0)     # Pconst(1.0f0, 2.0f0)
c = Cconst(0f0,1f0,0f0) # Cconst(0.0f0, 1.0f0, 0.0f0)
v = Vmutbl(p,c)         # Vmutbl(Pconst(1.0f0, 2.0f0), Cconst(0.0f0, 1.0f0, 0.0f0))
```

we can alter the top-level fields of `v`

```julia
v.p = Pconst(3f0,2f0)
v # Vmutbl(Pconst(3.0f0, 2.0f0), Cconst(0.0f0, 1.0f0, 0.0f0))
```

but not the more nested ones, because they are immutable

```julia
v.p.x = 5f0
# ERROR: setfield! immutable struct of type Pconst cannot be change
```

since we assume that these immutables do not need to stay _constant_, we can replace them at their memory location

```julia
@mutate v.p.x = 5f0
v # Vmutbl(Pconst(5.0f0, 3.0f0), Cconst(0.0f0, 1.0f0, 0.0f0))
```

More examples might be extracted from [test/mutate.jl](./test/mutate.jl).

### Implementation status

Already implemented features:
- written out `getfield` is considered, as in `getfield(a.b.c,:d).e =v`
- written out dereferencing with `[]` is considered, as in `a.b.c[].d = v`

To be done:
- currently we use `getfield` for `.` instead of `getproperty` (`getproperty` is what is called by `.`)
- `+=` operations and similiar
- continuous array indexing `a[10] = v`, e.g. for static arrays
