# For Developers

Developers may fork [the GTPSA.jl repo](https://github.com/bmad-sim/GTPSA.jl) and then `dev` their forked repo in the REPL. For example, if my Github username is `githubuser`, then after forking GTPSA.jl I would run in the REPL:

```julia
import Pkg
Pkg.develop(url="https://github.com/githubuser/GTPSA.jl")
```

The package consists of two layers: a low-level layer written in Julia that is 1-to-1 with the GTPSA C code, and a high-level, user-friendly layer that cleans up the notation for manipulating TPSs, manages temporaries generated during evaluation, and properly manages the memory in C when variables go out of scope in Julia. The low-level functions are listed at the bottom of this page.

When it comes to managing memory in C via Julia, there are certain intricacies that have to be considered. First, let's consider the `Descriptor`, which is the simplest: 

## Descriptor

The `Descriptor` stores all information about the GTPSA, including the indexing table for indexing specific monomials. This is a static object, only created once for a GTPSA, which all `TPS`s refer to. In C, these structs must exist for the entirety of the program execution. In Julia, because they must not be destroyed when out-of-scope, we wrap these objects in **immutable** structs. In a single program, up to 250 `Descriptor`s can exist simultanouesly, and they can be manually optionally destroyed using `GTPSA.mad_desc_del!`. At program termination all `Descriptor`s are destroyed. Given the significantly large number allowed, as well as the danger of still-existing `TPS` and `Descriptor` structs after destruction, no front-facing interface to the user is given for destroying existing `Descriptor`s.

The `Descriptor` struct simply wraps a C-pointer to a low-level struct called `Desc`: this struct is 1-to-1 equivalent to the C struct `desc` in GTPSA. See the documentation for [`GTPSA.Desc`](@ref) below. By having this struct in Julia, we can `unsafe_load` the struct and get values in the `desc`. For example, to access the first `TPS{Float64}` in the buffer of temporaries in the `Descriptor`, we can do


```@repl
using GTPSA
import GTPSA: Desc

d = Descriptor(5,8)

desc = unsafe_load(d.desc) # To access the low-level C struct
t_jl = unsafe_load(Base.unsafe_convert(Ptr{Ptr{TPS{Float64}}}, desc.t), 1) # 1 in Julia = 0 in C
```

In Julia, if we were to then `unsafe_load(t_jl)`, there would in fact be allocations, as its internally creating a copy of the `TPS{Float64}` in Julia. This is not well documented in the documentation of `unsafe_load` (see the discussion [here](https://discourse.julialang.org/t/c-struct-garbage-collection-not-run-frequently-enough/116919/27?u=mattsignorelli)).

## TPS

The `TPS{Float64}` struct in Julia corresponds exactly to the C struct `tpsa` and `TPS{ComplexF64}` struct in Julia corresponds exactly to `ctpsa` in C. To understand fully the `TPS` struct, some history is needed:

In early development versions of `GTPSA.jl`, the `TPS` struct was very similar to the `Descriptor` struct: it used to just wrap a C `Ptr` to a low-level struct, and instead be `mutable` so out-of-scope `TPS`s and temporaries are cleaned up. This at the time seemed like the simplest solution, since in Julia there is no way to tag C pointers for Julia garbage collection. This created some problems though: firstly, there is one indirection because Julia would tag that particular mutable wrapper struct for GC, but then the member `Ptr` of that struct pointed to a different place in memory that had to be cleaned up. Secondly, when calling GTPSA C functions that want a `Vector` of `TPS` (e.g. `Ptr` to `TPS`), you would need to do a `map(t->t.tpsa, x)` where `x` is a `Vector{TPS64}` and `tpsa` is the field member of the wrapper TPS struct. Thirdly, and perhaps most significantly, Julia is not aware of how much memory the C is using. Therefore, it will not call the garbage collector very often, even if actually >100GB of memory is being used. Sometimes the OS will just kill the Julia process.

In order to get around these problems, we must allocate the entire mutable `TPS` struct in Julia instead of in the C code (e.g. using `GTPSA.mad_tpsa_newd`). We then can use `pointer_from_objref` in Julia to get a pointer to that mutable, Julia-owned struct to pass to the C functions. 

Sounds simple enough, right? If only! In the GTPSA C code, the `coef` member array is something called a [flexible array member](https://en.wikipedia.org/wiki/Flexible_array_member). This is great in C, because instead of the struct storing a pointer to an array (which would cause an indirection every time the `coef` array is accessed in a `TPS`), it actually stores the array right there in the struct, with variable size. This gives some performance gains. In Julia, there is no such analog. For those who know about `StaticArrays.jl`, you might think an `SVector` could work, but surprise, it doesn't because you cannot mutate the fields of an `SVector` and neither can the C code.

So the only solution it seems is to change the actual C struct in `mad_tpsa_impl.h` and `mad_ctpsa_impl.h` to use regular arrays for `coef` instead of flexible array members, and indeed this is what is done for `GTPSA.jl`. There is a tiny speed reduction due to the indirection of accessing `coef`, however the benefits of Julia's garbage collector knowing how much memory it's using, and keeping memory usage sane, is worth the very tiny cost.

On the Julia side, it turns out you cannot just use a regular `Vector` for the `coef` array in the `TPS` struct, because Julia's `Vector` structs are quite complex and play a lot of tricks. You might actually be able to use an `MVector` from `StaticArrays`, however using this struct might make `GTPSA.jl` significantly more complex because the size of the array has to be known at compile-time or else you suffer the drastic performance reductions caused by type-instabilities. The complexity of using this could be checked at some point in the future.

The decided solution was to, in the Julia, `@ccall jl_malloc` for the `coef` array, and in the finalizer for the mutable `TPS` struct call `@ccall jl_free` for the `coef` array. This gives us C-style arrays that Julia's garbage collector knows about, and so will make sure to keep the memory usage sane.

When `@ccall` is used, the arguments are `Base.unsafe_convert`ed to the corresponding specified argument types. Therefore, for `TPS` all we had to do then was define
```julia
Base.unsafe_convert(::Type{Ptr{TPS{T}}}, t::TPS{T}) where {T} = Base.unsafe_convert(Ptr{TPS{T}},pointer_from_objref(t))
```
 and now we can pass our `TPS` structs to C using `@ccall`.


## TempTPS

Because `unsafe_load` of a `Ptr{<:TPS}` creates a copy and allocates, we cannot treat the constant buffer of pre-allocated temporaries in the `Descriptor` as bona-fide `TPS`s. Note that the memory addresses of the temporaries in the buffer are constant and do not need to be cleaned up; they are **immutable!**. The temporaries, which we give the type `GTPSA.TempTPS`, do not have any of the problems of just wrapping a pointer as do the `TPS`s, and so that's what they are. Also in Julia, in order to access the fields of a `TempTPS` (e.g. `mo`) via `unsafe_load` without allocating, we need an immutable struct having the same structure as `TPS`. This is the purpose of `GTPSA.LowTempTPS`. We need to use the `mo` field for `@FastGTPSA` to finally allocate the result `TPS` with the `mo` of the result `TempTPS`.

As with `TPS`, we also had to define `Base.unsafe_convert` for `TempTPS` so we can `@ccall`. In this case, the `unsafe_convert` returns the member `Ptr` of the `TempTPS`.

```@docs
GTPSA.TempTPS
```

## Library Structure

All operators have an in-place, mutating version specified with a bang (!). These are the lowest-level pure Julia code, following the convention that the first argument is the one to contain the result. In the GTPSA C library, the last argument contains the result, so this is accounted for in the file `inplace_operators.jl`. All in-place functions can receive either a regular `TPS` , which the user will be using, as well as a `GTPSA.TempTPS`, which the user should not concern themselves with. The constants `RealTPS` and `ComplexTPS` are defined respectively in `low_level/rtpsa.jl` and `low_level/ctpsa.jl` to simplify the notation. These are just:

```julia
# Internal constants to aid multiple dispatch including temporaries 
const RealTPS = Union{TempTPS{Float64}, TPS{Float64}}
const ComplexTPS = Union{TempTPS{ComplexF64}, TPS{ComplexF64}}
```

All this does is enforce correct types for the in-place functions, while keeping the notation/code simple. 

The in-place, mutating functions, defined in `inplace_operators.jl` **must all use the `RealTPS` and `ComplexTPS` "types"**. Then, the higher level out-of-place functions for both `TPS` and `TempTPS`, which do different things with the result, will use these in-place functions.

The out-of-place functions for `TPS` are defined in `operators.jl`, and the out-of-place functions for `TempTPS` are defined in `fastgtpsa/operators.jl`.

## Fast GTPSA Macros

The `@FastGTPSA`/`@FastGTPSA!` macros work by changes all arithmetic operators in different Julia arithmetic operators with the same operator precedence and unary operator capabilities. These special operators then dispatch on functions that use the temporaries when a `TPS` or `TempTPS` is passed, else default to their original operators, thereby making it completely transparent to non-TPS types. Both `+` and `-` must work as unary operators, and there is a very short list of allowed ones shown [here](https://github.com/JuliaLang/julia/blob/master/src/julia-parser.scm#L99). The other arithmetic operators were chosen somewhat randomly from the top of the same file, next to `prec-plus`, `prec-times`, and `prec-power` which defines the operator precedences. By taking this approach, we relieve ourselves of having to rewrite PEMDAS and instead let the Julia do it for us.

All arithmetic operators are changed to `GTPSA.:<special symbols>`, e.g. `+` → `GTPSA.:±`. All non-arithmetic operators that are supported by GTPSA are then changed to `GTPSA.__t_<operator>`, e.g. `sin` → `GTPSA.__t_sin`, where the prefix `__t_` is also chosen somewhat arbitrarily. These operators are all defined in `fastgtpsa/operators.jl`, and when they encounter a TPS type, they use the temporaries, and when other number types are detected, they fallback to the regular, non-`__t_` operator. This approach works extraordinarily well, and introduces no problems externally because none of these functions/symbols are exported.

## Low-Level

Below is documentation for every single 1-to-1 C function in the GTPSA library. If there is any function missing, please submit an issue to `GTPSA.jl`.

### Monomial
```@docs
GTPSA.mad_mono_add!
GTPSA.mad_mono_cat!
GTPSA.mad_mono_cmp
GTPSA.mad_mono_copy!
GTPSA.mad_mono_eq
GTPSA.mad_mono_eqn
GTPSA.mad_mono_fill!
GTPSA.mad_mono_le
GTPSA.mad_mono_lt
GTPSA.mad_mono_max
GTPSA.mad_mono_min
GTPSA.mad_mono_ord
GTPSA.mad_mono_ordp
GTPSA.mad_mono_ordpf
GTPSA.mad_mono_print
GTPSA.mad_mono_prt!
GTPSA.mad_mono_rcmp
GTPSA.mad_mono_rev!
GTPSA.mad_mono_str!
GTPSA.mad_mono_sub!
```

### Desc
```@docs
GTPSA.Desc
GTPSA.mad_desc_del!
GTPSA.mad_desc_getnv!
GTPSA.mad_desc_idxm
GTPSA.mad_desc_idxs
GTPSA.mad_desc_idxsm
GTPSA.mad_desc_info
GTPSA.mad_desc_isvalidm
GTPSA.mad_desc_isvalids
GTPSA.mad_desc_isvalidsm
GTPSA.mad_desc_maxlen
GTPSA.mad_desc_maxord
GTPSA.mad_desc_mono!
GTPSA.mad_desc_newv
GTPSA.mad_desc_newvp
GTPSA.mad_desc_newvpo
GTPSA.mad_desc_nxtbyord
GTPSA.mad_desc_nxtbyvar
GTPSA.mad_desc_paropsth!
```
### TPS{Float64}
```@docs
GTPSA.mad_tpsa_abs!
GTPSA.mad_tpsa_acc!
GTPSA.mad_tpsa_acos!
GTPSA.mad_tpsa_acosh!
GTPSA.mad_tpsa_acot!
GTPSA.mad_tpsa_acoth!
GTPSA.mad_tpsa_add!
GTPSA.mad_tpsa_asin!
GTPSA.mad_tpsa_asinc!
GTPSA.mad_tpsa_asinh!
GTPSA.mad_tpsa_asinhc!
GTPSA.mad_tpsa_atan!
GTPSA.mad_tpsa_atan2!
GTPSA.mad_tpsa_atanh!
GTPSA.mad_tpsa_ax2pby2pcz2!
GTPSA.mad_tpsa_axpb!
GTPSA.mad_tpsa_axpbypc!
GTPSA.mad_tpsa_axpsqrtbpcx2!
GTPSA.mad_tpsa_axypb!
GTPSA.mad_tpsa_axypbvwpc!
GTPSA.mad_tpsa_axypbzpc!
GTPSA.mad_tpsa_clear!
GTPSA.mad_tpsa_clrord!
GTPSA.mad_tpsa_compose!
GTPSA.mad_tpsa_convert!
GTPSA.mad_tpsa_copy!
GTPSA.mad_tpsa_cos!
GTPSA.mad_tpsa_cosh!
GTPSA.mad_tpsa_cot!
GTPSA.mad_tpsa_coth!
GTPSA.mad_tpsa_cpyi!
GTPSA.mad_tpsa_cpym!
GTPSA.mad_tpsa_cpys!
GTPSA.mad_tpsa_cpysm!
GTPSA.mad_tpsa_cutord!
GTPSA.mad_tpsa_cycle!
GTPSA.mad_tpsa_debug
GTPSA.mad_tpsa_del!
GTPSA.mad_tpsa_density
GTPSA.mad_tpsa_deriv!
GTPSA.mad_tpsa_derivm!
GTPSA.mad_tpsa_desc
GTPSA.mad_tpsa_dif!
GTPSA.mad_tpsa_div!
GTPSA.mad_tpsa_equ
GTPSA.mad_tpsa_erf!
GTPSA.mad_tpsa_erfc!
GTPSA.mad_tpsa_eval!
GTPSA.mad_tpsa_exp!
GTPSA.mad_tpsa_exppb!
GTPSA.mad_tpsa_fgrad!
GTPSA.mad_tpsa_fld2vec!
GTPSA.mad_tpsa_geti
GTPSA.mad_tpsa_getm
GTPSA.mad_tpsa_getord!
GTPSA.mad_tpsa_gets
GTPSA.mad_tpsa_getsm
GTPSA.mad_tpsa_getv!
GTPSA.mad_tpsa_hypot!
GTPSA.mad_tpsa_hypot3!
GTPSA.mad_tpsa_idxm
GTPSA.mad_tpsa_idxs
GTPSA.mad_tpsa_idxsm
GTPSA.mad_tpsa_init!
GTPSA.mad_tpsa_integ!
GTPSA.mad_tpsa_inv!
GTPSA.mad_tpsa_invsqrt!
GTPSA.mad_tpsa_isnul
GTPSA.mad_tpsa_isval
GTPSA.mad_tpsa_isvalid
GTPSA.mad_tpsa_len
GTPSA.mad_tpsa_liebra!
GTPSA.mad_tpsa_log!
GTPSA.mad_tpsa_logaxpsqrtbpcx2!
GTPSA.mad_tpsa_logpb!
GTPSA.mad_tpsa_logxdy!
GTPSA.mad_tpsa_maxord!
GTPSA.mad_tpsa_mconv!
GTPSA.mad_tpsa_minv!
GTPSA.mad_tpsa_mnrm
GTPSA.mad_tpsa_mo!
GTPSA.mad_tpsa_mono!
GTPSA.mad_tpsa_mord
GTPSA.mad_tpsa_mul!
GTPSA.mad_tpsa_nam
GTPSA.mad_tpsa_new
GTPSA.mad_tpsa_newd
GTPSA.mad_tpsa_nrm
GTPSA.mad_tpsa_ord
GTPSA.mad_tpsa_ordv
GTPSA.mad_tpsa_pminv!
GTPSA.mad_tpsa_poisbra!
GTPSA.mad_tpsa_pow!
GTPSA.mad_tpsa_powi!
GTPSA.mad_tpsa_pown!
GTPSA.mad_tpsa_print
GTPSA.mad_tpsa_scan
GTPSA.mad_tpsa_scan_coef!
GTPSA.mad_tpsa_scan_hdr
GTPSA.mad_tpsa_scl!
GTPSA.mad_tpsa_sclord!
GTPSA.mad_tpsa_seti!
GTPSA.mad_tpsa_setm!
GTPSA.mad_tpsa_setprm!
GTPSA.mad_tpsa_sets!
GTPSA.mad_tpsa_setsm!
GTPSA.mad_tpsa_setv!
GTPSA.mad_tpsa_setval!
GTPSA.mad_tpsa_setvar!
GTPSA.mad_tpsa_sin!
GTPSA.mad_tpsa_sinc!
GTPSA.mad_tpsa_sincos!
GTPSA.mad_tpsa_sincosh!
GTPSA.mad_tpsa_sinh!
GTPSA.mad_tpsa_sinhc!
GTPSA.mad_tpsa_sqrt!
GTPSA.mad_tpsa_sub!
GTPSA.mad_tpsa_tan!
GTPSA.mad_tpsa_tanh!
GTPSA.mad_tpsa_taylor!
GTPSA.mad_tpsa_taylor_h!
GTPSA.mad_tpsa_translate!
GTPSA.mad_tpsa_uid!
GTPSA.mad_tpsa_unit!
GTPSA.mad_tpsa_update!
GTPSA.mad_tpsa_vec2fld!
```
### TPS{ComplexF64} 
```@docs
GTPSA.mad_ctpsa_acc!
GTPSA.mad_ctpsa_acc_r!
GTPSA.mad_ctpsa_acos!
GTPSA.mad_ctpsa_acosh!
GTPSA.mad_ctpsa_acot!
GTPSA.mad_ctpsa_acoth!
GTPSA.mad_ctpsa_add!
GTPSA.mad_ctpsa_addt!
GTPSA.mad_ctpsa_asin!
GTPSA.mad_ctpsa_asinc!
GTPSA.mad_ctpsa_asinh!
GTPSA.mad_ctpsa_asinhc!
GTPSA.mad_ctpsa_atan!
GTPSA.mad_ctpsa_atanh!
GTPSA.mad_ctpsa_ax2pby2pcz2!
GTPSA.mad_ctpsa_ax2pby2pcz2_r!
GTPSA.mad_ctpsa_axpb!
GTPSA.mad_ctpsa_axpb_r!
GTPSA.mad_ctpsa_axpbypc!
GTPSA.mad_ctpsa_axpbypc_r!
GTPSA.mad_ctpsa_axpsqrtbpcx2!
GTPSA.mad_ctpsa_axpsqrtbpcx2_r!
GTPSA.mad_ctpsa_axypb!
GTPSA.mad_ctpsa_axypb_r!
GTPSA.mad_ctpsa_axypbvwpc!
GTPSA.mad_ctpsa_axypbvwpc_r!
GTPSA.mad_ctpsa_axypbzpc!
GTPSA.mad_ctpsa_axypbzpc_r!
GTPSA.mad_ctpsa_cabs!
GTPSA.mad_ctpsa_carg!
GTPSA.mad_ctpsa_clear!
GTPSA.mad_ctpsa_clrord!
GTPSA.mad_ctpsa_compose!
GTPSA.mad_ctpsa_conj!
GTPSA.mad_ctpsa_convert!
GTPSA.mad_ctpsa_copy!
GTPSA.mad_ctpsa_cos!
GTPSA.mad_ctpsa_cosh!
GTPSA.mad_ctpsa_cot!
GTPSA.mad_ctpsa_coth!
GTPSA.mad_ctpsa_cplx!
GTPSA.mad_ctpsa_cpyi!
GTPSA.mad_ctpsa_cpym!
GTPSA.mad_ctpsa_cpys!
GTPSA.mad_ctpsa_cpysm!
GTPSA.mad_ctpsa_cutord!
GTPSA.mad_ctpsa_cycle!
GTPSA.mad_ctpsa_debug
GTPSA.mad_ctpsa_del!
GTPSA.mad_ctpsa_density
GTPSA.mad_ctpsa_deriv!
GTPSA.mad_ctpsa_derivm!
GTPSA.mad_ctpsa_desc
GTPSA.mad_ctpsa_dif!
GTPSA.mad_ctpsa_dift!
GTPSA.mad_ctpsa_div!
GTPSA.mad_ctpsa_divt!
GTPSA.mad_ctpsa_equ
GTPSA.mad_ctpsa_equt
GTPSA.mad_ctpsa_erf!
GTPSA.mad_ctpsa_erfc!
GTPSA.mad_ctpsa_eval!
GTPSA.mad_ctpsa_exp!
GTPSA.mad_ctpsa_exppb!
GTPSA.mad_ctpsa_fgrad!
GTPSA.mad_ctpsa_fld2vec!
GTPSA.mad_ctpsa_geti
GTPSA.mad_ctpsa_geti_r!
GTPSA.mad_ctpsa_getm
GTPSA.mad_ctpsa_getm_r!
GTPSA.mad_ctpsa_getord!
GTPSA.mad_ctpsa_gets
GTPSA.mad_ctpsa_gets_r!
GTPSA.mad_ctpsa_getsm
GTPSA.mad_ctpsa_getsm_r!
GTPSA.mad_ctpsa_getv!
GTPSA.mad_ctpsa_hypot!
GTPSA.mad_ctpsa_hypot3!
GTPSA.mad_ctpsa_idxm
GTPSA.mad_ctpsa_idxs
GTPSA.mad_ctpsa_idxsm
GTPSA.mad_ctpsa_imag!
GTPSA.mad_ctpsa_init!
GTPSA.mad_ctpsa_integ!
GTPSA.mad_ctpsa_inv!
GTPSA.mad_ctpsa_inv_r!
GTPSA.mad_ctpsa_invsqrt!
GTPSA.mad_ctpsa_invsqrt_r!
GTPSA.mad_ctpsa_isnul
GTPSA.mad_ctpsa_isval
GTPSA.mad_ctpsa_isvalid
GTPSA.mad_ctpsa_len
GTPSA.mad_ctpsa_liebra!
GTPSA.mad_ctpsa_log!
GTPSA.mad_ctpsa_logaxpsqrtbpcx2!
GTPSA.mad_ctpsa_logaxpsqrtbpcx2_r!
GTPSA.mad_ctpsa_logpb!
GTPSA.mad_ctpsa_logxdy!
GTPSA.mad_ctpsa_maxord
GTPSA.mad_ctpsa_mconv!
GTPSA.mad_ctpsa_minv!
GTPSA.mad_ctpsa_mnrm
GTPSA.mad_ctpsa_mo!
GTPSA.mad_ctpsa_mono!
GTPSA.mad_ctpsa_mord
GTPSA.mad_ctpsa_mul!
GTPSA.mad_ctpsa_mult!
GTPSA.mad_ctpsa_nam
GTPSA.mad_ctpsa_new
GTPSA.mad_ctpsa_newd
GTPSA.mad_ctpsa_nrm
GTPSA.mad_ctpsa_ord
GTPSA.mad_ctpsa_ordv
GTPSA.mad_ctpsa_pminv!
GTPSA.mad_ctpsa_poisbra!
GTPSA.mad_ctpsa_poisbrat!
GTPSA.mad_ctpsa_polar!
GTPSA.mad_ctpsa_pow!
GTPSA.mad_ctpsa_powi!
GTPSA.mad_ctpsa_pown!
GTPSA.mad_ctpsa_pown_r!
GTPSA.mad_ctpsa_powt!
GTPSA.mad_ctpsa_print
GTPSA.mad_ctpsa_real!
GTPSA.mad_ctpsa_rect!
GTPSA.mad_ctpsa_scan
GTPSA.mad_ctpsa_scan_coef!
GTPSA.mad_ctpsa_scan_hdr
GTPSA.mad_ctpsa_scl!
GTPSA.mad_ctpsa_scl_r!
GTPSA.mad_ctpsa_sclord!
GTPSA.mad_ctpsa_seti!
GTPSA.mad_ctpsa_seti_r!
GTPSA.mad_ctpsa_setm!
GTPSA.mad_ctpsa_setm_r!
GTPSA.mad_ctpsa_setprm!
GTPSA.mad_ctpsa_setprm_r!
GTPSA.mad_ctpsa_sets!
GTPSA.mad_ctpsa_sets_r!
GTPSA.mad_ctpsa_setsm!
GTPSA.mad_ctpsa_setsm_r!
GTPSA.mad_ctpsa_setv!
GTPSA.mad_ctpsa_setval!
GTPSA.mad_ctpsa_setval_r!
GTPSA.mad_ctpsa_setvar!
GTPSA.mad_ctpsa_setvar_r!
GTPSA.mad_ctpsa_sin!
GTPSA.mad_ctpsa_sinc!
GTPSA.mad_ctpsa_sincos!
GTPSA.mad_ctpsa_sincosh!
GTPSA.mad_ctpsa_sinh!
GTPSA.mad_ctpsa_sinhc!
GTPSA.mad_ctpsa_sqrt!
GTPSA.mad_ctpsa_sub!
GTPSA.mad_ctpsa_subt!
GTPSA.mad_ctpsa_tan!
GTPSA.mad_ctpsa_tanh!
GTPSA.mad_ctpsa_taylor!
GTPSA.mad_ctpsa_taylor_h!
GTPSA.mad_ctpsa_tdif!
GTPSA.mad_ctpsa_tdiv!
GTPSA.mad_ctpsa_tpoisbra!
GTPSA.mad_ctpsa_tpow!
GTPSA.mad_ctpsa_translate!
GTPSA.mad_ctpsa_tsub!
GTPSA.mad_ctpsa_uid!
GTPSA.mad_ctpsa_unit!
GTPSA.mad_ctpsa_update!
GTPSA.mad_ctpsa_vec2fld!
```
