# Datafun compiler — single source of truth for new instances

## Project goal

Implement a compiler for **Datafun** with rigorous testing. Datafun is a functional language
with lattice/set semantics. The syntax and typing rules are in `llm_info/syntax.png` and
`llm_info/typing_rules.png` — read those images before touching the type-checker.

---

## Workflow (mandatory — do not skip steps)

1. Write the `.mli` signature (types + `val` declarations with one-line doc comments)
2. Write the tests (compile against the signature before the implementation exists)
3. Only then fill in the implementation

Every important function needs a one-line doc comment in its `.mli` and associated tests.

---

## Error handling conventions

**Exceptions = programmer bugs only** (treat like `assert false`). If compiler logic is
correct they must never fire.

**Program-level errors** (parse errors, type errors) use `result` or `option` — never exceptions.

Concrete rules:
- `Type_checker.synth` returns `(typ, error) result`
- `Frontend.parse_string` / `parse_channel` return `(expr, string) result`
- Grammar constraints (finiteness of `set T`, lattice of `fix`) are enforced by dedicated
  `fin_typ` / `lat_typ` grammar rules — no exceptions in parser actions
- Post-typecheck `synth` calls inside `codegen.ml` use `assert false` (cannot fail on
  a well-typed expression)

---

## Testing philosophy

A compiler has distinct phases; test each phase in isolation, not via full pipeline runs.

- **Parser**: call `Frontend.parse_string s` → compare `Pretty.show_expr result`
- **Type-checker**: construct AST nodes + hand-craft minimal ctx → call `Type_checker.synth`
  → compare `Pretty.show_typ result`; error cases print `"Error: ..."` via `show_error`
- **Helpers** (`typ_to_fin`, `typ_to_lattice`, `restrict`, `Alpha.rename`): pure functions,
  test directly with known inputs
- Use `ppx_expect` golden/snapshot style throughout; `dune runtest --auto-promote` to
  accept new outputs
- Do not modify test expectations to hide bugs — fix the code

---

## Codebase map

| File | Purpose |
|------|---------|
| `lib/ast.ml` | Core types: `expr`, `typ`, `fin_typ`, `lattice`, `binop` |
| `lib/lexer.mll` | ocamllex lexer |
| `lib/parser.mly` | Menhir parser; separate `fin_typ`/`lat_typ` grammar rules enforce type-annotation constraints statically |
| `lib/type_checker.ml/mli` | `synth : ctx -> expr -> (typ, error) result`; structured `error` type; `restrict` keeps only `Disc` vars |
| `lib/alpha.ml/mli` | Alpha-renaming; opaque `counter` type; `create_counter : unit -> counter`; `rename : counter -> subst -> expr -> expr` |
| `lib/frontend.ml` | `parse_string / parse_channel : ... -> (expr, string) result` |
| `lib/pretty.ml` | `show_typ`, `show_fin`, `show_lattice`, `show_expr` (S-expression form) — used in tests |
| `lib/runtime.ml` | `Value.t` + interpreter primitives: `bot`, `join`, `sing`, `fix`, `arith`, `for_set` |
| `lib/codegen.ml` | Transpiles Datafun AST → OCaml source string (naive evaluation); `compile_program` returns `(string, error) result` |
| `bin/main.ml` | CLI: parse → alpha-rename → codegen → ocamlopt |
| `test/test_helpers.ml` | Tests for `typ_to_fin`, `typ_to_lattice`, `restrict`, `fin_to_typ`, `lattice_to_typ`, `Alpha.rename` |
| `test/test_parser.ml` | Parser tests via single `parse : string -> string` helper |
| `test/test_typechecker.ml` | Typechecker tests via single `tc : ctx -> expr -> string` helper |

## Build system

- `dune-project`: lang dune 3.14, menhir 3.0
- `lib/dune`: library `datafun_lib`; modules include `pretty`, `alpha`, `type_checker`, etc.
- `test/dune`: `ppx_expect` inline-test library depending on `datafun_lib`
- opam: global `default` switch, OCaml 4.14.1; Jane Street v0.16 line (`ppx_expect.v0.16.2`)
- opam uses wget: `opam option --global download-command=wget`

---

## Task 1 — Parser + type-checker [DONE]

Implementation and tests complete. All `dune runtest` pass.

---

## Task 2 — Typeless IR + interpreter

### Design

The "IR" is the existing `expr` AST after alpha-renaming — no new IR type needed.
Evaluation strategy: **transpile to OCaml** (reuse the existing codegen pipeline), but with a
cleaner value model and runtime.

**Key design decision: remove `VFunc` from `Value.t`.**
Values represent ground/finite data only:
```
VUnit | VInt of int | VPair of value * value | VInl of value | VInr of value | VSet of VSet.t
```
OCaml functions are OCaml functions — they never appear as `value` objects. This is sound
because Datafun's type system ensures functions never appear inside sets or as fixpoint results.

Consequences for each expression form:
| Form | Old codegen | New codegen |
|------|------------|------------|
| `λx:A.e` | `VFunc (fun x -> e)` | `fun x -> e` |
| `e1 e2` | `apply (VFunc f) v` | `e1 e2` |
| `[e]` (box) | `compile (restrict ctx) e` | same — box is type-level only |
| `let [x] = e1 in e2` | `apply (VFunc (fun x -> e2)) e1` | `let x = e1 in e2` |
| `case(e, ι₁(x)→e1, ι₂(y)→e2)` | `case e (VFunc ...) (VFunc ...)` | OCaml `match e with VInl x -> e1 \| VInr y -> e2 \| _ -> assert false` |
| `fix x:L.e` | `fix l (VFunc (fun x -> e))` | `Runtime.fix l (fun x -> e)` |
| `for(e2).x∈e1` | `for_set l e1 (VFunc (fun x -> e2))` | `Runtime.for_set l e1 (fun x -> e2)` |

Alpha-rename ensures all binder names are unique (`x_0`, `x_1`, …) so no name collision risk
in generated OCaml — no extra naming pass needed.

### Changes to `runtime.ml`

- Remove `VFunc of (Value.t -> Value.t)` from `Value.t`
- Remove `apply` (no longer needed)
- `fix : lattice -> (value -> value) -> value` — takes plain OCaml function (not `VFunc`)
- `for_set : lattice -> value -> (value -> value) -> value` — same
- `case`, `projl`, `projr` — can be removed (codegen emits OCaml `match` directly)
- `value_to_string` — remove the `VFunc` case

### Workflow (TDD as always)

1. Write `runtime.mli` with the new `Value.t` and updated signatures
2. Write tests for `Runtime`: `bot`, `join`, `sing`, `fix`, `for_set`, `arith`, `value_to_string`
3. Update `runtime.ml` implementation
4. Update `codegen.ml` to emit the new forms
5. Write codegen integration tests (parse a Datafun expression → compile → check emitted OCaml string or run and check output)
6. `dune runtest`

---

## Task 3 — Seminaive evaluation

Idea is that fix points are evaluated using f and f'. See llm_info/course-notes.pdf from pages 40 to 61.
To facilitate this, we modify the code generation. We define 3 mutually recursive functions:
1. Compile: Takes in an expression and generates the compiled OCaml code
2. Zero change: Takes in an expression and generates a valid zero change for it. The zero change of an expression e
is another expression de of the "change" type such that updating the value that e evaluates to with the value that de
evaluates to will keep e unchanged.
For example, zero_change of a set is the empty set, zero_change of unit is unit (the update is join). Zero change of an
expression of type A + B is del A + del B. For example, zero change of inl({1, 2, 3}) is inl({}). As we do not know the specific
variant at compile time, a match case ocaml expression will need to be constructed. 
This also tells us that codegen needs the type information of expressions. How to pass over this information (so that codegen does not
need to recompute it) is a design decision that needs to be made.
The hardest part is the zero change of a function. As you know from course-notes.pdf, a function (f + df)(x) is f(x)+df(x, zero_change(x))
and (f+df)(x+dx) = f(x)+df(x, dx). So the zero change is the derivative of f. (a derivative of a function f: A -> B is df: A -> (del A) -> del B)
that tracks changes to the output wrt changes to the input (see course-notes.pdf).
This leads us to the third mutually recursive function:
3. Deriv: Takes in an expression e and creates an expression de representing the changes of e. de contains dx's (where x's are variables of the
context) that represent changes to the context variables, and constructs a valid change of e. 
For example:
e1 v e2 -> (deriv e1) v (deriv e2)
lam x: A. e -> fun x -> fun dx -> deriv e (note that when deriv is called, all the dx's are to be in scope already)
case(e, inl(x) -> e1, inr(y) -> e2) -> 
match (e, deriv e) with
| (inl(x), inl(dx)) -> deriv e1
| (inr(y), inr(dy)) -> deriv e2
| _ -> assert false
and so on

Now to link them all together, compilation proceeds normally except when we encounter the following:
let [x] = e1 in e2 should become:
let (x, dx) = (compile e1, zero_change e1) in e2 (because these x's can appear within a fix, which will need the dx to compute the change)
and fix x: L. e is compiled down to 
let (f, df) = (compile (lam x: L. e), deriv (lam x: L. e))

(also zero_change can recursively call deriv)

Please look at the semantics carefully in course-notes.pdf to figure out what needs to be done.
You might also look at seminaive-datafun.pdf if you are confused, although I think what I am doing is substantially different from what is presented
there (it is doing a source-to-source transformation, while I am directly compiling it to OCaml. Also the syntax differs (there is no split). As I 
am not doing a source-to-source transformation, I do not need to worry about stuff like the translated program not type-checking, that seminaive-datafun has to worry about.)

---

## Task 3 — Refined case analysis

There are exactly two mutually recursive codegen functions, corresponding to φ and δ from
seminaive-datafun.pdf (Fig. 7 and 8):
- `compile` (= φ): translates an expression to its OCaml value
- `deriv`   (= δ): translates an expression to its OCaml change (derivative w.r.t. Ord context vars)

There is no separate `zero_change` function. The zero change of a box expression is produced
inline by `compile(Box e_inner)` = a VPair of (value, deriv of inner). LetBox then unpacks it.

**Naming convention:** the change variable for `x` is `"d_" ^ x`. After alpha-renaming, `x` is
unique (e.g. `x_0`), so `d_x_0` is a valid, unique OCaml identifier.

**Type information:** both functions take the same `ctx : ctx` argument as the existing `compile`.
`synth_exn ctx e` is called on demand; no separate type-annotated IR is needed.

**The `change` type (specification only):** d_x for x:A has the following conceptual type:
- TUnit, TInt, TBox _ → TUnit (trivial change)
- TPow ft → TPow ft (lattice: change = same type)
- TProd(A,B) → TProd(change A, change B)
- TSum(A,B)  → TSum(change A, change B)
- TFun(A,B)  → TFun(TBox A, TFun(change A, change B)) — the derivative function

---

### `compile` — cases that differ from naive evaluation

All forms are identical to the naive compile except **Box**, **LetBox**, **Fix**, and **For** (For gains a `d_x` binding so nested Box sub-expressions can reference the element's zero change).

**Box e_inner** — produces a VPair of (value, zero-change of value):
```
(VPair (compile(restrict ctx)(e_inner), deriv(restrict ctx)(e_inner)))
```
The second component is `deriv` in the restricted context. Since all vars in `restrict ctx` are
Disc, their changes are already bound by their own LetBox/for-loop bindings, so `deriv` in that
context gives the correct zero change for each type:
- Lam → the derivative function (the zero change of a function IS its derivative)
- lattice expressions → bot (via the Ord Var or Bot cases)
- discrete/finite expressions → VUnit or structured VUnit for sums/products

**LetBox {x; e1; e2}** — unpacks the VPair from compile(e1):
```
(match (compile(e1)) with VPair (x, d_x) -> compile(e2) | _ -> assert false)
```
Both `x` (the value) and `d_x` (the zero change, type change(A)) are bound as OCaml variables
in scope for `compile(e2)`. No call to any zero_change function; the pair was built by Box.

**Fix {x; l; e}** — calls `seminaive_fix` instead of `fix`:
```
(seminaive_fix L_COMPILED (fun x -> compile(e)) (fun x d_x -> deriv(e)))
```
x:L so change(L)=L — both `x` and `d_x` are lattice values. Both `compile(e)` and `deriv(e)`
use context `extend x Ord (lattice_to_typ l) (restrict ctx)`.

**For {e1; x; e2}** — must also bind d_x (zero change of the element) for nested Box uses:
```
(for_set L (compile(e2)) (fun x -> let d_x = ZERO_CHANGE_FIN(ft, x) in compile(e1)))
```
`x` has finite type `ft` (from `synth_exn ctx e2 = TPow ft`). `ZERO_CHANGE_FIN(ft, x)` is a
helper (see below) that computes the zero change of `x` from its finite type — no functions
can appear in finite types so this is always straightforward (VUnit for unit/int, structured
VPair/VInl/VInr for compound finite types, bot for FPow).

---

### `zero_change_fin` helper

Since For-loop elements always have finite types (no functions), we need a small helper:
```
zero_change_fin : fin_typ -> string -> string
```
Takes a fin_typ and an OCaml value-string, returns the zero-change expression:

| fin_typ | zero_change_fin result |
|---------|------------------------|
| FUnit, FInt | `VUnit` |
| FBox _ | `VUnit` |
| FPow ft | `(bot (LPow FT))` |
| FProd(a,b) | `(match V with VPair(l_,r_) -> VPair(ZC_a l_, ZC_b r_) \| _ -> assert false)` |
| FSum(a,b) | `(match V with VInl l_ -> VInl(ZC_a l_) \| VInr r_ -> VInr(ZC_b r_) \| _ -> assert false)` |

---

### `deriv` — one case per expression form

`deriv ctx e` produces the OCaml change expression. For every Ord var `x` in ctx, `d_x` is in
scope. For Disc vars, `d_x` is in scope from the enclosing LetBox match or for-loop let-binding.

**Var x (Ord or Disc):**
```
"d_" ^ x
```
For Ord vars: d_x is the change passed by Fix or Lam's derivative.
For Disc vars: d_x was bound by the enclosing LetBox (from the VPair second component)
or for loop (from zero_change_fin). Either way, it has the right change structure.

**Lit n, Unit, BinOp:**
```
VUnit
```
Constants and integer arithmetic are discrete; change(TInt) = change(TUnit) = TUnit.

**Pair {e1; e2}:**
```
(VPair (deriv(e1), deriv(e2)))
```

**ProjL e:**
```
(match (deriv(e)) with VPair (dl_, _) -> dl_ | _ -> assert false)
```

**ProjR e:**
```
(match (deriv(e)) with VPair (_, dr_) -> dr_ | _ -> assert false)
```

**Inl {e; _}:**
```
(VInl (deriv(e)))
```

**Inr {e; _}:**
```
(VInr (deriv(e)))
```

**Case {e; x; e1; y; e2}:**
```
(match (compile(e), deriv(e)) with
 | (VInl x, VInl d_x) -> deriv(e1)
 | (VInr y, VInr d_y) -> deriv(e2)
 | _ -> assert false)
```
Match both value and change simultaneously. change(A+B) = change(A)+change(B), so the change
is VInl(da) or VInr(db). x/d_x (or y/d_y) are bound in the respective branches.

**Lam {x; a; e}:**
```
(fun x -> fun d_x -> deriv(e))
```
change(A→B) = [A]×change(A)→change(B). Context for deriv(e): extend ctx with `x : Ord a`.

**App {e1; e2}:**
```
(((deriv(e1)) (compile(e2))) (deriv(e2)))
```
Chain rule: df(x)(dx). `deriv(e1)` is the derivative of the function, applied to the current
argument `compile(e2)` and its change `deriv(e2)`.

**Bot l:**
```
(bot LATTICE_L)
```

**Join {e1; e2}:**
```
(join (deriv(e1)) (deriv(e2)))
```
Join is a lattice homomorphism so it is its own derivative.

**Sing e:**
```
(bot (LPow FT))
```
`e` is finite/discrete; singleton set has no change. FT from `synth_exn ctx e`.

**For {e1; x; e2}:**
```
(join
  (for_set L (deriv(e2))
             (fun x -> let d_x = ZERO_CHANGE_FIN(ft, x) in compile(e1)))
  (for_set L (join (compile(e2)) (deriv(e2)))
             (fun x -> let d_x = ZERO_CHANGE_FIN(ft, x) in deriv(e1))))
```
First arm: new elements each contribute their base value compile(e1).
Second arm: all current elements contribute their change deriv(e1).
d_x must be bound in both arms because compile(e1) may contain nested Box expressions that
call deriv internally and need d_x in scope.

**Box e_inner:**
```
VUnit
```
deriv of Box is trivial (δ[e] = () from Fig. 8). The zero change was already embedded into
compile(Box) as the VPair second component.

**LetBox {x; e1; e2}:**
```
(match (compile(e1)) with VPair (x, d_x) -> deriv(e2) | _ -> assert false)
```
Same unpacking as compile's LetBox — both use compile(e1) (not deriv(e1)) to get the VPair.
This matches δ(let [x] = e in f) = let [(x,dx)] = φe in δf from Fig. 8.

**Fix {x; l; e} (as sub-expression inside another fix body):**
```
(bot LATTICE_L)
```
Fix restricts context to [Γ], dropping all Ord vars. Inner fix has no Ord dependencies.
Confirmed by notes p.59: deriv(semifix) = * ↦ ⊥.

---

### `seminaive_fix` — new runtime function

```ocaml
(* Seminaive fixed-point: f is the step, df x dx is its derivative.
   Iterates x_{i+1} = x_i v dx_i, dx_{i+1} = df(x_i)(dx_i). *)
let seminaive_fix l f df =
  let rec loop x dx =
    let x' = join x dx in
    if x' = x then x
    else loop x' (df x dx)
  in
  loop (bot l) (f (bot l))
```

Stopping condition `x' = x` (OCaml structural equality) means dx added nothing new.
