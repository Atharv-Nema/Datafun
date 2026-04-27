# Datafun Compiler

A type checker and compiler for Datafun (Neel Krishnaswami), targeting OCaml with seminaive evaluation.

## Directory structure

```
lib/   — datafun_lib library (modules: ast alpha type_checker frontend runtime codegen)
bin/   — main executable (stub: just prints "Datafun compiler")
image.png — the reference grammar and typing rules (refer to this often)
```

## Grammar (from image.png)

```
Terms   e ::= x | λx:A.e | e₁e₂ | ⟨⟩ | ⟨e₁,e₂⟩ | πᵢe
            | ιᵢ(e) | case(e, ι₁(x)→e₁, ι₂(y)→e₂)
            | ⊥_L | e₁ ∨_L e₂ | for(e₁).x∈e₂ | {e}
            | [e] | let[x]=e₁ in e₂ | fix x:L.e

Types       A ::= 1 | A×B | A→B | A+B | 𝒫(T) | [A]
Finite types T ::= 1 | T×T | T+T | 𝒫(T) | [T]
Lattice types L ::= 1 | L×L | 𝒫(T)

Contexts    Γ ::= · | Γ, x:^q A
Qualifiers  q ::= D | ·
```

## lib/ast.ml

`fin_typ`, `lattice`, `typ` use plain tuples (not named fields). `expr` uses named record fields for all multi-argument constructors, with field names matching the grammar (`x`, `a`, `e`, `e1`, `e2`, `y`, `l`, `t`).

Key `expr` constructors:
- `Inl { e; t }` / `Inr { e; t }` — `t` is the full annotated sum type `A1+A2`. Needed because `+I` is a checking rule and synthesis alone cannot determine the missing branch type.
- `For { e1; x; e2 }` — `e1` is the **body** (x is bound here), `e2` is the **set**. Follows grammar `for(e₁).x∈e₂`. The Tchoose typing rule in image.png uses the **opposite** naming — the AST follows the grammar, not the rule's local names.
- `Bot of lattice`, `Join { l; e1; e2 }`, `Fix { x; l; e }` — `l` carries the lattice type at the term level.

## lib/alpha.ml

Alpha-renaming pass: renames all bound variables to fresh names (`x` → `x_0`, `x_1`, …). Entry point: `alpha_rename : expr -> expr`. **Run this before `compile` to avoid variable shadowing in generated OCaml.**

Scoping rules encoded:
- `For`: x is bound only in `e1` (body), not in `e2` (set)
- `Case`: x bound only in `e1`, y bound only in `e2`
- `LetBox`: x bound only in `e2`

## lib/type_checker.ml

**Context:**
```ocaml
type qualifier = Disc | Ord
type ctx = (var * qualifier * typ) list
```
`restrict ctx` implements `[Γ]` — keeps only `Disc` bindings.

**Coercion functions:**
- `lattice_to_typ` — always succeeds
- `fin_to_typ` — always succeeds
- `typ_to_fin` — fails on `TFun`
- `typ_to_lattice` — fails on `TFun`, `TSum`, `TBox`

**Entry point:** `synth : ctx -> expr -> typ` raises `TypeError of string` on failure.

**Typing rules** (names from image.png):
- Var/DVar, 1I, ×I, ×E, λI, λE, +I (with annotation), +E
- DI (`Box`): checks `e` under `restrict ctx`
- DE (`LetBox`): binds x as `Disc`
- T⊥ (`Bot`): returns `lattice_to_typ l`
- Tone (`Sing`): checks `e` under `restrict ctx`, requires result is `fin_typ`
- T∨ (`Join`): checks both operands against `lattice_to_typ l`
- Tchoose (`For`): synthesizes `e2` as `TPow t`, checks `e1` under `x:ᴰT`, verifies result is lattice type
- Tfix (`Fix`): checks `e` under `restrict ctx` extended with `x:Ord (lattice_to_typ l)`

## lib/runtime.ml

Runtime support module for compiled Datafun programs. **This is a new file added alongside codegen.**

**Value representation:**
```ocaml
module rec Value : sig
  type t = VUnit | VPair of t * t | VInl of t | VInr of t
         | VSet of VSet.t | VFunc of (Value.t -> Value.t)
  val compare : t -> t -> int
end
and VSet : Set.S with type elt = Value.t = Set.Make(Value)
```
- `VFunc` is used for all function-typed Datafun terms; functions are NOT lattice elements and are never joined or compared.
- `Box` is erased at runtime — no `VBox` constructor. `Box e` compiles to the value of `e`; `LetBox` just binds it.
- `compare` provides a total order for `Set.Make`. It is NOT the lattice partial order. Handles `VInl`/`VInr` cross-comparison: `VInl < VInr` (so sets of sum types work correctly).

**Key functions:**
- `bot : lattice -> value` — bottom element
- `join : value -> value -> value` — lattice join (union for sets, unit for unit, pairwise for products)
- `sing : value -> value` — singleton set `VSet {v}`
- `for_set : lattice -> value -> value -> value` — bigcup: `for_set l (VSet s) (VFunc f)` = `⊔{f(x) | x ∈ s}` starting from `bot l`
- `fix : lattice -> value -> value` — least fixpoint: iterates `x := x ∨ f(x)` from `bot l` until stable
- `apply`, `projl`, `projr`, `case` — straightforward; use irrefutable patterns (fail loudly on type mismatch)

## lib/codegen.ml

Compiles a Datafun `expr` to an OCaml source string. **Assumes the input is well-typed** (calls `synth` internally to recover type information where the AST omits it).

**Entry point:** `compile_program : expr -> string` — returns a complete `.ml` file with header `open Ast; open Runtime; open Value`.

**Key compilation choices:**
- `Box e` → `compile (restrict ctx) e` (erased; no runtime wrapper)
- `LetBox { x; e1; e2 }` → `apply (VFunc (fun x -> e2)) e1` (beta-reduces immediately)
- `For { e1; x; e2 }` → `for_set (l) (e2) (VFunc (fun x -> e1))` where `l` is recovered via `synth`
- `Fix { x; l; e }` → `fix (l) (VFunc (fun x -> e))` under `restrict ctx` with `x:Ord`
- `App { e1; e2 }` → `apply (e1) (e2)` (first arg must be VFunc)
- `Sing e` → `sing (e)` (note: codegen does NOT restrict ctx here, but type safety ensures e only uses discrete vars)

**`compile_fin` / `compile_lattice`:** Convert OCaml `fin_typ`/`lattice` values to their OCaml constructor strings (e.g., `LPow (FUnit)`), for embedding lattice annotations in generated code.

## lib/lexer.mll / lib/parser.mly

Lexer and parser for the surface syntax. Tokens and surface forms:

| Surface | AST constructor | Notes |
|---|---|---|
| `fun (x:A) -> e` | `Lam` | |
| `for x in e do e` | `For` | `e` (set) must be `app_expr`; use parens if needed |
| `fix (x:L) -> e` | `Fix` | |
| `let [x] = e1 in e2` | `LetBox` | |
| `case e of inl x -> e1 \| inr y -> e2` | `Case` | leading `\|` optional |
| `e1 V e2` | `Join` | left-associative, below application |
| `f x` | `App` | left-associative |
| `{e}` | `Sing` | |
| `[e]` | `Box` | |
| `bot(L)` | `Bot` | |
| `fst e` / `snd e` | `ProjL` / `ProjR` | |
| `(inl e : T)` / `(inr e : T)` | `Inl` / `Inr` | full sum type annotation required |
| `(e1, e2)` | `Pair` | |
| `()` | `Unit` | |

Type syntax: `unit`, `A * B`, `A + B`, `A -> B`, `set T` (for `𝒫(T)`), `[A]` (box type).

Binder forms (`fun`, `for`, `fix`, `let [...]`, `case`) extend as far right as possible. When used as the right operand of `V`, wrap in parentheses.

## lib/frontend.ml

`ParseError of string` exception + `parse_channel`/`parse_string` entry points. Catches `Lexer.Lexer_error` and `Parser.Error` and re-raises as `ParseError`.

## bin/main.ml

Full pipeline: parse `.df` file → alpha-rename → codegen (type-checks internally) → write to temp `.ml` → compile with `ocamlopt` against `_build/default/lib/datafun_lib.cmxa` → produce native executable named after the input file.

Compile with: `dune exec bin/main.exe -- <file.df>` then run `./<name>`.

The `ocamlopt` invocation uses `-I _build/default/lib -I _build/default/lib/.datafun_lib.objs/byte` so that `open Datafun_lib` resolves correctly.

## examples/

Sample programs in `examples/`. Compile with `dune exec bin/main.exe -- examples/<name>.df && ./<name>`.

- `hello.df` — unit value `()`
- `singleton.df` — `{()}`, a singleton set
- `union.df` — `{()} V {()}`, join of two sets
- `for_loop.df` — `let [s] = [...] in for x in s do {x}`
- `reachability.df` — fixpoint reachability over a 2-node graph using `fix`/`for`/`case`

## Current status

- [x] AST (`ast.ml`)
- [x] Alpha-renaming (`alpha.ml`)
- [x] Type checker (`type_checker.ml`)
- [x] Runtime library (`runtime.ml`)
- [x] Naive codegen (`codegen.ml`)
- [x] Lexer/parser (`lexer.mll`, `parser.mly`)
- [x] Full pipeline (`main.ml`)
- [x] Example programs (`examples/`)
- [ ] Seminaive evaluation (derivative transform `δe`, delta fixpoint loop)

## Seminaive evaluation (next major milestone)

The plan (Arntzenius-Krishnaswami) is to avoid redundant work in fixpoint iteration:
1. Implement the **derivative** `δe` which computes how the output changes given a change in inputs.
2. Rewrite `fix` to use `δe` between iterations, maintaining a delta set `ΔS`.
3. Loop terminates when `ΔS = ∅`.
4. Requires `union_and_diff` on sets: computes `S ∪ ΔS` and the new delta `(S ∪ ΔS) \ S`.

## Error handling convention

Three tiers — do not mix them:

| Tier | Exception | Where raised | Meaning |
|---|---|---|---|
| User syntax error | `Frontend.ParseError of string` | `frontend.ml` (catches `Lexer.Lexer_error` and `Parser.Error`) | Invalid surface syntax |
| User type error | `Type_checker.TypeError of string` | `type_checker.ml`, `parser.mly` helpers (`fin_exn`, `lat_exn`) | Ill-typed program |
| Internal invariant | `failwith "..."` | `runtime.ml`, `codegen.ml` | Should be unreachable on well-typed input; indicates a compiler bug |

`bin/main.ml` is the **only** catch site. It catches `ParseError` and `TypeError` and exits cleanly. `Failure` from `failwith` propagates as an unhandled OCaml exception (intentional — stack trace is useful for debugging compiler bugs).

**Do not use `Result` types for errors inside the library.** Raise exceptions and let them propagate.

## Design decisions

- `fin_typ`/`lattice`/`typ` are three separate OCaml types, not unified. Coercions are explicit functions in `type_checker.ml`.
- `Inl`/`Inr` require an explicit type annotation `t: typ` (the full `A1+A2`). Deliberate simplification — `+I` is inherently a checking rule.
- Multi-argument `expr` constructors use named record fields matching mathematical notation.
- `fin_typ`/`lattice`/`typ` constructors use plain tuples (not named fields).
- Context is `(var * qualifier * typ) list`, newest binding first.
- `VInl _ < VInr _` in `Value.compare` — arbitrary but consistent; enables sets of sum types.
