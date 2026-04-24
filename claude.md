# Datafun Compiler

A type checker and compiler for Datafun (Neel Krishnaswami), targeting OCaml with seminaive evaluation.

## Directory structure

```
lib/   — datafun_lib library (dune: modules ast alpha type_checker frontend codegen)
bin/   — main executable (dune: executable main, libraries datafun_lib)
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

## lib/ast.ml — current state

`fin_typ`, `lattice`, `typ` use plain tuples (not named fields). `expr` uses named record fields for all multi-argument constructors, with field names matching the grammar above (`x`, `a`, `e`, `e1`, `e2`, `y`, `l`, `t`).

Key `expr` constructors to be aware of:
- `Inl { e; t }` / `Inr { e; t }` — `t` is the full annotated sum type `A1+A2`. Needed because `+I` is a checking rule and synthesis alone cannot determine the missing branch type.
- `For { e1; x; e2 }` — `e1` is the **body** (x is bound here), `e2` is the **set**. This follows the grammar `for(e₁).x∈e₂`. Note: the Tchoose typing rule in image.png uses the **opposite** naming (`e1`=set, `e2`=body) — the AST follows the grammar, not the typing rule's local variable names.
- `Bot of lattice`, `Join { l; e1; e2 }`, `Fix { x; l; e }` — `l` carries the lattice type at the term level.

## lib/alpha.ml

Alpha-renaming pass: renames all bound variables to fresh names (`x` → `x_0`, `x_1`, …) using a global counter. Entry point: `alpha_rename : expr -> expr`. Run this before type checking if needed.

Scoping notes encoded in the implementation:
- `For`: x is bound only in `e1` (body), not in `e2` (set)
- `Case`: x bound only in `e1`, y bound only in `e2`
- `LetBox`: x bound only in `e2`

## lib/type_checker.ml — current state

**Context:**
```ocaml
type qualifier = Disc | Ord
type ctx = (var * qualifier * typ) list
```
`restrict ctx` implements `[Γ]` — keeps only `Disc` bindings.

**Coercion functions** (needed because the three type hierarchies are separate):
- `lattice_to_typ` — always succeeds
- `fin_to_typ` — always succeeds
- `typ_to_fin` — fails on `TFun` (functions are not finite)
- `typ_to_lattice` — fails on `TFun`, `TSum`, `TBox`

**Entry point:** `synth : ctx -> expr -> typ` raises `TypeError of string` on failure.

**Typing rules implemented** (names from image.png):
- Var/DVar — both Ord and Disc bindings allow variable use
- 1I, ×I, ×E, λI, λE, +I (with annotation), +E
- DI (`Box`): checks `e` under `restrict ctx`
- DE (`LetBox`): binds x as `Disc`
- T⊥ (`Bot`): returns `lattice_to_typ l`
- Tone (`Sing`): checks `e` under `restrict ctx`, requires result is `fin_typ`
- T∨ (`Join`): checks both operands against `lattice_to_typ l`
- Tchoose (`For`): synthesizes `e2` as `TPow t`, then checks `e1` under `x:ᴰT`; verifies result is a lattice type
- Tfix (`Fix`): checks `e` under `restrict ctx` extended with `x:Ord (lattice_to_typ l)`

## lib/frontend.ml, lib/codegen.ml

Both are currently empty stubs.

## What's next: compilation to OCaml (codegen.ml)

The plan (from Arntzenius-Krishnaswami) is to compile Datafun to OCaml with **seminaive evaluation**:

1. **Naive compilation** first — straightforward translation of each construct to OCaml:
   - `TPow t` → OCaml `Set` (need a runtime library module)
   - `TFun` → OCaml function
   - `fix x:L.e` → `while` loop iterating until fixpoint
   - `⊥_L` → bottom element of the lattice (empty set, unit, etc.)
   - `∨_L` → join (set union, etc.)

2. **Seminaive translation** — avoids redundant work in fixpoint iteration:
   - Implement the **derivative** `δe` which computes how the output changes given a change in inputs
   - Rewrite `fix` to use `δe` between iterations, maintaining a delta set `ΔS`
   - Loop terminates when `ΔS = ∅`
   - Requires `union_and_diff` on sets: computes `S ∪ ΔS` and the new delta `(S ∪ ΔS) \ S`

3. **Runtime library** — support module for compiled code:
   - Wrap OCaml's `Set` module
   - Equality via hash-consing (pointer equality for O(1) fixpoint convergence check)
   - `union_and_diff` operation

## Design decisions already made

- `fin_typ`/`lattice`/`typ` are three separate OCaml types (not unified), matching the grammar exactly. Coercions are explicit functions in `type_checker.ml`.
- `Inl`/`Inr` require an explicit type annotation `t: typ` (the full `A1+A2` type). This is a deliberate simplification — `+I` is inherently a checking rule.
- Multi-argument `expr` constructors use named record fields matching the mathematical notation from image.png (`x`, `a`, `e`, `e1`, `e2`, etc.).
- `fin_typ`/`lattice`/`typ` constructors use plain tuples (not named fields) — the user reverted named fields for these via linter.
- Context is a simple association list `(var * qualifier * typ) list`, newest binding first.
