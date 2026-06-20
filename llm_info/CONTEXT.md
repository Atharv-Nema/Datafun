# Datafun compiler — context for new instances

Read this after PROMPT.md to get up to speed immediately.

---

## Current status

**Task 1 (parser + type-checker): implementation complete, tests not yet written.**

The previous instance wrote `lib/pretty.ml` (pretty-printers for the AST, needed for tests)
as its last act before credits ran out. It is not yet wired into the build.

**Tasks 2 and 3 have not been started.**

---

## Codebase map

| File | Purpose |
|------|---------|
| `lib/ast.ml` | Core types: `expr`, `typ`, `fin_typ`, `lattice`, `binop` |
| `lib/lexer.mll` | ocamllex lexer |
| `lib/parser.mly` | Menhir parser — full precedence tower, all syntax forms |
| `lib/type_checker.ml` | Type synthesis (`synth`) matching `typing_rules.png`; ctx = `(var * qualifier * typ) list`; qualifiers `Disc`/`Ord`; `restrict` keeps only `Disc` vars |
| `lib/alpha.ml` | Alpha-renaming pass (`alpha_rename`) |
| `lib/frontend.ml` | Glue: `parse_string`, `parse_channel` |
| `lib/pretty.ml` | Pretty-printers: `show_typ`, `show_fin`, `show_lattice`, `show_expr` (S-expression form). **New — not yet in `lib/dune` modules list.** |
| `lib/runtime.ml` | Value type + interpreter primitives (bot, join, sing, fix, arith, for_set…) |
| `lib/codegen.ml` | Transpiles Datafun AST → OCaml source string (naive evaluation) |
| `bin/main.ml` | CLI: parse → alpha-rename → codegen → ocamlopt |

## Build system

- `dune-project`: lang dune 3.14, menhir 3.0
- `lib/dune`: library `datafun_lib`; **`pretty` is missing from the `(modules ...)` list — add it**
- opam: global `default` switch, OCaml 4.14.1
- `ppx_expect.v0.16.2` + `ppx_inline_test.v0.16.1` already installed (Jane Street v0.16 line)
- opam is configured to use wget (not curl): `opam option --global download-command=wget`

---

## TASK 1 — Concrete plan

### Step 0: Refactor error reporting in `type_checker.ml`

**Current state:** `type_checker.ml` raises `exception TypeError of string`. This is bad for testing (can only match on string content) and bad for callers (opaque).

**Target design:**

Define a structured error type and make `synth` return `result`:

```ocaml
type error =
  | UnboundVar of var
  | TypeMismatch of { expected: typ; got: typ }
  | NotAFunction of typ
  | NotAProduct of typ
  | NotASum of typ
  | NotFinite of typ        (* type is not a finite type *)
  | NotLattice of typ       (* type is not a lattice type *)
  | BranchMismatch of typ * typ
  | BoxBodyNotFinite of typ
  | FixBodyMismatch of { declared: typ; got: typ }

val show_error : error -> string   (* renders human-readable message *)

val synth : ctx -> expr -> (typ, error) result
```

Use `let*` (`Result.bind`) to thread the result through `synth`. Remove the `TypeError` exception and `fail` helper entirely. The CLI (`bin/main.ml`) and codegen render errors at the boundary via `show_error`.

**Tests to write for `show_error`** (snapshot/golden style via `ppx_expect`):
- One test per error constructor verifying the human-readable string
- E.g. `show_error (UnboundVar "x")` → `"unbound variable: x"`
- E.g. `show_error (TypeMismatch {expected=TInt; got=TUnit})` → `"type mismatch: expected int, got unit"`
- E.g. `show_error (NotAFunction TInt)` → `"expected a function type, got int"`
- etc. for every constructor

**Tests to write for `synth` error cases** now pattern-match on the variant, not the string:
- `synth [] (Var "x") = Error (UnboundVar "x")`
- `synth [] (App {e1=Lit 1; e2=Lit 2}) = Error (NotAFunction TInt)`
- `synth [] (ProjL (Lit 1)) = Error (NotAProduct TInt)`
- etc.

Do Step 0 before Step 1 (it changes the module interface that everything else depends on).

### Step 1: Wire up the build
- Add `pretty` to `(modules ...)` in `lib/dune`
- Create `test/dune`:
  ```
  (library
   (name datafun_tests)
   (libraries datafun_lib)
   (inline_tests)
   (preprocess (pps ppx_expect)))
  ```
- `dune build` to confirm everything compiles before writing any tests

### Step 2: Write tests for helper predicates in `type_checker.ml`

These are pure functions with no dependencies — easiest to test first.

**`typ_to_fin : typ -> fin_typ option`** — which types are finite?
- `TUnit` → `Some FUnit`
- `TInt` → `Some FInt`
- `TProd (TInt, TUnit)` → `Some (FProd (FInt, FUnit))`
- `TSum (TInt, TUnit)` → `Some (FSum (FInt, FUnit))`
- `TPow (FInt)` → `Some (FPow FInt)`
- `TBox TInt` → `Some (FBox FInt)`
- `TFun (TInt, TInt)` → `None` (functions are not finite)
- `TProd (TFun(...), TInt)` → `None` (product of non-finite is non-finite)

**`typ_to_lattice : typ -> lattice option`** — which types are lattice types?
- `TUnit` → `Some LUnit`
- `TProd (TUnit, TPow FInt)` → `Some (LProd (LUnit, LPow FInt))`
- `TPow FInt` → `Some (LPow FInt)`
- `TInt` → `None`
- `TFun (TInt, TInt)` → `None`
- `TSum (TUnit, TUnit)` → `None`

**`restrict : ctx -> ctx`** — context restriction keeps only `Disc`-qualified bindings
- `[(x, Ord, TInt); (y, Disc, TUnit)]` → `[(y, Disc, TUnit)]`
- Empty ctx → empty
- All-`Disc` ctx → unchanged

### Step 3: Write parser tests

Entry point: `Frontend.parse_string : string -> expr`
Output compared via `Pretty.show_expr`.

**Atoms:**
- `"5"` → `"(Lit 5)"`
- `"x"` → `"(Var x)"`
- `"()"` → `"Unit"`
- `"(1, 2)"` → `"(Pair (Lit 1) (Lit 2))"`
- `"fst (1, 2)"` → `"(ProjL (Pair (Lit 1) (Lit 2)))"`
- `"snd (1, 2)"` → `"(ProjR (Pair (Lit 1) (Lit 2)))"`
- `"{5}"` → `"(Sing (Lit 5))"`
- `"[x]"` → `"(Box (Var x))"`
- `"(inl 5 : int + unit)"` → `"(Inl (Lit 5) : (int + unit))"`
- `"(inr () : int + unit)"` → `"(Inr Unit : (int + unit))"`
- `"(bot : unit)"` → `"(Bot unit)"`

**Arithmetic precedence and associativity:**
- `"1 + 2 * 3"` → `"(BinOp Add (Lit 1) (BinOp Mul (Lit 2) (Lit 3)))"` (mul binds tighter)
- `"1 - 2 - 3"` → `"(BinOp Sub (BinOp Sub (Lit 1) (Lit 2)) (Lit 3))"` (left-assoc)
- `"1 == 2"` → `"(BinOp Eq (Lit 1) (Lit 2))"`
- `"1 <= 2"` → `"(BinOp Le (Lit 1) (Lit 2))"`

**Compound expressions:**
- `"fun (x : int) -> x"` → `"(Lam x : int . (Var x))"`
- `"f x"` → `"(App (Var f) (Var x))"`
- `"f x y"` → `"(App (App (Var f) (Var x)) (Var y))"` (left-assoc application)
- `"for x in s do x"` → `"(For x in (Var s) do (Var x))"`
- `"fix (x : unit) -> x"` → `"(Fix x : unit . (Var x))"`
- `"let [x] = e1 in e2"` → `"(LetBox x (Var e1) (Var e2))"`
- `"x V y"` → `"(Join (Var x) (Var y))"`
- `"case (e, inl (x) -> e1, inr (y) -> e2)"` → `"(Case (Var e) (inl x -> (Var e1)) (inr y -> (Var e2)))"`

**Error cases (expect `Frontend.ParseError`):**
- `"fun x -> x"` (missing type annotation)
- `""` (empty input)
- `"1 +"` (incomplete expression)

### Step 4: Write type-checker tests

Entry point: `Type_checker.synth : ctx -> expr -> typ`
Output compared via `Pretty.show_typ`. Use `Type_checker.{empty, extend, Disc, Ord}` to build contexts.

**Base rules:**
- `synth [] Unit` → `"unit"` (1I)
- `synth [] (Lit 5)` → `"int"`
- `synth [(x,Ord,TInt)] (Var "x")` → `"int"` (Var)
- `synth [(x,Disc,TInt)] (Var "x")` → `"int"` (DVar — disc vars also accessible)

**Product / sum:**
- `synth [] (Pair {e1=Lit 1; e2=Unit})` → `"(int * unit)"`
- `synth [] (ProjL (Pair {e1=Lit 1; e2=Unit}))` → `"int"`
- `synth [] (Inl {e=Lit 1; t=TSum(TInt,TUnit)})` → `"(int + unit)"`
- Case expression: scrutinee `int+unit`, branches return `int` → `"int"`

**Functions:**
- `synth [] (Lam {x="x"; a=TInt; e=Var "x"})` → `"(int -> int)"`
- `synth [] (App {e1=Lam{...id on int...}; e2=Lit 3})` → `"int"`

**Box (modal) rules — these are the trickiest, test carefully:**
- `synth [(x,Ord,TInt)] (Box (Var "x"))` → TypeError (Ord var not accessible under `[·]`)
- `synth [(x,Disc,TInt)] (Box (Var "x"))` → `"[int]"` (Disc var survives restriction)
- `synth [] (LetBox {x="x"; e1=Box(Lit 1); e2=Var "x"})` → `"int"` (DE: x bound as Disc)

**Lattice / set rules:**
- `synth [] (Bot LUnit)` → `"unit"` (T⊥)
- `synth [] (Bot (LPow FInt))` → `"set int"`
- `synth [(x,Disc,TInt)] (Sing (Var "x"))` → `"set int"` (Tone: uses restricted ctx)
- `synth [(x,Ord,TInt)] (Sing (Var "x"))` → TypeError (Ord var not accessible under `[·]`)
- `synth [] (Join {e1=Bot LUnit; e2=Bot LUnit})` → `"unit"` (TV)
- `synth [] (Join {e1=Bot LUnit; e2=Lit 1})` → TypeError (type mismatch)
- For expression: set of type `set int`, body returns `set int` → `"set int"` (Tchoose)
- Fix expression: `fix x : unit . ()` → `"unit"` (Tfix)

**Error cases (expect `TypeError`):**
- `synth [] (Var "x")` → unbound variable
- `synth [] (App {e1=Lit 1; e2=Lit 2})` → not a function
- `synth [] (ProjL (Lit 1))` → not a product

### Step 5: Run and fix
- `dune runtest --auto-promote` to accept golden outputs
- Investigate any failures; fix implementation bugs found (expected to be few)
- Do not modify tests to hide bugs — fix the code

---

## Error handling conventions

**Exceptions are reserved exclusively for programmer bugs** — treat them like `assert false`. If compiler logic is correct, they must never fire at runtime.

**Program-level errors** (bad user input, type errors, parse errors) must use `result`, `option`, or another monad. Never raise an exception for a condition that is part of the normal control flow.

Concrete implications:
- `Type_checker.synth` returns `(typ, error) result`
- `Frontend.parse_string` / `parse_channel` return `(expr, string) result` — no `ParseError` exception
- Grammar-level type constraints (finiteness, lattice) are enforced by separate `fin_typ` / `lat_typ` grammar rules rather than exceptions in parser actions
- Internal post-typecheck calls to `synth` inside `codegen.ml` use `assert false` (they truly cannot fail if the typechecker ran first)

---

## User preferences (important)

- **Strict top-down TDD workflow, always in this order:**
  1. Write the `.mli` signature (types + val declarations with one-line doc comments)
  2. Write the tests (stubs compile against the signature; bodies are `assert false` placeholders)
  3. Only then fill in the implementation
  Never skip steps or reorder them.
- Every important function needs a one-line doc comment in the `.mli` and associated tests.
- Tasks 2 and 3 are "to be refined by the human" — propose designs, wait for approval.
