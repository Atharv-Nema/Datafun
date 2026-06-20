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

> **To be designed with the human.** Fill in the plan here before starting.
