open Ast

type counter = int ref

let create_counter () = ref 0

let fresh (c : counter) x =
  let n = !c in
  incr c;
  Printf.sprintf "%s_%d" x n

let rec rename (c : counter) subst = function
  | Var x ->
    Var (Option.value ~default:x (List.assoc_opt x subst))
  | Lam { x; a; e } ->
    let x' = fresh c x in
    Lam { x = x'; a; e = rename c ((x, x') :: subst) e }
  | App { e1; e2 } ->
    App { e1 = rename c subst e1; e2 = rename c subst e2 }
  | Lit _ as e -> e
  | BinOp { op; e1; e2 } ->
    BinOp { op; e1 = rename c subst e1; e2 = rename c subst e2 }
  | Unit -> Unit
  | Pair { e1; e2 } ->
    Pair { e1 = rename c subst e1; e2 = rename c subst e2 }
  | ProjL e -> ProjL (rename c subst e)
  | ProjR e -> ProjR (rename c subst e)
  | Inl { e; t } -> Inl { e = rename c subst e; t }
  | Inr { e; t } -> Inr { e = rename c subst e; t }
  | Case { e; x; e1; y; e2 } ->
    let x' = fresh c x and y' = fresh c y in
    Case { e  = rename c subst e;
           x  = x';
           e1 = rename c ((x, x') :: subst) e1;
           y  = y';
           e2 = rename c ((y, y') :: subst) e2 }
  | Bot l -> Bot l
  | Join { e1; e2 } ->
    Join { e1 = rename c subst e1; e2 = rename c subst e2 }
  | For { e1; x; e2 } ->
    let x' = fresh c x in
    For { e1 = rename c ((x, x') :: subst) e1; x = x'; e2 = rename c subst e2 }
  | Sing e -> Sing (rename c subst e)
  | Box e -> Box (rename c subst e)
  | LetBox { x; e1; e2 } ->
    let x' = fresh c x in
    LetBox { x = x'; e1 = rename c subst e1; e2 = rename c ((x, x') :: subst) e2 }
  | Fix { x; l; e } ->
    let x' = fresh c x in
    Fix { x = x'; l; e = rename c ((x, x') :: subst) e }

let alpha_rename e = rename (create_counter ()) [] e
