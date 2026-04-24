open Ast

let counter = ref 0

let fresh x =
  let n = !counter in
  incr counter;
  Printf.sprintf "%s_%d" x n

let rec rename subst = function
  | Var x ->
    Var (Option.value ~default:x (List.assoc_opt x subst))
  | Lam { x; a; e } ->
    let x' = fresh x in
    Lam { x = x'; a; e = rename ((x, x') :: subst) e }
  | App { e1; e2 } ->
    App { e1 = rename subst e1; e2 = rename subst e2 }
  | Unit -> Unit
  | Pair { e1; e2 } ->
    Pair { e1 = rename subst e1; e2 = rename subst e2 }
  | Pil e -> Pil (rename subst e)
  | Pir e -> Pir (rename subst e)
  | Inl { e; t } -> Inl { e = rename subst e; t }
  | Inr { e; t } -> Inr { e = rename subst e; t }
  | Case { e; x; e1; y; e2 } ->
    let x' = fresh x and y' = fresh y in
    Case { e  = rename subst e;
           x  = x';
           e1 = rename ((x, x') :: subst) e1;
           y  = y';
           e2 = rename ((y, y') :: subst) e2 }
  | Bot l -> Bot l
  | Join { l; e1; e2 } ->
    Join { l; e1 = rename subst e1; e2 = rename subst e2 }
  | For { e1; x; e2 } ->
    (* for(e1).x∈e2: x is bound in e1 (body), e2 (set) is outside scope *)
    let x' = fresh x in
    For { e1 = rename ((x, x') :: subst) e1; x = x'; e2 = rename subst e2 }
  | Sing e -> Sing (rename subst e)
  | Box e -> Box (rename subst e)
  | LetBox { x; e1; e2 } ->
    let x' = fresh x in
    LetBox { x = x'; e1 = rename subst e1; e2 = rename ((x, x') :: subst) e2 }
  | Fix { x; l; e } ->
    let x' = fresh x in
    Fix { x = x'; l; e = rename ((x, x') :: subst) e }

let alpha_rename e = rename [] e
