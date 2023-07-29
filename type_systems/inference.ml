type typ = Tint | Tunit | Tvar of tvar | Tarrow of typ * typ
and tvar = { mutable def : typ option; id : int }

type variable = string

type term =
  | Vunit
  | Vint of int
  | Vvar of variable
  | Vfun of variable * term
  | Vapp of term * term
  | Vletin of variable * term * term

let rec print_typ = function
  | Tunit -> "unit"
  | Tint -> "int"
  | Tvar { id; _ } -> Format.sprintf "%c" (Char.chr (id + Char.code 'a'))
  | Tarrow (t1, t2) -> "(" ^ print_typ t1 ^ " -> " ^ print_typ t2 ^ ")"

let new_id =
  let i = ref (-1) in
  fun () ->
    incr i;
    !i

let rec inst = function Tvar { def = Some t; _ } -> inst t | t -> t
let new_typ_var () = Tvar { def = None; id = new_id () }

let rec fvars t =
  match inst t with
  | Tvar ({ def = None; _ } as t1) -> [ t1 ]
  | Tarrow (t1, t2) ->
      let l1 : tvar list = fvars t1 in
      let l2 : tvar list = fvars t2 in
      List.fold_left
        (fun acc l -> if List.mem l acc then acc else l :: acc)
        l2 l1
  | _ -> []

let rec unify t1 t2 =
  match (inst t1, inst t2) with
  | Tunit, Tunit -> ()
  | Tarrow (t11, t12), Tarrow (t21, t22) ->
      unify t11 t21;
      unify t12 t22
  | Tvar { id = i1; _ }, Tvar { id = i2; _ } when i1 = i2 -> ()
  | Tvar ({ def = None; _ } as t1), t2 -> t1.def <- Some t2
  | t1, Tvar ({ def = None; _ } as t2) -> t2.def <- Some t1
  | t1, t2 ->
      failwith
        (Format.sprintf "Failed unify %s with %s." (print_typ t1) (print_typ t2))

let rec infer (ctx : (variable * typ) list * tvar list) = function
  | Vunit -> Tunit
  | Vint _ -> Tint
  | Vvar name -> (
      try
        let t = List.find (fun (ss, _) -> ss = name) (fst ctx) |> snd in
        let s =
          List.fold_left (fun acc a -> (a, new_typ_var ()) :: acc) [] (snd ctx)
        in
        let rec subst t =
          match inst t with
          | Tvar x as t -> (
              try List.find (fun (v, _) -> x = v) s |> snd with Not_found -> t)
          | Tarrow (t1, t2) -> Tarrow (subst t1, subst t2)
          | t -> t
        in
        subst t
      with Not_found ->
        failwith (Format.sprintf "Variable %s not found!" name))
  | Vfun (s, e) ->
      let t = new_typ_var () in
      let ctx = ((s, t) :: fst ctx, snd ctx) in
      Tarrow (t, infer ctx e)
  | Vapp (e1, e2) ->
      let t1 = infer ctx e1 in
      let t2 = infer ctx e2 in
      let t = new_typ_var () in
      unify t1 (Tarrow (t2, t));
      inst t
  | Vletin (s, e1, e2) ->
      let t1 = infer ctx e1 in
      let ctx = ((s, inst t1) :: fst ctx, fvars t1 @ snd ctx) in
      infer ctx e2

let id = Vfun ("s", Vvar "s")
let _ = infer ([], []) id |> print_typ |> print_string |> print_newline
let id_unit = Vapp (Vfun ("s", Vvar "s"), Vunit)
let _ = infer ([], []) id_unit |> print_typ |> print_string |> print_newline
let lin = Vletin ("id", id, Vapp (Vvar "id", Vunit))
let _ = infer ([], []) lin |> print_typ |> print_string |> print_newline
let lin = Vletin ("id", id, Vvar "id")
let _ = infer ([], []) lin |> print_typ |> print_string |> print_newline

let lin =
  Vletin
    ( "id",
      id,
      Vletin ("unit", Vapp (Vvar "id", Vunit), Vapp (Vvar "id", Vint 1)) )

let _ = infer ([], []) lin |> print_typ |> print_string |> print_newline
