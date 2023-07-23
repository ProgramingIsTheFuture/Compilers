type typ = Tunit | Tproduct of typ * typ | Tarrow of typ * typ
type variable = string
type var_state = Unconsumed | Consumed

type term =
  | Vunit
  | Vvar of variable
  | Vletin of variable * term * term
  | Vproduct of term * term
  | Vfun of variable * typ * term
  | Vapp of term * term

let rec print_typ = function
  | Tunit -> "unit"
  | Tproduct (t1, t2) -> "(" ^ print_typ t1 ^ ", " ^ print_typ t2 ^ ")"
  | Tarrow (t1, t2) -> "(" ^ print_typ t1 ^ " -> " ^ print_typ t2 ^ ")"

type tbl = StateTable of (variable * typ * var_state) list

exception NotConsumed of string
exception AlreadyConsumed of string
exception VarNotFound of string
exception UnifyError of string

let table_rows tbl =
  let (StateTable rows) = tbl in
  rows

let get_entry tbl var =
  match List.find_opt (fun (n, _, _) -> var = n) (table_rows tbl) with
  | Some (_, ty, state) -> Some (ty, state)
  | None -> None

let add tbl var t =
  let (StateTable rows) = tbl in
  match get_entry tbl var with
  | None -> StateTable ((var, t, Unconsumed) :: rows)
  | Some _ -> failwith "Failed linear types?"

let update_tbl tbl var state =
  match get_entry tbl var with
  | None ->
      raise (VarNotFound (Format.sprintf "Variable %s was not found!" var))
  | Some (ty, old_state) ->
      if old_state = Consumed then
        raise
          (AlreadyConsumed
             (Format.sprintf "Variable %s was already consumed" var))
      else
        let (StateTable rows) = tbl in
        let other_entries = List.filter (fun (n, _, _) -> not (var = n)) rows in
        StateTable ((var, ty, state) :: other_entries)

let remove_tbl tbl var =
  match get_entry tbl var with
  | None ->
      raise (VarNotFound (Format.sprintf "Variable %s was not found!" var))
  | Some (_, state) ->
      if state = Unconsumed then
        raise (NotConsumed (Format.sprintf "Variable %s was not consumed!" var))
      else
        let (StateTable rows) = tbl in
        let others = List.filter (fun (n, _, _) -> not (var = n)) rows in
        StateTable others

let unify t1 t2 =
  if t1 = t2 then ()
  else
    raise
      (UnifyError
         (Format.sprintf "Expected %s but got %s." (print_typ t1) (print_typ t2)))

let rec check_linearity_infer tbl = function
  | Vunit -> (Tunit, tbl)
  | Vvar s -> (
      match get_entry tbl s with
      | None ->
          raise (VarNotFound (Format.sprintf "Variable %s was not found!" s))
      | Some (t, _) -> (t, update_tbl tbl s Consumed))
  | Vletin (s, t1, t2) ->
      let t, tbl = check_linearity_infer tbl t1 in
      let tbl = add tbl s t in
      let t, tbl = check_linearity_infer tbl t2 in
      (t, remove_tbl tbl s)
  | Vproduct (e1, e2) ->
      let t1, tbl = check_linearity_infer tbl e1 in
      let t2, tbl = check_linearity_infer tbl e2 in
      (Tproduct (t1, t2), tbl)
  | Vfun (s, t1, e) ->
      let tbl = add tbl s t1 in
      let t2, tbl = check_linearity_infer tbl e in
      (Tarrow (t1, t2), remove_tbl tbl s)
  | Vapp (e1, e2) -> (
      let t1, tbl = check_linearity_infer tbl e1 in
      let t2, tbl = check_linearity_infer tbl e2 in
      match t1 with
      | Tarrow (t11, t12) ->
          unify t11 t2;
          (t12, tbl)
      | _ -> failwith "Expected a function")

let id = Vfun ("s", Tunit, Vvar "s")
let _ = check_linearity_infer (StateTable []) id
let id_err = Vletin ("x", Vunit, Vfun ("y", Tunit, Vvar "x"))

let _ =
  try check_linearity_infer (StateTable []) id_err
  with NotConsumed _ -> (Tunit, StateTable [])

let p = Vfun ("x", Tunit, Vproduct (Vvar "x", Vunit))
let _ = check_linearity_infer (StateTable []) p
let product = Vfun ("s", Tproduct (Tunit, Tunit), Vvar "s")
let _ = check_linearity_infer (StateTable []) product

let app =
  Vfun ("a", Tunit, Vfun ("f", Tarrow (Tunit, Tunit), Vapp (Vvar "f", Vvar "a")))

let _ = check_linearity_infer (StateTable []) app
