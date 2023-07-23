type typ = Tunit | Tproduct of typ * typ | Tarrow of typ * typ
type variable = string

type term =
  | Vunit
  | Vvar of variable
  | Vproduct of term * term
  | Vfun of variable * typ * term
  | Vapp of term * term

let rec print_typ = function
  | Tunit -> "unit"
  | Tproduct (t1, t2) -> "(" ^ print_typ t1 ^ ", " ^ print_typ t2 ^ ")"
  | Tarrow (t1, t2) -> "(" ^ print_typ t1 ^ " -> " ^ print_typ t2 ^ ")"

let rec infer ctx = function
  | Vunit -> Tunit
  | Vvar s -> (
      try List.find (fun (a, _) -> a = s) ctx |> snd
      with _ -> failwith (Format.sprintf "Not found variable %s" s))
  | Vproduct (t1, t2) -> Tproduct (infer ctx t1, infer ctx t2)
  | Vfun (t1, typ, t2) ->
      let ctx = (t1, typ) :: ctx in
      Tarrow (typ, infer ctx t2)
  | Vapp (t1, t2) -> (
      match infer ctx t1 with
      | Tarrow (t1, t12) ->
          let t2 = infer ctx t2 in
          if t1 = t2 then t12
          else
            let () =
              Format.eprintf "Expected %s but got %s.\n" (print_typ t1)
                (print_typ t2)
            in
            exit 1
      | _ -> failwith "This expression is not a function")

let empty_ctx = []
let id = Vfun ("x", Tunit, Vvar "x")
let () = Format.printf "Id?\n"
let _ = assert (infer empty_ctx id = Tarrow (Tunit, Tunit))

let ff =
  Vfun
    ( "a",
      Tunit,
      Vfun
        ("f", Tarrow (Tunit, Tproduct (Tunit, Tunit)), Vapp (Vvar "f", Vvar "a"))
    )

let () = Format.printf "ff?\n"

let _ =
  assert (
    infer empty_ctx ff
    = Tarrow
        ( Tunit,
          Tarrow
            (Tarrow (Tunit, Tproduct (Tunit, Tunit)), Tproduct (Tunit, Tunit))
        ))

let app = Vapp (Vapp (ff, Vunit), Vfun ("f", Tunit, Vproduct (Vunit, Vunit)))
let () = Format.printf "app?\n"
let _ = assert (infer empty_ctx app = Tproduct (Tunit, Tunit))
let fst = Vfun ("x", Tproduct (Tunit, Tunit), Vvar "x")
let product = Vapp (fst, Vproduct (Vunit, Vunit))
let () = Format.printf "product?\n"
let _ = assert (infer empty_ctx product = Tproduct (Tunit, Tunit))
let () = Format.printf "Success!\n"
