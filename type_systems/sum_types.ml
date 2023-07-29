type variable = string
type typ = Tunit | Tint | Tsum of variable list

type term =
  | Vunit
  | Vint
  | Vvar of variable
  | Vmatch of term * (variable * term) list

let print_typ = function
  | Tunit -> "unit"
  | Tint -> "int"
  | Tsum s -> Format.sprintf "Tsum %s" (String.concat " | " s)

let rec infer ctx : term -> typ = function
  | Vunit -> Tunit
  | Vint -> Tint
  | Vvar name -> (
      try List.find (fun (a, _) -> a = name) ctx |> snd
      with Not_found -> failwith "variable not found!")
  | Vmatch (e, le) -> (
      let t1 = infer ctx e in
      match t1 with
      | Tsum sl ->
          let t, result =
            List.fold_left
              (fun (acc, missing_cases) (case, e1) ->
                if missing_cases = 0 then
                  let () = Format.printf "Cases avoided" in
                  (acc, missing_cases)
                else
                  match acc with
                  | None when case = "_" -> (Some (infer ctx e1), 0)
                  | None when List.mem case sl ->
                      (Some (infer ctx e1), missing_cases - 1)
                  | Some t_expected when case = "_" ->
                      let t_received = infer ctx e1 in
                      let correct = t_expected = t_received in
                      if correct then (acc, 0)
                      else
                        failwith
                          (Format.sprintf "Expected type %s but got %s"
                             (print_typ t_expected) (print_typ t_received))
                  | Some t_expected when List.mem case sl ->
                      let t_received = infer ctx e1 in
                      let correct = t_expected = t_received in
                      if correct then (acc, missing_cases - 1)
                      else
                        failwith
                          (Format.sprintf "Expected type %s but got %s"
                             (print_typ t_expected) (print_typ t_received))
                  | _ -> failwith "case not defined")
              (None, List.length sl)
              le
          in
          if result <> 0 then failwith "missing cases";
          Option.get t
      | _ -> failwith "Invalid type for match")

let a_var = Tsum [ "false"; "true"; "abc" ]
let _ = Vint
let match_case = Vmatch (Vvar "a", [ ("false", Vunit); ("_", Vunit) ])
let _ = infer [ ("a", a_var) ] match_case
