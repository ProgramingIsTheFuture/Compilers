type ty = TVar of string | TArr of ty * ty
type var = string

type term =
  | Var of var
  | App of term * term
  | Abs of var * term
  | Cast of term * ty

exception Cannot_infer
exception Type_error

let rec infer env = function
  | Var x -> ( try List.assoc x env with Not_found -> raise Type_error)
  | App (t, u) -> (
      match infer env t with
      | TArr (a, b) ->
          check env u a;
          b
      | _ -> raise Type_error)
  | Abs (_, _) -> raise Cannot_infer
  | Cast (t, a) ->
      check env t a;
      a

and check env t a =
  match (t, a) with
  | Abs (x, t), TArr (a, b) -> check ((x, a) :: env) t b
  | _ -> if infer env t <> a then raise Type_error

let _ = App (Var "x", Var "x")
let _ = infer [] (Cast (Abs ("x", Var "x"), TArr (TVar "A", TVar "A")))
