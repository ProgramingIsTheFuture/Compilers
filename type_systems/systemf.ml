type ident = string

module Kind = struct
  type t = Star

  let to_string = function Star -> "*"
end

module Types = struct
  type t =
    | Int
    | Var of ident
    | Forall of ident * Kind.t * t * t
    | Arrow of t * t

  let rec to_string = function
    | Int -> "int"
    | Var s -> s
    | Arrow (t1, t2) -> Format.sprintf "%s -> %s" (to_string t1) (to_string t2)
    | Forall (ident, k, t1, t2) ->
        Format.sprintf "all %s : %s. %s -> %s" ident (Kind.to_string k)
          (to_string t1) (to_string t2)

  let rec subst find_type = function
    | Int -> Int
    | Var s -> find_type s
    | Arrow (t1, t2) -> Arrow (subst find_type t1, subst find_type t2)
    | Forall (ident, k, t1, t2) ->
        Forall (ident, k, subst find_type t1, subst find_type t2)

  let rec compare find_type t1 t2 =
    match (t1, t2) with
    | Int, Int -> true
    | Var s, Var j when s = j -> true
    | Var s, Var j -> compare find_type (find_type s) (find_type j)
    | Arrow (t11, t12), Arrow (t21, t22) ->
        compare find_type t11 t21 && compare find_type t12 t22
    | Forall (_, _, t11, t12), Forall (_, _, t21, t22) ->
        compare find_type t11 t21 && compare find_type t12 t22
    | Var s, t | t, Var s -> compare find_type (find_type s) t
    | _ ->
        failwith
          (Format.sprintf "Failed unifying %s and %s" (to_string t1)
             (to_string t2))
end

module Terms = struct
  type t =
    | Var of ident
    | Abs of ident * Types.t * t
    | App of t * t
    | UAbs of ident * Kind.t * t
    | UApp of t * Types.t

  let rec to_string = function
    | Var s -> s
    | Abs (s, typ, t) ->
        Format.sprintf "(λ%s:%s. %s)" s (Types.to_string typ) (to_string t)
    | App (t1, t2) -> Format.sprintf "%s %s" (to_string t1) (to_string t2)
    | UAbs (i, k, t) ->
        Format.sprintf "Λ%s : %s. %s" i (Kind.to_string k) (to_string t)
    | UApp (t, typ) ->
        Format.sprintf "(%s) (%s)" (to_string t) (Types.to_string typ)
end

module Ctx = struct
  type t = {
    types : (ident * Kind.t * Types.t option) list;
    vars : (ident * Types.t) list;
  }

  let empty = { types = []; vars = [] }
  let find_var s ctx = List.find (fun (a, _) -> a = s) ctx.vars
  let find_type s ctx = List.find (fun (a, _, _) -> a = s) ctx.types
  let add_var i t ctx = { ctx with vars = (i, t) :: ctx.vars }

  let add_type i t ?(typ = None) ctx =
    { ctx with types = (i, t, typ) :: ctx.types }
end

let rec type_of (env : Ctx.t) : Terms.t -> Types.t = function
  | Terms.Var s ->
      Types.subst
        (fun s ->
          let _, _, t = Ctx.find_type s env in
          Option.get t)
        (Ctx.find_var s env |> snd)
  | Terms.Abs (s, typ, t) ->
      Types.Arrow
        ( Types.subst
            (fun s ->
              let _, _, t = Ctx.find_type s env in
              Option.get t)
            typ,
          type_of (Ctx.add_var s typ env) t )
  | Terms.App (t1, t2) ->
      let typ1 = type_of env t1 in
      let typ2 = type_of env t2 in
      let rec h t =
        begin
          match t with
          | Types.Arrow (typ11, typ12) ->
              assert (
                Types.compare
                  (fun s ->
                    let _, _, t = Ctx.find_type s env in
                    Option.get t)
                  typ11 typ2);
              Types.subst
                (fun s ->
                  let _, _, t = Ctx.find_type s env in
                  Option.get t)
                typ12
          | Types.Var s -> begin
              let _, _, t = Ctx.find_type s env in
              h (Option.get t)
            end
          | _ -> failwith "Expected an arrow function"
        end
      in
      h typ1
  | Terms.UAbs (s, k, t) ->
      let env' = Ctx.add_type s k env in
      let typ' = type_of env' t in
      begin
        match typ' with
        | Arrow (t1', t2') -> Types.Forall (s, k, t1', t2')
        | _ -> failwith "Bad usage of Universal Abs"
      end
  | Terms.UApp (t, typ) -> begin
      match t with
      | Terms.UAbs (s, k, t) ->
          let env = Ctx.add_type s k ~typ:(Some typ) env in
          type_of env t
      | _ -> assert false
    end

let () =
  let open Terms in
  let term =
    UApp
      ( UAbs
          ( "a",
            Kind.Star,
            Abs
              ("d", Types.Int, Abs ("s", Types.Var "a", App (Var "s", Var "d")))
          ),
        Arrow (Int, Int) )
  in
  Format.printf "%s : %s\n" (Terms.to_string term)
    (Types.to_string (type_of Ctx.empty term))

let () =
  let open Terms in
  let term =
    UApp (UAbs ("a", Kind.Star, Abs ("x", Types.Var "a", Var "x")), Types.Int)
  in
  Format.printf "%s : %s\n" (Terms.to_string term)
    (Types.to_string (type_of Ctx.empty term))
