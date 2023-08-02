open Ast
open Format

(* Excepção levantada para assinalar um erro durante a interpretação *)

exception Error of string

let error s = raise (Error s)

(* Os valores de Mini-Python

   - uma diferença notável com Python : utilizamos o tipo int enquanto
     os inteiros de Python são de precisão arbitrária ; poderiamos ter
     escolhido o tipo disponível no módulo Big_int d'OCaml, mas preferimos
     aqui a solução "fácil"

   - o que Python designa por lista é na realidade um vector que se pode
     redimensionar ; no fragmento considerado aqui, não existe a possibilidade
     de modificação do comprimento, assim um simples vector OCaml chega *)
type value =
  | Vnone
  | Vbool of bool
  | Vint of int
  | Vstring of string
  | Vlist of value array

(* Vizualização de um valor na saída standard *)
let rec print_value = function
  | Vnone -> printf "None"
  | Vbool true -> printf "True"
  | Vbool false -> printf "False"
  | Vint n -> printf "%d" n
  | Vstring s -> printf "%s" s
  | Vlist a ->
      let n = Array.length a in
      printf "[";
      for i = 0 to n - 1 do
        print_value a.(i);
        if i < n - 1 then printf ", "
      done;
      printf "]"

(* Interpretação booleana de um valor

   Em Python, qualquer valor pode ser utilizado como um boleano : None,
   a lista vazia, a string vazia e o inteiro 0 são considerados como
   falso/False e qualquer outro valor é considerado verdade/True *)

let is_false = function
  | Vlist [||] | Vnone | Vint 0 | Vstring "" | Vbool false -> true
  | _ -> false

let is_true v = not (is_false v)

(* Só consideramos aqui as funções globais *)

let functions = (Hashtbl.create 16 : (string, ident list * stmt) Hashtbl.t)

(* A instrução 'return' de Python é interpretada com a ajuda de uma excepção *)

exception Return of value

(* As variáveis locais (parâmetros de funções e variáveis introduzidos
   pelas atribuições) são arquivadas numa tabela de dispersão (hashtable)
   passada como argumento nas funções seguintes com o nome 'ctx' *)

type ctx = (string, value) Hashtbl.t

let rec compare_list a1 n1 a2 n2 i =
  if i = n1 && i = n2 then 0
  else if i = n1 then -1
  else if i = n2 then 1
  else
    let c = compare_value a1.(i) a2.(i) in
    if c <> 0 then c else compare_list a1 n1 a2 n2 (i + 1)

and compare_value v1 v2 =
  match (v1, v2) with
  | Vlist a1, Vlist a2 ->
      compare_list a1 (Array.length a1) a2 (Array.length a2) 0
  | Vbool b1, Vint _ -> compare_value (Vint (if b1 then 1 else 0)) v2
  | Vint _, Vbool b2 -> compare_value v1 (Vint (if b2 then 1 else 0))
  | _ -> compare v1 v2

let binop op v1 v2 =
  match (op, v1, v2) with
  | Badd, Vint n1, Vint n2 -> Vint (n1 + n2)
  | Badd, Vstring s1, Vstring s2 -> Vstring (s1 ^ s2)
  | Badd, Vlist l1, Vlist l2 -> Vlist (Array.append l1 l2)
  | Bsub, Vint n1, Vint n2 -> Vint (n1 - n2)
  | Bmul, Vint n1, Vint n2 -> Vint (n1 * n2)
  | (Bdiv | Bmod), Vint _, Vint 0 -> error "division by zero"
  | Bdiv, Vint n1, Vint n2 -> Vint (n1 / n2)
  | Bmod, Vint n1, Vint n2 -> Vint (n1 mod n2)
  | Beq, _, _ -> Vbool (compare_value v1 v2 = 0)
  | Bneq, _, _ -> Vbool (compare_value v1 v2 <> 0)
  | Blt, _, _ -> Vbool (compare_value v1 v2 < 0)
  | Ble, _, _ -> Vbool (compare_value v1 v2 <= 0)
  | Bgt, _, _ -> Vbool (compare_value v1 v2 > 0)
  | Bge, _, _ -> Vbool (compare_value v1 v2 >= 0)
  | _, _, _ -> error "unsupported operand types"

(* Interpretação de uma expressão (retorna um valor) *)

let rec expr ctx = function
  | Ecst Cnone -> Vnone
  | Ecst (Cstring s) -> Vstring s
  (* aritmética *)
  | Ecst (Cint n) -> Vint n
  | Ebinop
      ( ((Badd | Bsub | Bmul | Bdiv | Bmod | Beq | Bneq | Blt | Ble | Bgt | Bge)
        as op),
        e1,
        e2 ) ->
      let v1 = expr ctx e1 in
      let v2 = expr ctx e2 in
      binop op v1 v2
  | Eunop (Uneg, e1) -> (
      match expr ctx e1 with
      | Vint n -> Vint (-n)
      | _ -> error "unsupported operand types")
  (* boolean *)
  | Ecst (Cbool b) -> Vbool b
  | Ebinop (op, e1, e2) -> (
      match (op, expr ctx e1) with
      | Band, Vbool e1 ->
          if e1 then
            match expr ctx e2 with
            | Vbool e2 -> Vbool e2
            | _ -> error "Not bool expressions"
          else Vbool false
      | Bor, Vbool e1 -> (
          if e1 then Vbool true
          else
            match expr ctx e2 with
            | Vbool e2 -> Vbool e2
            | _ -> error "Not bool expressions")
      | _ -> error "unsupported operand types")
  | Eunop (Unot, e1) -> (
      match expr ctx e1 with
      | Vbool e1 -> Vbool (not e1)
      | _ -> error "unsupported operand types")
  | Eident id -> ( try Hashtbl.find ctx id with _ -> error "Id not found")
  (* chamadas de função *)
  | Ecall ("len", [ e1 ]) -> (
      match expr ctx e1 with
      | Vstring s -> Vint (String.length s)
      | Vlist e1 -> Vint (Array.length e1)
      | _ -> error "this value has no 'len'")
  | Ecall ("list", [ Ecall ("range", [ e1 ]) ]) -> (
      match expr ctx e1 with
      | Vint n -> Vlist (Array.init n (fun i -> Vint i))
      | _ -> error "Int was expected")
  | Ecall (f, el) -> (
      try
        let args, body = Hashtbl.find functions f in
        if List.length args <> List.length el then
          error "Bad number of arguments";
        let ctx' = Hashtbl.copy ctx in
        List.iter2 (fun a b -> Hashtbl.add ctx' a (expr ctx b)) args el;
        try
          stmt ctx' body;
          Vnone
        with Return n -> n
      with Not_found -> error "Unbound function")
  | Elist el -> Vlist (List.map (expr ctx) el |> Array.of_list)
  | Eget (e1, e2) -> (
      match (expr ctx e1, expr ctx e2) with
      | Vlist l1, Vint n -> (
          try l1.(n) with Invalid_argument _ -> error "index out of bounds")
      | _ -> error "Not valid")

(* interpretação de uma instrução ; não retorna nada *)

and stmt ctx = function
  | Seval e -> ignore (expr ctx e)
  | Sprint e ->
      print_value (expr ctx e);
      printf "@."
  | Sblock bl -> block ctx bl
  | Sif (e, s1, s2) -> if is_true (expr ctx e) then stmt ctx s1 else stmt ctx s2
  | Sassign (id, e1) -> Hashtbl.add ctx id (expr ctx e1)
  | Sreturn e -> raise (Return (expr ctx e))
  | Sfor (x, e, s) -> (
      match expr ctx e with
      | Vlist l1 ->
          Array.iter
            (fun a ->
              Hashtbl.replace ctx x a;
              stmt ctx s)
            l1
      | _ -> error "Expressions should be a list")
  | Sset (e1, e2, e3) -> (
      match (expr ctx e1, expr ctx e2) with
      | Vlist l, Vint n -> l.(n) <- expr ctx e3
      | _ -> error "list expected")

(* interpretação de um bloco i.e. de uma sequência de instruções *)

and block ctx = function
  | [] -> ()
  | s :: sl ->
      stmt ctx s;
      block ctx sl

(* interpretação de um ficheiro
   - dl é uma lista de definições de funcões (ver  Ast.def)
   - s é uma instrução que representa as instruções globais
*)

let file (dl, s) =
  List.iter (fun (f, args, body) -> Hashtbl.add functions f (args, body)) dl;
  stmt (Hashtbl.create 16) s
