
open Syntax
let parse_string = Lambda_parse.parse_string
let string_of_expr = string_of_expr

let r = ref 0

let fresh s =
  let v = !r in
  r := !r + 1;
  s ^ (string_of_int v)

(* Your implementation begins from here *)

let mem e l =
  List.fold_left( fun b x -> b || x = e) false l
let _ = assert (mem "b" ["a";"b";"c"] = true)
let _ = assert (mem "x" ["a";"b";"c"] = false)



let remove e l =
  (* YOUR CODE HERE *)
  List.fold_right (fun b a -> if e = b then a else b::a) l []



let rec remove_stutter l =
let rec remove l = 
  match l with
  |a::(b:: _ as c) -> if a = b then remove c else a::remove c
  |smaller -> smaller
in remove l 
let union l1 l2 =
  (* YOUR CODE HERE *)
  let temp1 = List.sort String.compare l1 in
  let temp2 = List.sort String.compare l2 in
  let a = remove_stutter temp1 in
  let b = remove_stutter temp2 in
  let rec merge x y = 
  match x, y with
   | [], _ -> y
   | _, [] -> x
   | hx :: txs, hy :: tys ->
       if hx < hy then hx :: merge txs y else hy :: merge x tys
   in remove_stutter (merge a b)



let add e l =
  (* YOUR CODE HERE *)
  let temp = List.sort String.compare l in
  let plux x y=
  match y with
  []->[x]
  |h::t->x::h::t
  in remove_stutter(List.sort String.compare (plux e temp))






let rec free_variables e =
  (* YOUR CODE HERE *)
  match e with
 Var x -> [x]
 | App (e1,e2) -> union(free_variables e1)(free_variables e2)
 | Lam (x,e0) ->
 remove x (free_variables e0)




let rec substitute expr a b =
  (* YOUR CODE HERE *)
 match expr with
 Var x ->
 if a = x then b (* substitute *)
 else expr (* don’t subst *)
 | App (e1,e2) ->
 App (substitute e1 a b, substitute e2 a b)
 | Lam (x,e0) ->
 if a = x then expr
 else if not (mem x (free_variables b)) then
 Lam (x, substitute e0 a b)
 else
 let z = fresh "x" in (* fresh *)
 let e0' = substitute e0 x (Var z) in
 Lam (z,substitute e0' a b)



let rec reduce_cbv e =
  (* YOUR CODE HERE *)
   

raise (Failure "Problem 7 not implemented")

let rec reduce_cbn e =
  (* YOUR CODE HERE *)
  raise (Failure "Problem 8 not implemented")



let rec reduce_normal e =
  (* YOUR CODE HERE *)
  let rec help e = 
   match e with
   App (Lam (x,e), e2) -> substitute e x e2
   | App (e1,e2) ->
   let e1' = help e1 in
   if e1' != e1 then App(e1',e2)
   else App (e1,help e2)
   | Lam (x,e) -> Lam (x, help e)
   | _ -> e
 in help e if e = help e then (e,false) else (help e,true)



(* Your implementation done here *)

(* Debug your code by printing out evaluation results *)
let rec eval log depth reduce expr =
  if depth = 0 then failwith "non-termination?"
  else begin
    let expr', reduced = reduce expr in
    if not reduced then expr else begin
      if log then print_endline ("= " ^ (string_of_expr expr'));
      eval log (depth-1) reduce expr'
    end
  end
let eval_cbv = eval true 1000 reduce_cbv
let eval_cbn = eval true 1000 reduce_cbn
let eval_normal = eval true 1000 reduce_normal

(* To debug and observe the evaluation steps of your `reduce_cbv`, `reduce_cbn`
 * or `reduce_normal` implementation, use the following code.
 *
 *let _ = eval_cbv (parse_string "(\\x.x) ((\\x.x) (\\z.(\\x.x) z))")
 *let _ = print_endline ""
 *
 *let _ = eval_cbn (parse_string "(\\x.x) ((\\x.x) (\\z.(\\x.x) z))")
 *let _ = print_endline ""
 *
 *let _ = eval_normal (parse_string "(\\x.x) ((\\x.x) (\\z.(\\x.x) z))")
 *let _ = print_endline ""
 *)
