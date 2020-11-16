type expr = 
  | Var of string
  | Lam of string * expr
  | App of expr * expr
let mem e l =
  List.fold_left( fun b x -> b || x = e) false l
let _ = assert (mem "b" ["a";"b";"c"] = true)
let _ = assert (mem "x" ["a";"b";"c"] = false)



let remove e l =
  (* YOUR CODE HERE *)
  List.fold_right (fun b a -> if e = b then a else b::a) l []
let _ = assert (remove "b" ["a";"b";"c"] = ["a";"c"])
let _ = assert (remove "x" ["a";"b";"c"] = ["a";"b";"c"])


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
let _ = assert (union ["a"; "c"; "b"] ["d"; "b"; "x"; "a"] = ["a"; "b"; "c"; "d"; "x"])


let add e l =
  (* YOUR CODE HERE *)
  let temp = List.sort String.compare l in
  let plux x y=
  match y with
  []->[x]
  |h::t->x::h::t
  in remove_stutter(List.sort String.compare (plux e temp))

let _ = assert (add "b" ["a";"c"] = ["a";"b";"c"])
let _ = assert (add "a" ["c"; "a"] = ["a";"c"])




let rec free_variables e =
  (* YOUR CODE HERE *)
  match e with
 Var x -> [x]
 | App (e1,e2) -> union(free_variables e1)(free_variables e2)
 | Lam (x,e0) ->
 remove x (free_variables e0)
let _ = assert (free_variables (parse_string "\\x.x") = []);
assert (free_variables (parse_string "\\x.y") = ["y"])



let rec substitute expr a b =
  (* YOUR CODE HERE *)
 match expr with
 Var x ->
 if a = x then b (* substitute *)
 else expr (* don’t subst *)
 | App (e1,e2) ->
 App (substitute e1 a b, substitute e2 a b)
 | Lam (x,e0) ->let rec substitute expr a b = match expr with
 | Lam (x,e0) ->
 if a = x then expr
 else if not (mem x (fvs b)) then
 Lam (x, substitute e0 a b)
 else
 let z = newvar() in (* fresh *)
 let e0' = substitute e0 x (Var z) in
 Lam (z,substitute e0' a b)



let rec reduce_cbv e =
  (* YOUR CODE HERE *)
   match e with
   App (Lam (x,e), e2) -> (substitute e x e2,true)
   | App (e1,e2) ->
   let e1' = reduce_cbv e1 in
   if e1' != e1 then (App(e1',e2),true)
   else App (e1,reduce_cbv e2)
   | Lam (x,e) -> Lam (x, reduce_cbv e)
   | _ -> (e,false)

