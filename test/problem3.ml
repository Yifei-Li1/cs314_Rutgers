
let rec remove_stutter l =
let rec remove l = 
  match l with
  |a::(b:: _ as c) -> if a = b then remove c else a::remove c
  |smaller -> smaller
in remove l 

let union l1 l2 =
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

let rec printlist l =
	match l with
	[]->()
	|h::t->print_string h;printlist t
;;
let _ = assert (union ["a"; "c"; "b"] ["d"; "b"; "x"; "a"] = ["a"; "b"; "c"; "d"; "x"])
let add e l = 
  let temp = List.sort String.compare l in
  let plux x y=
  match y with
  []->[x]
  |h::t->x::h::t
  in remove_stutter(List.sort String.compare (plux e temp))
;;
let _ = assert (add "b" ["a";"c"] = ["a";"b";"c"])
let _ = assert (add "a" ["c"; "a"] = ["a";"c"])