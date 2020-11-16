let test = [];;
let rec last a =
	match a with
	|[]->None
	|[h]->Some h
	|h::t->last t
;;
let res = last test;;
let convert a =
	match a with
	|None -> 0
	|Some c -> c
;;
let res' = convert res;;
Printf.printf"%d\n" res';;