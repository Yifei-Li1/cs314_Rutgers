let test = [1;2;3;4;5;6];;
let rec last2 a =
	match a with
	[]->None
	|[x1;x2]->Some [x1;x2]
	|h::t->last2 t
;;
let res = last2 test;;
let rec convert a =
	match a with
	None->[]
	|Some(h::t)->[h]@convert Some t
;;
let res' = convert res;;
(*let rec printlist l =
	match l with
	[]->[]
	|h::t->print_int h;printlist t
;;
printlist res;;
*)