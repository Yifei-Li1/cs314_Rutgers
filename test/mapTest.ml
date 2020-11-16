let f x = x + 1
let rec map f l =
	match l with
	[]->[]
	|h::t->f h::map f t

let res = map f [1;2;3;4;5]
let rec printlist l =
	match l with
	[]->()
	|h::t->Printf.printf("%d") h;printlist t
;;
printlist res