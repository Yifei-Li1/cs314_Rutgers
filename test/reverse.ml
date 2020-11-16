let test =[1;2;3;4;5;6];;
let rec reverse a =
	match a with
	[]->[]
	|h::t->reverse t @[h]
;;
let res = reverse test;;
let rec printlist l=
	match l with
	[]->[]
	|h::t->print_int h;printlist t
;;
printlist res;;