let test = [1;2;3;4;5;6;7]
let rec findLength a acc =
	match a with
	[]->acc
	|h::t->findLength t (acc+1)
;;
let res = findLength test 0;;
print_int res;;