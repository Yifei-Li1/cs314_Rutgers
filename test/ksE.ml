let test = [1;2;3;4;5;6];;
let rec k's k l = 
	match l with
	[]->
	|h::t->if k = 1 then h else k's (k-1) t
;;
let res = k's 5 test;;
print_int res;;
