let c = 5 and d = 0;;
let div a b =
	match b with
	0->None
	|_->Some (a/b)
;;
let result = div c d;;
let trans input =
	match input with
	None -> 0
	|Some t ->t
;;
let result' = trans result;;
Printf.printf"%d\n" result';;