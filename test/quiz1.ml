type tree =
	|Leaf
	|Node of tree * int * tree

let sumtailrec t =
	let rec sum a l =
		match l with
		|Leaf->a
		|Node(z,v,y)->sum v z+sum a y
	in sum 0 t
(*
let rec sumtailrec t = match t with
	Leaf->0
	|Node(f,n,t)->n + sumtailrec f + sumtailrec t
;;*)
let _ = assert(sumtailrec (Node(Node(Leaf,4,Leaf),3,Node(Leaf,9,Leaf))) = 16)