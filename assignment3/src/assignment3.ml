(******************** Problem 1 ********************)

type 'a element = {
  content : 'a;
  mutable next : 'a element option;
  mutable prev : 'a element option
}

let create () = ref None

let is_empty t = !t = None

let insert_first l c =
  let n = {content = c; next = !l; prev = None} in
  let _ = match !l with
    | Some o -> (o.prev <- Some n)
    | None -> () in
  let _ = (l := Some n) in
  n

let insert_after n c =
  let n' = {content = c; next = n.next; prev = Some n} in
  let _ =  match n.next with
    | Some o -> (o.prev <- (Some n'))
    | None -> () in
  let _ = (n.next <- (Some n')) in
  n'

let remove t elt =
  let prev, next = elt.prev, elt.next in
  let _ = match prev with
    | Some prev -> (prev.next <- next)
    | None -> t := next in
  let _ =  match next with
    | Some next -> (next.prev <- prev)
    | None -> () in
  let _ = (elt.prev <- None) in
  let _ = (elt.next <- None) in
  ()      (* return void *)

let iter t f =
  let rec loop node =
    match node with
      | None -> ()
      | Some el ->
        let next = el.next in
        let _ = f el in
        loop (next)
  in
  loop !t

let dll_of_list l =
let dll = create() in
match l with
[]->dll
|x::xs->let n = insert_first dll x in
let rec loop l n=
match l with
|[] -> dll
|h::t -> 
let n = insert_after n h
in loop t n
in loop xs n

let list_of_dll l =
  let s = ref [] in
   let _ = iter l (fun e -> s:=!s@[e.content])in !s

let length l =
  let i = ref 0 in
 let _ = iter l (fun e-> i:= !i+1) in !i
let rec dup l1 =
  match l1 with
  []->l1
  |h::t->h::h::dup t
let duplicate l =
  let l1 = list_of_dll l in
 let res = dll_of_list (dup l1) in
 l:=!res
let rec rev l1 =
  match l1 with
  []->l1
  |h::t->(rev t)@[h]
let reverse l =
  let l1 = list_of_dll l in
  let res = dll_of_list (rev l1) in
  l:=!res

(******************** Problem 2 ********************)

module type Serializable = sig
  type t
  type content

  val string_of_t : t -> string

  val fold : ('a -> content -> 'a) -> 'a -> t -> 'a
end

module SerializableList (C : Serializable) = struct
  type t = C.t list
  type content = C.t

  let string_of_t l =
    let rec loop acc l = match l with
      | [] -> acc
      | [x] -> acc ^ (C.string_of_t x)
      | x::xs -> loop (acc ^ (C.string_of_t x) ^ ";") xs
    in
    "[" ^ (loop "" l) ^ "]" 

  let  fold f accum l = 
    List.fold_left(fun accum x -> C.fold f accum x) accum l
end

module SerializableArray (C : Serializable) = struct
  type t = C.t array
  type content = C.t

  let string_of_t l =
    let length = ref (Array.length l) in
    let acc = ref "" in
    if !length = 1 then acc:= !acc^(C.string_of_t l.(0))
  else
    for i = 0 to !length - 1 do
      if i = !length - 1 then acc := !acc ^ (C.string_of_t l.(i))
    else
      acc := !acc ^ (C.string_of_t l.(i)) ^ ";"
    done;
    "[|" ^ !acc ^ "|]"

  let fold f accum l =
     Array.fold_left(fun accum x -> C.fold f accum x) accum l
end

module SerializableIntArray = SerializableArray (struct
  type t = int
  type content = int

  let string_of_t x = string_of_int x

  let fold f i res = f i res
end)

module SerializableIntList = SerializableList (struct
  type t = int
  type content = int

  let string_of_t x = string_of_int x

  let fold f i res = f i res
end)

module SerializableIntArrayArray = SerializableArray(SerializableIntArray)

module SerializableIntListArray = SerializableArray(SerializableIntList)

module SerializableIntArrayList = SerializableList(SerializableIntArray)
let _ = assert (SerializableIntList.fold (+) 0 [1;2;3] = 6) 
let _ = assert (SerializableIntList.fold (fun x y -> y::x) [] [3;2;1] = [1;2;3])
let string1 = SerializableIntArray.string_of_t [|1;2;3|]
let _ = Printf.printf("%s") string1