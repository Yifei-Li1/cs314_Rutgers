type 'a element = {
  content : 'a;
  mutable next : 'a element option;
  mutable prev : 'a element option
}
let create () = ref None
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
