open Stdlib

let _  = Random.self_init ()

type term =
  | Constant of string
  | Variable of string
  | Function of string * term list

type head = term
type body = term list

type clause = Fact of head | Rule of head * body

type program = clause list

type goal = term list

let rec string_of_f_list f tl =
  let _, s = List.fold_left (fun (first, s) t ->
    let prefix = if first then "" else s ^ ", " in
    false, prefix ^ (f t)) (true,"") tl
  in
  s

let rec string_of_term t =
  match t with
  | Constant c -> c
  | Variable v -> v
  | Function (f,tl) ->
      f ^ "(" ^ (string_of_f_list string_of_term tl) ^ ")"

let string_of_term_list fl =
  string_of_f_list string_of_term fl

let string_of_goal g =
  "?- " ^ (string_of_term_list g)

let string_of_clause c =
  match c with
  | Fact f -> string_of_term f ^ "."
  | Rule (h,b) -> string_of_term h ^ " :- " ^ (string_of_term_list b) ^ "."

let string_of_program p =
  let rec loop p acc =
    match p with
    | [] -> acc
    | [c] -> acc ^ (string_of_clause c)
    | c::t ->  loop t (acc ^ (string_of_clause c) ^ "\n")
  in loop p ""

let var v = Variable v
let const c = Constant c
let func f l = Function (f,l)
let fact f = Fact f
let rule h b = Rule (h,b)

(* Problem 1 *)

let rec occurs_check v t =
  match t with
  |Constant _ -> false
  |Function (_,y)-> let rec occ p q =
                        match q with 
                        []->false
                        |h::t->if occurs_check v h then true else occ p t
                      in occ v y
  |x -> if x = v then true else false
let _ = assert (occurs_check (var "X") (var "X"))
let _ = assert (not (occurs_check (var "X") (var "Y")))
let _ = assert (occurs_check (var "X") (func "f" [var "X"]))
let _ = assert (occurs_check (var "E") (func "cons" [const "a"; const "b"; var "E"]))
(* Problem 2 *)

module VarSet = Set.Make(struct type t = term let compare = Stdlib.compare end)
(* API Docs for Set : https://caml.inria.fr/pub/docs/manual-ocaml/libref/Set.S.html *)

let rec variables_of_term t =
  match t with
  |Constant _ -> VarSet.empty
  |Function (_,y)->let rec vari p =
                      match p with
                      []->VarSet.empty
                      |h::t->(VarSet.union (variables_of_term h) (vari t))
                    in vari y
  |x->VarSet.singleton x
let _ = assert (VarSet.equal (variables_of_term (func "f" [var "X"; var "Y"; const "a"]))
          (VarSet.of_list [var "X"; var "Y"]))
let rec varia p =
  match p with
  []->VarSet.empty
  |h::t->(VarSet.union (variables_of_term h) (varia t))
let variables_of_clause c =
  match c with
  |Fact x->variables_of_term x
  |Rule (h,b)->VarSet.union(variables_of_term h) (varia b)
let _ = assert (VarSet.equal (variables_of_clause (fact (func "p" [var "X"; var "Y"; const "a"])))
          (VarSet.of_list [var "X"; var "Y"]))
(* The variables in a Prolog rule p (X, Y, a) :- q (a, b, a) is [X; Y] *)
let _ = assert (VarSet.equal (variables_of_clause (rule (func "p" [var "X"; var "Y"; const "a"]) 
          [func "q" [const "a"; const "b"; const "a"]]))
          (VarSet.of_list [var "X"; var "Y"]))
(* Problem 3 *)

module Substitution = Map.Make(struct type t = term let compare = Stdlib.compare end)
(* See API docs for OCaml Map: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.S.html *)

let string_of_substitution s =
  "{" ^ (
    Substitution.fold (
      fun v t s ->
        match v with
        | Variable v -> s ^ "; " ^ v ^ " -> " ^ (string_of_term t)
        | Constant _ -> assert false (* substitution maps a variable to a term *)
        | Function _ -> assert false (* substitution maps a variable to a term *)
    ) s ""
  ) ^ "}"

let rec substitute_in_term s t =
  match t with
  |Function (name,y)->
    let rec subs s y= 
      match y with
      []->[]
      |h::tl-> (substitute_in_term s h)::(subs s tl)
    in
    Function (name,subs s y)
  |Constant _->t
  |_-> let p = Substitution.find_opt t s in
              match p with 
                  |None->t
                  |Some q->q          
let s =
  Substitution.add (var "Y") (const "0") (Substitution.add (var "X") (var "Y") Substitution.empty)
let _ = assert (substitute_in_term s (func "f" [var "X"; var "Y"; const "a"]) =
        func "f" [var "Y"; const "0"; const "a"])
let rec subst s l =
  match l with
  []->[]
  |h::t->(substitute_in_term s h)::(subst s t)
let substitute_in_clause s c =
  match c with
  |Fact x->Fact (substitute_in_term s x)
  |Rule (h,b)->Rule (substitute_in_term s h,subst s b)
let _ = assert (substitute_in_clause s (fact (func "p" [var "X"; var "Y"; const "a"])) =
        (fact (func "p" [var "Y"; const "0"; const "a"])))
(* Given a Prolog rule, p (X, Y, a) :- q (a, b, a), after doing substitution [Y/0, X/Y], 
                we have p (Y, 0, a) :- q (a, b, a) *)
let _ = assert (substitute_in_clause s (rule (func "p" [var "X"; var "Y"; const "a"]) [func "q" [const "a"; const "b"; const "a"]]) =
        (rule (func "p" [var "Y"; const "0"; const "a"]) [func "q" [const "a"; const "b"; const "a"]]))
(* Problem 4 *)

let counter = ref 0
let fresh () =
  let c = !counter in
  counter := !counter + 1;
  Variable ("_G" ^ string_of_int c)

let freshen c =
  let vars = variables_of_clause c in
  let s = VarSet.fold (fun v s -> Substitution.add v (fresh()) s) vars Substitution.empty in
  substitute_in_clause s c

(*
let c = (rule (func "p" [var "X"; var "Y"; const "a"]) [func "q" [var "X"; const "b"; const "a"]])
let _ = print_endline (string_of_clause c)
let _ = print_endline (string_of_clause (freshen c))
*)

exception Not_unifiable

let unify t1 t2 =
  match t1 with
  |Constant x -> 
     (match t2 with
      |Constant y -> if t1 = t2 then Substitution.empty else raise Not_unifiable
      |Variable y -> (Substitution.singleton (var y) (const x))
      |Function _ -> raise Not_unifiable)
  |Variable x -> 
    (match t2 with
      |Constant y -> Substitution.singleton t1 t2
      |Variable y -> if x = y then Substitution.empty else (Substitution.singleton (var y) (var x))
      |Function _ -> Substitution.singleton t1 t2)
  |Function (n1, b1) -> 
    (match t2 with
      |Constant _ -> raise Not_unifiable
      |Variable _ -> Substitution.singleton t1 t2
      |Function (n2, b2) -> if n1 = n2 then
         let rec bodycheck b1 b2 = 
          List.fold_left2 (fun s a b -> 
                            match a with
                            |Constant ac ->
                              (match b with
                                | Constant bc -> if ac = bc then s else raise Not_unifiable
                                | _ -> raise Not_unifiable
                              )
                            |Variable _ -> let a' = substitute_in_term s a in
                              let b' = substitute_in_term s b in
                              (match a' with
                                | Variable _ -> Substitution.add a' b' (Substitution.map (fun x -> if x = a' then b' else x) s)
                                | _ -> 
                                  (match b' with
                                    | Variable _ -> Substitution.add b' a' (Substitution.map (fun x -> if x = b' then a' else x) s)
                                    | _ -> raise Not_unifiable
                                  )
                              )
                            |Function _ ->
                              (match b with
                                | Function _ -> if a = b then s else raise Not_unifiable
                                | _ -> raise Not_unifiable
                              )
                            ) (Substitution.empty) b1 b2
       in
       bodycheck b1 b2
       else raise Not_unifiable
    )

(* Problem 5 *)
let rec deletList l tar =
  match l with
  |[]->[]
  |h::t->if h = tar then t else h::deletList t tar

let rec replace unifier goal =
        match goal with
        |[]->[]
        |h::t->(substitute_in_term unifier h)::replace unifier t


    let nondet_query program goal =
  let rec resulvent program goal =
  match goal with
  |[] -> []
  |hd::tl -> 
  let rprog = List.nth program (Random.int (List.length program)) in
  match rprog with
    |Fact f -> (try [(substitute_in_term (unify f hd) f)]
      with Not_unifiable -> resulvent program []
      )
    |Rule (h, b) -> resulvent program []
in
resulvent program goal
  (*let rec inner resolvent program goal=
      let resolvent = goal in
      let randR = List.nth resolvent (Random.int (List.length resolvent)) in
      let randP = List.nth program (Random.int (List.length program)) in
      let randP = freshen randP in
      
      match randP with 
      |Fact f->  let unifier = (try (unify f randR) 
    with Not_unifiable -> (inner resolvent program goal) ) in
      let resolvent = deletList resolvent randR in
      let resolvent = replace unifier resolvent in
      let goal = replace unifier goal in
      (match resolvent with
      |[]->goal
      |_->inner resolvent program goal)
      |Rule (h,b)-> let unifier = (try (unify h randR)
    with Not_unifiable -> (inner resolvent program goal) ) in
      let resolvent = deletList resolvent randR in
      let resolvent = resolvent@b in
      let resolvent = replace unifier resolvent in
      let goal = replace unifier goal in
      (match resolvent with
      |[] -> goal
      |_ -> inner resolvent program goal)
    in
    let goal = inner resolvent program in goal
  
  *)


    
(*
let nondet_query program goal =
  let resolvent = goal in
  let rec loop resolvent program =
    let rec randomChoose resolvent program goal =
    let randP = List.nth program (Random.int (List.length program)) in
    let randP = freshen randP in
    let randR = List.nth resolvent (Random.int (List.length resolvent)) in
    match randP with 
     |Fact f-> let unifier = (try (unify f randR) 
  with Not_unifiable -> (randomChoose resolvent program goal) )
     in 
    let resolvent' = deletList resolvent randR in
    let goal' = replace unifier goal in
    let resolvent'' = replace unifier resolvent'
    |Rule (h,b)-> let unifier = (try (unify h randR)
  with Not_unifiable -> (randomChoose resolvent program goal) )
  in
    let resolvent = deletList resolvent randR in
    let goal = replace unifier goal in
    let resolvent = resolvent@b in 
    let resolvent = replace unifier resolvent in randomChoose resolvent program goal in
  *)  
    

    



(* Problem Bonus *)

let det_query program goal =
  raise (Failure "Problem Bonus Not implemented")
