type bstree =
  | Lf
  | Br of int * bstree * bstree

let rec insert (t : bstree) (i : int) : bstree =
  match t with
  | Lf -> Br (i, Lf, Lf)
  | Br (n, left, right) ->
    if i = n then t
    else if i < n then Br (n, insert left i, right)
    else Br (n, left, insert right i)

let insert_all (t : bstree) (is : int list) : bstree =
  List.fold_left insert t is

type arith =
  | Plus of arith * arith
  | Times of arith * arith
  | Negate of arith
  | Var of string
  | Num of int

(* NOTE: you may modify the function declarations as long as the names and
   types remain the same. For example, you might need to add "rec" in some
   declarations. *)

let remove_duplicates (lst : 'a list) : 'a list =
  List.fold_left (fun acc cur -> if List.mem cur acc then acc else acc @ [cur]) [] lst

(* alternate impl that doesn't insert to end of list - not sure which is more performant *)
let rec remove_duplicates_2 (lst : 'a list) : 'a list =
  match lst with
  | [] -> []
  | e :: a' -> e :: (a' |> List.filter (fun v -> v <> e) |> remove_duplicates_2)

let suffix_sum (lst : int list) : int list =
  List.fold_right (fun cur acc -> (cur + (match acc with [] -> 0 | e :: _ -> e)) :: acc) lst []

let prefix_sum (lst : int list) : int list =
  lst |> List.rev |> suffix_sum |> List.rev

let table (f : 'a -> 'b -> 'c) (xs : 'a list)
          (ys : 'b list) : ('a * 'b * 'c) list list =
  List.map (fun x -> List.map (fun y -> (x, y, f x y)) ys) xs

let rec binaries (n : int) : string list =
  match n with
  | 0 -> [""]
  | 1 -> ["0"; "1"]
  | _ -> let r = binaries (n - 1) in List.map (fun v -> "0" ^ v) r @ List.map (fun v -> "1" ^ v) r

(* helper util for below *)
let rec insert_node (t : bstree) (nd : bstree) : bstree =
  match nd with
  | Lf -> t
  | Br(ndn, ndl, ndr) ->
      match t with
      | Lf -> nd
      | Br (n, left, right) ->
        if n > ndn then Br(n, insert_node left nd, right)
        else Br(n, left, insert_node right nd)

let rec without (t : bstree) (v : int) : bstree =
  match t with
  | Lf -> Lf
  | Br (n, left, right) ->
    if v = n then insert_node left right
    else Br (n, without left v, without right v)

 let rec flatten (t : bstree) : int list =
  match t with
  | Lf -> []
  | Br (n, left, right) ->
    flatten left @ (n :: flatten right)

let rec free_vars' (acc : string list) = function
  | Num n -> acc
  | Var v -> if List.mem v acc then acc else v :: acc
  | Negate a -> free_vars' acc a
  | Times (a, b)
  | Plus (a, b) -> let fa = free_vars' acc a in free_vars' fa b

let free_vars = free_vars' []

let rec subst (id : string) (e : arith) (expr : arith) : arith =
  match expr with
  | Var v when v = id -> e
  | Negate a -> Negate (subst id e a)
  | Times (a, b) -> Times (subst id e a, subst id e b)
  | Plus (a, b) -> Plus (subst id e a, subst id e b)
  | _ -> expr

type prop =
  | And of prop * prop
  | Or of prop * prop
  | Not of prop
  | Sym of string

let rec pprint (p : prop) : string =
  let dyadic op a b = Printf.sprintf "(%s %s %s)" (pprint a) op (pprint b) in
  let monadic op a = Printf.sprintf "%s(%s)" op (pprint a) in
  match p with
  | And (a, b) -> dyadic "&&" a b
  | Or (a, b) -> dyadic "||" a b
  | Not a -> monadic "~" a
  | Sym a -> a
