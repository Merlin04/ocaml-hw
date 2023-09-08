type 'a my_option =
  | None
  | Some of 'a

(* Write a function lookup : ('a * 'b) list -> 'a -> 'b my_option which takes as input a list of pairs (of type 'a * 'b) and a key of type 'a. If the given key is the first element of any of the pairs in the list, return the second element of that pair (the "value"). If none of the pairs match the given key, return nothing. (This is the same behavior as List.assoc_opt from OCaml's standard library. You can use that function for testing but please implement lookup without it.) *)

let rec lookup (l : ('a * 'b) list) (i : 'a) : 'b my_option = match l with
  | (a, b) :: _ when a = i -> Some (b)
  | _ :: t -> lookup t i
  | _ -> None


(* Write a type 'a tree which represents trees where each node may have any number of children. *)

type 'a tree = Node of 'a * ('a tree list) | Empty

(* Write a function size : 'a tree -> int which finds the total number of nodes in the tree. *)

let rec size (t : 'a tree) : int = match t with
  | Empty -> 0
  | Node (_, c) -> List.fold_left (fun acc cur -> acc + size cur) 1 c

type 'a btree = EmptyTree | Branch of ('a btree) * 'a * ('a btree)

(* Now write a function fold which accepts a tree with values of type 'a, a function of type 'b -> 'a -> 'b, and a starting value of type 'b. Much like we saw with lists, your fold function should use the given function to process each element of the tree to produce a single value of type 'b as a result. For example, if tr is a tree of integers, then
fold (fun x y -> x + y) 0 tr
should compute the sum of the values held in all nodes of the tree. Note that there are a variety of ways to write fold by changing the order in which nodes are processed. Any solution is fine for this exercise.
For an extra challenge, make sure your fold function visits the tree nodes using an in-order traversal. *)

let rec fold (t : 'a btree) (f : 'b -> 'a -> 'b) (i : 'b) : 'b = match t with
  | EmptyTree -> i
  | Branch (l, v, r) -> (*fold r f (fold l f (f i v))*) fold r f (f (fold l f i) v)

(* Write a data type str_concat to represent strings as concatenations of substrings. That is, a str_concat value is either empty, a base string, or a concatenation of two other str_concat values. For this exercise, we'll have a base string for each character, using OCaml's char datatype. *)

type str_concat = Empty | Base of char | Concat of str_concat * str_concat

(* Now write a function build_str which takes a str_concat and returns the string that object represents. You can use Char.escaped to convert a character into a single-character string, and the operator ^ to concatenate strings. *)

let rec build_str = function
  | Empty -> ""
  | Base (c) -> Char.escaped c
  | Concat (a, b) -> (build_str a) ^ (build_str b)

(* Now, generalize your str_concat type to a datatype 'a monoid_comp. This type represents a computation in a monoid, a mathematical structure consisting of a set of values, an identity element within that set, and an associative operator for combining values. In our strings setting, the set of values is the set of strings, the identity element is the empty string, and the combination operator is concatenation. Our 'a monoid_comp type will generalize this to an arbitrary type 'a. Concretely, a value of type 'a monoid_comp may be either an identity element (analagous to the empty string), a base value of type 'a, or a combination of two other values of type 'a monoid_comp. *)

type 'a monoid_comp = Id | Base of 'a | Comb of 'a monoid_comp * 'a monoid_comp

(* Similarly, generalize your build_str function to a new function run_monoid. Because we don't know in advance what our identity element or combination operator will be (since they depend on 'a), this function will take those two elements as input. Specifically,
run_monoid : 'a monoid_comp -> 'a -> ('a -> 'a -> 'a) -> 'a
The first argument is the computation, the second is the identity element, and the third is the combination operator. For example, you might represent a tree of additions using a value m : int monoid_comp, in which case run_monoid m 0 (fun x y -> x + y) would find the sum represented by that computation. You could also represent the string concatenation type as a string monoid_comp and then run it with run_monoid m "" (fun x y -> x ^ y). *)

let rec run_monoid (m : 'a monoid_comp) (id : 'a) (c : ('a -> 'a -> 'a)) : 'a = match m with
  | Id -> id
  | Base (a) -> a
  | Comb (a, b) -> c (run_monoid a id c) (run_monoid b id c)