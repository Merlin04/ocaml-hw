(* Write a function fib which takes an argument n and returns the nth Fibonacci number (recall that the Fibonacci numbers are defined as F[0] = 0, F[1] = 1, and F[n] = F[n-1] + F[n-2] for n > 1).
If you haven't already, add types to your Fibonacci code to specify that it takes an integer argument and returns an integer result. *)

let rec fib (n : int) : int =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib (n - 1) + fib (n - 2)

(* Write a function flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c. This function should take another function f as an argument, then take two arguments for that function and apply f to those arguments in the reverse order. *)

let flip (f : 'a -> 'b -> 'c) : ('b -> 'a -> 'c) =
  fun b a -> f a b

(* Write a function nth which takes a list lst and an integer i and returns the element at index i in lst (assume the first element is at index 0). *)

let rec nth (lst : 'a list) (i : int) : int =
  match lst with
  | l :: l' -> if i = 0 then l else nth l' (i - 1)
  | _ -> failwith "List is empty"

(* Write a function sum which returns the sum of a list of integers. *)

let rec sum (l : int list) : int =
  match l with
  | [] -> 0
  | [h] -> h
  | h :: t -> h + sum t

(* Write a function reverse which reverses the order of elements of a list. Hint: This is easier if you define a helper function. *)

(* Write a function append which takes two lists and puts one at the end of the other (e.g., append [1; 2] [3; 4; 5] should return [1; 2; 3; 4; 5]). *)

(* Write a function flatten which takes a list of lists and removes one layer of nesting. For example flatten [[1; 2]; [3; 4]; [5]; []] should return [1; 2; 3; 4; 5]. Hint: you may want to use your append function from above. *)

(* Write a function map : ('a -> 'b) -> 'a list -> 'b list which takes a function and a list and applies the given function to each element of the list. (Mapping is super useful, we'll be using it a lot later in the semester.) *)

(* Write a function fold : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a. Folding is a generalized way of combining the elements of a list using a given function. Specifically, we want to have
fold f a [x1; x2; ...; xn] = f (... (f (f a x1) x2) ...) xn
For example, if add is a function that adds two integers, then fold add 0 lst is equivalent to sum lst from above. *)

