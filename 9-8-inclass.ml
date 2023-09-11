(* Write a function factorial : int -> int which computes the factorial of its argument. *)

let rec fact (n : int) : int = if n = 0 then 1 else n * fact (n - 1)

(* Write a function power : int -> int -> int which raises its first argument to the power given by the second argument. *)

let power (a : int) (b : int) : int = ((float_of_int a) ** (float_of_int b)) |> int_of_float

type arith =
  | Plus of arith * arith
  | Times of arith * arith
  | Negate of arith
  | Num of int
  | If of comp * arith * arith
and comp =
  | Eq of arith * arith
  | Lt of arith * arith
  | Gt of arith * arith
  | Le of arith * arith
  | Ge of arith * arith

let rec eval_comp = function
  | Eq (a, b) -> (evaluate a) = (evaluate b)
  | Lt (a, b) -> (evaluate a) < (evaluate b)
  | Gt (a, b) -> (evaluate a) > (evaluate b)
  | Le (a, b) -> (evaluate a) <= (evaluate b)
  | Ge (a, b) -> (evaluate a) >= (evaluate b)
and evaluate (e : arith) : int =
  match e with
  | Plus (e1, e2) -> evaluate e1 + evaluate e2
  | Times (e1, e2) -> evaluate e1 * evaluate e2
  | Negate e1 -> - evaluate e1
  | Num i -> i
  | If (cond, a, b) -> if eval_comp cond then evaluate a else evaluate b

(* Write a function map : ('a -> 'b) -> 'a list -> 'b list which applies a given function to each element of a list. That is, *)
(* map f [x1; x2; ...; xn] = [f x1; f x2; ...; f xn] *)

let rec map (f : ('a -> 'b)) (l : 'a list) : 'b list = match l with
  | h :: t -> (f h) :: (map f t)
  | [] -> []

(* Write a function fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a which processes a list from left to right using a given function. Specifically,

fold_left f init [x1; x2; ...; xn] = f (... (f (f init x1) x2) ...) xn *)

let rec fold_left (f : ('a -> 'b -> 'a)) (init : 'a) = function
  | h :: t -> fold_left f (f init h) t
  | [] -> init

(* Write a function filter : ('a -> bool) -> 'a list -> 'a list which takes a function f and a list lst and returns a list which contains only the elements of lst for which f returns true. The order of elements in the returned list should be the same as in lst. For example, *)

let rec filter (f : 'a -> bool) = function
  | h :: t -> if (f h) then h :: (filter f t) else filter f t
  | [] -> []

(* Suppose we have a matrix represented as an int list list. Each int list element represents one row of the matrix. Write a function first_col which returns a list representing the first column of the matrix. That is, it should extract the first element from each sublist. For this function, you can assume none of the lists are empty. *)

let rec first_col = function
  | h :: t -> (List.nth h 0) :: first_col t
  | [] -> []

(* Write a function keys : ('a * 'b) list -> 'a list which takes a list of pairs and returns a list containing the first value in each pair. *)

let keys = map (fun (a, _) -> a)

(* Write a function rev : 'a list -> 'a list which reverses the order of elements in a list. *)

let rev = fold_left (fun a c -> c :: a) []

(* Write a function flatten : 'a list list -> 'a list which takes a list of lists and concatenates produces a single list containing the contents of each component list. For example *)

let flatten = fold_left (fun a c -> a @ c) []

(* Write a function get_values : ('a option) list -> 'a list which takes a list of options and extracts the values held in each option, ignoring any that are None. For example, *)

let get_values = fold_left (fun a c -> match c with Some n -> [n] @ a | None -> a) []