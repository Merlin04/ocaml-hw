(* Define a data type day which represents a day of the week (Monday, Tuesday, etc.). Write a function weekday which takes a day and returns true if it's a weekday and false otherwise. *)

type day =
  | Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday

(*let weekday = function
  | Sunday | Saturday -> true
  | _ -> false*)

let weekday (d : day) = d = Sunday || d = Saturday

(* Write a data type int_option which represents an optional integer value. That is, a value of type int_option may either be an integer or be nothing. *)

type int_option = None | Some of int

(* Write a function default : int -> int_option -> int. This function should return the value held in the int_option if one is present, and otherwise should return the default value supplied as its first argument. *)

let default (d : int) = function
  | Some (i) -> i
  | None -> d

(* Write a function nth : int list -> int -> int_option which takes a list l and an integer n and returns the nth element of l. Assume the first element is at index 0. If n is at least the length of l, return nothing instead using your int_option type. *)

(* could just use [List.nth_opt] but I'm assuming I'm not supposed to *)
let rec nth (l : int list) (n : int) = match l with
  | [] -> None
  | h :: t -> if n = 0 then Some(h) else nth t (n - 1)

(* Write a datatype color which represents colors in either RGB or CMYK format.  An RGB color is defined by three floats between zero and one--one each for red, green, and blue. A CMYK color is defined by four floats between zero and one--one each for cyan, magenta, yellow, and key. You can assume the floats wil be between zero and one, so you don't need to write code to check whether that is true. *)

type color =
  | RGB of float * float * float
  | CMYK of float * float * float * float

(* Now write a function toggle_representation : color -> color which takes a color and returns the same color represented using the other representation scheme (that is, if the input is an RGB color, the output should be a CMYK color and vice versa). To convert from CMYK to RGB, the formulas are:
R = (1 - C) * (1 - K)
G = (1 - M) * (1 - K)
B = (1 - Y) * (1 - K)

and to convert from RGB to CMYK, the formulas are:

K = 1 - max(R, G, B)
C = (1 - R - K) / (1 - K)
M = (1 - G - K) / (1 - K)
Y = (1 - B - K) / (1 - K)
*)

let rec max_l = function
  | a :: b :: t -> max_l ((if a > b then a else b) :: t)
  | [a] -> a
  | _ -> failwith "empty array"

let toggle_representation = function
  | RGB (r, g, b) -> let k = 1. -. max_l([r; g; b]) in
      CMYK((1. -. r -. k) /. (1. -. k), (1. -. g -. k) /. (1. -. k), (1. -. b -. k) /. (1. -. k), k)
  | CMYK (c, m, y, k) -> RGB((1. -. c) *. (1. -. k), (1. -. m) *. (1. -. k), (1. -. y) *. (1. -. k))