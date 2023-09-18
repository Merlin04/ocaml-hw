type prop =
  | And of prop * prop
  | Or of prop * prop
  | Not of prop
  | True
  | False
  | Sym of string
  | Let of string * prop * prop

let rec interp (c : (string * bool) list) : prop -> bool = function
  | Let (s, e, p) -> interp ((s, interp c e) :: c) p
  | Sym a -> (match List.assoc_opt a c with Some v -> v | None -> failwith "variable doesn't exist")
  | And (a, b) -> (interp c a) && (interp c b)
  | Or (a, b) -> (interp c a) || (interp c b)
  | Not a -> a |> interp c |> not
  | True -> true
  | False -> false

let interpret_prog p = interp [] p
