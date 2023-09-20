type command = Int of int | Plus | Minus | Times | Divide
             | Dup | Drop | Over | Rot | Swap

let lex (c : string) =
  let get_token = function
    | "+" -> Plus
    | "-" -> Minus
    | "*" -> Times
    | "/" -> Divide
    | "dup" -> Dup
    | "drop" -> Drop
    | "over" -> Over
    | "rot" -> Rot
    | "swap" -> Swap
    | c -> match int_of_string_opt c with
    | Some i -> Int i
    | None -> failwith ("Unexpected token " ^ c)
  in
  List.map get_token (String.split_on_char ' ' c)

let interpret (p : command list) : int list =
  let process (s : int list) = function
    | Int (i) -> i :: s
    | Plus -> (match s with a :: b :: t -> (b + a) :: t | _ -> failwith "too few elements on stack")
    | Minus -> (match s with a :: b :: t -> (b - a) :: t | _ -> failwith "too few elements on stack")
    | Times -> (match s with a :: b :: t -> (a * b) :: t | _ -> failwith "too few elements on stack")
    | Divide -> (match s with a :: b :: t -> (b / a) :: t | _ -> failwith "too few elements on stack")
    | Dup -> (match s with a :: t -> a :: a :: t | _ -> failwith "stack is empty")
    | Drop -> (match s with a :: t -> t | _ -> failwith "stack is empty")
    | Over -> (match s with a :: b :: t -> b :: a :: b :: t | _ -> failwith "need 2 els on stack")
    | Rot -> (match s with a :: b :: c :: t -> c :: a :: b :: t | _ -> failwith "need 3 els on stack")
    | Swap -> (match s with a :: b :: t -> b :: a :: t | _ -> failwith "need 2 els on stack")
  in
  List.fold_left process [] p