(*****************************************
 * Lexer and token type for the exercise *
 *****************************************)

exception LexError of string
exception ParseError of string

type tok =
  | Fun             (* keyword "fun" *)
  | Arrow           (* -> *)
  | LPar            (* ( *)
  | RPar            (* ) *)
  | Id of string    (* identifier *)

let lex (src : string) : tok list =
  let is_id_char c =
    ('a' <= c && c <= 'z') ||
    ('A' <= c && c <= 'z') ||
    ('0' <= c && c <= '9') || c = '_' in
  let consume_id i =
    let rec loop j =
      if j < String.length src && is_id_char src.[j] then loop (j + 1)
      else (String.sub src i (j - i), j) in
    loop i in
  let rec loop i =
    if i >= String.length src then []
    else match src.[i] with
      | '(' -> LPar :: loop (i + 1)
      | ')' -> RPar :: loop (i + 1)
      | '-' ->
        if src.[i+1] = '>' then Arrow :: loop (i + 2)
        else raise (LexError "Expected >")
      | c when List.mem c ['\n'; '\r'; '\t'; ' '] -> loop (i + 1)
      | c ->
        let (v, j) = consume_id i in
        if v = "fun" then Fun :: loop j
        else Id v :: loop j in
  loop 0

type fun_ast =
  | Function of string * fun_ast
  | Application of fun_ast * fun_ast
  | Var of string

type parser_res = (fun_ast * tok list)
type parser_fn = tok list -> parser_res option

let rec comb_parsers (ps : parser_fn list) (i : tok list) : parser_res option = match ps with
  | [] -> None
  | h :: t -> match h i with Some (s) -> Some (s) | None -> comb_parsers t i

let rec parse_opt_fun : parser_fn = function
  | Fun :: r ->
    (match r with Id (id) :: r2 ->
      (match r2 with Arrow :: r3 ->
        (match parse_expr r3 with
          | Some (e, r4) -> Some ((Function (id, e), r4))
          | None -> raise (ParseError "Expected expression in function body"))
      | _ -> raise (ParseError "Expected ->"))
    | _ -> raise (ParseError "Expected id"))
  | _ -> None

and parse_opt_parens : parser_fn = function
  | LPar :: r ->
    (match parse_expr r with
      | Some (e, r2) -> (match r2 with
        | RPar :: r3 -> Some ((e, r3))
        | _ -> raise (ParseError "Expected )"))
      | None -> raise (ParseError "Expected expression inside parens"))
  | _ -> None

and parse_opt_id : parser_fn = function
  | Id (id) :: r -> Some ((Var (id), r))
  | _ -> None

and parse_expr : parser_fn = fun source -> comb_parsers [parse_opt_fun; parse_appl_or_rest] source
and parse_simple : parser_fn = fun source -> comb_parsers [parse_opt_id; parse_opt_parens] source

and parse_appl_or_rest : parser_fn = fun source ->
  let parse_appl_part = comb_parsers [parse_opt_fun; parse_simple] in
  let rec try_add_appl_part ((a, r) : parser_res) : parser_res = match parse_appl_part r with
    | Some ((n, r2)) -> try_add_appl_part (Application (a, n), r2)
    | None -> (a, r) in
  match parse_simple source with
    | Some s -> Some (try_add_appl_part s)
    | None -> None

and parse (source : tok list) : fun_ast =
  match parse_expr source with Some((expr, [])) -> expr | Some s -> raise (ParseError "Unexpected token") | None -> raise (ParseError "Expected expression")


(*
<expr> ::= fun $id -> <expr> | <rest>
<rest> ::= <rest> <rest2> | <rest2>
<rest2 ::= $id | ( <expr> )
*)














(*************************************
 * Code from the information section *
 *************************************)

type token =
  | Plus
  | Dash
  | Star
  | LParen
  | RParen
  | Int of int



(***************************************************************************
 * NOTE: You don't need to understand this lexing code. I will always give *
 * you a lexer for homework assignments.                                   *
 ***************************************************************************)

let tokenize (src : string) : token list =
  let is_digit c = '0' <= c && c <= '9' in
  let consume_int i =
    let rec loop j =
      if j < String.length src && is_digit src.[j] then loop (j + 1)
      else (Int (int_of_string (String.sub src i (j - i))), j) in
    loop i in
  let rec loop i =
    if i >= String.length src then []
    else match src.[i] with
      | '+' -> Plus :: loop (i + 1)
      | '-' -> Dash :: loop (i + 1)
      | '*' -> Star :: loop (i + 1)
      | '(' -> LParen :: loop (i + 1)
      | ')' -> RParen :: loop (i + 1)
      | c when List.mem c ['\n'; '\r'; '\t'; ' '] -> loop (i + 1)
      | c when is_digit c -> let (v, j) = consume_int i in v :: loop j
      | c -> raise (LexError ("Unexpected char: " ^ Char.escaped c)) in
  loop 0

(*************
 * End Lexer *
 *************)



type arith =
  | Add of arith * arith
  | Sub of arith * arith
  | Mul of arith * arith
  | Const of int

module RightAssoc = struct

  (* <expr> ::= <term> + <expr> | <term> - <expr> | <term>
   * <term> ::= <fact> * <term> | <fact>
   * <fact> ::= $int | ( <expr> )
   *)

  let rec parse_expr (source : token list) : arith * token list =
    match parse_term source with
    | (term, Plus :: rest) ->
      let (right, r2) = parse_expr rest in
      (Add (term, right), r2)
    | (term, Dash :: rest) ->
      let (right, r2) = parse_expr rest in
      (Sub (term, right), r2)
    | (term, rest) -> (term, rest)

  and parse_term (source : token list) : arith * token list =
    match parse_factor source with
    | (factor, Star :: rest) ->
      let (right, r2) = parse_term rest in
      (Mul (factor, right), r2)
    | (factor, rest) -> (factor, rest)

  and parse_factor (source : token list) : arith * token list =
    match source with
    | Int i :: rest -> (Const i, rest)
    | LParen :: rest ->
      (match parse_expr rest with
       | (expr, RParen :: r2) -> (expr, r2)
       | _ -> raise (ParseError "Expected )"))
    | _ -> raise (ParseError "Expected ( or an integer")

  let parse (source : token list) : arith =
    match parse_expr source with
    | (expr, []) -> expr
    | _ -> raise (ParseError "Expected end-of-input")

end

module LeftAssoc = struct

  (* <expr> ::= <expr> + <term> | <expr> - <term> | <term>
   * <term> ::= <term> * <fact> | <fact>
   * <fact> ::= $int | ( <expr> )
   *)

  (* Let's try... *)

  let rec parse_expr (source : token list) : arith * token list =
    match parse_expr source with
    | (expr, Plus :: rest) ->
      let (term, r2) = parse_term rest in
      (Add (expr, term), r2)
    | (expr, Dash :: rest) ->
      let (term, r2) = parse_term rest in
      (Sub (expr, term), r2)
    | _ -> parse_term source

  and parse_term (source : token list) : arith * token list =
    match parse_term source with
    | (term, Star :: rest) ->
      let (factor, r2) = parse_factor rest in
      (Mul (term, factor), r2)
    | _ -> parse_factor source

  and parse_factor (source : token list) : arith * token list =
    match source with
    | Int i :: rest -> (Const i, rest)
    | LParen :: rest ->
      let (expr, r2) = parse_expr rest in
      (match r2 with
       | RParen :: r3 -> (expr, r3)
       | _ -> raise (ParseError "Expected end-of-input"))
    | _ -> raise (ParseError "Expected integer or end-of-input")

  let parse (source : token list) : arith =
    match parse_expr source with
    | (expr, []) -> expr
    | _ -> raise (ParseError "Expected end-of-input")

end

module Practical = struct

  let rec parse_expr (s : token list) : arith * token list =
    let rec help ex = function
      | Plus :: r ->
        let (e, rest) = parse_term r in
        help (Add (ex, e)) rest
      | Dash :: r ->
        let (e, rest) = parse_term r in
        help (Sub (ex, e)) rest
      | ts -> (ex, ts) in
    let (e, r) = parse_term s in
    help e r

  and parse_term (s : token list) : arith * token list =
    let rec help ex = function
      | Star :: r ->
        let (e, rest) = parse_factor r in
        help (Mul (ex, e)) rest
      | ts -> (ex, ts) in
    let (e, r) = parse_factor s in
    help e r

  and parse_factor : token list -> arith * token list = function
    | LParen :: r ->
      let (e, r2) = parse_expr r in
      (match r2 with
       | RParen :: rest -> (e, rest)
       | _ -> raise (ParseError "Expected )"))
    | Int i :: r -> (Const i, r)
    | _ -> raise (ParseError "Expected <int> or (")

  let parse (source : token list) : arith =
    match parse_expr source with
    | (expr, []) -> expr
    | _ -> raise (ParseError "Expected end-of-input")

end
