open List
open Nfa

(*********)
(* Types *)
(*********)

type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t

(***********)
(* Utility *)
(***********)

let fresh =
  let cntr = ref 0 in
  fun () ->
    cntr := !cntr + 1 ;
    !cntr

(*******************************)
(* Part 3: Regular Expressions *)
(*******************************)
(*remove duplicates*)
let rec removeduplicates inp ret = 
  match inp with
  | [] -> ret
  | (x::xt) -> if mem x ret = false then removeduplicates xt (x::ret) else removeduplicates xt ret

(*creates an empty transition from all fs of input to dest*)
let rec get_fs_trans (input:int list) (dest: int) 
(ret: (int, char) transition list) : (int, char) transition list = 
  match input with
  | [] -> ret
  | (x::xt) -> get_fs_trans xt dest ((x, None, dest)::ret)


(*new char nfa*)
let getfirst inp = match inp with
| [] -> -1
| (x::xt) -> x
let getsecond inp = match inp with
| [] -> -1
| (x::xt) -> getfirst xt

let new_char (inp: char) : (int, char) nfa_t =
  let y = [fresh();fresh()] in
{ 
  sigma = [inp];
  qs = y;
  q0 = getfirst y;
  fs = [getsecond y];
  delta = [(getfirst y, Some inp, getsecond y)];
}
(*new empty nfa*)
let new_empty (): (int, char) nfa_t =
  let inss = fresh() in
{
  sigma = [];
  qs = [inss];
  q0 = inss;
  fs = [inss];
  delta = [];
}
(*new concat nfa*)
let new_concat (inp1: (int, char) nfa_t) (inp2: (int, char)nfa_t) : (int, char) nfa_t =
  {
    sigma = removeduplicates (append inp1.sigma inp2.sigma) [];
    qs = append inp1.qs inp2.qs;
    q0 = inp1.q0;
    fs = inp2.fs;
    delta = append (get_fs_trans inp1.fs inp2.q0 []) (append inp1.delta inp2.delta) 
  }

(*new union*)
let new_union (inp1: (int, char) nfa_t) (inp2: (int, char)nfa_t) : (int, char) nfa_t =
let newbeginning = fresh() in
let newend = fresh() in
{
  sigma = removeduplicates (append inp1.sigma inp2.sigma) [];
  qs = newbeginning::newend::(append inp1.qs inp2.qs);
  q0 = newbeginning;
  fs = [newend];
  delta = let a = get_fs_trans inp2.fs newend [] in
          let b = get_fs_trans inp1.fs newend [] in      
          let c = (newbeginning, None, inp2.q0)::(newbeginning, None, inp1.q0)::(append inp1.delta inp2.delta) in
          append a (append b c)
}

(*new closure*)
let new_close (inp1: (int, char) nfa_t) : (int, char) nfa_t = 
let newbeginning = fresh() in
let newend = fresh() in
{
  sigma = inp1.sigma;
  qs = newbeginning::newend::inp1.qs;
  q0 = newbeginning;
  fs = [newend];
  delta = let a = get_fs_trans inp1.fs newend [] in
          let b = (newbeginning, None, newend)::(newbeginning, None, inp1.q0)::(newend, None, newbeginning)::inp1.delta in
          append a b

}
(********** *)
let rec regexp_to_nfa (regexp: regexp_t) : (int, char) nfa_t =
  match regexp with
  |Char x -> new_char x
  |Empty_String -> new_empty()
  |Union (x,y) -> new_union (regexp_to_nfa x) (regexp_to_nfa y)
  |Concat (x,y) -> new_concat (regexp_to_nfa x) (regexp_to_nfa y)
  |Star x -> new_close (regexp_to_nfa x)

(*****************************************************************)
(* Below this point is parser code that YOU DO NOT NEED TO TOUCH *)
(*****************************************************************)

exception IllegalExpression of string

(* Scanner *)
type token =
  | Tok_Char of char
  | Tok_Epsilon
  | Tok_Union
  | Tok_Star
  | Tok_LParen
  | Tok_RParen
  | Tok_END

let tokenize str =
  let re_var = Str.regexp "[a-z]" in
  let re_epsilon = Str.regexp "E" in
  let re_union = Str.regexp "|" in
  let re_star = Str.regexp "*" in
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let rec tok pos s =
    if pos >= String.length s then [Tok_END]
    else if Str.string_match re_var s pos then
      let token = Str.matched_string s in
      Tok_Char token.[0] :: tok (pos + 1) s
    else if Str.string_match re_epsilon s pos then
      Tok_Epsilon :: tok (pos + 1) s
    else if Str.string_match re_union s pos then Tok_Union :: tok (pos + 1) s
    else if Str.string_match re_star s pos then Tok_Star :: tok (pos + 1) s
    else if Str.string_match re_lparen s pos then Tok_LParen :: tok (pos + 1) s
    else if Str.string_match re_rparen s pos then Tok_RParen :: tok (pos + 1) s
    else raise (IllegalExpression ("tokenize: " ^ s))
  in
  tok 0 str

let tok_to_str t =
  match t with
  | Tok_Char v -> Char.escaped v
  | Tok_Epsilon -> "E"
  | Tok_Union -> "|"
  | Tok_Star -> "*"
  | Tok_LParen -> "("
  | Tok_RParen -> ")"
  | Tok_END -> "END"

(*
   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen

   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let parse_regexp (l : token list) =
  let lookahead tok_list =
    match tok_list with
    | [] -> raise (IllegalExpression "lookahead")
    | h :: t -> (h, t)
  in
  let rec parse_S l =
    let a1, l1 = parse_A l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Union ->
        let a2, l2 = parse_S n in
        (Union (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_A l =
    let a1, l1 = parse_B l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Char c ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_Epsilon ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_LParen ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_B l =
    let a1, l1 = parse_C l in
    let t, n = lookahead l1 in
    match t with Tok_Star -> (Star a1, n) | _ -> (a1, l1)
  and parse_C l =
    let t, n = lookahead l in
    match t with
    | Tok_Char c -> (Char c, n)
    | Tok_Epsilon -> (Empty_String, n)
    | Tok_LParen ->
        let a1, l1 = parse_S n in
        let t2, n2 = lookahead l1 in
        if t2 = Tok_RParen then (a1, n2)
        else raise (IllegalExpression "parse_C 1")
    | _ -> raise (IllegalExpression "parse_C 2")
  in
  let rxp, toks = parse_S l in
  match toks with
  | [Tok_END] -> rxp
  | _ -> raise (IllegalExpression "parse didn't consume all tokens")


let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str
