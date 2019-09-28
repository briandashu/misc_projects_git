open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)
(*Move*)
let rec addarray (a: 'q list) (b: 'q list): 'q list = 
  match a with  
  |[] -> b
  |x::xt -> if mem x b = false then addarray xt (x::b) else addarray xt b

let rec movehelp1 (qs: 'q) (trans: ('q, 's) transition list) (op: 's option) (ret: 'q list): 'q list =
  match trans with
  |[] -> ret
  |(a,b,c)::yt -> if a = qs && b = op then
    movehelp1 qs yt op (addarray [c] ret) else movehelp1 qs yt op ret

let rec movehelp (qs: 'q list) (trans: ('q, 's) transition list) (op: 's option) (ret: 'q list): 'q list = 
  match qs with
  |[] -> ret
  |(x::xt) -> movehelp xt trans op (addarray (movehelp1 x trans op []) ret) 


let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = 
  movehelp qs nfa.delta s []

(*E_Closure*)
(*call eclosure directly next to*)
let rec e_closurehelp1 (cons: ('q, 's) transition list) (qs:'q) (trans: ('q, 's) transition list) (ret: 'q list): 'q list =
  match trans with 
  |[] -> addarray [qs] ret
  |(a,b,c)::yt -> if a = qs && b = None && mem c ret = false then 
    e_closurehelp1 cons qs yt (addarray (e_closurehelp1 cons c (cons) [a]) ret) else e_closurehelp1 cons qs yt ret

let rec e_closurehelp2 (nfa: ('q,'s) nfa_t) (qs: 'q list) (ret: 'q list): 'q list =
  match qs with 
  | [] -> ret
  | (x::xt) -> let y = (addarray (e_closurehelp1 nfa.delta x nfa.delta ret) ret) in e_closurehelp2 nfa xt y


let rec e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  e_closurehelp2 nfa qs []
(*accept*)

let trunk inp = match inp with 
| [] -> []
| (x::xt) -> xt

let getfirst inp = match inp with
|[] -> None
| (x::xt) -> Some x

let rec oneintwo inp1 inp2 = match inp1 with
| [] -> false
| (x::xt) ->
  if mem x inp2 = true then true else oneintwo xt inp2

let rec accepthelp1 (nfa: ('q,char) nfa_t) (qs: 'q list) (str : char list): int =
  if length str = 0 && oneintwo qs nfa.fs = true then 1
  else if length str = 0 && oneintwo qs nfa.fs = false then 0 else
  match qs with
  |[] -> 0
  |(x::xt) ->
    let rec accepthelp2 (nfa: ('q,char) nfa_t) (qs: 'q list) (str : char list): int =
      match qs with
      |[] -> 0
      |(x::xt) ->
        accepthelp1 nfa (e_closure nfa [x]) (str) + accepthelp2 nfa xt str
    in accepthelp2 nfa (move nfa [x] (getfirst str)) (trunk str) + accepthelp1 nfa xt str

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  if accepthelp1 nfa (e_closure nfa [nfa.q0]) (explode s) > 0 then true else false

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

(*newstate*)

let rec reverselist inp =
  let rec aux acc = function
    | [] -> acc
    | (x::xt) -> aux (x::acc) xt in
  aux [] inp

let rec sethelp (nfa: ('q,'s) nfa_t) (qs: 'q list) (insig: 's list) (ret: 'q list list): 'q list list =
  match insig with 
  | [] -> ret
  | (x::xt) -> let y = (e_closure nfa (move nfa qs (Some x)))::ret in
    sethelp nfa qs xt y

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  sethelp nfa qs (reverselist (nfa.sigma)) []

(*newtrans*)

let rec transhelp (nfa: ('q,'s) nfa_t) (qs: 'q list) (alphabet: 's list) (ret:(('q list, 's) transition list)): ('q list, 's) transition list =
  match alphabet with
  | [] -> ret
  | (x::xt) -> 
    let y = e_closure nfa (move nfa qs (Some x))
    in transhelp nfa qs xt ((qs, Some x, y) :: ret) 

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  reverselist (transhelp nfa qs nfa.sigma [])
(*newfinals*)

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if oneintwo qs nfa.fs = true then [qs] else []
  
(*nfa to dfa, use mem to check if the move has already been used*)
let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  failwith "unimplemented"

(*checks if a 'qlist is already in a 'qlist list *)
let rec newsort = function
    | [] -> []
    | x :: l -> insert x (newsort l)
  and insert elem = function
    | [] -> [elem]
    | x :: l -> if elem < x then elem :: x :: l
                else x :: insert elem l;;
let listcompare a b  = 
  if newsort a = newsort b then true else false

let rec listincluded (inp: 'q list) (check: 'q list list) = 
  match check with
  |[] -> false
  |(x::xt) -> if listcompare inp x = true then true else listincluded inp xt
(*************************************************)

(*remove duplicates*)
let rec removeduplicates inp ret = 
  match inp with
  | [] -> ret
  | (x::xt) -> if listincluded x ret = false then removeduplicates xt (x::ret) else removeduplicates xt ret

(***************** *)
(*gets the sigma of the dfa*)
let rec get_dfa_sigma (nfa: ('q,'s) nfa_t) (start: 'q list) (alpha: 's list) (ret: 'q list list) : 'q list list = 
  match alpha with
  | [] -> ret
  | (x::xt) -> 
      let y = e_closure nfa (move nfa start (Some x)) in
      if y = [] || listincluded y ret = true then get_dfa_sigma nfa start xt (start::ret) 
      else get_dfa_sigma nfa start xt (get_dfa_sigma nfa y nfa.sigma (start::ret))

let get_dfa_sigma_clean (nfa: ('q,'s) nfa_t) (start: 'q list) (alpha: 's list) (ret: 'q list list): 'q list list = 
  removeduplicates (get_dfa_sigma nfa start alpha ret) []
(******************* *)

(*get the deltas of the dfa*)
let rec removeemptytrans (input: ('q list, 's) transition list) (ret :('q list, 's) transition list) : ('q list, 's) transition list = 
  match input with 
  |[] -> ret
  |(a,b,c) :: xt -> if c = [] then removeemptytrans xt ret else removeemptytrans xt ((a,b,c)::ret)

let rec get_dfa_delta (nfa: ('q,'s) nfa_t) (input: 'q list list) (ret :('q list, 's) transition list) : ('q list, 's) transition list =
  match input with
  | [] -> ret 
  |(x::xt) -> get_dfa_delta nfa xt (append (removeemptytrans (new_trans nfa x) []) ret)
(***************************)

(*get start*)
let rec get_dfa_start (nfa: ('q,'s) nfa_t) (input: 'q list list) : 'q list = 
  match input with
  | [] -> []
  | (x::xt) -> if listcompare (e_closure nfa [(nfa.q0)]) x then x else get_dfa_start nfa xt
(******** *)

(*get the final states*)
let rec get_dfa_finals (nfa: ('q,'s) nfa_t) (input: 'q list list) (ret: 'q list list) : 'q list list =
  match input with
  | [] -> ret
  | (x::xt) -> let y = new_finals nfa x in
    if y = [] then get_dfa_finals nfa xt ret else get_dfa_finals nfa xt (append y ret)
(******************** *)
let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let y = get_dfa_sigma_clean nfa (e_closure nfa [nfa.q0]) nfa.sigma [] in
  {sigma = nfa.sigma;
  qs = y;
  q0 = get_dfa_start nfa y;
  fs = get_dfa_finals nfa y [];
  delta = get_dfa_delta nfa y [];}
