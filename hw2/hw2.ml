type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* Problem 1 *)
let rec find_nt list nt  = match list with
|[] -> []
|head::tail -> if (fst head) = nt
then (snd head) :: find_nt tail nt
else find_nt tail nt;;

let convert_grammar g = ((fst g), find_nt (snd g));; 

(* Problem 2 *)

let rec recurse_tree tree = match tree with
   |[] -> []
   |head::tail -> match head with
      |Leaf t -> t::recurse_tree tail
      |Node (nt, subtree) -> recurse_tree subtree@recurse_tree tail;;

let parse_tree_leaves = function
|Leaf t -> [t]
|Node (nt, subtree) -> recurse_tree subtree

(*problem 3*)

let rec match_rules_list func rules accept frag = match rules with
   |[] -> None
   |head::tail -> 
      let output = match_rule func head accept frag in
      if output = None
      then match_rules_list func tail accept frag
      else output

and match_rule func rules accept frag = match rules with
   |[] -> accept frag
   |rules_head::rules_tail -> match rules_head with
   |N nt ->
      let next_rules = func nt in
      let shifted_accept = match_rule func rules_tail accept in 
(*need to pass the rest rules list with an accpetor function in order to match through the rest of the rules list before it returns result after finishing the following rules sublist.*)
      match_rules_list func next_rules shifted_accept frag
   |T t -> match frag with
      |[] -> None
      |frag_head::frag_tail -> if frag_head = t
      then match_rule func rules_tail accept frag_tail
      else None

let make_matcher g =
   let start = (fst g) in
   let func = (snd g) in
   let rules = func start in
   match_rules_list func rules

(* problem 4 *)

let parse_tree_acceptor tree frag =
  match frag with
  | [] -> Some tree
  | _ -> None

let rec parse_rules_tree start func rules_list path accept frag = match rules_list with
   |[] -> None
   |head::tail -> 
   let output = parse_rule start func head path accept frag in
   if output = None 
   then parse_rules_tree start func tail path accept frag
   else output 

and parse_rule start func rule path accept frag = match rule with
   |[] -> accept (Node(start, path)) frag
   |rule_head::rule_tail -> match rule_head with
   |N nt -> 
      let next_rules = func nt in
      let shifted_accept tail_path new_frag = parse_rule start func rule_tail (path@[tail_path]) accept new_frag in
      parse_rules_tree nt func next_rules [] shifted_accept frag
   |T t -> match frag with
   |[] -> None
   |frag_head::frag_tail -> 
      if frag_head = t 
      then parse_rule start func rule_tail (path@[Leaf t]) accept frag_tail
      else None

let make_parser g =
  let start = (fst g) in
  let func = (snd g) in
  let rules_list = (func start) in
  parse_rules_tree start func rules_list [] parse_tree_acceptor
