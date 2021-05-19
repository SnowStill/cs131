
type awksub_nonterminals =
| Code | Letter | Pair | Number | Rec

let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

let awkish_grammar = 
(Code,
 function
|Code ->
[
[N Pair; N Pair];
[N Pair];
[ N Number]
]

|Letter ->
[
[T "A"];
[T "B"]; 
[T "C"]
]

|Pair ->
[
[N Letter; N Number];
]

|Number ->
[
[T "3"];
[T "4"];
[T "5"; T "6"];
[T "7"; T "0"]
])

let rec_grammar = 
(Rec,
  function
|Rec ->
[
[N Rec]
])


(* Problem 5 *)
let make_matcher_test0 = 
((make_matcher awkish_grammar accept_all ["A"; "3"; "B"; "4"]) = Some [])
let make_matcher_test1 =
((make_matcher awkish_grammar accept_all ["A"; "B"; "C"; "2"]) = None)
let make_matcher_test2 =
((make_matcher awkish_grammar accept_all ["A"; "3"; "B"; "C"]) = Some ["B"; "C"])
let make_matcher_test3 =
(make_matcher rec_grammar accept_all ["A"; "3"; "B"; "C"])
(* Problem 6 *)
let make_parser_test0 = 
((make_parser awkish_grammar ["A"; "3"; "B"; "4"]) = 
	      Some (Node(Code, [Node(Pair, [Node(Letter, [Leaf "A"]); Node(Number,[Leaf "3"])]);

				Node(Pair, [Node(Letter, [Leaf "B"]); Node(Number,[Leaf "4"])])])))
