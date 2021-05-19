let my_subset_test0 = subset [] [1;2;3]
let my_subset_test1 = not (subset [1;2;3] [])
let my_subset_test2 = subset [1;1] [1]

let my_equal_sets_test0 = equal_sets [1;2] [2;1]
let my_equal_sets_test1 = equal_sets [1;1;2] [2;1]
let my_equal_sets_test2 = equal_sets [] []
let my_equal_sets_test3 = not (equal_sets [1;2;4] [1;2])

let my_set_union_test0 = equal_sets (set_union [1] [2;3]) [1;2;3]
let my_set_union_test1 = equal_sets (set_union [1;1;2;2] [3]) [1;2;3]
let my_set_union_test2 = equal_sets (set_union [1;1;2;2] [3]) [3;2;1]

let my_symdiff_test0 = equal_sets (set_symdiff [1;2] [1;2]) []
let my_symdiff_test1 = equal_sets (set_symdiff [1;1;1] [1]) []
let my_symdiff_test2 = equal_sets (set_symdiff [] [1;2]) [1;2]
let my_symdiff_test3 = equal_sets (set_symdiff [1;2] []) [1;2]
let my_symdiff_test4 = equal_sets (set_symdiff [1;1;2] []) [1;2]


let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x/2) 2 = 0
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x*. 2.0) 2.0 = infinity

type nonterminals = Expr | B | C | D | A
let rules = 
  [Expr, [N B];
   Expr, [N B; N D];
   Expr, [T"0"];
   Expr, [N D];
   Expr, [N C];
   Expr, [N A];
   B, [N Expr];
   C, [T "0"];
   D, [N C]]
let grammar = B, rules

let my_filter_reachable_test0 = filter_reachable grammar = grammar
let my_filter_reachable_test1 = filter_reachable (C, rules) = 
  (C,[C, [T"0"]])
let my_filter_reachable_test2 = filter_reachable (D, rules) = 
  (D,[C, [T "0"]; D, [N C]])
let my_filter_reachable_test2 = filter_reachable (D, rules) !=
  (D,[D, [N C]; C, [T "0"]])
