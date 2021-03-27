(*Test Cases *)

type nonterminals =
  | Fried | Chicken | IN | Sauce;;

let rules =
   [Fried, [N Chicken];
    Chicken, [N Fried];
    Fried, [N IN];
    IN, [T Sauce];]

let my_subset_test0 = subset [] []
let my_subset_test1 = subset [1;1] [1]
let my_subset_test2 = (subset [1;2] [2] = false)

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = equal_sets [0] [0; 0; 0]

let my_set_union_test0 = equal_sets (set_union [1] [1]) [1]

let my_symdiff_test0 = equal_sets (set_symdiff [] []) []
let my_symdiff_test1 = equal_sets (set_symdiff [1;1;2] [2;2;3]) [1;3]

let my_computed_fixed_point_test0 = (computed_fixed_point (=) (fun x -> x * x - 3 * x + 4) 2) = 2 

let my_self_member_test0 = ((self_member []) = false)

let my_filter_reachable_test0 = (filter_reachable (Fried ,rules) = (Fried, rules))
let my_filter_reachable_test1 = (filter_reachable (IN, rules) = (IN, [IN, [T Sauce]]))