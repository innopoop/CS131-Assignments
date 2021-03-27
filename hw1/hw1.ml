type ('a, 'b) symbol = N of 'a | T of 'b

(*Assignment 1*)
let subset a b = 
  if(a = b) then true 
  else List.for_all(fun x -> List.mem x b) a;;

(*Assignment 2*)
let equal_sets a b =
	subset a b && subset b a;;

(*Assignment 3 *)
let set_union a b = a@b 

(*Assignment 4*)
let set_symdiff a b = 
  let intersection = (List.filter(fun x -> List.mem x b) a) in
  let combined = a@b in
  List.filter(fun x -> not (List.mem x intersection)) combined;;

(*Assignment 5 *)
let self_member _ = false;;

(*Assignment 6 *)
let rec computed_fixed_point eq f x =
  if eq x (f x) then x
  else computed_fixed_point (eq) f (f x);;

(*Assignment 7 *)

(*Helper Functions *)
let allNonterminals l =
  List.fold_left (set_union) [] l

let set_intersection list_a list_b = List.filter_map (fun el -> if List.mem el list_b then Some el else None) list_a ;;

let rec getReachableSymbols params = 
  let rules = fst params in
  let reachable_symbols = snd params in
  match rules with
  | ls -> 
      if ((List.length (set_intersection (List.map(fun x -> fst x) rules) reachable_symbols))  = 0) then
        reachable_symbols
      else
        let reachable_set = 
          allNonterminals (List.filter_map(
              fun x ->
                let sym = fst x in
                let sym_list = snd x in
                if(List.mem sym reachable_symbols)
                then Some (List.filter_map(
                    fun y -> 
                      match y with
                      | N nt -> Some nt
                      | T _ -> None
                  ) sym_list)
                else None
            ) rules ) in
        let allReachables = set_union reachable_set reachable_symbols in
        getReachableSymbols (List.filter(fun x -> if(List.mem (fst x) reachable_symbols) then false else true) rules, allReachables) 
;;

(*Main Function *)
let filter_reachable g = 
  let start = fst g in
  let rules = snd g in
  let allReachables = getReachableSymbols (rules, [start]) in
  (start, List.filter(fun x -> List.mem (fst x) allReachables) rules)
;; 
