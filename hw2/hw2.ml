type ('nonterminal, 'terminal) symbol = N of 'nonterminal | T of 'terminal
type ('nonterminal, 'terminal) parse_tree = Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list | Leaf of 'terminal

(*Problem 1*)
let convert_grammar gram = 
  let start = fst gram in
  let rules = snd gram in
  let get_rules expr= 
    List.filter_map(fun rule -> 
        let sym = fst rule in
        let phrase = snd rule in
        if sym = expr
        then Some phrase
        else None
      ) rules in
  (start, fun some_expr -> 
      get_rules some_expr);;

(*Problem 2*)
let rec parse_tree_leaves tree =
  match tree with
  |Leaf l -> [l]
  | Node (_, lst) ->
      List.flatten (List.map(fun x -> parse_tree_leaves x) lst);; 


(*Problem 3*)
let make_matcher gram = 
	fun accept frag ->
	let start_symbol = fst gram in
	let prod_rules = snd gram in
	let rec check_alt_list nonterm alt_list acc f = match alt_list with
		| [] -> None
		| rules_hd :: rules_tl -> 
			let res = (check_symb_list acc rules_hd f) in 
			match res with 
			None -> (check_alt_list nonterm rules_tl acc f) 
			| _ -> res
	and check_symb_list acc rule f = 
		match rule with
		| [] -> acc f
		| hd :: tl -> 
			(match hd with
			| T t -> 
				(match f with
				| [] -> None
				| f_hd :: f_tl -> 
					if (t = f_hd)
					then (check_symb_list acc tl f_tl)
					else None)
			| N n -> (check_alt_list n (prod_rules n) (check_symb_list acc tl) f))
	in (check_alt_list start_symbol (prod_rules start_symbol) accept frag);;

(*Problem 4*)
let make_parser gram = 
  fun frag -> 
    let asdf_empty_suffix = function _::_ -> None | x -> Some x in
    let extended_acceptor x f = match (asdf_empty_suffix f) with None -> None | Some y -> Some (x, Some y) in
    let start_symbol = (fst gram) in
    let prod_rules = (snd gram) in
    let rec check_alt_list nonterm alt_list acc f rule_list = 
      match alt_list with
      | [] -> None
      | rules_hd :: rules_tl -> let res = (check_symb_list acc rules_hd (rule_list@[rules_hd]) f) in 
          match res with 
            None -> (check_alt_list nonterm rules_tl acc f rule_list) 
          | _ -> res
    and check_symb_list acc rule rule_list f = 
      match rule with
      | [] -> acc rule_list f
      | hd :: tl -> 
          (match hd with
           | T t -> 
               (match f with
                | [] -> None
                | f_hd :: f_tl -> 
                    if (t = f_hd)
                    then (check_symb_list acc tl rule_list f_tl)
                    else None)
           | N n -> (check_alt_list n (prod_rules n) (check_symb_list acc tl) f rule_list))
    in let rule_list = 
         match (check_alt_list start_symbol (prod_rules start_symbol) extended_acceptor frag []) with
         | None -> None
         | Some x -> Some (fst x)
    in let rec make_node root_val rules_used =
         let lst = (make_list (List.hd rules_used) rules_used) in
         ((Node (root_val, (fst lst))), (snd lst))
    and make_list lst rules_used = 
      match lst with
      | [] -> ([], rules_used)
      | hd :: tl -> 
          match hd with
          | T t -> let new_lst = (make_list tl rules_used) in ((Leaf t)::(fst new_lst), (snd new_lst))
          | N n -> 
              let node = (make_node n (List.tl rules_used)) in 
              let new_lst = (make_list tl (snd node)) in
              ((fst node)::(fst new_lst), (snd new_lst))
    in 
    match rule_list with 
    | None -> None 
    | Some x -> Some (fst (make_node start_symbol x))