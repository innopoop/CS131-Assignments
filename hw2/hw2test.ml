let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type awksub_nonterminals =
  | Phrase | Predicate | Noun | Verb | Adjective | Preposition | Adverb | Person | Place | Thing

let awkish_grammar =
  (Phrase,
   function
   | Phrase ->
   [[N Noun; N Predicate]; [N Noun; N Verb; N Adverb]]
     | Predicate -> 
	 [[N Adverb; N Verb; N Preposition; N Noun]; [N Verb; N Adverb; N Preposition; N Noun]]
     | Noun ->
         [[N Person]; [N Place]; [N Thing]]
     | Verb ->
	 [[T"jumps"];
	  [T"runs"];
	  [T"flies"]]
	  | Preposition ->
	  [[T"over"]; [T"under"]; [T"above"]]
	 |Adjective ->
	 [[T"brown"]; [T"cute"]; [T"small"]]
     | Person ->
	 [[T"Bennett"];
	  [T"Lisa"]]
     | Adverb ->
	 [[T"quickly"];
	  [T"swiftly"]]
     | Place ->
	 [[T"home"]; [T"school"]; [T"New York"]]
	  | Thing ->
	  [[T"plushie"];[T"fox"];[T"fence"]]);;

let frag1 = ["Bennett"; "flies"; "quickly"; "over";"New York"; "home"];;

let make_matcher_test = ((make_matcher awkish_grammar accept_all frag1 ) = Some ["home"]);;

let frag2 = ["Bennett"; "flies"; "quickly"; "over";"New York"];;

let make_parser_test = ((make_parser awkish_grammar frag2) = Some(Node (Phrase,
																[Node (Noun, [Node (Person, [Leaf "Bennett"])]);
																	Node (Predicate,
																[Node (Verb, [Leaf "flies"]); Node (Adverb, [Leaf "quickly"]);
																	Node (Preposition, [Leaf "over"]);
																	Node (Noun, [Node (Place, [Leaf "New York"])])])])));;

let parse_tree_leaves_test = (parse_tree_leaves (Node(Phrase, 
                                                      [Node(Noun, 
                                                            [Node (Person, [Leaf "Bennett"])]); 
                                                       Node(Predicate, 
                                                            [Node(Verb, 
                                                                  [Leaf "flies"]); 
                                                             Node(Adverb, 
                                                                  [Leaf "quickly"]); 
                                                             Node(Preposition, 
                                                                  [Leaf "over"]); 
                                                             Node(Noun, [Leaf "New York"])])])) = frag2);;

Printf.printf "Result of make_matcher_test: %s\n" (if make_matcher_test = true then "true" else "false");;

Printf.printf "Result of make_parser_test: %s\n" (if make_parser_test = true then "true" else "false");;

Printf.printf "Result of parse_tree_leaves_test: %s\n" (if parse_tree_leaves_test = true then "true" else "false");;