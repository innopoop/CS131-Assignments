#lang racket
(require racket/trace)

;PART 1

;======UTILITY FUNCTIONS======

;Check whether the function is an anonymous function
(define (lambda? x) (member x '(lambda λ)))

;Create a map recursively
(define (makeMap lastMap key value)
	(hash-set lastMap key value)
)

;Create a synonymous variable for functions
(define (combine-symbol a b)
	(string->symbol (string-append (symbol->string a) "!" (symbol->string b)))
)

;Undo a list
(define (unlist ls)
	(if (and (list? ls) (equal? (length ls) 1))
		(car ls)
		ls
	)
)

;Reverse a list
(define (reverseList ls)
  (define (innerReverse ls acc)
    (if (null? ls)
      acc
      (innerReverse (cdr ls) (cons (car ls) acc))))
  (innerReverse ls '())
)

;Given a hash map and an expression, replace all occurrences of the hashMap's keys with values inside of expr
(define (replace hashMap expr)
	(if (not (list? expr))
		(hash-ref hashMap expr expr)
		(cond
			[(empty? expr) `()]
			[(equal? (car expr) 'quote) expr]
			[(not (equal? (hash-ref hashMap (car expr) (car expr)) (car expr))) (cons (hash-ref hashMap (car expr) (car expr)) (replace hashMap (cdr expr)))]
			[else (cons (replace hashMap (car expr)) (replace hashMap (cdr expr)))]
		)
	)
)

;======Assignment-related functions======

;Hash the parameters and create a map for the first and second expression
(define (hashParameters lastMapX lastMapY paramsX paramsY)
	(cons 
		(foldl (lambda (elem lastMap) (makeMap lastMap (car elem) (cdr elem)))
			lastMapX
			(map (lambda (x y)
				(cond 
					[(equal? x y) (cons x x)]
					[else (cons x (combine-symbol x y))]
				)
			)
				paramsX paramsY
			)	
		)
		(foldl (lambda (elem lastMap) (makeMap lastMap (car elem) (cdr elem)))
			lastMapY
			(map (lambda (x y)
				(cond 
					[(equal? x y) (cons x x)]
					[else (cons y (combine-symbol x y))]
				)
			)
				paramsX paramsY
			)	
		)
	)
)

;Substitute all list items with their new values using the hash map created by hash parameters and recursively evaluate any nested functions
(define (substituteParams exprX exprY hashMapX hashMapY)
	(cond
		[(and (list? exprX) (list? exprY))
			(let ( (revX (reverseList exprX)) (revY (reverseList exprY)) )
				(foldl (lambda (pair lst) 
					(let ( (carPair (car pair)) (cdrPair (cdr pair)) )
						(cond
							[(and (lambda? carPair) (lambda? cdrPair) (empty? lst)) (cons (evaluateLambda carPair cdrPair hashMapX hashMapY) lst)]
							[(and (or (lambda? carPair) (lambda? cdrPair)) (empty? lst)) (cons (lambda-compare carPair cdrPair hashMapX hashMapY) lst)]
							[(and (list? carPair) (equal? (length carPair) (length cdrPair))) (cons (list-equal-lambda carPair cdrPair hashMapX hashMapY) lst)]
							[(not (empty? list)) (cons (lambda-compare (replace hashMapX carPair) (replace hashMapY cdrPair) hashMapX hashMapY) lst)]
						)
					)
				)
					'()
					(map (lambda (x y) 
							(let (
									(xHash (hash-ref hashMapX x x))
									(yHash (hash-ref hashMapY y y))
								)
								(cons xHash yHash)
							)
						)
						revX revY
					)
				)
			)
		]
		[else (expr-compare (replace hashMapX exprX) (replace hashMapY exprY))]
	)
)

;Evaluate a lambda function: separate evaluation of parameters with evaluation of the rest of the function
(define (evaluateLambda x y lastMapX lastMapY)
	(let ( (lambdaX (car x)) (lambdaY (car y)) (cdrX (cdr x)) (cdrY (cdr y)) (hashMaps (hashParameters lastMapX lastMapY (car (cdr x)) (car (cdr y)))) )
		(let ( (hashMapX (car hashMaps)) (hashMapY (cdr hashMaps)) (paramsX (car cdrX)) (paramsY (car cdrY)) (restX (cdr cdrX)) (restY (cdr cdrY)) ) 
			(if (or (equal? (car x) 'λ) (equal? (car y) 'λ))
				(cons 'λ 
					(cons 
						(substituteParams paramsX paramsY hashMapX hashMapY)
						(substituteParams restX restY hashMapX hashMapY)
					)
				)
				(cons 'lambda
					(cons 
						(substituteParams paramsX paramsY hashMapX hashMapY)
						(substituteParams restX restY hashMapX hashMapY)
					)
				)
			)
		)
	) 
)

;When two list lengths are equal, return an empty list if empty and otherwise the comparison of the two lists, but passes down lambda hashmap to allow for nested variable substitution
(define (list-equal-lambda x y lastMapX lastMapY)
	(cond
		[(empty? x) '()]
		[else (cons (lambda-compare (car x) (car y) lastMapX lastMapY) (list-equal-lambda (cdr x) (cdr y) lastMapX lastMapY))]
	)
)

;Comparing two expressions with the hashmap passed down to allow for variable substitution
(define (lambda-compare x y lastMapX lastMapY)

	(cond
		[(and (equal? x y) (list? x)) (expr-compare (replace lastMapX x) (replace lastMapY y))]
		[(and (equal? x y) (equal? (replace lastMapX x) (replace lastMapY y))) (replace lastMapX x)]
		[(equal? (replace lastMapX x) (replace lastMapY y)) (replace lastMapX x)]
        [(and (boolean? x) (boolean? y)) (if x '% '(not %))]
        [(and (list? x) (list? y) (equal? (length x) (length y))) 
			(let ( (carX (car x)) (carY (car y)) (replacedX (replace lastMapX x)) (replacedY (replace lastMapY y)) )
				(cond 
					[(and (lambda? carX) (lambda? carY) (equal? (length x) 3)) (evaluateLambda x y lastMapX lastMapY) ]
					[(and (list? x) (lambda? carX) (equal? (length x) 3)) (list 'if '% replacedX replacedY)]
					[(and (list? y) (lambda? carY) (equal? (length y) 3)) (list 'if '% replacedX replacedY)]
					[(equal? (not (equal? carX 'if)) (equal? carY 'if)) (list 'if '% replacedX replacedY)]
					[(or (equal? carX 'quote) (equal? carY 'quote)) (list 'if '% replacedX replacedY)]
					[else (list-equal-lambda x y lastMapX lastMapY)]
				)
			)
		]
        [else (list 'if '% (replace lastMapX x) (replace lastMapY y))]
    )
)

;When two list lengths are equal with no variable substitution
(define (list-equal x y)
	(let ( (carX (car x)) (carY (car y)) (cdrX (cdr x)) (cdrY (cdr y)) )
		(cons (expr-compare carX carY) (expr-compare cdrX cdrY))
	)
)

;Compare two expressions normally
(define (expr-compare x y)
    (cond
        [(equal? x y) x]
        [(and (boolean? x) (boolean? y)) (if x '% '(not %))]
        [(and (list? x) (list? y) (equal? (length x) (length y))) 
		(let ( (carX (car x)) (carY (car y)) (unlistedX (unlist x)) (unlistedY (unlist y)) )
			(cond
				[(and (lambda? carX) (lambda? carY) (equal? (length x) 3)) (evaluateLambda x y (hash) (hash))]
				[(or (lambda? carX) (lambda? carY)) (list (list 'if '% unlistedX unlistedY))]
				[(equal? (not (equal? carX 'if)) (equal? carY 'if)) (list 'if '% unlistedX unlistedY)]
				[(or (equal? carX 'quote) (equal? carY 'quote)) (list 'if '% unlistedX unlistedY)]
				[else (list-equal x y)]
			)
		)
		]
        [else (list 'if '% x y)]
    )
)



;PART 2
(define (test-expr-compare x y)
	(and 
	 (equal? (eval x) (eval (list 'let '([% #t]) (expr-compare x y))))
	 (equal? (eval y) (eval (list 'let '([% #f]) (expr-compare x y))))
	)
)

;PART 3
(define test-expr-x 
	'((lambda (quote x) (lambda (lambda) (if x lambda quote))) (lambda (if cons) (cons if cons)) #t )
)
(define test-expr-y
	'((lambda (a x) (lambda (λ) (if x quote λ))) (lambda (if cons) (list if cons)) #f )
)

;Personal test cases
(equal? (expr-compare test-expr-x test-expr-y) '((lambda (quote!a x) (lambda ((if % lambda λ)) (if x (if % lambda quote) (if % quote!a λ)))) (lambda (if cons) ((if % cons list) if cons)) %))

(display 'Provided test cases')
;Provided test cases
(equal? (expr-compare 12 12) '12)
(equal? (expr-compare 12 20) '(if % 12 20))
(equal? (expr-compare #t #t) #t)
(equal? (expr-compare #f #f) #f)
(equal? (expr-compare #t #f) '%)
(equal? (expr-compare #f #t) '(not %))
(equal? (expr-compare 'a '(cons a b)) '(if % a (cons a b)))
(equal? (expr-compare '(cons a b) '(cons a b)) '(cons a b))
(equal? (expr-compare '(cons a lambda) '(cons a λ)) '(cons a (if % lambda λ)))
(equal? (expr-compare '(cons (cons a b) (cons b c)) '(cons (cons a c) (cons a c))) '(cons (cons a (if % b c)) (cons (if % b a) c)))
(equal? (expr-compare '(cons a b) '(list a b)) '((if % cons list) a b))
(equal? (expr-compare '(list) '(list a)) '(if % (list) (list a)))
(equal? (expr-compare ''(a b) ''(a c)) '(if % '(a b) '(a c)))
(equal? (expr-compare '(quote (a b)) '(quote (a c))) '(if % '(a b) '(a c)))
(equal? (expr-compare '(quoth (a b)) '(quoth (a c))) '(quoth (a (if % b c))))
(equal? (expr-compare '(if x y z) '(if x z z)) '(if x (if % y z) z))
(equal? (expr-compare '(if x y z) '(g x y z)) '(if % (if x y z) (g x y z)))
(equal? (expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2)) '((lambda (a) ((if % f g) a)) (if % 1 2)))
(equal? (expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2)) '((λ (a) ((if % f g) a)) (if % 1 2)))
(equal? (expr-compare '((lambda (a) a) c) '((lambda (b) b) d)) '((lambda (a!b) a!b) (if % c d)))
(equal? (expr-compare ''((λ (a) a) c) ''((lambda (b) b) d)) '(if % '((λ (a) a) c) '((lambda (b) b) d)))
(equal? (expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))'(+ #t ((lambda (a c) (f a c)) 1 2))) '(+ (not %) ((λ (a b!c) (f a b!c)) 1 2)))
(equal? (expr-compare '((λ (a b) (f a b)) 1 2)  '((λ (a b) (f b a)) 1 2)) '((λ (a b) (f (if % a b) (if % b a))) 1 2))
(equal? (expr-compare '((λ (a b) (f a b)) 1 2)  '((λ (a c) (f c a)) 1 2)) '((λ (a b!c) (f (if % a b!c) (if % b!c a))) 1 2))
(equal? (expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)  '((lambda (if) (+ if if (f λ))) 3)) '((lambda (lambda!if) (+ lambda!if (if % if lambda!if) (f (if % lambda!if λ)))) 3))
(equal? (expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))a (lambda (a) a))))(lambda (b a) (b a)))  '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))a (λ (b) a))))(lambda (a b) (a b)))) '((λ (a)  ((if % eq? eqv?) a ((λ (a!b b!a) ((λ (a b) (a b)) (if % b!a a!b) (if % a!b b!a)))a (λ (a!b) (if % a!b a))))) (lambda (b!a a!b) (b!a a!b))))