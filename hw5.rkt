;; Programming Languages, Homework 5 version 1.1
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem A-1
(define (mupllist->racketlist lst)
  (cond [(apair? lst) (cons (mupllist->racketlist (apair-e1 lst)) (mupllist->racketlist (apair-e2 lst)))]
        [(int? lst) lst]
        [(var? lst) lst]
        [(aunit? lst) null]))

;; Problem A-2
(define (racketlist->mupllist lst)
  (cond [(null? lst) (aunit)]
        [(int? lst) lst]
        [(var? lst) lst]
        [(list? lst) (apair (racketlist->mupllist (car lst)) (racketlist->mupllist (cdr lst)))]))

;; Problem B

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; "CHANGE" add more cases here
        ;; one for each type of expression
        [(int? e) e] ;simple add
        [(fst? e) (let([sub-expression-eval (eval-under-env (fst-e e) env)])
                    (if (apair? sub-expression-eval) (apair-e1 sub-expression-eval) (error "MUPL fst applied to non-pair")))]
        [(snd? e) (let([sub-expression-eval (eval-under-env (snd-e e) env)])
                    (if (apair? sub-expression-eval) (apair-e2 sub-expression-eval) (error "MUPL snd applied to non-pair")))]
        [(aunit? e) e]
        [(apair? e) (apair  (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env ))]
        [(isaunit? e) (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0 ))]
        [(ifgreater? e) (let*([e1-val (int-num (eval-under-env (ifgreater-e1 e) env))]
                              [e2-val (int-num (eval-under-env (ifgreater-e2 e) env))])
                          (if (> e1-val e2-val)
                            (eval-under-env (ifgreater-e3 e) env) ;e1 > e2 then e3
                            (eval-under-env (ifgreater-e4 e) env) ;e1 <= e2 then e4
                          ))]


        ;mlet: (eval-exp (mlet "x" (int 5) (var "x"))) returns (int 5)
        ;we want to store the pair ("x" . (int 5)) in the env (which is a list of pairs)
        ;*the env variable is an empty list to start
        ;*****(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)

        ; [[[   mlet s e1 e2 <==> mlet var e body   ]]]
        [(mlet? e)  (let*([first-expression-value (eval-under-env (mlet-e e) env)]
                          [extended-environment (cons (cons (mlet-var e) first-expression-value) env)])
                      (eval-under-env (mlet-body e) extended-environment)
                     )]        

        ;If s1 and s2 are Racket strings and e is a MUPL expression, then (fun s1 s2 e)
        ;is a MUPL expression (a function). In e, s1 is bound to the function itself (for recursion)
        ;and s2 is bound to the (one) argument. Also, (fun #f s2 e) is allowed for anonymous
        ;nonrecursive functions.

        
        ;in e, s1 is bound to the function itself (for recursion)
        ;Either the function call is anonymous (ie f#) or its not (ie "incr")
        ;if "fun-nameopt e" is anonymous, it will evaluate to #f, in that case, expression 2 will execute.
        
        [(fun? e) (if (not(fun-nameopt e))
                        ;"function IS anonymous" --> the result is a closure (a pair: (code,env))
                        (closure env e)
                        ;"function NOT anonymous" --> exact same as result but maybe change in future
                        (closure env e) ;***If the closure is not anon, then we extend the end with the fun name and its params
                        )]


        ;(struct closure (env fun))
        ;(struct fun  (nameopt formal body))
        ;(struct call (funexp actual))
        [(call? e) (let* ([closure-val (eval-under-env (call-funexp e) env)] ;first eval the closure to a value
                          [arg-val (eval-under-env (call-actual e) env)] ;second eval the argument to a value
                          [closure-vals-env (if (closure? closure-val) (closure-env closure-val) (error "MUPL call with non-function"))] ;<== closure-vals environment
                          [closure-fun-body (fun-body (closure-fun closure-val))] ;get the closures function-body (which will be evaled using the closures extended env)
                          [closure-fun-name (fun-nameopt (closure-fun closure-val))]
                          [closure-fun-arg-name (fun-formal (closure-fun closure-val))]
                          [ext-closure-env-map-fn-name-to-closure (cons (cons closure-fun-name closure-val) closure-vals-env)]
                          [ext-closure-env-map-fn-arg-name-to-argval (cons (cons closure-fun-arg-name arg-val) ext-closure-env-map-fn-name-to-closure)])
                     (eval-under-env closure-fun-body ext-closure-env-map-fn-arg-name-to-argval)

                     ;(eval-exp (call (fun #f "x" (var "x")) (int 5) )) //Normal call - works
                     ;(eval-exp (call (mlet "x" (int 5)(fun "test" "x" (var "x"))) (int 100))) //Basic scope with call - fails
                   )
         ]

        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))


;(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
;(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
;(struct add  (e1 e2)  #:transparent)  ;; add two expressions
;(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
;(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
;(struct call (funexp actual)       #:transparent) ;; function call
;(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
;(struct apair (e1 e2)     #:transparent) ;; make a new pair
;(struct fst  (e)    #:transparent) ;; get first part of a pair
;(struct snd  (e)    #:transparent) ;; get second part of a pair
;(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
;(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; Problem C

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3)
)

(define (mlet* lstlst e2)
  ;need to resolve (var "x")s to values
  ;since the environment (lstlst) used with envlookup
  ;matches left to right, we need to match right to left
  ;since later bindings shadow earlier bindings
  ;need to reverse lstlst in base case
  (cond [(var? e2) (envlookup (reverse lstlst) (var-string e2))]
        [(int? e2) e2]
        [(add? e2) (letrec ([v1 (mlet* lstlst (add-e1 e2))]
                            [v2 (mlet* lstlst (add-e2 e2))])
                     (add v1 v2)
                     )]
  )
)

(define (ifeq e1 e2 e3 e4)
  (ifgreater (ifgreater e1 e2 e1 e2) (ifgreater e2 e1 e1 e2) e4 e3)
)

;; Problem D
;; this binding is a bit tricky. it must return a function.
;; the first two lines should be something like this:
;;
;;   (fun "mupl-map" "f"    ;; it is  function "mupl-map" that takes a function f
;;       (fun #f "lst"      ;; and it returns an anonymous function
;;          ...
;;
;; also remember that we can only call functions with one parameter, but
;; because they are curried instead of
;;    (call funexp1 funexp2 exp3)
;; we do
;;    (call (call funexp1 funexp2) exp3)
;;

;(struct closure (env fun))
;(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
;(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
;(struct add  (e1 e2)  #:transparent)  ;; add two expressions
;(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
;(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
;(struct call (funexp actual)       #:transparent) ;; function call
;(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
;(struct apair (e1 e2)     #:transparent) ;; make a new pair
;(struct fst  (e)    #:transparent) ;; get first part of a pair
;(struct snd  (e)    #:transparent) ;; get second part of a pair
;(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
;(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;1) We're binding a mupl-function to the Racket variable mupl-map
;2) This muple-function "acts" like a map (applies a function to elements of a list)
;3) This muple-function: TAKES a muple-function and RETURNS a muple-function.
;4) The RETURNED function should take a mupl-list
;(call (call mupl-map (fun "addone" "x" (add (var "x") (int 2)))) (aunit))

;first try to make a basic map-like mupl function that applies a mupl function over a mupl list
(define mupl-map
  (fun "mupl-map" "f"
       (fun #f "lst"
            (ifaunit (var "lst") (aunit) (apair (call (var "f") (fst (var "lst"))) (call (call (var "mupl-map") (var "f")) (snd (var "lst"))))))))
                                                                       
  
;  (letrec ([xs (list 1 2 3)]
;           [aux (lambda(xs-prime acc) (cond[(aunit? xs-prime) acc]
;                                           [(apair? xs-prime) (aux (apair-e2 xs-prime) (cons (f (int-num (apair-e1 xs-prime))) acc))]))])
;    (aux xs null)
;  )

  ;does a recursive let binding with: xs ==> mupllist, and aux ==> a helper function that iterates over the mupllist
  ;Need:
  ; 1) mupl function that iterates over a mupllist
  ; 2) on each evaluation on that mupllist, we have an x, which is (apair-e1 xs)
  ; 3) every time x is encountered, we need to apply a mupl-function to that value

  
  


(define mupl-mapAddN
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))
