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
        [(fst? e) (apair-e1 (fst-e e))]
        [(snd? e) (apair-e2 (snd-e e))]
        [(aunit? e) e]
        [(apair? e) e]
        [(isaunit? e) (if (aunit? (isaunit-e e)) (int 1) (int 0))]
        [(ifgreater? e) (if (> (int-num (ifgreater-e1 e)) (int-num (ifgreater-e2 e)))
                            (eval-under-env (ifgreater-e3 e) env) ;e1 > e2 then e3
                            (eval-under-env (ifgreater-e4 e) env))];e1 <= e2 then e4


        ;mlet: (eval-exp (mlet "x" (int 5) (var "x"))) returns (int 5)
        ;we want to store the pair ("x" . (int 5)) in the env (which is a list of pairs)
        ;*the env variable is an empty list to start
        ;*****(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
        
        [(mlet? e) (eval-under-env ;we have to call eval-under-env in order to store the new env, its not enough to just cons onto env
                    (mlet-body e) ;we call eval-under-env using (var "x") for example, (mlet-body e) should evaluate to the 3rd param of mlet
                    (cons (cons (mlet-var e) (mlet-e e)) env))] ;then we cons the pair, for example, ("x" . (int 3)) onto the current env, and pass it as the new env


        

        ;If s1 and s2 are Racket strings and e is a MUPL expression, then (fun s1 s2 e)
        ;is a MUPL expression (a function). In e, s1 is bound to the function itself (for recursion)
        ;and s2 is bound to the (one) argument. Also, (fun #f s2 e) is allowed for anonymous
        ;nonrecursive functions.

        
        ;in e, s1 is bound to the function itself (for recursion)
        ;Either the function call is anonymous (ie f#) or its not (ie "incr")
        ;if "fun-nameopt e" is anonymous, it will evaluate to #f, in that case, expression 2 will execute.
        
        [(fun? e) (if (not(fun-nameopt e))
                        ;"function IS anonymous" --> the result is a closure (a pair: (code,env))
                        (closure null e)
                        ;"function NOT anonymous" --> exact same as result but maybe change in future
                        (closure null e) ;***If the closure is not anon, then we extend the end with the fun name and its params
                        )]


        ;(struct closure (env fun))
        ;(fun s1 s2 e) ==> (fun name-opt formal body)
        ;(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
        ;(struct call (funexp actual) #:transparent) ;; function call

        ;evaluate the closures functions body, using the closures environment
        ;need the environment of the function when calling eval-under-env (MIGHT NEED TO DO A LOOKUP!)

        ;(call closure argument) --> we need to call the closure with the argument
        ;ie// we call [ (closure (fun "incr" "x" (add (var "x") (int 1))) '()) ] with (int 42) should eval to (int 43)
        ;need to call the closure with the argument
        ;[(call? e) (eval-under-env (fun-body (call-funexp e)) (closure-env (eval-exp (call-funexp e))))]
        [(call? e) (let* ([closure-val (eval-under-env (call-funexp e) env)]
                          [arg-val (eval-under-env (call-actual e) env)]
                          [closure-vals-env (closure-env closure-val)] ;<== closure-vals environment
                          [closure-fun-body (fun-body (closure-fun closure-val))]
                          
                          ;*****(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
                          ;(mlet "x" (int 5) (var "x")) <==extend the env (mlet s e1 e2)
                          [extended-closure-result
                           (eval-under-env (mlet (fun-formal (closure-fun closure-val)) arg-val closure-fun-body) closure-vals-env)])

                     ;here we evaluate the closures-functions-body (ie (fun-body closure-val))
                     ;using closure-val's environment
                     ;(eval-under-env closure-fun-body extended-closure)
                     ;closure-fun-body
                     ;closure-vals-env
                     ;arg-val
                     extended-closure-result
                   )
         ]

        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem C

(define (ifaunit e1 e2 e3) "CHANGE")

(define (mlet* lstlst e2) "CHANGE")

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem D

(define mupl-map "CHANGE")
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

(define mupl-mapAddN
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))
