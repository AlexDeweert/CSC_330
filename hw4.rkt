#lang racket

; 0) cube an integer
(define (cube x)
  (* x x x))

; 1) sequence
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride ))))

; 2) string-append-map
(define (string-append-map xs suffix)
  (map (lambda(x) (string-append x suffix)) xs))

; 3) list-nth-mod
;TODO make an error handler for the test file
(define (list-nth-mod xs n)
  (cond [ (< n 0) (error "list-nth-mod: negative number")]
        [ (eq? xs null) (error "list-nth-mod: empty list")]
        [ #t (list-ref xs (remainder n (length xs)))]))


; 4) stream-for-n-steps
(define (stream-for-n-steps stream n)
  ( letrec ([f (lambda (stream acc)
               ( if ( = n (length acc))
               (reverse acc)
               (f (cdr (stream)) (cons (car (stream)) acc ))))
             ])
    (f stream '() )))

; 5) funny-number-stream
(define funny-number-stream
  (
   letrec(
          [ f (lambda(x)
                (cons x
                      (lambda() (f (if (= 0 (remainder (+ x 1) 5))
                                   ;then
                                   (* -1 (+ x 1))
                                   ;else
                                   ( if (< (+ x 1) 0)
                                     ;then
                                     (* -1 (- x 1))
                                     ;else
                                     (+ x 1 )))
                                 )
                      )
                )
              )
          ]
         )
   (lambda() (f 1))
  )
)

; 6) cat-then-dog
(define cat-then-dog (
   letrec( [ f (lambda(x) (cons x (lambda() (f (if (string=? x "cat.jpg") "dog.jpg" "cat.jpg" )) )))])
   (lambda() (f "cat.jpg"))
  )
)

; try to merge two streams (zip per Ben)
; 7) stream-add-zero
(define (stream-add-zero s)
   (letrec( [ return-stream (lambda(x) (stream-add-zero (cdr s)))]))
   (cons ((cons 0 (car s)))return-stream)
)



(provide sequence)
(provide string-append-map)
(provide list-nth-mod)
(provide stream-for-n-steps)
(provide funny-number-stream)
(provide cat-then-dog)