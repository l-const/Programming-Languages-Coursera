#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file


; 1)
(define (sequence low high stride)
   (if (> low high)
    null
    (cons low (sequence (+ low stride) high stride))
   )
)

; 2)
(define (string-append-map xs suffix)
    (map (lambda(str) 
            (string-append str suffix)
         ) xs)
)


(string-append-map (list "a" "b" "c") "_")


; 3)
(define (list-nth-mod xs n)
    (cond [(< n 0) (error "list-nth-mod: negative number")]
          [(empty? xs) (error "list-nth-mod: empty list")]
          [#t (let* (
                    [len (length xs)]
                    [rm (remainder n len)]
                    ) (car (list-tail xs rm)))]  
    )
)

; 4)
; STREAMS
(define ones (lambda () (cons 1 ones)))
(define powers-of-two
    (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))


(define (stream-for-n-steps stream n) 
    (letrec ([csm (lambda (stream ans) 
                    (let 
                      ([count ans]
                      [str-res (stream)]) 
                        (if (= count 0)
                            null
                            (cons (car str-res) (csm (cdr str-res) (- count 1))))                   
                    )
                 )
             ])
     (csm stream n)
    ) 
)

(stream-for-n-steps ones 2)
(stream-for-n-steps powers-of-two 3)

; 5)
(define funny-number-stream 
    (letrec ([f (lambda (x) (if (= (modulo x 5) 0)
                                (cons (- 0 x) (lambda () (f (+ x 1))))
                                (cons x  (lambda () (f (+ x 1))))))])
        (lambda () (f 1))
))

(stream-for-n-steps funny-number-stream 16)

; 6)
(define dan-then-dog 
    (letrec ([f (lambda(x) (if (string=? x "dan.jpg") 
                               (cons "dog.jpg" (lambda ()(f "dog.jpg")))
                               (cons "dan.jpg" (lambda ()(f "dan.jpg")))
             ))])
             (lambda () (f "dog.jpg"))      
    )
)

(stream-for-n-steps dan-then-dog 4)

; 7)
(define (stream-add-zero stream)
    (letrec ([f (lambda() (cons (cons 0 (car (stream))) f)) 
            ]) 
      (lambda () (f))
    )
)

(stream-for-n-steps (stream-add-zero ones) 4)

; 8)
(define (cycle-lists xs ys)
    (letrec ([fun (lambda (xl yl)
                     (cond 
                           [(and (empty? (cdr xl) )(empty? (cdr yl)))(cons (cons (car xl) (car yl) ) (lambda() (fun xs ys)))]
                           [(empty? (cdr xl))(cons (cons (car xl) (car yl) ) (lambda() (fun  xs (cdr yl) )))]
                           [(empty? (cdr yl)) (cons (cons (car xl) (car yl) ) (lambda () (fun (cdr xl)  ys)))]
                           [#t (cons (cons (car xl) (car yl) ) (lambda () (fun (cdr xl) (cdr yl))))]
                           
                    )
    )])
    
    (lambda() (fun xs ys))
    )

)

(stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 8)

; 9)

(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (if (> n (- (vector-length vec) 1))
                    #f
                    (if (pair? (vector-ref vec n))
                        (if (equal? (car (vector-ref vec n)) v)
                            (vector-ref vec n)
                            (f (+ n 1)))
                        (f (+ n 1)))))])
    (f 0)))

; (10
(define (cached-assoc lst n)
  (let ([cache (make-vector n #f)]
        [c-slot 0])
    (lambda (v)
      (or (vector-assoc v cache)
          (let ([ans (assoc v lst)])
            (and ans
                 (begin (vector-set! cache c-slot ans)
                        (set! c-slot 
                              (if (= (+ c-slot 1) n)
                                  0
                                  (+ c-slot 1)))
                        ans)))))))

((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3)


; 11)
(define-syntax while-less 
    (syntax-rules (do)
        [(while-less e1 do e2)
            (let ([exp1 e1]
                  )
                (letrec ([loop (lambda ()
                    (if (< (- exp1 1) e2) 
                        #t
                        (loop)))])  
                 (loop))
        )]
    )    
)

;;; (define a 2)

;;; (while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
;;; (while-less 7 do (begin (set! a (+ a 1)) (print "x") a))