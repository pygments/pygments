#lang racket

;; Note: This Racket file has some deliberate errors (such as invalid
;; number literals) to demonstrate the lexer correctly highlighting
;; them as errors.

; Single-line comment style.

;; Single-line comment style.

#| Multi-line comment style ... on one line |#

#|
Multi-line comment style ...
... on multiple lines
|#

(define (a-function x #:keyword [y 0])
  (define foo0 'symbol) ; ()
  [define foo1 'symbol] ; []
  {define foo2 'symbol} ; {}
  (and (append (car '(1 2 3))))
  (regexp-match? #rx"foobar" "foobar")
  (regexp-match? #px"foobar" "foobar")
  (define a 1))
  (let ([b "foo"])
    (displayln b))
  (for/list ([x (in-list (list 1 2 (list 3 4)))])
      (cond
       [(pair? x) (car x)]
       [else x])))

;; Literal number examples
(values
 ;; #b
 #b1.1
 #b-1.1
 #b1e1
 #b0/1
 #b1/1
 #b1e-1
 #b101
 #b2     ;highlight as error
 
 ;; #d
 #d-1.23
 #d1.123
 #d1e3
 #d1e-22
 #d1/2
 #d-1/2
 #d1
 #d-1
 #dZ     ;highlight as error

 ;; No # reader prefix -- same as #d
 -1.23
 1.123
 1e3
 1e-22
 1/2
 -1/2
 1
 -1

 ;; #e
 #e-1.23
 #e1.123
 #e1e3
 #e1e-22
 #e1
 #e-1
 #e1/2
 #e-1/2
 #eZ     ;highlight as error

 ;; #i always float
 #i-1.23
 #i1.123
 #i1e3
 #i1e-22
 #i1/2
 #i-1/2
 #i1
 #i-1
 #iZ     ;highlight as error

 ;; #o
 #o777.777
 #o-777.777
 #o777e777
 #o777e-777
 #o3/7
 #o-3/7
 #o777
 #o-777
 #o8     ;highlight as error

 ;; #x
 #x-f.f
 #xf.f
 #x-f
 #xf
 #xG     ;highlight as error
 )
