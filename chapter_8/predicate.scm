(equal? (list 1 2 3) (list 1 2 3)) ;;#t

(equal? (list 1 2 (list 3 4) 5) (cons 1 (cons 2 '((3 4) . (5))))) ;;#t

(equal? (list 1 2 3) (list 1 2 4)) ;;#f


(eq? (cons 1 2) (cons 1 2)) ;;#f

(define p (cons 1 2))

(eq? p p) ;;#t


'(x y z)
'xyz

(eq? 'xyz 'xyz) ;;#t


(eqv? 1 1) ;;#t

(eqv? 1.0 1.0) ;;#t

(eqv? 1 1.0) ;;#f


(= 1 1.0) ;;#t


(char=? #\a #\a) ;;#t

(char=? #\a #\A) ;;#f

(string=? "abc" "abc") ;;#t

(string=? "abc" "ABC") ;;#f


(char-ci=? #\a #\A) ;;#t

(string-ci=? "abc" "ABC") ;;#t


(define p (cons 1 2))

(define a (list p p))

(define b (list (cons 1 2) (cons 1 2)))

(equal? a b) ;;#t

(use util.isomorph)
(isomorphic? a b) ;;#f

(isomorphic? a (list p p)) ;;#t

(use srfi-1)
(lset= eqv? '(1 2 3) '(3 2 1)) ;;#t

(not (= 2 3)) ;;#t

(not (= 2 2)) ;;#f

(use srfi-1)
(any odd? '(1 2 3 4)) ;;#t

(any odd? '(2 4 8)) ;;#f 

(every odd? '(1 2 3 4)) ;;#f

(every odd? '(1 3 5)) ;;#t


;;any-pred, every-pred
(define positive-integer? (every-pred integer? positive?))

(positive-integer? 4)

(positive-integer? -3)

(positive-integer? 2.4)

;;complement
(define nonnegative? (complement negative?))

(nonnegative? 2)

(nonnegative? 0)

(nonnegative? -1)

(define (complement pred)
  (lambda (x)
    (not (pred x))))

;;練習問題
(use util.match)
(define (any-pred . args)
  (lambda (x)
    (match args
	   (() #f)
	   ((a) (a x))
	   ((a . b) (or (a x)
			((apply any-pred b) x))))))

(define (every-pred . args)
  (lambda (x)
    (match args
	   (() #f)
	   ((a) (a x))
	   ((a . b) (and (a x)
			 ((apply every-pred b) x))))))

(define positive-or-integer? (any-pred integer? positive?))
(positive-or-integer? 4)
(positive-or-integer? -4)
(positive-or-integer? 0.1)
(positive-or-integer? -0.1)

(use util.match)
(letrec ((sum (lambda (lis)
		(match lis
		       [() 0]
		       [(a . b) (+ a (sum b))]))))
  (sum (iota 100 1)))




(if (eqv? 1 2) 'foo)

