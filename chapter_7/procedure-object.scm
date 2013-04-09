(define (sum-of-numbers lis)
  (fold + 0 lis))

(define x sum-of-numbers)
(x '(1 2 3 4 5))

(define square-sum (lambda (a b) (+ (* a a) (* b b))))

(square-sum 3 4) ;; 25

(define (length lis)
  (define (increment2 a b) (+ b 1))
  (fold increment2 0 lis))

(define (length lis)
  (fold (lambda (a b) (+ b 1)) 0 lis))

(define (max-number lis)
  (if (null? lis)
      (error "max-number needs at least one number")
      (fold (lambda (a b) (if (> a b) a b)) (car lis) (cdr lis))))

(define (print-elements lis)
  (fold (lambda (a b) (print a)) #f lis))


;;for-each
(for-each (lambda (x) (print "> " x)) '(a b c))

(define (tree-walk walker proc tree)
  (walker (lambda (elt)
	    (if (list? elt)
		(tree-walk walker proc elt)
		(proc elt)))
	  tree))

(tree-walk for-each print
	   '((1 2 3) 4 5 (6 (7 8))))

(define (reverse-for-each proc lis)
  (for-each proc (reverse lis)))

(tree-walk reverse-for-each print
	   '((1 2 3) 4 5 (6 (7 8))))


;;map
(map (lambda (x) (* x 2)) '(1 2 3 4))

(tree-walk map (lambda (x) (* 2 x))
	   '((1 2 3) 4 5 (6 (7 8))))

(define (reverse-map proc lis)
  (map proc (reverse lis)))

(tree-walk reverse-map identity
	   '((1 2 3) 4 5 (6 (7 8))))

;;reversed
(define (reverse-xxx proc lis)
  (xxx proc (reverse lis)))

(define (reversed walker)
  (lambda (proc lis)
    (walker proc (reverse lis))))

(define reverse-for-each (reversed for-each))

(define reverse-map (reversed map))

(tree-walk (reversed map) identity
	   '((1 2 3) 4 5 (6 (7 8))))

;;練習問題
(define (for-each-numbers proc lis)
  (for-each proc (filter number? lis)))

(define (map-numbers proc lis)
  (map proc (filter number? lis)))

(define (numbers-only f)
  (lambda (proc lis)
    (f proc (filter number? lis))))

(define (numbers-only-for-tree f)
  (lambda (proc lis)
    (f proc (filter (lambda (x)
		      (or (list? x)
			  (number? x)))
		    lis))))

;;test
(for-each-numbers print '(1 2 #f 3 4 #t))

(map-numbers (lambda (x) (* 2 x)) '(1 2 #f 3 4 #t))

((numbers-only map) (lambda (x) (* 2 x)) '(1 2 #f 3 4 #t))

(tree-walk (numbers-only-for-tree map) (lambda (x) x)
	   '((1 #f #t 2 3) 4 5 (6 (7 #\t 8))))


;;local-variable
((lambda (a b) (+ (* a a) (* b b))) 3 4) ;;25

(let ((a 3)
      (b 4))
  (+ (* a a) (* b b))) ;;25

(let ((a 3)
      (b 4))
  (let ((a b)
	(b a))
    (cons a b)))

(let ((x (print 1))
      (y (print 2)))
  (print 3))

(let* ((x (some-function1))
       (y (some-function2 x))
       (z (some-function3 y)))
  (some-function4 z))

(letrec ((a 1)
	 (b (lambda (x) (+ a x))))
  (b 4))

(letrec ((sum (lambda (lis)
		(cond ((null? lis) 0)
		      ((number? (car lis)) (+ (car lis) (sum (cdr lis))))
		      (else (sum (cdr lis)))))))
  (sum '(1 3 #f 6 #t 9)))

(letrec ((even? (lambda (n)
		  (cond ((zero? n) #t)
			((> n 0) (odd? (- n 1)))
			(else    (odd? (+ n 1))))))
	 (odd? (lambda (n)
		 (cond ((zero? n) #f)
		       ((> n 0) (even? (- n 1)))
		       (else    (even? (+ n 1)))))))
  (even? 10))

;;variable-length
(define (func a b .c) ...)

(define (func a b . c) (print "a=" a ", b=" b ", c=" c))
(func 1 2)
(func 1 2 3)
(func 1 2 3 4)
(func 1 2 3 4 5)

;;練習問題
(define (list . l)
  (letrec ((iter (lambda (l)
		   (cond [(null? l) ()]
			 [else (cons (car l) (iter (cdr l)))]))))
    (iter l)))

(list 1 2 3)
(list 1)
(list)


;;variable-length pass
(define (append/log . args)
  (print "args=" args)
  (apply append args))

(append/log '(a b c) '(1 2 3) '(7 8 9))

(define (make-logger func)
  (lambda args
    (print "args=" args)
    (apply func args)))

(define append/log (make-logger append))

(append/log '(1 2 3) '(a b c) '(X Y Z))

(define cons/log (make-logger cons))

(cons/log 1 2)

(define fold/log (make-logger fold))

(fold/log + 0 '(1 2 3 4 5))


;;patern match
(define (append2 a b)
  (if (null? a)
      b
      (cons (car a) (append2 (cdr a) b))))

(define (append . args)
  (cond ((null? args) '())
	((null? (car args)) (car args))
	(else (append2 (car args) (apply append (cdr args))))))


(use util.match)
(define (append . args)
  (match args
	 (() '())
	 ((a) a)
	 ((a . b) (append2 a (apply append b)))))

(append '(1 2 3) '(a b c) '(X Y Z))


;;keyword
(define (make-list num . args)
  (define (maker n init)
    (if (zero? n)
	'()
	(cons init (maker (- n 1) init))))
  (maker num (if (null? args) #f (car args))))

(make-list 10)
(make-list 10 'a)

(define (make-list num . args)
  (let-optionals* args ((init #f))
		  (define (maker n)
		    (if (zero? n)
			'()
			(cons init (maker (- n 1)))))
		  (maker num)))


(define (person . args)
  (let-keywords args ((name "Anonymous")
		      (age  "unknown")
		      . other-info)
		(print name " is " age " year(s) old.")
		(print "Other info: " other-info)))

(person :name "ayato" :age 21 :born-in "Hawaii")
(person :name "ayato")
(person)

;;

(find (lambda (num) (< num 3)) lis)

;;↓

(use srfi-26)
(define (cut-test lis)
  (find (cut < <> 3) lis))

(let ((func (lambda (x) x))
      (x 10))
  (cut func <> x <...>))

