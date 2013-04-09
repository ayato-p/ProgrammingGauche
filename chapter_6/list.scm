((1 2) (3 4)) ;;error

'((1 2) (3 4)) ;;((1 2) (3 4))

(+ 1 2);; 3

(append '(1 2 3) '(4 5 6)) ;;(1 2 3 4 5 6)

'((1 2 3) (4 5 6)) ;;((1 2 3) (4 5 6))

'('(1 2 3) '(4 5 6)) ;;('(1 2 3) '(4 5 6))


;;basic operation
(car '(1 2 3)) ;; 1

(cdr '(1 2 3)) ;; (2 3)

(cdr '(2 3)) ;; (3)

(cdr '(3)) ;; ()

(cons 1 '(2 3)) ;; (1 2 3)

(cons 3 '()) ;; (3)

(cons 1 (cons 2 (cons 3 '()))) ;; (1 2 3)

(list 1 2 3) ;; (1 2 3)

(list 3) ;; (3)

(list) ;; ()

;;以下は等しくない
'(1 2 3), (list 1 2 3)

;;dotted pair
(cons 1 2) ;; (1 . 2)

(null? '(1 2 3)) ;; #f

(null? '()) ;; #t

(null? 3) ;; #f

(pair? '(1 2 3)) ;; #t

(pair? '()) ;; #f

(pair? 3) ;; #f


#|
・空リスト()は正式なリストである
・オブジェクトが対であり、そのcdrが正式なリストであれば、そのオブジェクトも正式なリストである
|#
(define (list? obj)
  (or (null? obj)
      (and (pair? obj) (list? (cdr obj)))))
;;test
(list? '(1 2 3))




;;list invesigation

(fold + 0 '(1 2 3 4 5)) ;; 15

(define (sum-of-numbers lis)
  (fold + 0 lis))

(sum-of-numbers '(1/2 1/3 1/4 1/5 1/6)) ;; 29/20

(sum-of-numbers '(1.23 4.56 7.89 0.13)) ;; 13.81


(define (product-of-numbers lis)
  (fold * 1 lis))

(product-of-numbers '( 1 2 3 4 5)) ;; 120

(product-of-numbers '(1/2 1/3 1/4 1/5 1/6)) ;; 1/720


(define (pick-greater a b)
  (if (> a b) a b))

(pick-greater 2 4) ;; 4

(pick-greater 4 2) ;; 4

(pick-greater -4 2) ;; 2

(define (max-number lis)
  (let ((pick-greater (lambda (a b)
			(if (> a b) a b))))
    (if (null? lis)
	(error "max-number needs at leat one number")
	(fold pick-greater -inf.0 lis))))

(max-number '(4 17 -2 0 9)) ;; 17
(max-number '()) ;; max-number needs at leat one number


(define (length lis)
  (define (increment2 a b)
    (display a)
    (display ",")
    (display b)
    (newline)
    (+ b 1))
  (fold increment2 0 lis))

(length '(10 20 30 40 50)) ;;5

(define (print-elements lis)
  (define (print-one-element a b) (print a))
  (fold print-one-element #f lis))

(print-elements '(1 2 3 7 6 5 4)) ;;1 2 3 7 6 5 4

(fold cons '() '(a b c d e))

;;fold
(define (fold proc init lis)
  (if (null? lis)
      init
      (fold proc (proc (car lis) init) (cdr lis))))

(fold + 0 '(1 2 3 4 5))

(define (last-pair lis)
  (if (pair? (cdr lis))
      (last-pair (cdr lis))
      lis))

(last-pair '(1 2 3))

(last-pair '(1))

(pair? (last-pair '(1 2 . 3)))

(define (copy-list lis)
  (if (pair? lis)
      (cons (car lis) (copy-list (cdr lis)))
      lis))

(copy-list '( (1 2 3) (4 5) 6))

(define (deep-copy-list lis)
  (if (pair? lis)
      (if (pair? (car lis))
	  (cons (deep-copy-list (car lis))
		(deep-copy-list (cdr lis)))
	  (cons (car lis)
		(deep-copy-list (cdr lis))))
      lis))

(deep-copy-list '(1 (2 3) ((4 5) 6) 7))


(define (append2 a b)
  (if (pair? a)
      (cons (car a)
	    (append2 (cdr a) b))
      b))

(append2 '(1 2 3) '(4 5 6))

(define (reverse lis)
  (fold cons '() lis))

(reverse '( 1 2 3))


(define (find pred lis)
  (cond
   [(null? lis) #f]
   [(pred (car lis)) (car lis)]
   [else
    (find pred (cdr lis))]))

(find odd? '(2 4 3 5 ))

(find char-alphabetic? '(#\1 #\2 #\3 #\t #\5))

(define (length lis)
  (if (null? lis)
      0
      (+ 1 (length (cdr lis)))))

(define (length lis)
  (define (length-rec lis n)
    (if (null? lis)
	n
	(length-rec (cdr lis) (+ n 1))))
  (length-rec lis 0))

(length '(1 2 3))
    
