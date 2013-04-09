;;行コメント

#|
  #|
    ブロックコメントはネストできる
  |#
|#

;;S式コメント
#;(begin (display "Hello, ")
		(display "world"))


;;ex1
(define-simple-struct $label $LABEL $label
  (src    ; original source for debugging
   label  ; label. #f in Pass 2. Assigned in Pass 3.
   body   ; IForm for the body
))

;;ex2
;; 内部的な手続き
(define (%run-process proc argv iomap sigmask toclose wait fork)
  (if fork
      (let1 pid ...
	    (when wait
		  ;; 子プロセスがexitするまで待つ
		  (set! (ref proc 'status) (values-ref (sys-waitpid pid) 1))
		  ))))

;;ex3
;;; comile.scm - The compiler
;;;
;;;   Copylight (c) 2004-2007 Shiro Kawai <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without

   