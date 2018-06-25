(define (reverse list1)
  (if (null? list1)
      () ;; nil
      (append (reverse (cdr list1))
	      (list (car list1)))))
