;; Iterative Sum

;; In each iteration result -> result + term a, and a -> next a. The procedure
;; stops when a > b.

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))
