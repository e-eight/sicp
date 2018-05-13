;; Fast iterative multiplication
;;
;; This is an iterative algorithm that computes integer
;; multiplication in a logarithmic number of steps using
;; successive doubling.
;;
;; As this is an iterative algorith, to compute a*b, we will
;; need to additional state variables, product and count,
;; with the starting state being product = 0 and count = b/2
;; (for even b). The transformation step will be product ->
;; product + 2*a and count -> count - 1. The process will
;; stop when count is zero and the answer will be given by
;; the then value of product. It can be easily generalized
;; to odd b as a*b = a + a*(b-1).

(define (fast-mul-iter a b)
  (define (even? x)
    (= (remainder x 2) 0))
  (define (double x)
    (* x 2))
  (define (half x)
    (/ x 2))
  (define (iter product count)
    (cond ((= count 0) product)
	  (else (iter (+ product (double a)) (- count 1)))))
  (cond ((even? b) (iter 0 (half b)))
	(else (+ b (iter 0 (half (- b 1)))))))
