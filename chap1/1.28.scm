;; Miller-Rabin Test

;; The helper functions square and even?.

(define (square x) (* x x))
(define (even? n) (= (remainder n 2) 0))

;; Modified expmod procedure that signals zero when it finds a non-trivial
;; square root of 1.

(define (expmod base exp m)
  (define (non-trivial-root x n)
    (if
     (and
      (= (remainder (square x) n) 1)
      (not (= x 1))
      (not (= x (- n 1))))
     0
     x))
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder
	  (square
	   (non-trivial-root (expmod base (/ exp 2) m) m)) m))
	(else
	 (remainder (* base (expmod base (- exp 1) m)) m))))

;; Miller-Rabin Test using modified expmod

(define (millar-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (define (iter times)
    (cond ((= times 0) true)
	  ((try-it (+ 1 (random (- n 1)))) (iter (- times 1)))
	  (else false)))
  (iter 20))

;; This test works as advertised in the book. Even the Carmichael numbers cannot
;; fool it. Possibly a lesser number of iterations will give the same result,
;; but I do not know if there are any prescribed number of iterations.
