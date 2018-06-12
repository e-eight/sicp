;; Product of sequences

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))


;; Factorial in terms of product

;; n! = n(n-1)(n-2)...1

(define (factorial n)
  (product id 1 inc n))


;; Estimating pi by the Wallis formula

;; (pi / 4) = (2/3)(4/3)(4/5)(6/5)(6/7)(8/7)...

(define (pi-estimate n)
  (define (pi-term x)
    (if (odd? x)
	(/ (+ x 1) (+ x 2))
	(/ (+ x 2) (+ x 1))))
  (* 4 (product pi-term 1 inc n)))

;; This slowly converges to pi. For 100000 terms it gave 3.141608.


;; Recursive product

(define (recursive-product term a next b)
  (if (a > b)
      1
      (* (term a)
	 (recursive-product term (next a) next b))))


;; Helper procedures

(define (id x) x)
(define (inc x) (+ x 1))
(define (odd? x) (= (remainder x 2) 1))


