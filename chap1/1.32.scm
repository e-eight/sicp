;; Sum and Product as specific cases of a more general Accumulate

(define (accumulate combiner null-value term a next b)
  (define (iter a null-value)
    (if (> a b)
	null-value
	(iter (next a) (combiner (term a) null-value))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;; As a test we can define factorial as before.

(define (factorial n)
  (product id 1 inc n))

;; Helper procedures for testing

(define (inc x) (+ x 1))
(define (id x) x)


;; Recursive Accumulate

(define (recursive-accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(recursive-accumulate
		 combiner null-value term (next a) next b))))

