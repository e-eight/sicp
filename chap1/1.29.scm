;; Simpson's Rule

;; Simpson's Rule is given by
;; Integral[f(x)] = (h/3)(y_0 + 4y_1 + 2y_2 + .... + 2y_{n-2} + 4y_{n-1} + y_n)
;;                = (h/3)(y_0 + 4(y_1 + y_3 + ... + y_{n-3} + y_{n-1})
;;                  + 2(y_2 + y_4 + ... + y_{n-4} + y_{n-2}) + y_n),
;; where h = (b - a) / n, with n being the number of function evaluations, and
;; a, and b are the limits of the integration. The y_k are the function
;; evaluations at points x_k = a + kh.

;; Essetially the crux of the problem is summing the two series.

;; Sum procedure from section 1.3.1

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

;; For the two series in Simpson's Rule, next will increase the index by 2, and
;; term is the y_k.

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (inc2 k) (+ k 2))
  (define (yk k) (f (+ a (* k h))))
  (*
   (/ h 3)
   (+
    (yk 0)
    (* 4 (sum yk 1 inc2 (- n 1)))
    (* 2 (sum yk 2 inc2 (- n 2)))
    (yk n))))

;; Function for testing

(define (cube x) (* x x x))

;; The accuracy of Simpson's Rule is far superior to the naive integration
;; procedure described in section 1.3.1. It gives 1/4 as the result for
;; integrating cube over the range [0,1] in as few as 8 function evaluations.
