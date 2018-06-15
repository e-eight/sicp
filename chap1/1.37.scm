;; Adapted from https://github.com/qiao/sicp-solutions/blob/master/chapter1/1.37.scm
;; The k-th term is N_k/D_k and all the previous terms are
;; N_i / (D_i + N_(i-1) / (D_(i-1) + ...)).


(define (cont-frac num denom terms)
  (define (recur index)
    (/ (num index)
       (+ (denom index)
	  (if (= terms index)
	      0
	      (recur (+ index 1))))))
  (recur 1))

;; This procedure needs just 10 terms to give the golden ratio upto 4
;; significant figures, and 100 terms for 11 significant figures.


(define (cont-frac-iter num denom terms)
  (define (iter index result)
    (if (= index 0)
	result
	(iter (- index 1) (/ (num index) (+ (denom index) result)))))
  (iter terms 0))
