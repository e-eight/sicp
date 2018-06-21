(define (make-center-percent c p)
  (let ((w (/ (* p c) 100)))
    (make-center-width c w)))

(define (percent i)
  (* (/ (width i) (center i)) 100))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-interval a b) (cons a b))
(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))
