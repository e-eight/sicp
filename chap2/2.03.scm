;; A rectangle can be implemented in multiple ways:
;;
;; 1. As a list of one of the vertices, and the width and the height of the
;; rectangle. The entire rectangle can be constructed from this
;; information. However to avoid ambiguity we need to adopt a convention
;; regarding which of the four vertices we will use. Here I adopt the convention
;; of using the upper-left vertex.
;;
;; 2. The second convention involves identifying the rectangle with one of its
;; diagonals, or equivalently the starting and ending points of one of its
;; diagonals. Again we need to specify the convention. I will use the diagonal
;; joining the upper-left and  lower-right vertices.
;;
;; 3. Instead of supplying the width and the height of the rectangle, as in the
;; first implementation, we can supply the area and the perimeter. Depending on
;; how the rectangle object is being used this might be a more useful
;; representation than the first one.
;;
;; I will just implement the first two here.

(define (make-rectangle-1 upper-left width height)
  (cons upper-left (cons width height)))

(define (make-rectangle-2 diagonal)
  (cons (start-segment diagonal)
	(end-segment diagonal)))

(define (area rect)
  (* (width rect) (height rect)))

(define (perimeter rect)
  (* 2 (+ (width rect) (height rect))))

;; For the first implementation
;; (define (width rect) (car (cdr rect)))
;; (define (height rect) (cdr (cdr rect)))

;; For the second implementation
(define (width rect)
  (let ((end (cdr rect))
	(start (car rect)))
    (- (x-point end) (x-point start))))
(define (height rect)
  (let ((start (car rect))
	(end (cdr rect)))
    (- (y-point start) (y-point end))))


(define (make-segment start end)
  (cons start end))

(define (start-segment line) (car line))
(define (end-segment line) (cdr line))

(define (make-point x y)
  (cons x y))

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
