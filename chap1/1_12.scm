;; Pascal's triangle
;;       1
;;      1 1
;;     1 2 1
;;    1 3 3 1
;;   1 4 6 4 1
;; ------------
;;
;; We can restructure this as a table.
;;
;; -----------
;; |1| | | | |
;; |1|1| | | |
;; |1|2|1| | |
;; |1|3|3|1| |
;; |1|4|6|4|1|
;; -----------
;;
;; The table form makes it quite clear that the elements of
;; the triangle are given by
;; f(row, column) = 1, if column=1 or row=column,
;; f(row, column) = f(row-1, column-1) + f(row-1, column),
;; otherwise.

(define (pascals-triangle row column)
  (cond ((or (= column 1) (= row column)) 1)
	(else (+ (pascals-triangle (- row 1) (- column 1))
		 (pascals-triangle (- row 1) column)))))
