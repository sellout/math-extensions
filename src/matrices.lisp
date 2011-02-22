(in-package #:math-extensions)

(defclass matrix ()
  ())

(defmethod binary-+ ((first matrix) (second matrix))
  (map-matrix #'(lambda (row column)
		  (+ (entry first row column) (entry second row column)))
	      first))

(defmethod binary-* ((first matrix) (second matrix))
  (let ((result (make-instance 'matrix
			       :rows (rows first) :columns (columns second))))
    (map-matrix (lambda (row column)
                  (setf (entry result row column)
                        (summation (m 1 (columns first))
                                   (mult (entry first row m)
                                         (entry second m column)))
                        result)))