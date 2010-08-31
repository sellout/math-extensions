(in-package #:math-extensions)

;;; This adds support for the new functions provided by this library
;;; to the existing CL real numbers.

;;; interval functions
(defmethod minimum ((interval real))
  interval)

(defmethod maximum ((interval real))
  interval)

(defmethod difference ((interval real))
  0)

;;; numeric-set functions

(defmethod +- ((value real))
  (make-instance 'enumerated-set :initial-contents (list (- value) value)))

;;; quaternion and octonion functions
(defmethod scalar ((number real))
  number)
(defmethod i ((number real))
  0)
(defmethod j ((number real))
  0)
(defmethod k ((number real))
  0)
(defmethod l ((number real))
  0)
(defmethod il ((number real))
  0)
(defmethod jl ((number real))
  0)
(defmethod kl ((number real))
  0)

(defmethod scalar ((number complex))
  (realpart number))
(defmethod i ((number complex))
  (imagpart number))
(defmethod j ((number complex))
  0)
(defmethod k ((number complex))
  0)
(defmethod l ((number complex))
  0)
(defmethod il ((number complex))
  0)
(defmethod jl ((number complex))
  0)
(defmethod kl ((number complex))
  0)
