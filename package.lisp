(defpackage math-extensions
  (:documentation "An expansion of the usual math functionality
  that's a part of Common Lisp.")
  #.(loom.utilities:use-with-cl :numbers :conses)
  (:export #:define-commutative-method
           #:numeric-set
           #:enumerated-set #:+-
           #:interval #:minimum #:maximum #:difference
           #:quaternion #:scalar #:i #:j #:k
           #:octonion #:l #:il #:jl #:kl))

(in-package #:math-extensions)

(defgeneric +- (value)
  (:documentation "Given X, returns the set of +X and -X."))

(defgeneric minimum (interval)
  (:documentation "Returns the smallest value in an interval."))
(defgeneric maximum (interval)
  (:documentation "Returns the largest value in an interval."))
(defgeneric difference (interval)
  (:documentation "Returns the difference between the smallest and largest values in an
                   interval."))

(defgeneric scalar (number)
  (:documentation "Returns the scalar dimension of a number (alias for REALPART)."))
(defgeneric i (number)
  (:documentation "Returns the imaginary dimension of a number (alias for IMAGPART)."))
(defgeneric j (number)
  (:documentation "Returns the j dimension of a number."))
(defgeneric k (number)
  (:documentation "Returns the k dimension of a number."))
(defgeneric l (number)
  (:documentation "Returns the l dimension of a number."))
(defgeneric il (number)
  (:documentation "Returns the il dimension of a number."))
(defgeneric jl (number)
  (:documentation "Returns the jl dimension of a number."))
(defgeneric kl (number)
  (:documentation "Returns the kl dimension of a number."))
