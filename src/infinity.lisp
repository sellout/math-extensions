(in-package #:math-extensions)

(defclass infinity () ())

(defconstant negative-infinity (make-instance 'infinity))
(defconstant positive-infinity (make-instance 'infinity))

;;; Defines negative-infinity and :positive-infinity

(define-commutative-method binary-+ ((augend (eql negative-infinity)) addend)
  negative-infinity)

(define-commutative-method binary-+ ((augend (eql positive-infinity)) addend)
  positive-infinity)

(define-commutative-method binary-+
    ((augend (eql positive-infinity)) (addend (eql negative-infinity)))
  (error "Can not add alternate infinities."))

(define-commutative-method binary-*
    ((multiplicand (eql negative-infinity)) multiplier)
  (case (signum multiplier)
    (-1 positive-infinity)
    (0 (error "Can not multiply infinity by 0."))
    (1 negative-infinity)))

(define-commutative-method binary-*
    ((multiplicand (eql positive-infinity)) multiplier)
  (case (signum multiplier)
    (-1 negative-infinity)
    (0 (error "Can not multiply infinity by 0."))
    (1 positive-infinity)))

(defmethod binary-* ((multiplicand (eql positive-infinity))
                     (multiplier (eql negative-infinity)))
  negative-infinity)

(defmethod binary-* ((multiplicand (eql negative-infinity))
                     (multiplier (eql positive-infinity)))
  positive-infinity)

(defmethod binary-/ (dividend (divisor infinity))
  0)

(defmethod binary-/ ((dividend infinity) (divisor infinity))
  (error "Can't divide infinity by itself."))

(defmethod 1+ ((number infinity))
  number)

(defmethod 1- ((number infinity))
  number)

(defmethod expt ((base infinity) (exponent (eql 0)))
  (error "Can't raise infinity to the 0 power."))

(defmethod expt (base (exponent (eql positive-infinity)))
  (cond ((< base -1) (error "I don't know."))
        ((< base 1) 0)
        (t positive-infinity)))

(defmethod expt ((base (eql 1)) (exponent infinity))
  (error "Can't raise 1 to an infinite power."))

;; (defmethod < ((arg1 (eql negative-infinity)) arg2)
;;   t)

;; (defmethod < (arg1 (arg2 (eql negative-infinity)))
;;   nil)

;; (defmethod < ((arg1 (eql negative-infinity)) (arg2 (eql negative-infinity)))
;;   nil)

;; (defmethod <= ((arg1 (eql negative-infinity)) arg2)
;;   t)

;; (defmethod <= (arg1 (arg2 (eql negative-infinity)))
;;   nil)

;; (defmethod <= ((arg1 (eql negative-infinity)) (arg2 (eql negative-infinity)))
;;   t)

;; (defmethod > ((arg1 (eql negative-infinity)) arg2)
;;   nil)

;; (defmethod > (arg1 (arg2 (eql negative-infinity)))
;;   t)

;; (defmethod > ((arg1 (eql negative-infinity)) (arg2 (eql negative-infinity)))
;;   nil)

;; (defmethod >= ((arg1 (eql negative-infinity)) arg2)
;;   nil)

;; (defmethod >= (arg1 (arg2 (eql negative-infinity)))
;;   t)

;; (defmethod >= ((arg1 (eql negative-infinity)) (arg2 (eql negative-infinity)))
;;   t)









;; (defmethod < ((arg1 (eql positive-infinity)) arg2)
;;   nil)

;; (defmethod < (arg1 (arg2 (eql positive-infinity)))
;;   t)

;; (defmethod < ((arg1 (eql positive-infinity)) (arg2 (eql positive-infinity)))
;;   nil)

;; (defmethod <= ((arg1 (eql positive-infinity)) arg2)
;;   nil)

;; (defmethod <= (arg1 (arg2 (eql positive-infinity)))
;;   t)

;; (defmethod <= ((arg1 (eql positive-infinity)) (arg2 (eql positive-infinity)))
;;   t)

;; (defmethod > ((arg1 (eql positive-infinity)) arg2)
;;   t)

;; (defmethod > (arg1 (arg2 (eql positive-infinity)))
;;   nil)

;; (defmethod > ((arg1 (eql positive-infinity)) (arg2 (eql positive-infinity)))
;;   nil)

;; (defmethod >= ((arg1 (eql positive-infinity)) arg2)
;;   t)

;; (defmethod >= (arg1 (arg2 (eql positive-infinity)))
;;   nil)

;; (defmethod >= ((arg1 (eql positive-infinity)) (arg2 (eql positive-infinity)))
;;   t)
