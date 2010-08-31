(in-package #:math-extensions)

(defclass interval (numeric-set)
  ((minimum :initarg :minimum :reader minimum)
   (maximum :initarg :maximum :reader maximum)))

(defmethod difference ((interval interval))
  (with-slots (minimum maximum) interval
    (- maximum minimum)))

(defmethod initialize-instance :after ((object interval)
                                       &key minimum maximum difference
                                       &allow-other-keys)
  "Initializes the object from any two of the data. If fewer
  parameters are passed, an error is signaled. If all three are
  passed, one will be ignored. It is implementation-defined which one
  is ignored."
  (cond ((null minimum)
         (setf (slot-value object 'minimum) (- maximum difference)))
        ((null maximum)
         (setf (slot-value object 'maximum) (+ minimum difference)))))

(set-dispatch-macro-character #\# #\[
  (lambda (stream subchar arg)
    (declare (ignore subchar arg))
    (let* ((lower (read stream nil nil t))
           (char (read-char stream nil nil t))
           (str (make-string-output-stream)))
      (make-instance 'interval
                     :minimum lower
                     :maximum (read-from-string
                               (loop while (not (eql char #\]))
                                  do (write-char char str)
                                  (setf char (read-char stream nil nil t))
                                  finally
                                 (return (get-output-stream-string str))))))))

(defmethod print-object ((object interval) stream)
  (format stream "#[~d ~d]" (minimum object) (maximum object)))

(defun intervalp (number)
  (/= (difference number) 0))

(defmethod abs ((value interval))
  (with-slots (minimum maximum) value
    (cond
      ((< maximum 0) (make-instance 'interval
                                    :minimum (abs maximum)
                                    :maximum (abs minimum)))
      ((< minimum 0) (make-instance 'interval
                                    :minimum 0
                                    :maximum (max maximum (abs minimum))))
      ((= (abs minimum) (abs maximum)) (abs minimum))
      (t value))))

(defmethod binary-+ ((augend interval) (addend interval))
  (make-instance 'interval
                 :minimum (+ (minimum augend) (minimum addend))
                 :maximum (+ (maximum augend) (maximum addend))))

(define-commutative-method binary-+ ((augend interval) (addend real))
  (make-instance 'interval
                 :minimum (+ (minimum augend) addend)
                 :maximum (+ (maximum augend) addend)))

(defmethod binary-- ((minuend interval) (subtrahend interval))
  (make-instance 'interval
                 :minimum (- (minimum minuend) (maximum subtrahend))
                 :maximum (- (maximum minuend) (minimum subtrahend))))

(defmethod binary-* ((multiplicand interval) (multiplier interval))
  (let ((ac (* (minimum multiplicand) (minimum multiplier)))
        (ad (* (minimum multiplicand) (maximum multiplier)))
        (bc (* (maximum multiplicand) (minimum multiplier)))
        (bd (* (maximum multiplicand) (maximum multiplier))))
    (make-instance 'interval
                   :minimum (min ac ad bc bd)
                   :maximum (max ac ad bc bd))))

(define-commutative-method binary-* ((multiplicand interval) (multiplier real))
  (let ((a (* (minimum multiplicand) multiplier))
        (b (* (maximum multiplicand) multiplier)))
    (make-instance 'interval
                   :minimum (min a b)
                   :maximum (max a b))))

(defmethod binary-/ ((dividend interval) (divisor interval))
  (let ((ac (/ (minimum dividend) (minimum divisor)))
        (ad (/ (minimum dividend) (maximum divisor)))
        (bc (/ (maximum dividend) (minimum divisor)))
        (bd (/ (maximum dividend) (maximum divisor))))
    (make-instance 'interval
                   :minimum (min ac ad bc bd)
                   :maximum (max ac ad bc bd))))

(defmethod binary-/ ((dividend interval) (divisor real))
  (make-instance 'interval
    :minimum (/ (minimum dividend) divisor)
    :maximum (/ (maximum dividend) divisor)))

(defmethod binary-/ ((dividend real) (divisor interval))
  (make-instance 'interval
    :minimum (/ dividend (maximum divisor))
    :maximum (/ dividend (minimum divisor))))

(defmethod 1+ ((number interval))
  (make-instance 'interval
    :minimum (1+ (minimum number))
    :maximum (1+ (maximum number))))

(defmethod 1- ((number interval))
  (make-instance 'interval
    :minimum (1- (minimum number))
    :maximum (1- (maximum number))))

(defmethod conjugate ((number interval))
  number)

(defmethod signum ((number interval))
  (cond ((< (maximum number) 0) -1)
        ((> (minimum number) 0) 1)
        ((= (maximum number) 0) '(-1 0))
        ((= (minimum number) 0) '(0 1))
        (t '(-1 0 1))))

(defmethod expt ((base interval) (power real))
  )

(defmethod expt ((base interval) (power interval))
  (make-instance 'interval))

(defmethod member ((item real) (set interval) &key &allow-other-keys)
  (and (<= item (maximum set))
       (>= item (minimum set))))

(defmethod subset ((sub interval) (super interval))
  (and (<= (maximum sub) (maximum super))
       (>= (minimum sub) (minimum super))))

(defmethod intersection
           ((left interval) (right interval) &key &allow-other-keys)
  (cond ((subset left right) left)
        ((subset right left) right)
        ((member (minimum left) right)
         (make-instance 'interval
           :minimum (minimum left) :maximum (maximum right)))
        ((member (minimum right) left)
         (make-instance 'interval
           :minimum (minimum right) :maximum (maximum left)))
        (t '())))
(defmethod nintersection
           ((left interval) (right interval) &key &allow-other-keys)
  (nintersection left right))

(defmethod union ((left interval) (right interval) &key &allow-other-keys)
  (cond ((subset left right) right)
        ((subset right left) left)
        ((member (minimum left) right)
         (make-instance 'interval
           :minimum (minimum right) :maximum (maximum left)))
        ((member (minimum right) left)
         (make-instance 'interval
           :minimum (minimum left) :maximum (maximum right)))
        (t (list left right))))
(defmethod nunion ((left interval) (right interval) &key &allow-other-keys)
  (union left right))
