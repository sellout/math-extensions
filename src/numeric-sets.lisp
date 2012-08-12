(in-package #:math-extensions)

(defclass numeric-set ()
  ())

(defclass enumerated-set (numeric-set)
  ((values :initform ())))

(defgeneric add-value (value set)
  (:method (value (set enumerated-set))
    (when (not (find value (slot-value set 'values)))
      (push value (slot-value set 'values))))
  (:method ((value enumerated-set) (set enumerated-set))
    (mapcar (lambda (v) (add-value v set))
            (slot-value value 'values))))

(defmethod initialize-instance :after ((object enumerated-set)
                                       &key initial-contents &allow-other-keys)
  (mapc (lambda (value)
          (add-value value object))
        (remove-duplicates initial-contents :test #'=)))

(set-dispatch-macro-character
 #\# #\{
 (lambda (stream subchar arg)
   (declare (ignore subchar arg))
   (let* ((min (read stream nil nil t))
          (char (read-char stream nil nil t))
          (str (make-string-output-stream)))
     (apply #'make-set
            min
            (list (read-from-string
                   (loop while (not (eql char #\}))
                      do (write-char char str)
                      (setf char (read-char stream nil nil t))
                      finally (return (get-output-stream-string
                                       str)))))))))

(defmethod print-object ((object enumerated-set) stream)
  (format stream "#{~{~a~^ ~}}" (slot-value object 'values)))

(defun make-set (&rest values)
  (let ((unique-values (remove-duplicates values :test #'=)))
    (if (= 1 (length unique-values))
        (car unique-values)
        (make-instance 'enumerated-set :initial-contents unique-values))))

(defmethod map ((result-type (eql 'enumerated-set)) function &rest sequences)
  (apply #'make-set (apply #'map 'list function sequences)))

(defmethod +- ((value enumerated-set))
  (map 'enumerated-set #'+- (slot-value value 'values)))

;;; Comparison

(define-commutative-method binary-= ((left enumerated-set) (right number))
  nil)

(defmethod binary-= ((left enumerated-set) (right enumerated-set))
  (not (slot-value (set-exclusive-or left right :test #'=) 'values)))

;;; Rounding

(defmacro make-set-rounding-function (name)
  `(defmethod ,name ((number enumerated-set) &optional (divisor 1))
     (let ((results (mapcar (lambda (num) (multiple-value-list (,name num divisor)))
                            (slot-value number 'values))))
       (values (map 'enumerated-set #'first results)
               (map 'enumerated-set #'second results)))))

(make-set-rounding-function floor)
(make-set-rounding-function ffloor)
(make-set-rounding-function ceiling)
(make-set-rounding-function fceiling)
(make-set-rounding-function truncate)
(make-set-rounding-function ftruncate)
(make-set-rounding-function round)
(make-set-rounding-function fround)

;;; Arithmetic

(define-commutative-method binary-* ((multiplicand enumerated-set) multiplier)
  (map 'enumerated-set
       (lambda (number) (binary-* number multiplier))
       (slot-value multiplicand 'values)))

(defmethod binary-* ((multiplicand enumerated-set) (multiplier enumerated-set))
  (reduce (lambda (first second) (union first second :test #'=))
          (mapcar (lambda (number) (binary-* multiplicand number))
                  (slot-value multiplier 'values))))

(define-commutative-method binary-+ ((augend enumerated-set) addend)
  (map 'enumerated-set
       (lambda (number) (binary-+ number addend))
       (slot-value augend 'values)))

(defmethod binary-+ ((augend enumerated-set) (addend enumerated-set))
  (reduce (lambda (first second) (union first second :test #'=))
          (mapcar (lambda (number) (binary-+ augend number))
                  (slot-value addend 'values))))

(defmethod unary-- ((number enumerated-set))
  (map 'enumerated-set #'unary-- (slot-value number 'values)))

(defmethod binary-- ((minuend enumerated-set) subtrahend)
  (map 'enumerated-set
       (lambda (number) (binary-- number subtrahend))
       (slot-value minuend 'values)))

(defmethod binary-- (minuend (subtrahend enumerated-set))
  (map 'enumerated-set
       (lambda (number) (binary-- minuend number))
       (slot-value subtrahend 'values)))

(defmethod binary-- ((minuend enumerated-set) (subtrahend enumerated-set))
  (reduce (lambda (first second) (union first second :test #'=))
          (mapcar (lambda (number) (binary-- minuend number))
                  (slot-value subtrahend 'values))))

(defmethod unary-/ ((number enumerated-set))
  (map 'enumerated-set #'unary-/ (slot-value number 'values)))

(defmethod binary-/ ((dividend enumerated-set) divisor)
  (map 'enumerated-set
       (lambda (number) (binary-/ number divisor))
       (slot-value dividend 'values)))

(defmethod binary-/ (dividend (divisor enumerated-set))
  (map 'enumerated-set
       (lambda (number) (binary-/ dividend number))
       (slot-value divisor 'values)))

(defmethod binary-/ ((dividend enumerated-set) (divisor enumerated-set))
  (reduce (lambda (first second) (union first second :test #'=))
          (mapcar (lambda (number) (binary-/ dividend number))
                  (slot-value divisor 'values))))

(defmethod 1+ ((number enumerated-set))
  (map 'enumerated-set #'1+ (slot-value number 'values)))

(defmethod 1- ((number enumerated-set))
  (map 'enumerated-set #'1+ (slot-value number 'values)))

(defmethod abs ((value enumerated-set))
  (map 'enumerated-set #'abs (slot-value value 'values)))

(defmethod exp ((power enumerated-set))
  (map 'enumerated-set #'exp (slot-value power 'values)))

(defmethod expt ((base enumerated-set) power)
  (map 'enumerated-set
       (lambda (number) (expt number power))
       (slot-value base 'values)))

(defmethod expt (base (power enumerated-set))
  (map 'enumerated-set
       (lambda (number) (expt base number))
       (slot-value power 'values)))

(defmethod expt ((base enumerated-set) (power enumerated-set))
  (reduce (lambda (first second) (union first second :test #'=))
          (mapcar (lambda (number) (expt base number))
                  (slot-value power 'values))))

(defmethod log ((number enumerated-set) &optional base)
  (if base
      (map 'enumerated-set
           (lambda (num) (log num base))
           (slot-value number 'values))
      (map 'enumerated-set #'log (slot-value number 'values))))

(defmethod sqrt ((number enumerated-set))
  (map 'enumerated-set #'sqrt (slot-value number 'values)))

;;; SET OPERATIONS

(defmethod size ((set enumerated-set))
  (length (slot-value set 'values)))

(defmethod intersection
    ((list-1 enumerated-set) (list-2 enumerated-set) &rest args)
  (apply #'make-set (apply #'intersection
                           (slot-value list-1 'values)
                           (slot-value list-2 'values)
                           args)))

(defmethod set-difference
    ((list-1 enumerated-set) (list-2 enumerated-set) &rest args)
  (apply #'make-set (apply #'set-difference
                           (slot-value list-1 'values)
                           (slot-value list-2 'values)
                           args)))

(defmethod set-exclusive-or
    ((list-1 enumerated-set) (list-2 enumerated-set) &rest args)
  (apply #'make-set (apply #'set-exclusive-or
                           (slot-value list-1 'values)
                           (slot-value list-2 'values)
                           args)))

(defmethod union ((list-1 enumerated-set) (list-2 enumerated-set) &rest args)
  (apply #'make-set (apply #'union
                           (slot-value list-1 'values)
                           (slot-value list-2 'values)
                           args)))
