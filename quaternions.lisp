(in-package #:math-extensions)

(defclass quaternion ()
  ((scalar :initform 0 :initarg :scalar :reader scalar :reader realpart :type real)
   (i :initform 0 :initarg :i :reader i :type real :reader imagpart)
   (j :initform 0 :initarg :j :reader j :type real)
   (k :initform 0 :initarg :k :reader k :type real))
  (:documentation "A non-commutative extension of complex
  numbers. They form a 4-dimensional normed division algebra over
  real numbers."))

;; (defmethod vector ((number quaternion))
;;   (make-array 3 :initial-contents (list (i number) (j number) (k number))))

(defun make-quaternion (scalar &key (i 0) (j 0) (k 0))
  (if (= j k 0)
      (complex scalar i)
      (make-instance 'quaternion :scalar scalar :i i :j j :k k)))

(defun read-quaternion-number (stream char arg)
  (if arg (warn "A numeric argument was ignored in #~d~c." arg char))
  (let ((list (read stream nil (values) t)))
    (if (and (consp list) (= (length list) 4))
        (make-instance 'quaternion
                       :scalar (first list)
                       :i (second list)
                       :j (third list)
                       :k (fourth list))
        (error 'reader-error
               :stream stream
               :format-control "illegal quaternion number format: #~c~a"
               :format-arguments (list char list)))))

(set-dispatch-macro-character #\# #\H #'read-quaternion-number)

(defmethod print-object ((object quaternion) stream)
  (with-slots (scalar i j k) object
    (format stream "#H(~a ~a ~a ~a)" scalar i j k)))

(defmethod binary-+ ((augend quaternion) (addend quaternion))
  (make-instance 'quaternion
                 :scalar (+ (scalar augend) (scalar addend))
                 :i (+ (i augend) (i addend))
                 :j (+ (j augend) (j addend))
                 :k (+ (k augend) (k addend))))

(defmethod binary-* ((multiplicand quaternion) (multiplier quaternion))
  (with-slots ((lscalar scalar) (li i) (lj j) (lk k)) multiplicand
    (with-slots ((rscalar scalar) (ri i) (rj j) (rk k)) multiplier
      (+ (make-quaternion (* lscalar rscalar) :i (* lscalar ri)
                          :j (* lscalar rj) :k (* lscalar rk))
         (make-quaternion (- (* li ri)) :i (* li rscalar)
                          :j (- (* li rk)) :k (* li rj))
         (make-quaternion (- (* lj rj)) :i (* lj rk)
                          :j (* lj rscalar) :k (- (* lj ri)))
         (make-quaternion (- (* lk rk)) :i (- (* lk rj))
                          :j (* lk ri) :k (* lk rscalar))))))

(defmethod binary-dot-product ((left quaternion) (right quaternion))
  (+ (* (scalar left) (scalar right))
     (* (i left) (i right))
     (* (j left) (j right))
     (* (k left) (k right))))

(define-commutative-method binary-dot-product ((left quaternion) (right octonion))
  (+ (* (scalar left) (scalar right))
     (* (i left) (i right))
     (* (j left) (j right))
     (* (k left) (k right))))

(define-commutative-method binary-dot-product ((left quaternion) (right complex))
  (+ (* (scalar left) (realpart right))
     (* (i left) (imagpart right))))

(defmethod binary-= ((left quaternion) (right quaternion))
  (and (= (scalar left) (scalar right))
       (= (i left) (i right))
       (= (j left) (j right))
       (= (k left) (k right))))

(define-commutative-method binary-= ((left quaternion) (right number))
  (declare (ignore left right))
  nil)

(defmethod conjugate ((number quaternion))
  (with-slots (scalar i j k) number
    (make-instance 'quaternion :scalar scalar :i (- i) :j (- j) :k (- k))))