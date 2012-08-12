(in-package #:math-extensions)

(defclass octonion ()
  ((scalar :initform 0 :initarg :scalar :reader scalar :reader realpart)
   (i :initform 0 :initarg :i :reader i :reader imagpart)
   (j :initform 0 :initarg :j :reader j)
   (k :initform 0 :initarg :k :reader k)
   (l :initform 0 :initarg :l :reader l)
   (il :initform 0 :initarg :il :reader il)
   (jl :initform 0 :initarg :jl :reader jl)
   (kl :initform 0 :initarg :kl :reader kl))
  (:documentation "A non-associative extension of the
  quaternions. Their 8-dimensional normed division algebra over
  the real numbers is the widest possible that can be obtained
  from Cayley-Dickson construction."))

(defun make-octonion (scalar &key (i 0) (j 0) (k 0) (l 0) (il 0) (jl 0) (kl 0))
  (if (= l il jl kl 0)
      (make-quaternion scalar :i i :j j :k k)
      (make-instance 'octonion
                     :scalar scalar :i i :j j :k k :l l :il il :jl jl :kl kl)))

(defun read-octonion-number (stream char arg)
  (if arg (warn "A numeric argument was ignored in #~d~c." arg char))
  (let ((list (read stream nil (values) t)))
    (if (and (consp list) (= (length list) 8))
        (make-instance 'octonion
                       :scalar (first list)
                       :i (second list)
                       :j (third list)
                       :k (fourth list)
                       :l (fifth list)
                       :il (sixth list)
                       :jl (seventh list)
                       :kl (eighth list))
        (error 'reader-error
               :stream stream
               :format-control "illegal octonion number format: #~c~a"
               :format-arguments (list char list)))))

(set-dispatch-macro-character #\# #\O #'read-octonion-number)

(defmethod print-object ((object octonion) stream)
  (with-slots (scalar i j k l il jl kl) object
    (format stream "#O(~a ~a ~a ~a ~a ~a ~a ~a)" scalar i j k l il jl kl)))

(defmethod binary-+ ((augend octonion) (addend octonion))
  (make-octonion (+ (scalar augend) (scalar addend))
                 :i (+ (i augend) (i addend))
                 :j (+ (j augend) (j addend))
                 :k (+ (k augend) (k addend))
                 :l (+ (l augend) (l addend))
                 :il (+ (il augend) (il addend))
                 :jl (+ (jl augend) (jl addend))
                 :kl (+ (kl augend) (kl addend))))

(define-commutative-method binary-+ ((augend octonion) (addend real))
  (with-slots (scalar i j k l il jl kl) augend
    (make-octonion (+ scalar addend) :i i :j j :k k :l l :il il :jl jl :kl kl)))

(define-commutative-method binary-+ ((augend octonion) (addend complex))
  (with-slots (scalar i j k l il jl kl) augend
    (make-octonion (+ scalar (realpart addend))
                   :i (+ i (imagpart addend))
                   :j j :k k :l l :il il :jl jl :kl kl)))

(defmethod norm ((value octonion))
  (sqrt (* (conjugate value) value)))

(defmethod inverse ((value octonion))
  (/ (conjugate value) (expt (norm value) 2)))

(defmethod binary-dot-product ((left octonion) (right octonion))
  (+ (* (scalar left) (scalar right))
     (* (i left) (i right))
     (* (j left) (j right))
     (* (k left) (k right))
     (* (l left) (l right))
     (* (il left) (il right))
     (* (jl left) (jl right))
     (* (kl left) (kl right))))

(define-commutative-method binary-dot-product ((left octonion) (right complex))
  (+ (* (scalar left) (realpart right))
     (* (i left) (imagpart right))))

(defmethod abs ((number octonion))
  (with-slots (scalar i j k l il jl kl) number
    (sqrt (+ (expt scalar 2) (expt i 2) (expt j 2) (expt k 2)
             (expt l 2) (expt il 2) (expt jl 2) (expt kl 2)))))

(defmethod numberp ((object octonion))
  t)

(defmethod conjugate ((number octonion))
  (with-slots (scalar i j k l il jl kl) number
    (make-instance 'octonion
                   :scalar scalar :i (- i) :j (- j) :k (- k)
                   :l (- l) :il (- il) :jl (- jl) :kl (- kl))))
