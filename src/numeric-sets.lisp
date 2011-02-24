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
     (make-instance 'enumerated-set
                    :initial-contents
                    (list min (read-from-string
                               (loop while (not (eql char #\}))
                                  do (write-char char str)
                                  (setf char (read-char stream nil nil t))
                                  finally (return (get-output-stream-string
                                                   str)))))))))

(defmethod +- ((value enumerated-set))
  (make-instance 'enumerated-set :initial-contents (mapcar #'+- value)))

(defmethod abs ((value enumerated-set))
  (make-instance 'numeric-set
                 :initial-contents (remove-duplicates (mapcar #'abs value)
                                                      :test #'=)))

(defmethod sqrt ((value enumerated-set))
  (reduce #'merge (mapcar #'sqrt value)))

(defmethod unary-- ((value enumerated-set))
  (make-instance 'numeric-set :initial-contents (mapcar #'- value)))

;;; ADDITION

(define-commutative-method binary-+ ((augend enumerated-set) addend)
  (make-instance 'enumerated-set
                 :initial-contents (mapcar (lambda (number) (+ number addend))
                                           augend)))

(defmethod binary-+ ((augend enumerated-set) (addend enumerated-set))
  (reduce #'merge (mapcar (lambda (number) (+ addend number)) augend)))

;;; EQUALITY

(define-commutative-method binary-= ((left enumerated-set) (right number))
  nil)

(defmethod binary-= ((left enumerated-set) (right enumerated-set))
  (= (size left) (size right) (size (intersection left right))))

;;; SET OPERATIONS

(defmethod size ((set enumerated-set))
  (length (slot-value set 'values)))

(defmethod intersection
    ((list-1 enumerated-set) (list-2 enumerated-set) &rest args)
  (make-instance 'enumerated-set
                 :initial-contents (apply #'intersection
                                          (slot-value list-1 'values)
                                          (slot-value list-2 'values)
                                          args)))

(defmethod set-difference
    ((list-1 enumerated-set) (list-2 enumerated-set) &rest args)
  (make-instance 'enumerated-set
                 :initial-contents (apply #'set-difference
                                          (slot-value list-1 'values)
                                          (slot-value list-2 'values)
                                          args)))

(defmethod set-exclusive-or
    ((list-1 enumerated-set) (list-2 enumerated-set) &rest args)
  (make-instance 'enumerated-set
                 :initial-contents (apply #'set-exclusive-or
                                          (slot-value list-1 'values)
                                          (slot-value list-2 'values)
                                          args)))

(defmethod union ((list-1 enumerated-set) (list-2 enumerated-set) &rest args)
  (make-instance 'enumerated-set
                 :initial-contents (apply #'union
                                          (slot-value list-1 'values)
                                          (slot-value list-2 'values)
                                          args)))
