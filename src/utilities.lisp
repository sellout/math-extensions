(in-package #:math-extensions)

;;; FIXME: Currently doesn't allow documentation to be given to the
;;; inverted version
(defmacro define-commutative-method (name (left right) &body body)
  "Given a function (f a b) it generates (f b a) as well."
  (flet ((maybe-car (object) (if (consp object) (car object) object)))
    `(progn
       (defmethod ,name (,right ,left)
         (,name ,(maybe-car left) ,(maybe-car right)))
       (defmethod ,name (,left ,right)
         ,@body))))
