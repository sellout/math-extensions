(in-package :math-extensions)

(defclass surreal ()
  ((left-set :initarg :left-set :reader left-set :type numeric-set)
   (right-set :initarg :right-set :reader right-set :type numeric-set)))

(defmethod binary-<= ((left surreal) (right surreal))
  (and (every (lambda (l) (not (<= right l))) (left-set left))
       (every (lambda (r) (not (<= r left))) (right-set right))))

(defmethod binary-= ((left surreal) (right surreal))
  (and (<= left right) (<= right left)))

(defmethod union ((left surreal) (right surreal) &key &allow-other-keys)
  )

(defmethod binary-+ ((augend surreal) (addend surreal))
  (make-instance 'surreal
    :left-set (union (+ (left-set augend) addend) (+ augend (left-set addend)))
    :right-set (union (+ (right-set augend) addend)
                      (+ augend (right-set addend)))))

(defmethod unary-- ((number surreal))
  (make-instance 'surreal
    :left-set (- (right-set number)) :right-set (- (left-set number))))

(defmethod binary-* ((multiplicand surreal) (multiplier surreal))
  (let ((XLy (* (left-set multiplicand) multiplier))
        (XRy (* (right-set multiplicand) multiplier))
        (xYL (* multiplicand (left-set multiplier)))
        (xYR (* multiplicand (right-set multiplier))))
    (make-instance 'surreal
      :left-set (union (- (+ XLy xYL)
                          (* (left-set multiplicand) (left-set multiplier)))
                       (- (+ XRy xYR)
                          (* (right-set multiplicand) (right-set multiplier))))
      :right-set (union (- (+ XLy xYR)
                           (* (left-set multiplicand) (right-set multiplier)))
                        (- (+ XRy xYL)
                           (* (right-set multiplicand)
                              (left-set multiplier)))))))