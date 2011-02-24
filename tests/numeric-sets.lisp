(in-package #:math-extensions-tests)

(def-suite numeric-sets-tests :in math-extensions-tests)

(in-suite numeric-sets-tests)

(test should-create-set-from-+-
  (is (= (make-instance 'enumerated-set :initial-contents '(-7 7))
         (+- 7))))

(test should-collapse-set-with-abs
  (is (= 7 (abs (+- 7)))))

(test should-add-mixed-values
  (is (= (make-set 4 6) (+ 3 (make-set 1 3)))))

(test should-add-sets
  (is (= (make-set -7 -3 3 7) (+ (+- 2) (+- 5)))))

(test should-work-for-quadratic-formula
  (labels ((quad (a b c)
             (/ (+ (- b) (sqrt (- (expt b 2) (* 4 a c)))) (* 2 a)))
           (compare (a b c)
             (let ((set (quad a b c)))
               (round (map 'enumerated-set
                           (lambda (x) (+ (* a (expt x 2)) (* b x) c))
                           (slot-value set 'values))))))
    (is (= 0 (compare 2 35 4)))))