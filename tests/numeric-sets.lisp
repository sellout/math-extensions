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
