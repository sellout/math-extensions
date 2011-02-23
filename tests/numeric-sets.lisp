(in-package #:math-extensions-tests)

(def-suite numeric-sets-tests :in math-extensions-tests)

(in-suite numeric-sets-tests)

(test should-create-set-from-+-
  (is (= (+- 7)
         (make-instance 'enumerated-set :initial-contents '(-7 7)))))
