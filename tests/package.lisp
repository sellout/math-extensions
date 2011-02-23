(defpackage math-extensions-tests
  #.(loom.utilities:use-with-cl :numbers :conses)
  (:use #:math-extensions #:fiveam))

(in-package #:math-extensions-tests)

(def-suite math-extensions-tests)
