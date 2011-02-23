(defpackage math-extensions-system
  (:use #:cl #:asdf))

(in-package #:math-extensions-system)

(defsystem math-extensions
  :description "A collection of new functions as well as
  integration of additional number systems (E.G., quaternion and
  surreal numbers) with the existing system."
  :depends-on (loom)
  :pathname "src/"
  :components ((:file "package")
               (:file "utilities" :depends-on ("package"))
               (:file "numeric-sets" :depends-on ("utilities"))
               (:file "intervals" :depends-on ("utilities" "numeric-sets"))
               (:file "octonions" :depends-on ("utilities"))
               (:file "quaternions" :depends-on ("utilities" "octonions"))
               (:file "surreals" :depends-on ("utilities" "numeric-sets"))
               (:file "reals" :depends-on ("package" "numeric-sets")))
  :in-order-to ((test-op (load-op math-extensions-tests)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN!" :math-extensions-tests)
                             (intern "MATH-EXTENSIONS-TESTS"
                                     :math-extensions-tests))))

(defmethod operation-done-p
    ((op test-op) (c (eql (find-system :math-extensions))))
  (values nil))

(defsystem math-extensions-tests
  :depends-on (math-extensions fiveam)
  :pathname "tests/"
  :components ((:file "package")
               (:file "numeric-sets" :depends-on ("package"))))
