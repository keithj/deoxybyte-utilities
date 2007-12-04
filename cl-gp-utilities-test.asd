
(in-package #:cl-gp-utilities-system)

(defsystem cl-gp-utilities-test
    :depends-on (:cl-gp-utilities :fiveam)
    :components ((:module :cl-gp-utilities-test
                          :serial t
                          :pathname "src/test/"
                          :components ((:file "package")
                                       (:file "cl-gp-utilities-test")))))
