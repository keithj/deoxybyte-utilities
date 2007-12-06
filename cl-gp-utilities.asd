
(in-package :cl-user)

(defpackage #:cl-gp-utilities-system
  (:use :common-lisp :asdf)
  (:export #:testsuite))


(in-package #:cl-gp-utilities-system)

(defsystem cl-gp-utilities
    :name "cl-gp-utilities"
    :author "Keith James"
    :version "0.1.0"
    :licence "GPL"
    :components
    ((:module :cl-gp-utilities
              :serial t
              :pathname "src/"
              :components ((:file "package")
                           (:file "cl-gp-utilities")))))


(in-package #:asdf)

(defmethod perform ((op test-op) (c (eql (find-system
                                          'cl-gp-utilities))))
  (operate 'load-op :cl-gp-utilities-test)
  (funcall (intern (string :run!) (string :fiveam))
           'cl-gp-utilities-system:testsuite))

(defmethod operation-done-p ((op test-op) (c (eql (find-system
                                                   'cl-gp-utilities))))
  nil)
